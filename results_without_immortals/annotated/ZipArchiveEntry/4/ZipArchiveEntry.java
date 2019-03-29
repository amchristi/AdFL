package org.apache.commons.compress.archivers.zip;

import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.EntryStreamOffsets;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.zip.ZipException;
import java.io.*;

/**
 * Extension that adds better handling of extra fields and provides
 * access to the internal and external file attributes.
 *
 * <p>The extra data is expected to follow the recommendation of
 * <a href="http://www.pkware.com/documents/casestudies/APPNOTE.TXT">APPNOTE.TXT</a>:</p>
 * <ul>
 *   <li>the extra byte array consists of a sequence of extra fields</li>
 *   <li>each extra fields starts by a two byte header id followed by
 *   a two byte sequence holding the length of the remainder of
 *   data.</li>
 * </ul>
 *
 * <p>Any extra data that cannot be parsed by the rules above will be
 * consumed as "unparseable" extra data and treated differently by the
 * methods of this class.  Versions prior to Apache Commons Compress
 * 1.1 would have thrown an exception if any attempt was made to read
 * or write extra data not conforming to the recommendation.</p>
 *
 * @NotThreadSafe
 */
public class ZipArchiveEntry extends java.util.zip.ZipEntry implements ArchiveEntry, EntryStreamOffsets {

    public static final int PLATFORM_UNIX = 3;

    public static final int PLATFORM_FAT = 0;

    public static final int CRC_UNKNOWN = -1;

    private static final int SHORT_MASK = 0xFFFF;

    private static final int SHORT_SHIFT = 16;

    private static final byte[] EMPTY = new byte[0];

    /**
     * Indicates how the name of this entry has been determined.
     * @since 1.16
     */
    public enum NameSource {

        /**
         * The name has been read from the archive using the encoding
         * of the archive specified when creating the {@link
         * ZipArchiveInputStream} or {@link ZipFile} (defaults to the
         * platform's default encoding).
         */
        NAME, /**
         * The name has been read from the archive and the archive
         * specified the EFS flag which indicates the name has been
         * encoded as UTF-8.
         */
        NAME_WITH_EFS_FLAG, /**
         * The name has been read from an {@link UnicodePathExtraField
         * Unicode Extra Field}.
         */
        UNICODE_EXTRA_FIELD
    }

    /**
     * Indicates how the comment of this entry has been determined.
     * @since 1.16
     */
    public enum CommentSource {

        /**
         * The comment has been read from the archive using the encoding
         * of the archive specified when creating the {@link
         * ZipArchiveInputStream} or {@link ZipFile} (defaults to the
         * platform's default encoding).
         */
        COMMENT, /**
         * The comment has been read from an {@link UnicodeCommentExtraField
         * Unicode Extra Field}.
         */
        UNICODE_EXTRA_FIELD
    }

    /**
     * The {@link java.util.zip.ZipEntry} base class only supports
     * the compression methods STORED and DEFLATED. We override the
     * field so that any compression methods can be used.
     * <p>
     * The default value -1 means that the method has not been specified.
     *
     * @see <a href="https://issues.apache.org/jira/browse/COMPRESS-93"
     *        >COMPRESS-93</a>
     */
    private int method = ZipMethod.UNKNOWN_CODE;

    /**
     * The {@link java.util.zip.ZipEntry#setSize} method in the base
     * class throws an IllegalArgumentException if the size is bigger
     * than 2GB for Java versions &lt; 7 and even in Java 7+ if the
     * implementation in java.util.zip doesn't support Zip64 itself
     * (it is an optional feature).
     *
     * <p>We need to keep our own size information for Zip64 support.</p>
     */
    private long size = SIZE_UNKNOWN;

    private int internalAttributes = 0;

    private int versionRequired;

    private int versionMadeBy;

    private int platform = PLATFORM_FAT;

    private int rawFlag;

    private long externalAttributes = 0;

    private int alignment = 0;

    private ZipExtraField[] extraFields;

    private UnparseableExtraFieldData unparseableExtra = null;

    private String name = null;

    private byte[] rawName = null;

    private GeneralPurposeBit gpb = new GeneralPurposeBit();

    private static final ZipExtraField[] noExtraFields = new ZipExtraField[0];

    private long localHeaderOffset = OFFSET_UNKNOWN;

    private long dataOffset = OFFSET_UNKNOWN;

    private boolean isStreamContiguous = false;

    private NameSource nameSource = NameSource.NAME;

    private CommentSource commentSource = CommentSource.COMMENT;

    /**
     * Creates a new zip entry with the specified name.
     *
     * <p>Assumes the entry represents a directory if and only if the
     * name ends with a forward slash "/".</p>
     *
     * @param name the name of the entry
     */
    public ZipArchiveEntry(final String name) {
        super(name);
        setName(name);
    }

    /**
     * Creates a new zip entry with fields taken from the specified zip entry.
     *
     * <p>Assumes the entry represents a directory if and only if the
     * name ends with a forward slash "/".</p>
     *
     * @param entry the entry to get fields from
     * @throws ZipException on error
     */
    public ZipArchiveEntry(final java.util.zip.ZipEntry entry) throws ZipException {
        super(entry);
        setName(entry.getName());
        final byte[] extra = entry.getExtra();
        if (extra != null) {
            setExtraFields(ExtraFieldUtils.parse(extra, true, ExtraFieldUtils.UnparseableExtraField.READ));
        } else {
            setExtra();
        }
        setMethod(entry.getMethod());
        this.size = entry.getSize();
    }

    /**
     * Creates a new zip entry with fields taken from the specified zip entry.
     *
     * <p>Assumes the entry represents a directory if and only if the
     * name ends with a forward slash "/".</p>
     *
     * @param entry the entry to get fields from
     * @throws ZipException on error
     */
    public ZipArchiveEntry(final ZipArchiveEntry entry) throws ZipException {
        this((java.util.zip.ZipEntry) entry);
        setInternalAttributes(entry.getInternalAttributes());
        setExternalAttributes(entry.getExternalAttributes());
        setExtraFields(getAllExtraFieldsNoCopy());
        setPlatform(entry.getPlatform());
        final GeneralPurposeBit other = entry.getGeneralPurposeBit();
        setGeneralPurposeBit(other == null ? null : (GeneralPurposeBit) other.clone());
    }

    /**
     */
    protected ZipArchiveEntry() {
        this("");
    }

    /**
     * Creates a new zip entry taking some information from the given
     * file and using the provided name.
     *
     * <p>The name will be adjusted to end with a forward slash "/" if
     * the file is a directory.  If the file is not a directory a
     * potential trailing forward slash will be stripped from the
     * entry name.</p>
     * @param inputFile file to create the entry from
     * @param entryName name of the entry
     */
    public ZipArchiveEntry(final File inputFile, final String entryName) {
        this(inputFile.isDirectory() && !entryName.endsWith("/") ? entryName + "/" : entryName);
        if (inputFile.isFile()) {
            setSize(inputFile.length());
        }
        setTime(inputFile.lastModified());
    }

    /**
     * Overwrite clone.
     * @return a cloned copy of this ZipArchiveEntry
     */
    @Override
    public Object clone() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "45003636-5608-45d9-8647-edad5215f3e7");
        final ZipArchiveEntry e = (ZipArchiveEntry) super.clone();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "92ddd33d-a80b-40d8-9bf8-331ad4715e48");
        e.setInternalAttributes(getInternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f124353a-f509-415e-83e0-5506bfb55ac7");
        e.setExternalAttributes(getExternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "76dc9d29-18f3-410c-a44f-52699d8ab986");
        e.setExtraFields(getAllExtraFieldsNoCopy());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "4a186d7e-63ec-43df-9db7-1ff7bfcb750c");
        return e;
    }

    /**
     * Returns the compression method of this entry, or -1 if the
     * compression method has not been specified.
     *
     * @return compression method
     *
     * @since 1.1
     */
    @Override
    public int getMethod() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "be3786bd-5e5c-4eb8-b88b-f4167a4964b2");
        return method;
    }

    /**
     * Sets the compression method of this entry.
     *
     * @param method compression method
     *
     * @since 1.1
     */
    @Override
    public void setMethod(final int method) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "429548a6-f25f-4483-a72d-14cffa3a5ce6");
        if (method < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2cd9dde1-739c-47f4-916c-c5cfa98d88af");
            throw new IllegalArgumentException("ZIP compression method can not be negative: " + method);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "514e609b-84fa-4bb5-aace-d60751777b83");
        this.method = method;
    }

    /**
     * Retrieves the internal file attributes.
     *
     * <p><b>Note</b>: {@link ZipArchiveInputStream} is unable to fill
     * this field, you must use {@link ZipFile} if you want to read
     * entries using this attribute.</p>
     *
     * @return the internal file attributes
     */
    public int getInternalAttributes() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f00e0b82-d252-4a24-a0a5-f29c0bbc6af5");
        return internalAttributes;
    }

    /**
     * Sets the internal file attributes.
     * @param value an <code>int</code> value
     */
    public void setInternalAttributes(final int value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c86dcb4a-b3ac-4bac-a95b-db25a58c3fe2");
        internalAttributes = value;
    }

    /**
     * Retrieves the external file attributes.
     *
     * <p><b>Note</b>: {@link ZipArchiveInputStream} is unable to fill
     * this field, you must use {@link ZipFile} if you want to read
     * entries using this attribute.</p>
     *
     * @return the external file attributes
     */
    public long getExternalAttributes() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "dd164f06-e8ce-4d65-b734-515d229d9c68");
        return externalAttributes;
    }

    /**
     * Sets the external file attributes.
     * @param value an <code>long</code> value
     */
    public void setExternalAttributes(final long value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "339f9839-ad90-45ed-9091-88b4b41b6f21");
        externalAttributes = value;
    }

    /**
     * Sets Unix permissions in a way that is understood by Info-Zip's
     * unzip command.
     * @param mode an <code>int</code> value
     */
    public void setUnixMode(final int mode) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "708c5abe-8ae6-4fad-9367-7ef7bb69abc5");
        setExternalAttributes((mode << SHORT_SHIFT) | ((mode & 0200) == 0 ? 1 : 0) | (isDirectory() ? 0x10 : 0));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "abaf095e-7804-4204-aca0-07f71a942b92");
        platform = PLATFORM_UNIX;
    }

    /**
     * Unix permission.
     * @return the unix permissions
     */
    public int getUnixMode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "647d4ca8-3044-4f64-b160-9a1f00815640");
        return platform != PLATFORM_UNIX ? 0 : (int) ((getExternalAttributes() >> SHORT_SHIFT) & SHORT_MASK);
    }

    /**
     * Returns true if this entry represents a unix symlink,
     * in which case the entry's content contains the target path
     * for the symlink.
     *
     * @since 1.5
     * @return true if the entry represents a unix symlink, false otherwise.
     */
    public boolean isUnixSymlink() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "df5c7b0e-4a3e-44e0-ac52-1645974507f7");
        return (getUnixMode() & UnixStat.FILE_TYPE_FLAG) == UnixStat.LINK_FLAG;
    }

    /**
     * Platform specification to put into the &quot;version made
     * by&quot; part of the central file header.
     *
     * @return PLATFORM_FAT unless {@link #setUnixMode setUnixMode}
     * has been called, in which case PLATFORM_UNIX will be returned.
     */
    public int getPlatform() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "5df65447-33bc-456f-8e4d-495575934a89");
        return platform;
    }

    /**
     * Set the platform (UNIX or FAT).
     * @param platform an <code>int</code> value - 0 is FAT, 3 is UNIX
     */
    protected void setPlatform(final int platform) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2472987e-3fbe-4e3f-8f56-1e419f4828ab");
        this.platform = platform;
    }

    /**
     * Gets currently configured alignment.
     *
     * @return
     *      alignment for this entry.
     * @since 1.14
     */
    protected int getAlignment() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d220f5ba-8c5a-4da3-a2cb-224313021cbd");
        return this.alignment;
    }

    /**
     * Sets alignment for this entry.
     *
     * @param alignment
     *      requested alignment, 0 for default.
     * @since 1.14
     */
    public void setAlignment(int alignment) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "03a2aeb4-1259-4363-ba17-2203d1b589a8");
        if ((alignment & (alignment - 1)) != 0 || alignment > 0xffff) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "282b5538-9e5f-45c1-9645-6a928df031c8");
            throw new IllegalArgumentException("Invalid value for alignment, must be power of two and no bigger than " + 0xffff + " but is " + alignment);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f18be990-2ed3-4533-abc0-1f3c2b61fde6");
        this.alignment = alignment;
    }

    /**
     * Replaces all currently attached extra fields with the new array.
     * @param fields an array of extra fields
     */
    public void setExtraFields(final ZipExtraField[] fields) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "df175a40-69b3-4223-9198-cb1176c0fef2");
        final List<ZipExtraField> newFields = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "304673d3-1635-4ad5-a2b7-cd806df26c0a");
        for (final ZipExtraField field : fields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f6b7b4fe-eff1-425a-b395-de4d899715fb");
            if (field instanceof UnparseableExtraFieldData) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "9770b0c4-af44-42e5-b642-b04b07ed1efe");
                unparseableExtra = (UnparseableExtraFieldData) field;
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "534009bf-6f00-429e-9c9c-3ae2bb9a1174");
                newFields.add(field);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f5624837-b3f8-4eb7-9b15-f7b57905efd4");
        extraFields = newFields.toArray(new ZipExtraField[newFields.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "19cc4fd1-d29b-46f9-b49d-4b7b64005a82");
        setExtra();
    }

    /**
     * Retrieves all extra fields that have been parsed successfully.
     *
     * <p><b>Note</b>: The set of extra fields may be incomplete when
     * {@link ZipArchiveInputStream} has been used as some extra
     * fields use the central directory to store additional
     * information.</p>
     *
     * @return an array of the extra fields
     */
    public ZipExtraField[] getExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ff1a07b4-6da3-4997-9df1-03bf0ef96448");
        return getParseableExtraFields();
    }

    /**
     * Retrieves extra fields.
     * @param includeUnparseable whether to also return unparseable
     * extra fields as {@link UnparseableExtraFieldData} if such data
     * exists.
     * @return an array of the extra fields
     *
     * @since 1.1
     */
    public ZipExtraField[] getExtraFields(final boolean includeUnparseable) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "24d1a15f-6d82-45b5-ad0a-52f8aca2fca0");
        return includeUnparseable ? getAllExtraFields() : getParseableExtraFields();
    }

    private ZipExtraField[] getParseableExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "81eadd69-c51a-4bfc-acae-4ccb903e41c7");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2a0090a9-df87-4328-a1c5-83e966185bd7");
            return noExtraFields;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2337d81d-d735-4a4c-98bc-39ca879859d7");
        return extraFields;
    }

    private ZipExtraField[] getParseableExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "3b7e386d-a9b2-41d1-b8ba-aabde4391225");
        final ZipExtraField[] parseableExtraFields = getParseableExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "65d2165a-e35b-49ed-bac2-b42c6789665a");
        return (parseableExtraFields == extraFields) ? copyOf(parseableExtraFields) : parseableExtraFields;
    }

    /**
     * Get all extra fields, including unparseable ones.
     * @return An array of all extra fields. Not necessarily a copy of internal data structures, hence private method
     */
    private ZipExtraField[] getAllExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "10983cb3-c7c1-4b59-9038-aa120076c934");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f596dcbc-10b5-455d-b686-07118b064560");
            return getUnparseableOnly();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2721c51e-9726-4cd1-975f-aa4089b6a6e8");
        return unparseableExtra != null ? getMergedFields() : extraFields;
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e7e51556-4520-4285-88ef-1175ca6ff12f");
        return copyOf(src, src.length);
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src, final int length) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "b86b21cd-dafc-42de-be4e-1cbf6286853f");
        final ZipExtraField[] cpy = new ZipExtraField[length];
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ac2b963f-3383-43d6-9b1e-34e927a24b00");
        System.arraycopy(src, 0, cpy, 0, Math.min(src.length, length));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "9e89ec52-d6d4-483f-ba17-2f85d29a271f");
        return cpy;
    }

    private ZipExtraField[] getMergedFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "82642af6-7cb9-4040-b6fa-3b436ee5fb9d");
        final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "3ed60a2f-15b0-438f-9837-3e880c3f4dbb");
        zipExtraFields[extraFields.length] = unparseableExtra;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "47833523-82af-471e-9abe-9c0bb3ba34c1");
        return zipExtraFields;
    }

    private ZipExtraField[] getUnparseableOnly() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "b02435b1-c54b-4d6e-a732-9cf9de10d8b7");
        return unparseableExtra == null ? noExtraFields : new ZipExtraField[] { unparseableExtra };
    }

    private ZipExtraField[] getAllExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "19e1c7cd-912c-4fa0-846b-b4c23db8f209");
        final ZipExtraField[] allExtraFieldsNoCopy = getAllExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "9b2f9807-b551-4e61-a4d1-6324fc728a2b");
        return (allExtraFieldsNoCopy == extraFields) ? copyOf(allExtraFieldsNoCopy) : allExtraFieldsNoCopy;
    }

    /**
     * Adds an extra field - replacing an already present extra field
     * of the same type.
     *
     * <p>If no extra field of the same type exists, the field will be
     * added as last field.</p>
     * @param ze an extra field
     */
    public void addExtraField(final ZipExtraField ze) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "aa6386c9-07f5-43c6-8d89-869d7dc61e83");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e54a2d03-2aa5-4bf3-873e-c05fa17693d9");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "eb7c4e83-4e11-49aa-85b3-2a3299a47e0e");
            if (extraFields == null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "521cc75a-5226-4772-b676-9b88e9b9088c");
                extraFields = new ZipExtraField[] { ze };
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "fbb5a262-cb7a-4a20-878f-80971e282a4f");
                if (getExtraField(ze.getHeaderId()) != null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c463b4ee-3309-4eba-beae-4ef03ef2d4da");
                    removeExtraField(ze.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "26db8147-a417-4d9e-9f5c-004bf88be868");
                final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e2f2105d-d9f5-4bb4-8ed3-79e9bb1030aa");
                zipExtraFields[zipExtraFields.length - 1] = ze;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c1724c2f-6c22-439a-b0f3-4ee15e7a5e3c");
                extraFields = zipExtraFields;
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8953d5f6-c559-4223-a6cd-f1cd7c2845bf");
        setExtra();
    }

    /**
     * Adds an extra field - replacing an already present extra field
     * of the same type.
     *
     * <p>The new extra field will be the first one.</p>
     * @param ze an extra field
     */
    public void addAsFirstExtraField(final ZipExtraField ze) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f44e1875-c946-45ff-816d-c2b8e06d6a47");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "bd4a51da-8960-4fe7-a4e0-67f2d0044e2c");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ce5a1e6f-692d-439e-9b30-1cc56b858938");
            if (getExtraField(ze.getHeaderId()) != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d9e3f43f-bf85-4ff8-9a00-79cd38b1fd41");
                removeExtraField(ze.getHeaderId());
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c86a15fb-9fd8-40ff-83b9-e9a96fcfc549");
            final ZipExtraField[] copy = extraFields;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "950b1966-b5ca-4a74-8ee8-4bec5b87c17e");
            final int newLen = extraFields != null ? extraFields.length + 1 : 1;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d5638f3b-713c-4194-a0d7-8f504fa63300");
            extraFields = new ZipExtraField[newLen];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "6ed9b0c3-bfc2-4132-b7cb-4bed520a44b4");
            extraFields[0] = ze;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d4721e61-5412-4240-8575-837d341bbb90");
            if (copy != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "59050951-e480-4ff7-ad87-89ee8a9c9ed7");
                System.arraycopy(copy, 0, extraFields, 1, extraFields.length - 1);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "b3b3b8c9-8985-4a0d-a7bd-a6a56f73cd6f");
        setExtra();
    }

    /**
     * Remove an extra field.
     * @param type the type of extra field to remove
     */
    public void removeExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e08d075b-bf41-4b09-9121-990dc1f05f9b");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "5e5eb530-3eee-4158-b71d-678d99340d72");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "abb9d58f-42c2-4918-812e-eef3d0a03e74");
        final List<ZipExtraField> newResult = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "96f0e4e1-14f3-4e70-8110-6aad5856a4bb");
        for (final ZipExtraField extraField : extraFields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "cd46631c-6ca6-41df-a45a-6e9581471c64");
            if (!type.equals(extraField.getHeaderId())) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "3bbf5612-c292-49ab-9eb9-bf551858855f");
                newResult.add(extraField);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c94a9f22-c03c-4b42-a103-467c226ffd17");
        if (extraFields.length == newResult.size()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "408b4948-d5d3-44e1-b38c-dc5fa2c77c3a");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "5709f29c-6108-48ba-b305-c8948af8cdc3");
        extraFields = newResult.toArray(new ZipExtraField[newResult.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "a22dce81-2fc8-4cfe-a179-32cb8ef64a09");
        setExtra();
    }

    /**
     * Removes unparseable extra field data.
     *
     * @since 1.1
     */
    public void removeUnparseableExtraFieldData() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2724c15d-be93-4ba6-8518-0aa03345b5b6");
        if (unparseableExtra == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "62520647-b141-4031-8c7e-c4114f6a09a6");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "96ac94b6-9aae-4d62-a1d6-bfc402f72b67");
        unparseableExtra = null;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "1d12cb23-5872-4443-85dc-596e9f09daa0");
        setExtra();
    }

    /**
     * Looks up an extra field by its header id.
     *
     * @param type the header id
     * @return null if no such field exists.
     */
    public ZipExtraField getExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2fb60afc-1cd6-45c0-b076-b786f0d4d0ed");
        if (extraFields != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "6d62e3e9-a7d9-4fdc-b6da-0280da54b38b");
            for (final ZipExtraField extraField : extraFields) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0e1d1fa1-0dbb-4ea4-bb12-d2fa624c91e2");
                if (type.equals(extraField.getHeaderId())) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "83ebf75a-edb0-4b62-aade-d645c8317f40");
                    return extraField;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "b1144342-ca75-4f96-9412-468c95cb7f52");
        return null;
    }

    /**
     * Looks up extra field data that couldn't be parsed correctly.
     *</>
     * @return null if no such field exists.
     *
     * @since 1.1
     */
    public UnparseableExtraFieldData getUnparseableExtraFieldData() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0b891b84-e77e-46d6-8d9f-7631623136c9");
        return unparseableExtra;
    }

    /**
     * Parses the given bytes as extra field data and consumes any
     * unparseable data as an {@link UnparseableExtraFieldData}
     * instance.
     * @param extra an array of bytes to be parsed into extra fields
     * @throws RuntimeException if the bytes cannot be parsed
     * @throws RuntimeException on error
     */
    @Override
    public void setExtra(final byte[] extra) throws RuntimeException {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8891489a-528e-4365-9052-5fd7912f8fe6");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8beaebae-08c3-4974-a026-f16ca3d0c6fa");
            final ZipExtraField[] local = ExtraFieldUtils.parse(extra, true, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c65ce830-6b64-4b78-892d-58a35944c45b");
            mergeExtraFields(local, true);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0fc35f62-ce38-4c9a-9e12-ea45c641a044");
            throw new RuntimeException("Error parsing extra fields for entry: " + getName() + " - " + e.getMessage(), e);
        }
    }

    /**
     * Unfortunately {@link java.util.zip.ZipOutputStream
     * java.util.zip.ZipOutputStream} seems to access the extra data
     * directly, so overriding getExtra doesn't help - we need to
     * modify super's data directly.
     */
    protected void setExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "717e0a95-4660-4518-bf4a-d5f12a1c5ca1");
        super.setExtra(ExtraFieldUtils.mergeLocalFileDataData(getAllExtraFieldsNoCopy()));
    }

    /**
     * Sets the central directory part of extra fields.
     * @param b an array of bytes to be parsed into extra fields
     */
    public void setCentralDirectoryExtra(final byte[] b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0fc66564-49aa-4fdc-9420-40a7d21a3d3c");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d425294e-7b74-4498-b4bf-fddc32b1ef54");
            final ZipExtraField[] central = ExtraFieldUtils.parse(b, false, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "73ddf25e-25f3-4411-8419-f130db8bc7bb");
            mergeExtraFields(central, false);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "6c83eeb5-d46b-4cdf-9f2b-d1f3f63f93ad");
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Retrieves the extra data for the local file data.
     * @return the extra data for local file
     */
    public byte[] getLocalFileDataExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "15aeb414-d148-4f23-8022-204c1899c62e");
        final byte[] extra = getExtra();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "dff8e609-6ff4-4f2e-8ed8-74ee6de00e96");
        return extra != null ? extra : EMPTY;
    }

    /**
     * Retrieves the extra data for the central directory.
     * @return the central directory extra data
     */
    public byte[] getCentralDirectoryExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2cf3c459-33dc-42b0-a2f8-0144afeb8d3a");
        return ExtraFieldUtils.mergeCentralDirectoryData(getAllExtraFieldsNoCopy());
    }

    /**
     * Get the name of the entry.
     * @return the entry name
     */
    @Override
    public String getName() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "79b51f80-ed4d-488b-8d33-e39989b3c1fd");
        return name == null ? super.getName() : name;
    }

    /**
     * Is this entry a directory?
     * @return true if the entry is a directory
     */
    @Override
    public boolean isDirectory() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "18b49328-1998-47bf-bac3-0d1130ded4f9");
        return getName().endsWith("/");
    }

    /**
     * Set the name of the entry.
     * @param name the name to use
     */
    protected void setName(String name) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "af52a141-6c0b-471b-9f71-96f74b9e2520");
        if (name != null && getPlatform() == PLATFORM_FAT && !name.contains("/")) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "08100e56-7d41-4e2d-a847-5795486c38d5");
            name = name.replace('\\', '/');
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "87e102cd-002c-41c1-a944-012807d866cd");
        this.name = name;
    }

    /**
     * Gets the uncompressed size of the entry data.
     *
     * <p><b>Note</b>: {@link ZipArchiveInputStream} may create
     * entries that return {@link #SIZE_UNKNOWN SIZE_UNKNOWN} as long
     * as the entry hasn't been read completely.</p>
     *
     * @return the entry size
     */
    @Override
    public long getSize() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "6d9b5a41-e91f-43c9-b773-648d6ecd3058");
        return size;
    }

    /**
     * Sets the uncompressed size of the entry data.
     * @param size the uncompressed size in bytes
     * @throws IllegalArgumentException if the specified size is less
     *            than 0
     */
    @Override
    public void setSize(final long size) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8c2e3f86-81ec-494b-a826-6570a66b2b83");
        if (size < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "77475110-6ca3-4f4a-9488-8e84b4dc10de");
            throw new IllegalArgumentException("invalid entry size");
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "da35b5c8-b597-492c-850d-4843a10cc50f");
        this.size = size;
    }

    /**
     * Sets the name using the raw bytes and the string created from
     * it by guessing or using the configured encoding.
     * @param name the name to use created from the raw bytes using
     * the guessed or configured encoding
     * @param rawName the bytes originally read as name from the
     * archive
     * @since 1.2
     */
    protected void setName(final String name, final byte[] rawName) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e5d09ddf-cd13-43c4-99c8-d11e3be02617");
        setName(name);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d836b72d-a606-4022-adc2-0b9963903b77");
        this.rawName = rawName;
    }

    /**
     * Returns the raw bytes that made up the name before it has been
     * converted using the configured or guessed encoding.
     *
     * <p>This method will return null if this instance has not been
     * read from an archive.</p>
     *
     * @return the raw name bytes
     * @since 1.2
     */
    public byte[] getRawName() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "40e8ca02-ad5c-42a1-885a-932910b11540");
        if (rawName != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "eb4de751-c6cf-4272-817f-6edf7c5cc8d0");
            final byte[] b = new byte[rawName.length];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "487d9902-5c7c-4e5f-a559-f754b222d08c");
            System.arraycopy(rawName, 0, b, 0, rawName.length);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "b35be608-90f8-4b47-b8ce-679d5a0da7d7");
            return b;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2b6d440e-670b-41e0-83d3-428a2b1b9164");
        return null;
    }

    protected long getLocalHeaderOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "4f3f5f0b-2cf1-41ad-ae09-0a4b0c52d5ec");
        return this.localHeaderOffset;
    }

    protected void setLocalHeaderOffset(long localHeaderOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "3aebc844-d49f-430e-a8a8-be5dcc1456f1");
        this.localHeaderOffset = localHeaderOffset;
    }

    @Override
    public long getDataOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c86cf9e5-9501-4e42-943f-acc91a798439");
        return dataOffset;
    }

    /**
     * Sets the data offset.
     *
     * @param dataOffset
     *      new value of data offset.
     */
    protected void setDataOffset(long dataOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "dff6a2bf-a881-4599-8940-ecba41dcb9dc");
        this.dataOffset = dataOffset;
    }

    @Override
    public boolean isStreamContiguous() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d5fe915f-86e9-4f0a-b0d8-8c1cc9c20640");
        return isStreamContiguous;
    }

    protected void setStreamContiguous(boolean isStreamContiguous) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ea468ab7-afb4-43ad-9589-b3a3fc810fb0");
        this.isStreamContiguous = isStreamContiguous;
    }

    /**
     * Get the hashCode of the entry.
     * This uses the name as the hashcode.
     * @return a hashcode.
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "f344f98e-bacc-4763-a0cb-86c994489f43");
        return getName().hashCode();
    }

    /**
     * The "general purpose bit" field.
     * @return the general purpose bit
     * @since 1.1
     */
    public GeneralPurposeBit getGeneralPurposeBit() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8d8646fd-af21-47bc-aab3-ae06a12ceab0");
        return gpb;
    }

    /**
     * The "general purpose bit" field.
     * @param b the general purpose bit
     * @since 1.1
     */
    public void setGeneralPurposeBit(final GeneralPurposeBit b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "88a5e59f-ee97-4036-9144-a9cc8d21f396");
        gpb = b;
    }

    /**
     * If there are no extra fields, use the given fields as new extra
     * data - otherwise merge the fields assuming the existing fields
     * and the new fields stem from different locations inside the
     * archive.
     * @param f the extra fields to merge
     * @param local whether the new fields originate from local data
     */
    private void mergeExtraFields(final ZipExtraField[] f, final boolean local) throws ZipException {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "9fdad4a5-129c-4dfd-989f-97ce8075f240");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c41c06bc-de1a-4366-a469-9aacdfbe7ae0");
            setExtraFields(f);
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "a6c970a3-3a6f-4720-b910-82608bde3d54");
            for (final ZipExtraField element : f) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "fac1b9cb-91d8-4f9c-a45d-6ab254d5434c");
                ZipExtraField existing;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "cc512abc-058c-4d0c-a62e-dc9df7c03955");
                if (element instanceof UnparseableExtraFieldData) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ed63e8ea-3bb0-437f-a6d2-a362f2579c41");
                    existing = unparseableExtra;
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2e19a2e3-c961-4933-a8ae-87b8649b090e");
                    existing = getExtraField(element.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "7ab0c5da-06de-4f0b-ba2b-a6f5076ac75b");
                if (existing == null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "43786ce3-688d-41a1-a078-bd7719b55e08");
                    addExtraField(element);
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "87899c6d-619b-461d-91f9-2b342f5431f8");
                    if (local) {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "811e0b4b-0d7f-45b6-93d1-dcc3430beee8");
                        final byte[] b = element.getLocalFileDataData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0bd15163-221c-4ac0-b61f-16100eda2d93");
                        existing.parseFromLocalFileData(b, 0, b.length);
                    } else {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d56a5b7a-b520-4f04-b4ef-72385e4000cc");
                        final byte[] b = element.getCentralDirectoryData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "009ae90e-19b9-4103-afbf-4d3ddf2e20e1");
                        existing.parseFromCentralDirectoryData(b, 0, b.length);
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d44b3f57-9546-4647-b358-3d40d223a855");
            setExtra();
        }
    }

    /**
     * Wraps {@link java.util.zip.ZipEntry#getTime} with a {@link Date} as the
     * entry's last modified date.
     *
     * <p>Changes to the implementation of {@link java.util.zip.ZipEntry#getTime}
     * leak through and the returned value may depend on your local
     * time zone as well as your version of Java.</p>
     */
    @Override
    public Date getLastModifiedDate() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0597a18d-e4dc-46a6-91ff-4cdf0a9d4b6c");
        return new Date(getTime());
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "a01b6968-eb0e-443a-9393-2a82177fc99e");
        if (this == obj) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "3574d1e5-79d3-4955-bcb6-cfd40f98cf9c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "cfa15fbc-cea2-4056-a269-d5b917553d1d");
        if (obj == null || getClass() != obj.getClass()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "1de96d01-ab86-402a-9312-8488f8a8b0c2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d7cbe604-dc43-45cf-a7b3-a86dd21be53d");
        final ZipArchiveEntry other = (ZipArchiveEntry) obj;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "cd6ee587-e00a-4a6d-af6f-8520e37b850f");
        final String myName = getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "386155bc-498c-48aa-981e-724ec42972be");
        final String otherName = other.getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "632e8441-e140-4690-bfbf-deee544d994e");
        if (myName == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ce70618b-735f-4986-9d1c-29c85ddda956");
            if (otherName != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "0219ce88-921a-42d9-8a47-ae876a5ba77e");
                return false;
            }
        } else if (!myName.equals(otherName)) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "d7b25a5e-846c-4ffa-8a47-492e981318df");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "dd512de1-f747-49cf-848f-e8462cca219b");
        String myComment = getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8d9d97ba-4290-4f8d-95a7-61895fd774d2");
        String otherComment = other.getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e14e15e6-5e3e-464d-a1d8-fdec7e78f819");
        if (myComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "7a2c8703-c49c-42f5-81c9-afb9de25a631");
            myComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c2ffd196-6322-42c2-8a90-3b060793ac95");
        if (otherComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "04305b24-b5ea-4ddf-b9b5-e1d59f527157");
            otherComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "2e2fcdca-228d-4991-98ed-331fa8fd877f");
        return getTime() == other.getTime() && myComment.equals(otherComment) && getInternalAttributes() == other.getInternalAttributes() && getPlatform() == other.getPlatform() && getExternalAttributes() == other.getExternalAttributes() && getMethod() == other.getMethod() && getSize() == other.getSize() && getCrc() == other.getCrc() && getCompressedSize() == other.getCompressedSize() && Arrays.equals(getCentralDirectoryExtra(), other.getCentralDirectoryExtra()) && Arrays.equals(getLocalFileDataExtra(), other.getLocalFileDataExtra()) && localHeaderOffset == other.localHeaderOffset && dataOffset == other.dataOffset && gpb.equals(other.gpb);
    }

    /**
     * Sets the "version made by" field.
     * @param versionMadeBy "version made by" field
     * @since 1.11
     */
    public void setVersionMadeBy(final int versionMadeBy) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "6789c101-e0ed-4013-8232-53afdf11db17");
        this.versionMadeBy = versionMadeBy;
    }

    /**
     * Sets the "version required to expand" field.
     * @param versionRequired "version required to expand" field
     * @since 1.11
     */
    public void setVersionRequired(final int versionRequired) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "a5cd536e-7cf1-4752-b7c4-9fc96c9ffd96");
        this.versionRequired = versionRequired;
    }

    /**
     * The "version required to expand" field.
     * @return "version required to expand" field
     * @since 1.11
     */
    public int getVersionRequired() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "c227bbda-6925-4a87-9af3-7d94f8172228");
        return versionRequired;
    }

    /**
     * The "version made by" field.
     * @return "version made by" field
     * @since 1.11
     */
    public int getVersionMadeBy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "42331bfd-7942-4080-9d0d-111b55754fba");
        return versionMadeBy;
    }

    /**
     * The content of the flags field.
     * @return content of the flags field
     * @since 1.11
     */
    public int getRawFlag() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "4a2e21a7-7bd4-4398-8fc9-99cb22965f38");
        return rawFlag;
    }

    /**
     * Sets the content of the flags field.
     * @param rawFlag content of the flags field
     * @since 1.11
     */
    public void setRawFlag(final int rawFlag) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "ed01b698-16ce-4a44-baf8-bc8b6f94843c");
        this.rawFlag = rawFlag;
    }

    /**
     * The source of the name field value.
     * @return source of the name field value
     * @since 1.16
     */
    public NameSource getNameSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "4d80f316-97d6-4694-8175-2c6b45e76d6e");
        return nameSource;
    }

    /**
     * Sets the source of the name field value.
     * @param nameSource source of the name field value
     * @since 1.16
     */
    public void setNameSource(NameSource nameSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "666b7cf8-be00-4494-a2bf-24702043e140");
        this.nameSource = nameSource;
    }

    /**
     * The source of the comment field value.
     * @return source of the comment field value
     * @since 1.16
     */
    public CommentSource getCommentSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "8aadc7bb-568a-4513-9635-aeb0a6a2f4c2");
        return commentSource;
    }

    /**
     * Sets the source of the comment field value.
     * @param commentSource source of the comment field value
     * @since 1.16
     */
    public void setCommentSource(CommentSource commentSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_4_10.coverage", "e41ea5b4-f600-4364-8a79-a913682f304d");
        this.commentSource = commentSource;
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
