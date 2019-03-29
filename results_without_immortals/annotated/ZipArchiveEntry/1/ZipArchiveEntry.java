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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "48a9a51a-1127-44e0-8a17-42d742e0239a");
        final ZipArchiveEntry e = (ZipArchiveEntry) super.clone();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e96f9ad1-2305-466a-a2eb-cd67f66860ed");
        e.setInternalAttributes(getInternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6d32bc20-9b7f-45e0-9a46-f913869b534e");
        e.setExternalAttributes(getExternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1e4fca86-a7a4-450e-b030-34a15b033b72");
        e.setExtraFields(getAllExtraFieldsNoCopy());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "8fd1f0d5-f259-48ea-85b0-ce97db4a7b4c");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "b008fca1-7e7b-4d36-aa44-2d107af217ff");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "780bc15f-ac06-4cd8-a124-85ae000e2445");
        if (method < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d8e5b0c3-e968-4f57-9bff-506553bab1a6");
            throw new IllegalArgumentException("ZIP compression method can not be negative: " + method);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6d5c4620-d322-4521-8361-970f7863601d");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "5a661ab2-4d36-4365-bd6c-6cc5c2f1d476");
        return internalAttributes;
    }

    /**
     * Sets the internal file attributes.
     * @param value an <code>int</code> value
     */
    public void setInternalAttributes(final int value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e4212671-570e-49c2-8ac6-bdb2f1494342");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "15fabcff-592a-4e77-a055-18b480d219ac");
        return externalAttributes;
    }

    /**
     * Sets the external file attributes.
     * @param value an <code>long</code> value
     */
    public void setExternalAttributes(final long value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "ec4047cb-f251-487f-8d27-5a21541462c2");
        externalAttributes = value;
    }

    /**
     * Sets Unix permissions in a way that is understood by Info-Zip's
     * unzip command.
     * @param mode an <code>int</code> value
     */
    public void setUnixMode(final int mode) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6ed55992-e2cb-4983-973c-6ddc44cfa364");
        setExternalAttributes((mode << SHORT_SHIFT) | ((mode & 0200) == 0 ? 1 : 0) | (isDirectory() ? 0x10 : 0));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f2cb5407-c197-4264-85b2-46758005e527");
        platform = PLATFORM_UNIX;
    }

    /**
     * Unix permission.
     * @return the unix permissions
     */
    public int getUnixMode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "a317641d-c765-44e6-9350-85c4b5800816");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d67dbef3-d2c9-4e4a-b689-3267ab9f6796");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "9f48baf0-bf3e-4d68-9457-3da17f66cb1b");
        return platform;
    }

    /**
     * Set the platform (UNIX or FAT).
     * @param platform an <code>int</code> value - 0 is FAT, 3 is UNIX
     */
    protected void setPlatform(final int platform) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "38377cdf-4c97-4ee3-8fec-691961aa3e87");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "a68459ea-3d44-4cc9-a631-bc4f1326e8f4");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3834bb89-4350-4783-898e-363df44087c1");
        if ((alignment & (alignment - 1)) != 0 || alignment > 0xffff) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "4223fc12-b8b1-4017-98e4-3a3d25abea32");
            throw new IllegalArgumentException("Invalid value for alignment, must be power of two and no bigger than " + 0xffff + " but is " + alignment);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "ff15a037-bfd8-483f-a397-198a3f761411");
        this.alignment = alignment;
    }

    /**
     * Replaces all currently attached extra fields with the new array.
     * @param fields an array of extra fields
     */
    public void setExtraFields(final ZipExtraField[] fields) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "45654183-b784-4c0e-a705-7d4d48502601");
        final List<ZipExtraField> newFields = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "5a947438-91ab-4b53-b96a-fe96fc1e6675");
        for (final ZipExtraField field : fields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "b304fa6f-4544-442e-8019-fac6739092b4");
            if (field instanceof UnparseableExtraFieldData) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "63ae3089-ed7a-4a27-a2c8-b157b2910645");
                unparseableExtra = (UnparseableExtraFieldData) field;
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "83c01eb2-9714-430a-8e89-5704bcb3543e");
                newFields.add(field);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d350bbb9-0301-4cfa-9b53-ab2cf31b78e6");
        extraFields = newFields.toArray(new ZipExtraField[newFields.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "9c8eecb4-aa2f-4044-b215-4d03a76060f7");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "ce737e6a-45a0-41a4-beda-dce1e930570f");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d78cdbef-724f-4164-a6f6-9125fb12f2d9");
        return includeUnparseable ? getAllExtraFields() : getParseableExtraFields();
    }

    private ZipExtraField[] getParseableExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "4dcc3ec0-93e5-4900-a709-dd366fb8e939");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "b8a833bb-37cf-4bf7-a06d-96d09dcc06bd");
            return noExtraFields;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "75bfcff9-699f-4560-98fe-d7c973324cdd");
        return extraFields;
    }

    private ZipExtraField[] getParseableExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "c0920495-bfa2-4fab-9d3a-622d9e94e753");
        final ZipExtraField[] parseableExtraFields = getParseableExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7727231f-d945-4017-842f-3569095f3b32");
        return (parseableExtraFields == extraFields) ? copyOf(parseableExtraFields) : parseableExtraFields;
    }

    /**
     * Get all extra fields, including unparseable ones.
     * @return An array of all extra fields. Not necessarily a copy of internal data structures, hence private method
     */
    private ZipExtraField[] getAllExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "a9bc437f-f747-4933-9996-28d77083f154");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "9e015ef8-4b87-45c5-b161-527e1e7498a8");
            return getUnparseableOnly();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "db09d056-b2b8-4ddf-9750-e89a7b098365");
        return unparseableExtra != null ? getMergedFields() : extraFields;
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "83cb8b17-3ec3-4c79-9c9c-59302b1216f5");
        return copyOf(src, src.length);
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src, final int length) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "74d9ef83-4dd0-49eb-b07a-3adb8c8a26ac");
        final ZipExtraField[] cpy = new ZipExtraField[length];
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1afe413f-2e86-48dc-8b7a-3ae204467b8d");
        System.arraycopy(src, 0, cpy, 0, Math.min(src.length, length));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7ae97a7b-708e-4250-8544-246033c837cf");
        return cpy;
    }

    private ZipExtraField[] getMergedFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "31c1fd85-5007-48b1-afd3-11fd85319d8d");
        final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "78c5e16c-6deb-4dfa-89a8-cb09115abe34");
        zipExtraFields[extraFields.length] = unparseableExtra;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "8fa5ccd4-ef25-4077-a62f-c051d174464b");
        return zipExtraFields;
    }

    private ZipExtraField[] getUnparseableOnly() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "517be517-a495-4b09-8c88-a4838fb63ac8");
        return unparseableExtra == null ? noExtraFields : new ZipExtraField[] { unparseableExtra };
    }

    private ZipExtraField[] getAllExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "330f6617-5e0b-4943-9066-1f282629524c");
        final ZipExtraField[] allExtraFieldsNoCopy = getAllExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "cdfb4c70-dcf5-4640-bb23-86a1118e7a20");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6c1b7651-49fa-4146-bb4d-b4454fe18a88");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "42c39115-facd-4653-9567-5ed0f54eac13");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "175e68df-bd37-406d-8199-7cba05dede18");
            if (extraFields == null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "64eb6b52-30aa-4c1c-891c-5add24760f71");
                extraFields = new ZipExtraField[] { ze };
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "235ba14d-56bf-45dc-a08e-03fa329c9fc5");
                if (getExtraField(ze.getHeaderId()) != null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f37fc6f3-f65d-496f-8a67-5d16a786ed48");
                    removeExtraField(ze.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "911c1213-0d59-4109-bd9b-1ea6ade1b3fe");
                final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e49fbe50-68d9-4e67-8d35-5430fed2ec36");
                zipExtraFields[zipExtraFields.length - 1] = ze;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7c1ac446-97b6-4ee5-b28f-fdd94442769e");
                extraFields = zipExtraFields;
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7da5f746-8645-4f51-8c33-6d4f8500b84e");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e394bd69-88a0-4aa8-a50c-0425c4f51c42");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "56b208a5-28e1-4dd7-8fdd-db3466f74737");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "552f6c36-44f9-485b-b2d8-4b6158053d56");
            if (getExtraField(ze.getHeaderId()) != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d2c7c4be-cb25-4ef5-9d2e-f0a6c9102276");
                removeExtraField(ze.getHeaderId());
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "90938ede-41a7-4433-b99c-77a553e85340");
            final ZipExtraField[] copy = extraFields;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "5faf6149-2b3a-4478-a6ae-5079f4f46efe");
            final int newLen = extraFields != null ? extraFields.length + 1 : 1;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3648ead9-d201-4541-bfdb-2770b318f4f7");
            extraFields = new ZipExtraField[newLen];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "c93c8508-743d-49c1-8825-d158513f9716");
            extraFields[0] = ze;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6d4c6189-be2a-4d0e-95c7-70ab3c448f71");
            if (copy != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f88235c7-33db-49e1-87ba-cf16a54ba28a");
                System.arraycopy(copy, 0, extraFields, 1, extraFields.length - 1);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "2e59101f-198c-4157-a2ed-97065e8eb7c6");
        setExtra();
    }

    /**
     * Remove an extra field.
     * @param type the type of extra field to remove
     */
    public void removeExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "36deafde-dfa6-4903-bb15-332d44666d3e");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "0733c100-843e-4ee3-ae0f-dd1d2b8a6261");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "c3483228-1b58-4462-a684-75eb3191ffa5");
        final List<ZipExtraField> newResult = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "171a9bb7-8543-4efa-b7b6-93af085162bc");
        for (final ZipExtraField extraField : extraFields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "93f813c9-45ef-41e8-8da3-abe1e166351a");
            if (!type.equals(extraField.getHeaderId())) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "014a8139-cf06-4442-8a6a-6ae49f16c34c");
                newResult.add(extraField);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "a2614683-a716-4c55-b975-1cd9178501bb");
        if (extraFields.length == newResult.size()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "912e2088-1c27-4a34-82d5-b416079abc07");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "07be1f64-522c-4354-825b-9e59f8dd2b47");
        extraFields = newResult.toArray(new ZipExtraField[newResult.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "494d933a-1a23-4704-aeab-8c9d55c83551");
        setExtra();
    }

    /**
     * Removes unparseable extra field data.
     *
     * @since 1.1
     */
    public void removeUnparseableExtraFieldData() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d5183b3e-d802-43e9-9650-a07066b70bc6");
        if (unparseableExtra == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3220fc98-3202-4ca5-8188-79231d746a04");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "60a7fb24-6762-4b96-8531-4cf9a1894b4a");
        unparseableExtra = null;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "5f826660-1111-4db0-88b3-7377c952bfce");
        setExtra();
    }

    /**
     * Looks up an extra field by its header id.
     *
     * @param type the header id
     * @return null if no such field exists.
     */
    public ZipExtraField getExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6d32fc2d-b12b-4c6f-a64a-43fbb9adeb37");
        if (extraFields != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "230c57ea-dddc-441f-bbed-4f8e6feded05");
            for (final ZipExtraField extraField : extraFields) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "73c07483-767b-47f4-901a-955574917192");
                if (type.equals(extraField.getHeaderId())) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "ed20b22c-07be-4e6a-954e-ed9015dd9008");
                    return extraField;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "68699069-61eb-4e6c-9e0a-e4ef6c33ed78");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "26dda824-b3d4-40d5-909b-89ea663c3f00");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "998e02d9-d4a2-4966-91b8-61d9cb9d909b");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "a2623c3a-0905-4b3d-bb0b-37fe820c457e");
            final ZipExtraField[] local = ExtraFieldUtils.parse(extra, true, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3ea594aa-af2f-4bb9-9e73-6ce261d592eb");
            mergeExtraFields(local, true);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "4c74cd19-de7c-40c8-a8f8-956a0310da96");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3d866fd8-86b8-46c6-8ed2-eb3bfa67f13d");
        super.setExtra(ExtraFieldUtils.mergeLocalFileDataData(getAllExtraFieldsNoCopy()));
    }

    /**
     * Sets the central directory part of extra fields.
     * @param b an array of bytes to be parsed into extra fields
     */
    public void setCentralDirectoryExtra(final byte[] b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "c20b7357-6e67-43db-b3d4-5d25d145ecd1");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1794665a-db58-4f8c-a0b3-bcf7aac7ac9e");
            final ZipExtraField[] central = ExtraFieldUtils.parse(b, false, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "0e9aac98-dc72-4b1a-b3b1-65de570dbadf");
            mergeExtraFields(central, false);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "5ed843c5-7f27-4b9c-a1a1-27d849e3cbe4");
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Retrieves the extra data for the local file data.
     * @return the extra data for local file
     */
    public byte[] getLocalFileDataExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3f18435b-31b0-4041-97c9-e76b5b678c60");
        final byte[] extra = getExtra();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "828f8039-10b6-424a-a073-226988a651f4");
        return extra != null ? extra : EMPTY;
    }

    /**
     * Retrieves the extra data for the central directory.
     * @return the central directory extra data
     */
    public byte[] getCentralDirectoryExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "2af264ad-2098-4f92-aef3-ded3138fac73");
        return ExtraFieldUtils.mergeCentralDirectoryData(getAllExtraFieldsNoCopy());
    }

    /**
     * Get the name of the entry.
     * @return the entry name
     */
    @Override
    public String getName() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "0e21983c-c9fb-4cfd-aa1c-ff7ccd11d3a3");
        return name == null ? super.getName() : name;
    }

    /**
     * Is this entry a directory?
     * @return true if the entry is a directory
     */
    @Override
    public boolean isDirectory() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "871be99c-8e3b-4500-b936-e211c6685d62");
        return getName().endsWith("/");
    }

    /**
     * Set the name of the entry.
     * @param name the name to use
     */
    protected void setName(String name) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "46c211f8-6dde-4cdc-a5a4-ee15f2c4212f");
        if (name != null && getPlatform() == PLATFORM_FAT && !name.contains("/")) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e6a861f4-a6ce-42a5-83cc-fa9e37e0cf11");
            name = name.replace('\\', '/');
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e753c451-7cba-41f0-93bd-dd5957315cb1");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "b059ea67-0935-4f39-90bb-ed48c5a25d59");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f57b9722-711c-4c3c-ae80-0de2f47dfb60");
        if (size < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "473f43b3-e45a-4096-83b7-8f2d2a34b1d8");
            throw new IllegalArgumentException("invalid entry size");
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7c5c463b-ff96-4ce9-8792-4dbb5f168389");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6e05a7bb-1e51-4d53-b203-03a85920617d");
        setName(name);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "92788900-4350-4850-85e1-60bdf2107eaf");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "83356882-9629-475d-bdba-dd2c2ed5ceb9");
        if (rawName != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "8c080f2c-98ec-4542-a47b-481150769353");
            final byte[] b = new byte[rawName.length];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "87b69658-253d-4d2b-94c6-678a9c43911c");
            System.arraycopy(rawName, 0, b, 0, rawName.length);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f126a29f-15d4-4364-b23c-f48189b7a633");
            return b;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "36d04598-6da7-423d-8414-dde3de1ada73");
        return null;
    }

    protected long getLocalHeaderOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "fe2d681c-6643-4c6c-a7ef-6574344f5b54");
        return this.localHeaderOffset;
    }

    protected void setLocalHeaderOffset(long localHeaderOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "51071b22-1cbd-4526-84af-a164b065b06f");
        this.localHeaderOffset = localHeaderOffset;
    }

    @Override
    public long getDataOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "217d2117-162d-4bef-8bdc-ef90db4e68f5");
        return dataOffset;
    }

    /**
     * Sets the data offset.
     *
     * @param dataOffset
     *      new value of data offset.
     */
    protected void setDataOffset(long dataOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7ba49ef9-0f3d-4376-a370-99db04e81674");
        this.dataOffset = dataOffset;
    }

    @Override
    public boolean isStreamContiguous() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "602f26e8-1f8c-4e0e-8d18-9a897068e8cf");
        return isStreamContiguous;
    }

    protected void setStreamContiguous(boolean isStreamContiguous) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3e3345c1-b2ec-4f95-8344-3a07fd165b77");
        this.isStreamContiguous = isStreamContiguous;
    }

    /**
     * Get the hashCode of the entry.
     * This uses the name as the hashcode.
     * @return a hashcode.
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e42fab34-1089-4112-867e-84a4f0d986eb");
        return getName().hashCode();
    }

    /**
     * The "general purpose bit" field.
     * @return the general purpose bit
     * @since 1.1
     */
    public GeneralPurposeBit getGeneralPurposeBit() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "0e7b56ae-20e0-4baf-9462-af21094f9f45");
        return gpb;
    }

    /**
     * The "general purpose bit" field.
     * @param b the general purpose bit
     * @since 1.1
     */
    public void setGeneralPurposeBit(final GeneralPurposeBit b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "94217e84-baf1-4ba8-a0a5-8580c154055e");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "e6c73a74-b636-40c3-809f-b9ca2921dbc5");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7aacda8e-bff3-404b-8f01-c8d861b40a55");
            setExtraFields(f);
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "b46c3869-1f3e-4a08-a973-6b96b64dd448");
            for (final ZipExtraField element : f) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "ea765a84-742f-4f09-a152-40ec024a664d");
                ZipExtraField existing;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1915608a-86d2-42cb-9bdf-a90c0a5f93f9");
                if (element instanceof UnparseableExtraFieldData) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d4e99ec6-261f-44ca-8471-5858618d08de");
                    existing = unparseableExtra;
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "05975f83-9590-44d2-8b28-6912574f5af6");
                    existing = getExtraField(element.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "30f1f450-efb7-4a3a-9e36-54d510da091c");
                if (existing == null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1b459fe9-248d-46a3-b460-2048a0a35919");
                    addExtraField(element);
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "859640b0-cef6-4aeb-a61e-b31ed674e86d");
                    if (local) {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f093ff87-0ea8-4dd2-9829-6e65e07a378f");
                        final byte[] b = element.getLocalFileDataData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "9256477c-9831-4729-bbbb-4a1ffd77736a");
                        existing.parseFromLocalFileData(b, 0, b.length);
                    } else {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "49748e25-3e0c-4d9c-baee-b29a1248452d");
                        final byte[] b = element.getCentralDirectoryData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "9b4f0c16-773c-4596-838f-e22a22390d7a");
                        existing.parseFromCentralDirectoryData(b, 0, b.length);
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7b835fe5-7436-454d-b6ad-5f1fef5c2666");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "097ee6cb-518d-4ff4-98a0-1c8082398611");
        return new Date(getTime());
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d76f5173-d557-45eb-91b1-7609c6c02fb2");
        if (this == obj) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "9348a495-fe90-4421-82a4-358d3dd2d334");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "10f71344-a621-4be5-815c-917c60fd89c2");
        if (obj == null || getClass() != obj.getClass()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1923bc42-e614-4d21-9e80-3084b9786b84");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "cceb96ea-5e38-41f0-8876-906b9e94b049");
        final ZipArchiveEntry other = (ZipArchiveEntry) obj;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3d6315a7-a48d-4a50-aa18-2ca44e90b575");
        final String myName = getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "f3cb67b3-9a52-4521-9d69-d3eedecec24a");
        final String otherName = other.getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "4aa67fd0-ebbb-4a51-a590-41db60942df7");
        if (myName == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "31628a1a-6107-4447-960f-4cc0f8355477");
            if (otherName != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "479f896c-589a-4661-ae79-6c1ffac63213");
                return false;
            }
        } else if (!myName.equals(otherName)) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "382609db-2607-49a4-91be-e863df33ec58");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "ed35e623-e88a-46f2-9522-a3698e0521e2");
        String myComment = getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "6215dd09-3545-41b7-b19d-1004436f091c");
        String otherComment = other.getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "1019cfe5-78fb-4f76-a0d0-ef6437438039");
        if (myComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "2a010546-c912-4a19-9bf4-8508d5d7ccb5");
            myComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "083b8d6b-de45-4d8a-996f-3af0acd8799f");
        if (otherComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "c8524b05-ef1c-424e-b3cc-a4cf8e73c399");
            otherComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d14b6e3b-641d-4947-85a4-3cc1df46f5cc");
        return getTime() == other.getTime() && myComment.equals(otherComment) && getInternalAttributes() == other.getInternalAttributes() && getPlatform() == other.getPlatform() && getExternalAttributes() == other.getExternalAttributes() && getMethod() == other.getMethod() && getSize() == other.getSize() && getCrc() == other.getCrc() && getCompressedSize() == other.getCompressedSize() && Arrays.equals(getCentralDirectoryExtra(), other.getCentralDirectoryExtra()) && Arrays.equals(getLocalFileDataExtra(), other.getLocalFileDataExtra()) && localHeaderOffset == other.localHeaderOffset && dataOffset == other.dataOffset && gpb.equals(other.gpb);
    }

    /**
     * Sets the "version made by" field.
     * @param versionMadeBy "version made by" field
     * @since 1.11
     */
    public void setVersionMadeBy(final int versionMadeBy) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "603d6244-bb7b-4d46-86a8-8526d1221886");
        this.versionMadeBy = versionMadeBy;
    }

    /**
     * Sets the "version required to expand" field.
     * @param versionRequired "version required to expand" field
     * @since 1.11
     */
    public void setVersionRequired(final int versionRequired) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "bd590edb-4f07-4280-bc34-38eea7705ba3");
        this.versionRequired = versionRequired;
    }

    /**
     * The "version required to expand" field.
     * @return "version required to expand" field
     * @since 1.11
     */
    public int getVersionRequired() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d00f9bd5-14c3-4b67-9a3f-01869a810768");
        return versionRequired;
    }

    /**
     * The "version made by" field.
     * @return "version made by" field
     * @since 1.11
     */
    public int getVersionMadeBy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "3023061a-42d2-4d65-8950-41a750338126");
        return versionMadeBy;
    }

    /**
     * The content of the flags field.
     * @return content of the flags field
     * @since 1.11
     */
    public int getRawFlag() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "40593194-0d0e-435b-8550-123bb4c53ae2");
        return rawFlag;
    }

    /**
     * Sets the content of the flags field.
     * @param rawFlag content of the flags field
     * @since 1.11
     */
    public void setRawFlag(final int rawFlag) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "2cea0847-0bef-4aea-8f92-5fbc6dfafa26");
        this.rawFlag = rawFlag;
    }

    /**
     * The source of the name field value.
     * @return source of the name field value
     * @since 1.16
     */
    public NameSource getNameSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "d83e4795-fd62-4a93-b69c-37160c4acef7");
        return nameSource;
    }

    /**
     * Sets the source of the name field value.
     * @param nameSource source of the name field value
     * @since 1.16
     */
    public void setNameSource(NameSource nameSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "7baebcfa-e6a1-4bb9-9035-642f81b392ac");
        this.nameSource = nameSource;
    }

    /**
     * The source of the comment field value.
     * @return source of the comment field value
     * @since 1.16
     */
    public CommentSource getCommentSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "0d1eb0eb-48f5-423a-8618-f3ec14f919d8");
        return commentSource;
    }

    /**
     * Sets the source of the comment field value.
     * @param commentSource source of the comment field value
     * @since 1.16
     */
    public void setCommentSource(CommentSource commentSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_1_10.coverage", "99ec290b-5475-45f9-b957-23c1e3d6e1db");
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
