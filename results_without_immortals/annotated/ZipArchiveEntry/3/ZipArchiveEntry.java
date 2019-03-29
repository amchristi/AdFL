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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "0f615c6f-a484-4b6b-a7bd-ec68f65dcc7d");
        final ZipArchiveEntry e = (ZipArchiveEntry) super.clone();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "988f2e9d-4ce0-4ce0-bdcf-858e9c697034");
        e.setInternalAttributes(getInternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "6ec5a057-570c-46ab-8e25-af40f48e1d00");
        e.setExternalAttributes(getExternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d47d1dbe-f571-414a-9592-74040b9ee6a2");
        e.setExtraFields(getAllExtraFieldsNoCopy());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "a3c2929c-ad06-4bb8-9332-ecedc0501700");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c6f8c38c-22a3-4e57-a902-5c5c999f2f0d");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "00a11202-17c0-4a3f-b19e-b12cb30c772f");
        if (method < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "2f8db5b5-dc6d-45c3-81e5-26055c732714");
            throw new IllegalArgumentException("ZIP compression method can not be negative: " + method);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d24d65fd-a284-4182-8ab7-a153a47d2416");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "33aaecd7-3159-4393-bb0e-6bc04a96ffc3");
        return internalAttributes;
    }

    /**
     * Sets the internal file attributes.
     * @param value an <code>int</code> value
     */
    public void setInternalAttributes(final int value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "95cdb462-1f06-4446-9117-8330eff4f6ef");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "bb6bb534-f266-4528-97f2-f3f8b12fb11b");
        return externalAttributes;
    }

    /**
     * Sets the external file attributes.
     * @param value an <code>long</code> value
     */
    public void setExternalAttributes(final long value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "1674f976-b3ab-411d-87e3-67bc90a0680f");
        externalAttributes = value;
    }

    /**
     * Sets Unix permissions in a way that is understood by Info-Zip's
     * unzip command.
     * @param mode an <code>int</code> value
     */
    public void setUnixMode(final int mode) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "5e7f5a7e-e081-4bef-9382-dacbc57f61f3");
        setExternalAttributes((mode << SHORT_SHIFT) | ((mode & 0200) == 0 ? 1 : 0) | (isDirectory() ? 0x10 : 0));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "3e38d0ec-e6c5-4ef3-ae68-91003d4b9668");
        platform = PLATFORM_UNIX;
    }

    /**
     * Unix permission.
     * @return the unix permissions
     */
    public int getUnixMode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "4c8265bd-115e-4743-9f36-89fdee1d7060");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "cc12586b-035c-4c0e-a5ce-20f23069f727");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "5e5b841f-58af-4fd6-b028-60031be1a73c");
        return platform;
    }

    /**
     * Set the platform (UNIX or FAT).
     * @param platform an <code>int</code> value - 0 is FAT, 3 is UNIX
     */
    protected void setPlatform(final int platform) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ad6f8971-6d03-44e7-a0b6-b299a25288f7");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "4419e730-970d-47f6-b9c9-29410b99ba9b");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b167e87a-8818-4e88-9a26-21a948fe7884");
        if ((alignment & (alignment - 1)) != 0 || alignment > 0xffff) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "337f000a-3485-435b-b5bd-e455a6438be9");
            throw new IllegalArgumentException("Invalid value for alignment, must be power of two and no bigger than " + 0xffff + " but is " + alignment);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "80768afe-6ba3-4e3e-b33c-509ba48c92e2");
        this.alignment = alignment;
    }

    /**
     * Replaces all currently attached extra fields with the new array.
     * @param fields an array of extra fields
     */
    public void setExtraFields(final ZipExtraField[] fields) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "851ea468-c958-403e-b0c1-74dd7ee0c4a7");
        final List<ZipExtraField> newFields = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "0f4dcec7-71da-4ff6-b5f5-7dfb3456fb0f");
        for (final ZipExtraField field : fields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "1426b9ce-6426-45c9-b603-eba1cb16aed5");
            if (field instanceof UnparseableExtraFieldData) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "a1ee2eb7-af71-4187-9ed4-3a616543388d");
                unparseableExtra = (UnparseableExtraFieldData) field;
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "517a46a1-cc48-4b1e-89b6-f7cc50c6d5c7");
                newFields.add(field);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "5aab9632-df29-4fcf-9d5b-fee469c6ca84");
        extraFields = newFields.toArray(new ZipExtraField[newFields.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "160e57b4-ffb9-49ed-9683-1bb26ff929d9");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f89eedb0-92c1-4d01-bdd3-fbcba315f250");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "72b50714-a7dc-4d6d-bb43-d672cb010e5a");
        return includeUnparseable ? getAllExtraFields() : getParseableExtraFields();
    }

    private ZipExtraField[] getParseableExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "cfa229e5-1f3c-4c9a-a14b-b11f7c5002ee");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "94fc5fef-904e-4fec-bd00-4331adf3d17e");
            return noExtraFields;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "715c211e-4d92-4995-bd80-42d09313f167");
        return extraFields;
    }

    private ZipExtraField[] getParseableExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e615e1ea-3c2d-47b5-b5f0-7fe42591ad63");
        final ZipExtraField[] parseableExtraFields = getParseableExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d51dd228-ffc3-4080-a8a9-90dd44fb3964");
        return (parseableExtraFields == extraFields) ? copyOf(parseableExtraFields) : parseableExtraFields;
    }

    /**
     * Get all extra fields, including unparseable ones.
     * @return An array of all extra fields. Not necessarily a copy of internal data structures, hence private method
     */
    private ZipExtraField[] getAllExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "737b7144-0aa9-4c32-a000-fb595931a830");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "9e4026f3-2da9-4bbf-b752-373cf5bcf6cb");
            return getUnparseableOnly();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b17ff4e8-0826-4349-9e12-7bdbffe89998");
        return unparseableExtra != null ? getMergedFields() : extraFields;
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e0bcc6c0-c207-462b-8cfc-e94a9b756f86");
        return copyOf(src, src.length);
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src, final int length) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "00340a69-7c0a-4605-be49-9f9bbb642f00");
        final ZipExtraField[] cpy = new ZipExtraField[length];
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e5d02a83-0b4e-4716-a57c-9ae4b6bee7ab");
        System.arraycopy(src, 0, cpy, 0, Math.min(src.length, length));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f0f38955-b9ca-4787-851b-8d64f0b39e46");
        return cpy;
    }

    private ZipExtraField[] getMergedFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "bdc46453-2e3f-4486-b59e-090d53aa553c");
        final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "9aafef58-d6bb-4aae-9147-7d4a1bf87938");
        zipExtraFields[extraFields.length] = unparseableExtra;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ca67dc0f-f7ab-4f64-8a38-167871d06466");
        return zipExtraFields;
    }

    private ZipExtraField[] getUnparseableOnly() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "055683d0-3067-4da5-8741-b82a8275a753");
        return unparseableExtra == null ? noExtraFields : new ZipExtraField[] { unparseableExtra };
    }

    private ZipExtraField[] getAllExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "83f6528f-5edd-4300-b320-f442ff966299");
        final ZipExtraField[] allExtraFieldsNoCopy = getAllExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b9cf292c-acee-408c-b8c9-1871872d7f06");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d40f42b8-8b47-47af-9dd4-939d0c0758c6");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "cd00c4ef-f917-44b4-abe9-d72fff74564b");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d6fd35d0-b85f-47fa-b6e4-905805caf03d");
            if (extraFields == null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f96d4dc4-d046-40d7-b990-587f746e81e5");
                extraFields = new ZipExtraField[] { ze };
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "5850cc62-ad4a-4ddb-b104-d5ea461053ef");
                if (getExtraField(ze.getHeaderId()) != null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "226bb38d-9f36-4794-aa8f-9db995427a5b");
                    removeExtraField(ze.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "2bb35afa-1cde-4d29-998b-3ab605b1c05d");
                final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d3b35d83-9207-4cba-ba6f-8b4c68dc2b99");
                zipExtraFields[zipExtraFields.length - 1] = ze;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c8295c5f-4b8e-4bda-9bd8-4c912ddc04b1");
                extraFields = zipExtraFields;
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d5b525dc-52e9-4db6-b28e-9fde39a37dd0");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "6b446e60-cd31-43f6-928e-3db07a63035c");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "dbe92103-a136-4411-ac91-fbc6d7271611");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "707e8a8c-64c3-4d45-b779-185b14f7ce69");
            if (getExtraField(ze.getHeaderId()) != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c4e3a995-4d88-474f-bdff-97ec54ff0754");
                removeExtraField(ze.getHeaderId());
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "72b82741-2b3f-4e4f-b58d-5a0fe4a0314c");
            final ZipExtraField[] copy = extraFields;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "02bfae71-5783-4732-8c83-3d4eea269d6d");
            final int newLen = extraFields != null ? extraFields.length + 1 : 1;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c92a3f62-0e3a-41ef-8ead-12f9186ac8a8");
            extraFields = new ZipExtraField[newLen];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ce4e3d0a-d5f5-4997-929a-881410d0677a");
            extraFields[0] = ze;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b8c6de15-519e-4479-b84b-221c761de50b");
            if (copy != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "3e749a01-a658-4550-8864-e0dd90588719");
                System.arraycopy(copy, 0, extraFields, 1, extraFields.length - 1);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f66c8a1e-af4e-4db3-aed1-078b1f65015d");
        setExtra();
    }

    /**
     * Remove an extra field.
     * @param type the type of extra field to remove
     */
    public void removeExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "601ead8a-50d8-49e1-a299-8f32484a05fc");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e1f39475-73c9-40e2-aebc-c9bdb1615bee");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "bcc10074-70e3-4360-9e63-db69fe657df7");
        final List<ZipExtraField> newResult = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "479dcdec-0711-4b81-9bde-7bde96e5c835");
        for (final ZipExtraField extraField : extraFields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e3309570-4d61-4bdb-9c6b-813b8a27a9f0");
            if (!type.equals(extraField.getHeaderId())) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "a029569e-d1d7-4445-afae-cecdc59b6961");
                newResult.add(extraField);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ab70c082-726b-446f-af1e-0f3d1e9c64d4");
        if (extraFields.length == newResult.size()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "191091b4-b42f-46b4-9dfb-4654b98d6a0e");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "5c9d8502-7dca-4f2b-a47a-3907bb903fde");
        extraFields = newResult.toArray(new ZipExtraField[newResult.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "6744dfed-15d0-4bfd-b9df-2ec5cb61d078");
        setExtra();
    }

    /**
     * Removes unparseable extra field data.
     *
     * @since 1.1
     */
    public void removeUnparseableExtraFieldData() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "319fcc8c-6bba-4f15-8071-b03b5953069e");
        if (unparseableExtra == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "76291527-8613-4b9c-9b34-9e033d745b26");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "6a546cf0-4309-4c2b-bbe5-ebcfb3d042b1");
        unparseableExtra = null;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "de5c1e01-7168-4b02-93e7-e83c142e99f7");
        setExtra();
    }

    /**
     * Looks up an extra field by its header id.
     *
     * @param type the header id
     * @return null if no such field exists.
     */
    public ZipExtraField getExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b043a3c5-1f4f-45eb-ac04-143b8711677f");
        if (extraFields != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d37702bd-5640-4171-88e4-b6024ccfc85d");
            for (final ZipExtraField extraField : extraFields) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "04051ae7-4e2d-4fc1-836e-99be1c10228e");
                if (type.equals(extraField.getHeaderId())) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "46dba85c-21d2-444d-92e5-fc269ce6b52b");
                    return extraField;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c4100be3-0785-496d-8f0b-1ce99e77582b");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "8e37aef8-f38e-48b6-b512-4ce7796c3a57");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "612e59ee-cea5-481a-9f73-daaf87be383e");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "7f3422c5-39cc-45b0-a9aa-c3a9555bcb30");
            final ZipExtraField[] local = ExtraFieldUtils.parse(extra, true, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f3d14080-da53-44fb-99b2-1811089975a9");
            mergeExtraFields(local, true);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "9d613467-d49e-473b-9bcd-5f6ea281c859");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "65ddf2bc-c49d-47a2-84ca-416bf3139872");
        super.setExtra(ExtraFieldUtils.mergeLocalFileDataData(getAllExtraFieldsNoCopy()));
    }

    /**
     * Sets the central directory part of extra fields.
     * @param b an array of bytes to be parsed into extra fields
     */
    public void setCentralDirectoryExtra(final byte[] b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "24c4420f-7d3e-46bb-a5d3-7fa31d698352");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "3c998001-df97-46c9-a22b-e4061ac55e63");
            final ZipExtraField[] central = ExtraFieldUtils.parse(b, false, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "892314e6-7aea-4a3f-b493-df5409aef218");
            mergeExtraFields(central, false);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ce4df764-c6d9-4adc-9e1c-128a964c7531");
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Retrieves the extra data for the local file data.
     * @return the extra data for local file
     */
    public byte[] getLocalFileDataExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "3f4a0e6d-8032-4065-860b-c76b4d9deed0");
        final byte[] extra = getExtra();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "db1d6258-a343-4c23-8a43-092b13ad1627");
        return extra != null ? extra : EMPTY;
    }

    /**
     * Retrieves the extra data for the central directory.
     * @return the central directory extra data
     */
    public byte[] getCentralDirectoryExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "00d40112-f068-4899-8fa4-46112ec30520");
        return ExtraFieldUtils.mergeCentralDirectoryData(getAllExtraFieldsNoCopy());
    }

    /**
     * Get the name of the entry.
     * @return the entry name
     */
    @Override
    public String getName() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "11f2e94e-662f-43ae-8a60-5fb79e46b9b5");
        return name == null ? super.getName() : name;
    }

    /**
     * Is this entry a directory?
     * @return true if the entry is a directory
     */
    @Override
    public boolean isDirectory() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "27be12f6-f15e-4240-ab54-5a91733a27d6");
        return getName().endsWith("/");
    }

    /**
     * Set the name of the entry.
     * @param name the name to use
     */
    protected void setName(String name) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "2bff0f38-ab18-4086-9064-a54c09b99a22");
        if (name != null && getPlatform() == PLATFORM_FAT && !name.contains("/")) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "05175352-7f7d-4a75-9274-03f389228198");
            name = name.replace('\\', '/');
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "81129e57-0e91-442f-b8f7-0bea5ee2112d");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "0887d52d-8061-4fdf-9a45-73c469de3819");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f3208d8b-d734-44d5-9639-bde1e3e20cb5");
        if (size < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f0896a1c-9d15-4ce1-ac1a-7d4db1cf1444");
            throw new IllegalArgumentException("invalid entry size");
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "15bf8c78-0590-4d49-b681-3fbd214c0b4f");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "688a5204-adb5-429b-be37-5c50cc4bb219");
        setName(name);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "984d3b1f-1657-48db-943c-a2412fa3d319");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "4d009a64-59a6-4ee0-aad2-187bf434f3cb");
        if (rawName != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "0fa5419d-f98f-4a59-9264-d34d813cb299");
            final byte[] b = new byte[rawName.length];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "1116d3a9-f5e1-485b-8ebb-a0801df17a5e");
            System.arraycopy(rawName, 0, b, 0, rawName.length);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c59512a3-667e-4506-9d52-55a6379ce40f");
            return b;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "09d4bc12-b324-43c1-b5bf-883633050844");
        return null;
    }

    protected long getLocalHeaderOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "108474bc-e4f2-47ec-8da8-73810fee18b8");
        return this.localHeaderOffset;
    }

    protected void setLocalHeaderOffset(long localHeaderOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "dea20625-3c44-491c-8af0-867ec4d04577");
        this.localHeaderOffset = localHeaderOffset;
    }

    @Override
    public long getDataOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "0a957245-c70a-4b12-9f93-a2a3923c04ac");
        return dataOffset;
    }

    /**
     * Sets the data offset.
     *
     * @param dataOffset
     *      new value of data offset.
     */
    protected void setDataOffset(long dataOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "79aec750-2d79-4382-ac81-378d290a2e03");
        this.dataOffset = dataOffset;
    }

    @Override
    public boolean isStreamContiguous() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b70e454c-c907-46ee-9567-9b684cebb1e4");
        return isStreamContiguous;
    }

    protected void setStreamContiguous(boolean isStreamContiguous) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "3a1da482-3727-4409-bfb0-d9d664e3da8a");
        this.isStreamContiguous = isStreamContiguous;
    }

    /**
     * Get the hashCode of the entry.
     * This uses the name as the hashcode.
     * @return a hashcode.
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "cc8ecdd7-f5fc-4701-8145-656b9018e878");
        return getName().hashCode();
    }

    /**
     * The "general purpose bit" field.
     * @return the general purpose bit
     * @since 1.1
     */
    public GeneralPurposeBit getGeneralPurposeBit() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ae5be75c-9f74-4fd3-a26b-2629e11991d3");
        return gpb;
    }

    /**
     * The "general purpose bit" field.
     * @param b the general purpose bit
     * @since 1.1
     */
    public void setGeneralPurposeBit(final GeneralPurposeBit b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "016bdf40-d98b-42e0-8dc6-10821c7d2cf7");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "76357b0e-b3e7-4ad4-8300-5000a77171f3");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "8bc1d524-2ee8-4484-bd60-e70391005fa3");
            setExtraFields(f);
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "9a92244e-47fb-4040-b8e1-60883a9faf11");
            for (final ZipExtraField element : f) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "deab474c-bbf6-4120-9045-89f352e9ee6e");
                ZipExtraField existing;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "4cfe18fc-1051-4176-a69e-de49528fb625");
                if (element instanceof UnparseableExtraFieldData) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "6abe782a-4a51-462c-8ada-45110f616be8");
                    existing = unparseableExtra;
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "7ad33f8e-b469-4ee3-ad42-866a32db556a");
                    existing = getExtraField(element.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "570b10b2-11c3-4d52-b9ad-c9201638a5f3");
                if (existing == null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "57512206-1b33-48a1-a85d-beb877505986");
                    addExtraField(element);
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f3b4915c-8254-40b7-a314-2b640e3fafd7");
                    if (local) {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "539d1756-4536-4dd1-a114-777629789710");
                        final byte[] b = element.getLocalFileDataData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "d4f28eca-fcbd-42db-a24e-d6d7c02a2aff");
                        existing.parseFromLocalFileData(b, 0, b.length);
                    } else {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "0d7f0a48-bcf0-4633-98bf-0dd5107600a6");
                        final byte[] b = element.getCentralDirectoryData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "1f4655d5-5f9e-4416-8af8-10df3f3e221d");
                        existing.parseFromCentralDirectoryData(b, 0, b.length);
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "50728cd5-dbc0-4663-b32c-4c4fe8556b19");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "3acc9e6a-2945-452d-bdb7-1733e674163b");
        return new Date(getTime());
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "4f92e430-6efe-406f-955b-1cf45fe1792f");
        if (this == obj) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ba72f00f-e0de-4db0-b4a0-99d654f05b6c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "20fb292f-56b8-4f7b-9ef2-693791220ebd");
        if (obj == null || getClass() != obj.getClass()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "59d75acd-32e0-4bbc-b67f-cd0e5c05411e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "17ef7907-1c72-4fa1-8fef-ecfdc6e2df04");
        final ZipArchiveEntry other = (ZipArchiveEntry) obj;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "44075c16-967a-4711-899c-a9805eddc21c");
        final String myName = getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "7b5e308d-eb65-4da9-990b-064b250cbbd3");
        final String otherName = other.getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "595d4063-70ff-460f-a400-8175c5219709");
        if (myName == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "42599fd6-fb57-4f16-9344-86d914378383");
            if (otherName != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "2a3ef61f-b642-4c6a-ae78-f73da661f36f");
                return false;
            }
        } else if (!myName.equals(otherName)) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "da90220e-8770-4aa6-b002-e35cdab168ed");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "1e6ad889-5ffe-4b60-9557-2d984c809e74");
        String myComment = getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "a6ffdb39-eba5-4be7-a42c-29d25dcde136");
        String otherComment = other.getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "248bb495-fd5e-4406-b57f-a2a70566aa79");
        if (myComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "7bbc2600-f65e-4932-82e8-bf09b649e562");
            myComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "bfd65829-4632-48ee-95f7-579082138f26");
        if (otherComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "8641d47d-c7fc-4630-8b98-39b3f437c8ce");
            otherComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "f758418f-34da-42e6-a258-2528f36c2713");
        return getTime() == other.getTime() && myComment.equals(otherComment) && getInternalAttributes() == other.getInternalAttributes() && getPlatform() == other.getPlatform() && getExternalAttributes() == other.getExternalAttributes() && getMethod() == other.getMethod() && getSize() == other.getSize() && getCrc() == other.getCrc() && getCompressedSize() == other.getCompressedSize() && Arrays.equals(getCentralDirectoryExtra(), other.getCentralDirectoryExtra()) && Arrays.equals(getLocalFileDataExtra(), other.getLocalFileDataExtra()) && localHeaderOffset == other.localHeaderOffset && dataOffset == other.dataOffset && gpb.equals(other.gpb);
    }

    /**
     * Sets the "version made by" field.
     * @param versionMadeBy "version made by" field
     * @since 1.11
     */
    public void setVersionMadeBy(final int versionMadeBy) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "c81bb614-11ed-48a7-b04c-8f283d702fa3");
        this.versionMadeBy = versionMadeBy;
    }

    /**
     * Sets the "version required to expand" field.
     * @param versionRequired "version required to expand" field
     * @since 1.11
     */
    public void setVersionRequired(final int versionRequired) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "91a6bdf4-8844-4eaa-9f6d-e2ad8be9a5d6");
        this.versionRequired = versionRequired;
    }

    /**
     * The "version required to expand" field.
     * @return "version required to expand" field
     * @since 1.11
     */
    public int getVersionRequired() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "ebacc372-baab-4dd1-b037-fee86e744c2b");
        return versionRequired;
    }

    /**
     * The "version made by" field.
     * @return "version made by" field
     * @since 1.11
     */
    public int getVersionMadeBy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "2cd33f72-1b9b-4a40-a8dc-3a2a555af62d");
        return versionMadeBy;
    }

    /**
     * The content of the flags field.
     * @return content of the flags field
     * @since 1.11
     */
    public int getRawFlag() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "caabea1c-e7fc-44ea-974f-e1cb1feefd8a");
        return rawFlag;
    }

    /**
     * Sets the content of the flags field.
     * @param rawFlag content of the flags field
     * @since 1.11
     */
    public void setRawFlag(final int rawFlag) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "b0c76c6e-960e-4ab9-a7d1-a11b4c1a2015");
        this.rawFlag = rawFlag;
    }

    /**
     * The source of the name field value.
     * @return source of the name field value
     * @since 1.16
     */
    public NameSource getNameSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "dbac7b1b-5b9a-44e6-882e-4005b4ec289f");
        return nameSource;
    }

    /**
     * Sets the source of the name field value.
     * @param nameSource source of the name field value
     * @since 1.16
     */
    public void setNameSource(NameSource nameSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e8021cb4-c952-4186-b409-f2dc921ebb2d");
        this.nameSource = nameSource;
    }

    /**
     * The source of the comment field value.
     * @return source of the comment field value
     * @since 1.16
     */
    public CommentSource getCommentSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "7e755753-e79b-49a0-9f54-635533bb36df");
        return commentSource;
    }

    /**
     * Sets the source of the comment field value.
     * @param commentSource source of the comment field value
     * @since 1.16
     */
    public void setCommentSource(CommentSource commentSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_3_10.coverage", "e2749a5f-1af6-4c56-a8bf-453c691eeef3");
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
