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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "65c55cf0-811a-40f2-a105-65bb7677aaca");
        final ZipArchiveEntry e = (ZipArchiveEntry) super.clone();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "48cac9b1-ab18-4a2f-8ef6-70eac0dab4e0");
        e.setInternalAttributes(getInternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9dee3927-d757-4275-a42d-4f31175f8043");
        e.setExternalAttributes(getExternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "2c7bf30b-87fa-4e88-9685-697b542944f4");
        e.setExtraFields(getAllExtraFieldsNoCopy());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "b40af3e5-8ad8-4be7-a71c-aceedb470ad9");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "b89def63-26ff-4cb0-98cc-9b6cee3e0e29");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "54a8c40a-c402-4e95-b55e-128eadd7ce43");
        if (method < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "15e017f2-249f-4d43-98d8-0c8d443b41e1");
            throw new IllegalArgumentException("ZIP compression method can not be negative: " + method);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "56bd380b-c6b3-42c7-a9bd-9aa9fe8d98a3");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "d44145d2-2c51-4269-9c73-47b3b69d086d");
        return internalAttributes;
    }

    /**
     * Sets the internal file attributes.
     * @param value an <code>int</code> value
     */
    public void setInternalAttributes(final int value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c66dec57-e94d-47cc-bad0-17c4cbe2e595");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "53136915-bf2c-434e-bde2-db75e094818a");
        return externalAttributes;
    }

    /**
     * Sets the external file attributes.
     * @param value an <code>long</code> value
     */
    public void setExternalAttributes(final long value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "8f4b22cd-9e75-4a38-9e27-30ba1da16ade");
        externalAttributes = value;
    }

    /**
     * Sets Unix permissions in a way that is understood by Info-Zip's
     * unzip command.
     * @param mode an <code>int</code> value
     */
    public void setUnixMode(final int mode) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a13d2215-1fc7-4e0b-b063-a68e25c9c4e3");
        setExternalAttributes((mode << SHORT_SHIFT) | ((mode & 0200) == 0 ? 1 : 0) | (isDirectory() ? 0x10 : 0));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c0f46ba4-3aa9-49bd-8d58-a3ffdd50c894");
        platform = PLATFORM_UNIX;
    }

    /**
     * Unix permission.
     * @return the unix permissions
     */
    public int getUnixMode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "10c35fda-1899-4fd6-94a3-9a734472ab26");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "84f6051d-bdd8-4173-bc7a-b13069f78695");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "8e4b7d2f-bcd2-42b5-8fda-e0bd25a4b205");
        return platform;
    }

    /**
     * Set the platform (UNIX or FAT).
     * @param platform an <code>int</code> value - 0 is FAT, 3 is UNIX
     */
    protected void setPlatform(final int platform) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "40349ab7-0b3b-403c-96e1-7b2987154812");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "12abfd3b-e43a-46a7-92b9-993595740399");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0d789012-3f1e-4d4b-b335-4dd81825c6f2");
        if ((alignment & (alignment - 1)) != 0 || alignment > 0xffff) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "252dca05-27a1-4fd3-bd09-346445020cc4");
            throw new IllegalArgumentException("Invalid value for alignment, must be power of two and no bigger than " + 0xffff + " but is " + alignment);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a3b0ab7b-7be3-4be3-89fd-5fe6bd660d8c");
        this.alignment = alignment;
    }

    /**
     * Replaces all currently attached extra fields with the new array.
     * @param fields an array of extra fields
     */
    public void setExtraFields(final ZipExtraField[] fields) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e57536ae-1bcc-4181-a421-ac13bbd5a9a0");
        final List<ZipExtraField> newFields = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "b6219fa6-a243-4666-ac05-1dfe5f55e146");
        for (final ZipExtraField field : fields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "ff160a4e-3868-4d55-ae65-f3ec497e79e8");
            if (field instanceof UnparseableExtraFieldData) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "5ad9dbad-a4cf-4a98-bf17-e11f9d54bd2b");
                unparseableExtra = (UnparseableExtraFieldData) field;
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "78045b40-4eb7-43f0-b243-cee81dfeef37");
                newFields.add(field);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "519ff1f6-cf34-43b1-81ca-518811f79459");
        extraFields = newFields.toArray(new ZipExtraField[newFields.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "fae5b51d-497d-46f3-b460-cece6c3bfcd3");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "dc3cfe25-f43d-4a70-bbfe-7bc808e78c71");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "ece035d2-93a5-4866-9c7d-fa12b79c1c37");
        return includeUnparseable ? getAllExtraFields() : getParseableExtraFields();
    }

    private ZipExtraField[] getParseableExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "2c3d35cb-f6c2-465a-bf84-0ebb8c7cff8d");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c7dfc26c-12ca-4cce-a5de-26920e044613");
            return noExtraFields;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a0a3f635-d769-4f68-b5b1-d2852d8c624b");
        return extraFields;
    }

    private ZipExtraField[] getParseableExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "8e82b36e-2e4b-450f-b1e4-2d0d8235b726");
        final ZipExtraField[] parseableExtraFields = getParseableExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "296697e3-86ca-4f42-b099-9f6fc3248589");
        return (parseableExtraFields == extraFields) ? copyOf(parseableExtraFields) : parseableExtraFields;
    }

    /**
     * Get all extra fields, including unparseable ones.
     * @return An array of all extra fields. Not necessarily a copy of internal data structures, hence private method
     */
    private ZipExtraField[] getAllExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "3fa9af0d-52ec-4dfb-8f8f-664dab78c7a2");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e10aa90d-39cc-4c84-8523-509199acd725");
            return getUnparseableOnly();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6f46f8f8-2f8d-4363-895b-687a45be39ed");
        return unparseableExtra != null ? getMergedFields() : extraFields;
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "d03ff75c-dfda-42fb-a5cc-2e2625d1f39b");
        return copyOf(src, src.length);
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src, final int length) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "aa86a445-45c1-40d8-845e-806d97c2ca73");
        final ZipExtraField[] cpy = new ZipExtraField[length];
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "52151795-3b55-418e-a316-1266a2f8167e");
        System.arraycopy(src, 0, cpy, 0, Math.min(src.length, length));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9b0aa70c-0112-4e80-b752-ce778415d716");
        return cpy;
    }

    private ZipExtraField[] getMergedFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "178c1414-75e9-459b-b87e-d427bd41ea89");
        final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "ccc6c14e-db09-43a6-91de-7642fb37b5c8");
        zipExtraFields[extraFields.length] = unparseableExtra;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "85ef0cde-015a-4e1d-acbb-9ca88cd11c95");
        return zipExtraFields;
    }

    private ZipExtraField[] getUnparseableOnly() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a3a6ebd3-709a-48d5-942e-809eab66fb06");
        return unparseableExtra == null ? noExtraFields : new ZipExtraField[] { unparseableExtra };
    }

    private ZipExtraField[] getAllExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "af7dd889-e370-4288-befb-9f70906b05e0");
        final ZipExtraField[] allExtraFieldsNoCopy = getAllExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0987c31e-cb57-4d31-b957-669876738ac1");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "04a0a86a-8034-4044-aeed-cebe7016e2d1");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6b9a9172-8ea2-4aed-8433-17b19d1a9891");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c4c62a77-912a-4047-b4bc-d8a05e95bc86");
            if (extraFields == null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "450fc98b-d668-4300-b141-2c5907baab75");
                extraFields = new ZipExtraField[] { ze };
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "5d8f1c11-1f27-4f98-923b-c42685d35e95");
                if (getExtraField(ze.getHeaderId()) != null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7cc8a651-6175-4ec7-ab56-688a497910a8");
                    removeExtraField(ze.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "aa062502-071a-4ff6-8692-1c1202b14fec");
                final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e30c5825-289a-4c05-a5ec-8c2d6bec09e8");
                zipExtraFields[zipExtraFields.length - 1] = ze;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "3f326300-d61d-4afa-ae59-fe45855b45a6");
                extraFields = zipExtraFields;
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "944dff45-ab9b-4e44-b913-045a4e38da7a");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4c28d4a6-8986-40c6-b210-d044aab8251a");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "3d9f9c08-e83a-436a-a8e6-86f86bd970af");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c35acbb6-3608-4438-9a3b-2e13d04a75a3");
            if (getExtraField(ze.getHeaderId()) != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "11874c15-028a-440f-8a51-54f6de7d8834");
                removeExtraField(ze.getHeaderId());
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "1eda37ab-9698-47a7-98da-b090770a9fd5");
            final ZipExtraField[] copy = extraFields;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "52861454-df58-45a4-a4ea-fadccf33506e");
            final int newLen = extraFields != null ? extraFields.length + 1 : 1;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "bd7052b3-7eb3-4bec-8203-290549716189");
            extraFields = new ZipExtraField[newLen];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "f4f61f84-b9ec-4ec0-b697-37d032a7a8fb");
            extraFields[0] = ze;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "86004d7e-add5-4495-b688-e71892b6f3ee");
            if (copy != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4e30f42c-8c61-4810-b892-2d8b24ce71f8");
                System.arraycopy(copy, 0, extraFields, 1, extraFields.length - 1);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "74b18f9f-5af2-497f-a1b9-1aaa00af7a4f");
        setExtra();
    }

    /**
     * Remove an extra field.
     * @param type the type of extra field to remove
     */
    public void removeExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "70852260-a6a7-4a20-80e7-2845726cb793");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "1cdd41c5-cc00-4c20-a3c8-6a48c9b2e38f");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "2fbc1b80-8711-4927-9154-0594662dc65e");
        final List<ZipExtraField> newResult = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "8b80c44d-009c-4470-aff3-4763097b9f9a");
        for (final ZipExtraField extraField : extraFields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6be62b96-f85a-4404-b95f-7cac16120acf");
            if (!type.equals(extraField.getHeaderId())) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6215ed97-256f-4341-8f39-b74bd2fe2749");
                newResult.add(extraField);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "189c7f29-fa24-4106-be1a-8b02c592ebaa");
        if (extraFields.length == newResult.size()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c986e14c-19c0-4d38-9f2d-d08e6c1c6f1b");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "f10934cb-d037-4768-bb8a-7ad1da158142");
        extraFields = newResult.toArray(new ZipExtraField[newResult.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7235b8ea-0326-4ddb-afac-2da6c4483bd7");
        setExtra();
    }

    /**
     * Removes unparseable extra field data.
     *
     * @since 1.1
     */
    public void removeUnparseableExtraFieldData() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c3763808-2050-4e13-a0c1-1c775bd5e0ee");
        if (unparseableExtra == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4003f30c-e5f3-4c15-b6da-adb3b7a4a1a1");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c7621793-4ba2-47c3-8bce-7c8b5c05ed8e");
        unparseableExtra = null;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "daca2b5a-ddf7-4a9f-b64f-153927dc0b18");
        setExtra();
    }

    /**
     * Looks up an extra field by its header id.
     *
     * @param type the header id
     * @return null if no such field exists.
     */
    public ZipExtraField getExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "c35bc23b-cd1d-468b-b68e-692eafa99687");
        if (extraFields != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "1b755abf-52e3-4078-bdc6-5e4f6d2c1b92");
            for (final ZipExtraField extraField : extraFields) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a06856ff-f7ac-4fa2-a2ea-3a766b8fd17b");
                if (type.equals(extraField.getHeaderId())) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "cf217f83-0921-4e72-8141-ee4971b0fc6f");
                    return extraField;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "b03a283c-6bc4-4892-b1c9-06986a4697b2");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a945f49c-9e0e-4d61-96d9-614c3b2abe51");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "882e5906-f80f-4472-b114-2481c4e7d518");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "70453c4d-2730-4077-bdd4-aab74ab331dd");
            final ZipExtraField[] local = ExtraFieldUtils.parse(extra, true, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "f36c8cd4-ff93-4ce8-939b-77b3ed1d5283");
            mergeExtraFields(local, true);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "380e8eeb-7470-4679-aa62-c5365aaf67d0");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7881ebde-08af-4efe-baf0-93ab0895763f");
        super.setExtra(ExtraFieldUtils.mergeLocalFileDataData(getAllExtraFieldsNoCopy()));
    }

    /**
     * Sets the central directory part of extra fields.
     * @param b an array of bytes to be parsed into extra fields
     */
    public void setCentralDirectoryExtra(final byte[] b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "89a3a99b-365b-4317-bfee-582aaa630e25");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "85be46de-f133-46e2-97fa-c2bb581c965f");
            final ZipExtraField[] central = ExtraFieldUtils.parse(b, false, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "72731a66-fafa-4ef7-8d6c-3232a244d4bc");
            mergeExtraFields(central, false);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "cba51103-9583-4f7a-9f89-5ab18d4dd046");
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Retrieves the extra data for the local file data.
     * @return the extra data for local file
     */
    public byte[] getLocalFileDataExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0a848c50-13a5-41b9-817e-28d7235fee87");
        final byte[] extra = getExtra();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "1f7ca2fd-527c-4d39-9a26-89a6c51e4dca");
        return extra != null ? extra : EMPTY;
    }

    /**
     * Retrieves the extra data for the central directory.
     * @return the central directory extra data
     */
    public byte[] getCentralDirectoryExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4fa8d327-bc11-4fde-af7f-4ffefd98f1e4");
        return ExtraFieldUtils.mergeCentralDirectoryData(getAllExtraFieldsNoCopy());
    }

    /**
     * Get the name of the entry.
     * @return the entry name
     */
    @Override
    public String getName() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9f48d6b7-640e-4370-9625-0cdc9bbeef0b");
        return name == null ? super.getName() : name;
    }

    /**
     * Is this entry a directory?
     * @return true if the entry is a directory
     */
    @Override
    public boolean isDirectory() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "1f3b9904-58ed-482f-bbe0-a0dd875fcf23");
        return getName().endsWith("/");
    }

    /**
     * Set the name of the entry.
     * @param name the name to use
     */
    protected void setName(String name) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a7eb4750-e93f-41a2-ab4f-3fcf8cb1f39b");
        if (name != null && getPlatform() == PLATFORM_FAT && !name.contains("/")) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6d8baf11-928b-4d13-a982-c28e6d5c3bb7");
            name = name.replace('\\', '/');
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e80bbe2d-caf4-4e25-b64c-8fef2f2969dd");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "02be8d2c-74ff-4ee6-9328-71a16a3cb9d0");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "324a1517-24fc-4321-b6db-b6298682fdff");
        if (size < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7216d875-4e8a-4414-96cb-4aced232dd32");
            throw new IllegalArgumentException("invalid entry size");
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7b4aac03-5acf-4fa5-b192-8b1ca4b2eb07");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e9c9991c-deb4-4a9e-9541-fb55f1b77235");
        setName(name);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "edb6edae-5e7e-4c40-80af-001747a0cacb");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "80c1d0d6-ecf6-46ce-8974-d51c0eec65f5");
        if (rawName != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "1419ab4b-022a-4d7f-bb47-0e698baa585d");
            final byte[] b = new byte[rawName.length];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "081dbe56-1e24-4924-a9ff-87041e808c2e");
            System.arraycopy(rawName, 0, b, 0, rawName.length);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "06a15faf-53f9-4981-9869-338771474e21");
            return b;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6b8d5462-042b-4f13-9788-9947eca21cba");
        return null;
    }

    protected long getLocalHeaderOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "ced3a8b3-5d1f-426c-978a-6484fd599fe6");
        return this.localHeaderOffset;
    }

    protected void setLocalHeaderOffset(long localHeaderOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e095b4ed-91a7-4b2a-9bb3-b8251a72da0d");
        this.localHeaderOffset = localHeaderOffset;
    }

    @Override
    public long getDataOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7cfb74b9-e650-4839-8348-3f3a947ceb48");
        return dataOffset;
    }

    /**
     * Sets the data offset.
     *
     * @param dataOffset
     *      new value of data offset.
     */
    protected void setDataOffset(long dataOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "88c62ab3-98d7-46cb-8121-80be7def254d");
        this.dataOffset = dataOffset;
    }

    @Override
    public boolean isStreamContiguous() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6e59399b-56d3-48bc-b18a-492b271e1ada");
        return isStreamContiguous;
    }

    protected void setStreamContiguous(boolean isStreamContiguous) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "26e5fcd0-4e8e-4d7c-aa0f-2f579af1d3ea");
        this.isStreamContiguous = isStreamContiguous;
    }

    /**
     * Get the hashCode of the entry.
     * This uses the name as the hashcode.
     * @return a hashcode.
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6ebbad60-c086-483f-99f5-ae1b69cbca99");
        return getName().hashCode();
    }

    /**
     * The "general purpose bit" field.
     * @return the general purpose bit
     * @since 1.1
     */
    public GeneralPurposeBit getGeneralPurposeBit() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "892f16be-9f3a-431a-a6d9-28dcc530435b");
        return gpb;
    }

    /**
     * The "general purpose bit" field.
     * @param b the general purpose bit
     * @since 1.1
     */
    public void setGeneralPurposeBit(final GeneralPurposeBit b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "ba5ac644-8158-4d0a-afb5-466e2a767476");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4660ef72-97af-4c77-a8c0-451c0176ea1f");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "fc349413-df04-4074-b3da-fdacb54199ad");
            setExtraFields(f);
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4b996740-6cfc-4dbf-b3af-764a31cbfe77");
            for (final ZipExtraField element : f) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "bacdc8b5-2a38-484f-aa61-b042212a08c4");
                ZipExtraField existing;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "d08c42a0-26d8-46ea-bb0e-aa534ce634f2");
                if (element instanceof UnparseableExtraFieldData) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e96717ff-128b-4b11-aa83-243e5cdd44ba");
                    existing = unparseableExtra;
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "611a8005-5c56-445f-b013-ae0feef0b77f");
                    existing = getExtraField(element.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "cc9ae2dd-d591-403f-bfc0-3565f7413957");
                if (existing == null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "b3059034-a6ec-4f5c-92ab-02329c1e2936");
                    addExtraField(element);
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9cf4a3a6-31a3-468f-b1d1-797ef9894394");
                    if (local) {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "32f3d1f3-8ae4-4c18-8839-1c1fb6a4aaa5");
                        final byte[] b = element.getLocalFileDataData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "532ad3fc-c060-4f5d-9f0e-6b3aca602760");
                        existing.parseFromLocalFileData(b, 0, b.length);
                    } else {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "31ca8fd7-a4ff-49df-88a1-30617762fb79");
                        final byte[] b = element.getCentralDirectoryData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "f2794a07-51ae-4509-8ae5-c37f1cff97cd");
                        existing.parseFromCentralDirectoryData(b, 0, b.length);
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0f523d07-2102-4c81-b83c-cf863a880764");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "7bb4824a-0f19-4127-b53c-7422db52f24a");
        return new Date(getTime());
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "f2466b09-6668-4899-b9f2-4f86a737d060");
        if (this == obj) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0283b082-41af-43d7-8c3e-7bf5c3912911");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "eb552882-da2b-4596-a75f-df96e008c229");
        if (obj == null || getClass() != obj.getClass()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "84b8b530-10dc-4051-9eae-dab19f4ad273");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "45e95185-c284-4e64-b803-e0814726c067");
        final ZipArchiveEntry other = (ZipArchiveEntry) obj;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "d64ddee9-64f4-456d-97d1-33619486c768");
        final String myName = getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9fbb3123-1bcc-47cd-8f95-d3580b45b5ca");
        final String otherName = other.getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0dccc2b3-e631-4d8b-ad74-247664611b5d");
        if (myName == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9b46d541-8e59-4c92-84df-377c2030ef64");
            if (otherName != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "19ba6c15-3ad9-40f1-bdb2-8a6fc2bfcea8");
                return false;
            }
        } else if (!myName.equals(otherName)) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "bf589046-15cf-4aaa-b4b1-fbe892a2200e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "aa2b915f-fa0a-43ca-b00b-f5f2f6b7e454");
        String myComment = getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "6f2a3dcb-3c2b-4358-a24d-2ecc70868a64");
        String otherComment = other.getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "24378a60-a39c-4bf8-bdf1-03fdb1707de2");
        if (myComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "9b488a77-18db-4d5f-b748-2c13616564db");
            myComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "281a74a3-c57e-4c78-ad0c-33d8cfcdd0a9");
        if (otherComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "fb458a79-790a-4d42-ab90-729d5059fa79");
            otherComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "01101fec-85d2-46d2-b141-19817fab3636");
        return getTime() == other.getTime() && myComment.equals(otherComment) && getInternalAttributes() == other.getInternalAttributes() && getPlatform() == other.getPlatform() && getExternalAttributes() == other.getExternalAttributes() && getMethod() == other.getMethod() && getSize() == other.getSize() && getCrc() == other.getCrc() && getCompressedSize() == other.getCompressedSize() && Arrays.equals(getCentralDirectoryExtra(), other.getCentralDirectoryExtra()) && Arrays.equals(getLocalFileDataExtra(), other.getLocalFileDataExtra()) && localHeaderOffset == other.localHeaderOffset && dataOffset == other.dataOffset && gpb.equals(other.gpb);
    }

    /**
     * Sets the "version made by" field.
     * @param versionMadeBy "version made by" field
     * @since 1.11
     */
    public void setVersionMadeBy(final int versionMadeBy) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "4f3a4f9c-a6d0-45d5-99d0-cedc79ad4b94");
        this.versionMadeBy = versionMadeBy;
    }

    /**
     * Sets the "version required to expand" field.
     * @param versionRequired "version required to expand" field
     * @since 1.11
     */
    public void setVersionRequired(final int versionRequired) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "a3cfc1d1-f42d-4774-a02f-08271b842f43");
        this.versionRequired = versionRequired;
    }

    /**
     * The "version required to expand" field.
     * @return "version required to expand" field
     * @since 1.11
     */
    public int getVersionRequired() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "ec31c6e5-34ef-44da-8808-414620998e2a");
        return versionRequired;
    }

    /**
     * The "version made by" field.
     * @return "version made by" field
     * @since 1.11
     */
    public int getVersionMadeBy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "284fe80a-e714-45e1-aa2f-65828d5889c7");
        return versionMadeBy;
    }

    /**
     * The content of the flags field.
     * @return content of the flags field
     * @since 1.11
     */
    public int getRawFlag() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0811c831-ddd9-4df1-b31c-c5d631e39389");
        return rawFlag;
    }

    /**
     * Sets the content of the flags field.
     * @param rawFlag content of the flags field
     * @since 1.11
     */
    public void setRawFlag(final int rawFlag) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "e6d9783a-df0e-4db2-926b-f8935921c8da");
        this.rawFlag = rawFlag;
    }

    /**
     * The source of the name field value.
     * @return source of the name field value
     * @since 1.16
     */
    public NameSource getNameSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "0a7eceea-d228-42ea-bf8b-7fe5bc03373b");
        return nameSource;
    }

    /**
     * Sets the source of the name field value.
     * @param nameSource source of the name field value
     * @since 1.16
     */
    public void setNameSource(NameSource nameSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "acfe2e92-6b9b-4a4d-962c-3dd62389739d");
        this.nameSource = nameSource;
    }

    /**
     * The source of the comment field value.
     * @return source of the comment field value
     * @since 1.16
     */
    public CommentSource getCommentSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "8a9b1fe1-6895-465a-bd8d-8b98409b313e");
        return commentSource;
    }

    /**
     * Sets the source of the comment field value.
     * @param commentSource source of the comment field value
     * @since 1.16
     */
    public void setCommentSource(CommentSource commentSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_5_10.coverage", "45c406f7-67cc-42e3-b564-2494c21698c2");
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
