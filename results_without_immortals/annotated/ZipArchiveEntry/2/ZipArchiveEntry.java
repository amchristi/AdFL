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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "8a2ccacb-f54c-4494-9145-733d19da806c");
        final ZipArchiveEntry e = (ZipArchiveEntry) super.clone();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "9173734d-b9cc-48af-b0fe-ac70ed1c2d8d");
        e.setInternalAttributes(getInternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "7f32ee07-8003-40b4-b481-2dd06f309808");
        e.setExternalAttributes(getExternalAttributes());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1b887ef8-86f9-48e4-a594-f37fb21a439b");
        e.setExtraFields(getAllExtraFieldsNoCopy());
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "130ef777-2ff0-4dab-bd12-343e6e8abf40");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "4349645f-27be-4f24-a695-a8c96808a2d3");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "184ea039-fd84-4e0d-be2b-bd75dd8fbf9e");
        if (method < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "e4ecda8c-2af7-41ba-a548-c2211f68433a");
            throw new IllegalArgumentException("ZIP compression method can not be negative: " + method);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "27db5295-cb8b-41f1-bf4f-62983b1d261e");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d5c68571-7efc-4896-9925-4e0d6e8b9b9f");
        return internalAttributes;
    }

    /**
     * Sets the internal file attributes.
     * @param value an <code>int</code> value
     */
    public void setInternalAttributes(final int value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "abce226b-1323-4313-8c5c-43687479786d");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "2e6a4d18-e575-4db4-ba7c-1b7caf87532c");
        return externalAttributes;
    }

    /**
     * Sets the external file attributes.
     * @param value an <code>long</code> value
     */
    public void setExternalAttributes(final long value) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d188965e-527d-4c49-8ac5-4dc996083f4d");
        externalAttributes = value;
    }

    /**
     * Sets Unix permissions in a way that is understood by Info-Zip's
     * unzip command.
     * @param mode an <code>int</code> value
     */
    public void setUnixMode(final int mode) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "08474e80-8e7a-4d18-af86-d15d7bbb998d");
        setExternalAttributes((mode << SHORT_SHIFT) | ((mode & 0200) == 0 ? 1 : 0) | (isDirectory() ? 0x10 : 0));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "5e30849a-d25d-475b-ae59-99e940643346");
        platform = PLATFORM_UNIX;
    }

    /**
     * Unix permission.
     * @return the unix permissions
     */
    public int getUnixMode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "505fc2ea-3fcf-42fc-95c1-803ac07af96a");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "730ced6c-5a03-4211-8c1e-cb247af525bb");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "11f499f6-6f0a-4b7e-aa87-560d84058f74");
        return platform;
    }

    /**
     * Set the platform (UNIX or FAT).
     * @param platform an <code>int</code> value - 0 is FAT, 3 is UNIX
     */
    protected void setPlatform(final int platform) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d0217135-8aa4-4595-85f7-e155a26a76d1");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "a57c9c7e-798f-464c-b608-46da0ed37c1e");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f1fa0bb1-3457-4b7f-bec6-591f780d44c7");
        if ((alignment & (alignment - 1)) != 0 || alignment > 0xffff) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d0dce338-7af1-4dc5-b04a-0718ff30e545");
            throw new IllegalArgumentException("Invalid value for alignment, must be power of two and no bigger than " + 0xffff + " but is " + alignment);
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "a608a47b-b1c9-4232-9ac6-604efcfbfd09");
        this.alignment = alignment;
    }

    /**
     * Replaces all currently attached extra fields with the new array.
     * @param fields an array of extra fields
     */
    public void setExtraFields(final ZipExtraField[] fields) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "81069763-f032-487f-a7f8-c0275fdfbfd4");
        final List<ZipExtraField> newFields = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d5fa6de6-ebd4-467d-a3c7-e58ad1140a0b");
        for (final ZipExtraField field : fields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b9da00d6-d0c7-4e8d-8a05-efe666526ea5");
            if (field instanceof UnparseableExtraFieldData) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "8ac8b6b6-02a8-40b4-81ee-9a62cb7d9894");
                unparseableExtra = (UnparseableExtraFieldData) field;
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "aaecd45a-bab6-4d86-bb87-f2d69ae7fb97");
                newFields.add(field);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "7bde56fb-1214-4067-8036-440fa2a49c67");
        extraFields = newFields.toArray(new ZipExtraField[newFields.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "0458b4c7-a24a-41a3-a216-acf69eb8b364");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f5dc6c27-3f92-4d81-8bf2-4859da9cad7a");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f6b5d781-45d2-41af-bcc3-e356d4fe9ecc");
        return includeUnparseable ? getAllExtraFields() : getParseableExtraFields();
    }

    private ZipExtraField[] getParseableExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b0ba5328-1142-4a79-b72d-63ad71f23495");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "95286c08-2560-421f-b347-79132be27362");
            return noExtraFields;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d545d0dd-e58a-45c4-a3b2-13a35a0349e2");
        return extraFields;
    }

    private ZipExtraField[] getParseableExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "18a580d0-bd19-4c6f-9fce-6f141eef74e3");
        final ZipExtraField[] parseableExtraFields = getParseableExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "84ad70a0-4d5e-464e-8f9c-de9e3d1a83d1");
        return (parseableExtraFields == extraFields) ? copyOf(parseableExtraFields) : parseableExtraFields;
    }

    /**
     * Get all extra fields, including unparseable ones.
     * @return An array of all extra fields. Not necessarily a copy of internal data structures, hence private method
     */
    private ZipExtraField[] getAllExtraFieldsNoCopy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "9d698cce-ce7a-4a31-84f8-32c1fee3d7d0");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d728057a-67fb-4582-976e-99fe5a39d929");
            return getUnparseableOnly();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "36fff407-cc32-44e0-b5c0-840f279f0325");
        return unparseableExtra != null ? getMergedFields() : extraFields;
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "576bf707-3906-4a98-aa29-0abef1bcc2d8");
        return copyOf(src, src.length);
    }

    private ZipExtraField[] copyOf(final ZipExtraField[] src, final int length) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "8ed2b124-91a8-4245-8934-07bcba156aef");
        final ZipExtraField[] cpy = new ZipExtraField[length];
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "19b75bc7-16d0-4817-8ac5-1af08b756ecc");
        System.arraycopy(src, 0, cpy, 0, Math.min(src.length, length));
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f669c7c7-d1b7-4782-baf1-a67bb0ae7338");
        return cpy;
    }

    private ZipExtraField[] getMergedFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "4f36a7aa-57d3-4b81-b845-5cce7001849f");
        final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "70832405-42ec-4f7d-ac9f-548d2accb960");
        zipExtraFields[extraFields.length] = unparseableExtra;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d494af79-17b8-4f45-8ade-b28dae41aa52");
        return zipExtraFields;
    }

    private ZipExtraField[] getUnparseableOnly() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "2713cd30-72bc-4bd9-8745-e992a5a7ed71");
        return unparseableExtra == null ? noExtraFields : new ZipExtraField[] { unparseableExtra };
    }

    private ZipExtraField[] getAllExtraFields() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "652f4e98-a9df-49f3-99b9-58fab18b7444");
        final ZipExtraField[] allExtraFieldsNoCopy = getAllExtraFieldsNoCopy();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d7f18be4-54be-421f-91e7-0b60958bf859");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "4511a37d-536e-4699-922c-47be5ef336f4");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "3391b612-b8c0-47d5-b5fa-2e440307e774");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "5fe0d85a-c60f-42a4-8197-966ad1535301");
            if (extraFields == null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "ac061098-4588-4cfa-8919-e8aec4182696");
                extraFields = new ZipExtraField[] { ze };
            } else {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "ec9a5fdf-2757-4c18-ad77-46b2a767fba3");
                if (getExtraField(ze.getHeaderId()) != null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "2060e1be-be8e-40a0-996b-80999565f6bf");
                    removeExtraField(ze.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f2bef9f5-4862-4a6a-b5cc-6c8098c9797e");
                final ZipExtraField[] zipExtraFields = copyOf(extraFields, extraFields.length + 1);
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "10789cc0-c8f7-497b-a0a5-593dc2e46cad");
                zipExtraFields[zipExtraFields.length - 1] = ze;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "0e85f7ce-640b-4ea9-8a61-86c7bc6db295");
                extraFields = zipExtraFields;
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "256cc9a0-ac3c-4008-84d5-97a0549415bc");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "639227fd-cb6c-4de0-97fd-2275fbe4279e");
        if (ze instanceof UnparseableExtraFieldData) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "27a238e1-e70e-4c38-8278-7c43bfc7a841");
            unparseableExtra = (UnparseableExtraFieldData) ze;
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b093b58d-9c95-49c0-a8e1-aa017771d538");
            if (getExtraField(ze.getHeaderId()) != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "adac4e4e-9b2e-4c72-a94d-a067ff612859");
                removeExtraField(ze.getHeaderId());
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1f658d66-2c9a-4ae1-8f26-92f6e185d308");
            final ZipExtraField[] copy = extraFields;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d8fbdb8f-e3a0-4c6e-a554-b5f9bde941e0");
            final int newLen = extraFields != null ? extraFields.length + 1 : 1;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "159884f6-167d-47de-802f-8b7ec4dd1d67");
            extraFields = new ZipExtraField[newLen];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "987bf9c4-dce7-4a03-8720-609f85bd0236");
            extraFields[0] = ze;
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "7d678445-0f32-4fa5-99cf-d4a0401e2159");
            if (copy != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b39c7097-24cc-4ec1-8773-cf3a2ae2a691");
                System.arraycopy(copy, 0, extraFields, 1, extraFields.length - 1);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "8c5b31a7-8e1d-444e-9a54-c40e797e4195");
        setExtra();
    }

    /**
     * Remove an extra field.
     * @param type the type of extra field to remove
     */
    public void removeExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "022bbe5f-c6bd-455e-a273-8356403db0b4");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "840235d8-10cb-4c50-b4be-089ba798f9a4");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "60b4bb63-c98d-4cd0-9f5a-f732c1ef6b18");
        final List<ZipExtraField> newResult = new ArrayList<ZipExtraField>();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "15e1093d-b91e-4008-a586-7cf303f71208");
        for (final ZipExtraField extraField : extraFields) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "70adb374-6538-4f26-a4e3-8004541f82eb");
            if (!type.equals(extraField.getHeaderId())) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "e2d5936f-fe49-4cd8-a79c-d1c20e04fe26");
                newResult.add(extraField);
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "520f2ba9-dde9-4d37-b62d-64cdc0188145");
        if (extraFields.length == newResult.size()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "3de398ce-e112-4372-a42b-0e1742c1b23e");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "8bd82376-9695-42b8-86c1-426216d30f1f");
        extraFields = newResult.toArray(new ZipExtraField[newResult.size()]);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "4618ce77-044b-4396-9e33-831156ccfab4");
        setExtra();
    }

    /**
     * Removes unparseable extra field data.
     *
     * @since 1.1
     */
    public void removeUnparseableExtraFieldData() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "78c2af41-31d0-4cc4-b1b2-2535702966b9");
        if (unparseableExtra == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "80e329a0-43d2-4952-92ef-1e6e9dfbebef");
            throw new java.util.NoSuchElementException();
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "0a861eb7-450d-4b83-a39a-0b95c8f26cf8");
        unparseableExtra = null;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "6e7c3383-e2a9-44b6-ab6c-fec386a753e6");
        setExtra();
    }

    /**
     * Looks up an extra field by its header id.
     *
     * @param type the header id
     * @return null if no such field exists.
     */
    public ZipExtraField getExtraField(final ZipShort type) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "32fe9b9c-fd16-4f15-afa8-c06a14b1a516");
        if (extraFields != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "47ca0d76-2623-49b6-8f64-a7e833e12f6e");
            for (final ZipExtraField extraField : extraFields) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "bd7cd54c-d10c-4227-826e-eb008a4626a2");
                if (type.equals(extraField.getHeaderId())) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "540b38f7-ed5a-4a70-96fd-f16925bad26d");
                    return extraField;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b077d7e7-05ce-49ad-b0ac-65bbfae8f606");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "423e3431-8345-4d63-9109-9e719c55ed28");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "59b38805-ce73-449c-8a08-c5f975d3eb43");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "3cfa50fc-b217-4762-bfb2-78744326fd88");
            final ZipExtraField[] local = ExtraFieldUtils.parse(extra, true, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "4ce24ee1-a443-4075-9909-f3e2dafd3ec1");
            mergeExtraFields(local, true);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "7d1ef115-c073-4b4f-b518-ddc043820ac6");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1dd44695-d82b-4e8a-abac-6ddc607c36d3");
        super.setExtra(ExtraFieldUtils.mergeLocalFileDataData(getAllExtraFieldsNoCopy()));
    }

    /**
     * Sets the central directory part of extra fields.
     * @param b an array of bytes to be parsed into extra fields
     */
    public void setCentralDirectoryExtra(final byte[] b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "635ff64a-f789-4153-bf27-98392df359d1");
        try {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "2a94e4cf-5fae-4b0f-bbc9-fbe1e752f9d7");
            final ZipExtraField[] central = ExtraFieldUtils.parse(b, false, ExtraFieldUtils.UnparseableExtraField.READ);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "9b725d50-29ab-4cb9-ab14-7e410f0bb51f");
            mergeExtraFields(central, false);
        } catch (final ZipException e) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "da2f1735-4a66-4616-884b-6a625e83d100");
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Retrieves the extra data for the local file data.
     * @return the extra data for local file
     */
    public byte[] getLocalFileDataExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d8011fe5-6239-4df2-923e-05762f2ce849");
        final byte[] extra = getExtra();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f8666ef7-350c-4fa8-8db9-cabe4b5a6b88");
        return extra != null ? extra : EMPTY;
    }

    /**
     * Retrieves the extra data for the central directory.
     * @return the central directory extra data
     */
    public byte[] getCentralDirectoryExtra() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "38ff7e11-7ced-423f-80ad-f73abff92fe6");
        return ExtraFieldUtils.mergeCentralDirectoryData(getAllExtraFieldsNoCopy());
    }

    /**
     * Get the name of the entry.
     * @return the entry name
     */
    @Override
    public String getName() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "dc8234e3-e287-4ea8-9fed-c584f5a071d2");
        return name == null ? super.getName() : name;
    }

    /**
     * Is this entry a directory?
     * @return true if the entry is a directory
     */
    @Override
    public boolean isDirectory() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "a55e1147-641e-45fa-b612-da317ec31af9");
        return getName().endsWith("/");
    }

    /**
     * Set the name of the entry.
     * @param name the name to use
     */
    protected void setName(String name) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "2408a7c3-49a2-4d9d-9a2e-a4064ac6e36d");
        if (name != null && getPlatform() == PLATFORM_FAT && !name.contains("/")) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "7832b657-3b39-4907-9225-375275776ce1");
            name = name.replace('\\', '/');
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "0c2eb01d-0433-406b-bf74-ec36bd010010");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "0f212a90-41e7-4416-85a0-d8359b9a09f0");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "bda625d7-4b61-43a7-b9ed-5e26298cb009");
        if (size < 0) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "2c2aad30-1efb-4324-ad78-75df9c05d841");
            throw new IllegalArgumentException("invalid entry size");
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "ebe45cce-5cc5-46fd-b87e-1319cc3e0fa2");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "0328154e-f1b6-4775-a696-a04e7466b06e");
        setName(name);
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "f4fa6ff1-6633-44c4-a03c-69954c553319");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "ca3b7353-520d-4a6d-b4e8-3fd49c0806c0");
        if (rawName != null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d5610631-eb95-4356-80a3-e42cf1ef21c6");
            final byte[] b = new byte[rawName.length];
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "245e3c98-f692-46a3-88f7-ec1a3cb53d8b");
            System.arraycopy(rawName, 0, b, 0, rawName.length);
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "49da4fde-e571-4706-8898-b1a5014b9c21");
            return b;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "6b965070-aa3c-4e6c-a812-413de120d2c2");
        return null;
    }

    protected long getLocalHeaderOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "76104214-47e2-4825-9344-f73db2cd394e");
        return this.localHeaderOffset;
    }

    protected void setLocalHeaderOffset(long localHeaderOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "ac46ed1d-d5fc-4c69-ad82-9fc1895c53f2");
        this.localHeaderOffset = localHeaderOffset;
    }

    @Override
    public long getDataOffset() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "602ed965-e432-481f-a93f-2baba8a7e2c5");
        return dataOffset;
    }

    /**
     * Sets the data offset.
     *
     * @param dataOffset
     *      new value of data offset.
     */
    protected void setDataOffset(long dataOffset) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b7cf29d8-5cc1-4322-a826-03b7be02fd5d");
        this.dataOffset = dataOffset;
    }

    @Override
    public boolean isStreamContiguous() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "3a1a086a-30bc-4551-8426-2e7d8e950939");
        return isStreamContiguous;
    }

    protected void setStreamContiguous(boolean isStreamContiguous) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "bc8242c5-c9f2-4974-bb64-20c942a0d281");
        this.isStreamContiguous = isStreamContiguous;
    }

    /**
     * Get the hashCode of the entry.
     * This uses the name as the hashcode.
     * @return a hashcode.
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "9e492ce3-2aec-4bab-a08b-1a665b26da01");
        return getName().hashCode();
    }

    /**
     * The "general purpose bit" field.
     * @return the general purpose bit
     * @since 1.1
     */
    public GeneralPurposeBit getGeneralPurposeBit() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "bf4d9ca1-30a3-46ac-a85c-3171eac7b840");
        return gpb;
    }

    /**
     * The "general purpose bit" field.
     * @param b the general purpose bit
     * @since 1.1
     */
    public void setGeneralPurposeBit(final GeneralPurposeBit b) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "a4d6633c-87f9-4c3c-b76b-91bec0d9f56a");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "cbdd58d7-b342-4702-b010-0304daa7eca3");
        if (extraFields == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "bf8d57de-bcd9-4201-8eb7-b2c356f3f48f");
            setExtraFields(f);
        } else {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "024275ba-1f8e-4524-b4e7-8c124a4286b7");
            for (final ZipExtraField element : f) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1fd7e59a-a890-4fad-9b12-5694509f0c1e");
                ZipExtraField existing;
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b95090e3-1683-404c-9801-7d690aee23b7");
                if (element instanceof UnparseableExtraFieldData) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "20adbe84-60f1-4077-a6ba-bc286b33229c");
                    existing = unparseableExtra;
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "79721f5d-a413-4cbf-b293-6f509c052beb");
                    existing = getExtraField(element.getHeaderId());
                }
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "160fe90b-11de-47f9-96f8-a00965edafe4");
                if (existing == null) {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "fabb2753-b674-40b0-b9ec-498accc9f0f1");
                    addExtraField(element);
                } else {
                    writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "e7a69afc-c647-4147-89bb-17415dd41c20");
                    if (local) {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1f855f17-0f2d-43ae-8052-aee52b1bb93f");
                        final byte[] b = element.getLocalFileDataData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "896f3620-f4bd-4276-8f38-567588e46419");
                        existing.parseFromLocalFileData(b, 0, b.length);
                    } else {
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d15af3ea-1714-4ab0-9081-8aebde16377c");
                        final byte[] b = element.getCentralDirectoryData();
                        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d5d03756-c5c5-4087-a5be-78c3c8921fbd");
                        existing.parseFromCentralDirectoryData(b, 0, b.length);
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b48dcef0-31cf-45b9-9717-7c115b9f449e");
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
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "dbfdcc7b-6cf7-4cd8-b351-2a58c3cfe27f");
        return new Date(getTime());
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "565df30e-971a-47cd-876f-9eb56cf99b9e");
        if (this == obj) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "50a5bfbd-b677-4837-91d6-3492797aa4c7");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "a9850300-11fb-4c4e-9eab-bb80f8a7249c");
        if (obj == null || getClass() != obj.getClass()) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1f495d54-0f16-4a26-954a-0eb35fa46fbe");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b9dd68b5-04b9-4c4a-9e58-662e676f8c0d");
        final ZipArchiveEntry other = (ZipArchiveEntry) obj;
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "5896138e-515c-424e-90b4-5360d69d8bec");
        final String myName = getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "1e760afc-b72d-423f-85f9-f258e3d07a2f");
        final String otherName = other.getName();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "22face2c-bdee-4f09-a403-a77b38e5803e");
        if (myName == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "9d1765a9-58ff-4841-b622-72dcf3a8713f");
            if (otherName != null) {
                writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "5c0bbde3-665d-40f0-b6b2-2ade50646a07");
                return false;
            }
        } else if (!myName.equals(otherName)) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "62297cbb-9007-41d9-9d5c-0bd726146b7c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "d21d2f06-5641-4ad5-a01e-f2e6fa5361c5");
        String myComment = getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "c8edd756-fd6f-48e2-ac71-e1bcffda298d");
        String otherComment = other.getComment();
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "7ca21266-3e72-45fd-b3e1-c9a2d5dd1765");
        if (myComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "3a5888c5-7186-45a8-9164-fb3369108bb1");
            myComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "ca5efa9e-7ff5-47d7-a618-25969f8ede11");
        if (otherComment == null) {
            writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "61c06dee-05e8-4542-8290-319c39c4c7c2");
            otherComment = "";
        }
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "33c313ef-5460-45f1-b5e0-32c850e68fca");
        return getTime() == other.getTime() && myComment.equals(otherComment) && getInternalAttributes() == other.getInternalAttributes() && getPlatform() == other.getPlatform() && getExternalAttributes() == other.getExternalAttributes() && getMethod() == other.getMethod() && getSize() == other.getSize() && getCrc() == other.getCrc() && getCompressedSize() == other.getCompressedSize() && Arrays.equals(getCentralDirectoryExtra(), other.getCentralDirectoryExtra()) && Arrays.equals(getLocalFileDataExtra(), other.getLocalFileDataExtra()) && localHeaderOffset == other.localHeaderOffset && dataOffset == other.dataOffset && gpb.equals(other.gpb);
    }

    /**
     * Sets the "version made by" field.
     * @param versionMadeBy "version made by" field
     * @since 1.11
     */
    public void setVersionMadeBy(final int versionMadeBy) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "102df2d5-5541-496a-a8b1-678a7388409c");
        this.versionMadeBy = versionMadeBy;
    }

    /**
     * Sets the "version required to expand" field.
     * @param versionRequired "version required to expand" field
     * @since 1.11
     */
    public void setVersionRequired(final int versionRequired) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "de7f0103-4a9c-44a9-83fd-9ff0cd478b7d");
        this.versionRequired = versionRequired;
    }

    /**
     * The "version required to expand" field.
     * @return "version required to expand" field
     * @since 1.11
     */
    public int getVersionRequired() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "33c30c01-fc9f-4e66-8fba-fc7a206c2035");
        return versionRequired;
    }

    /**
     * The "version made by" field.
     * @return "version made by" field
     * @since 1.11
     */
    public int getVersionMadeBy() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b7079dda-f1e8-48ce-a88f-ca588bb97105");
        return versionMadeBy;
    }

    /**
     * The content of the flags field.
     * @return content of the flags field
     * @since 1.11
     */
    public int getRawFlag() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "70ac3d93-9c1b-4c6a-8bb0-0bcc57fab7ca");
        return rawFlag;
    }

    /**
     * Sets the content of the flags field.
     * @param rawFlag content of the flags field
     * @since 1.11
     */
    public void setRawFlag(final int rawFlag) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "b819e92e-ef74-4557-9b89-1c31cb2700fe");
        this.rawFlag = rawFlag;
    }

    /**
     * The source of the name field value.
     * @return source of the name field value
     * @since 1.16
     */
    public NameSource getNameSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "01ba4092-6c8a-4aae-ba40-7232b7973876");
        return nameSource;
    }

    /**
     * Sets the source of the name field value.
     * @param nameSource source of the name field value
     * @since 1.16
     */
    public void setNameSource(NameSource nameSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "bb30251c-13e5-4445-b832-540b63c71796");
        this.nameSource = nameSource;
    }

    /**
     * The source of the comment field value.
     * @return source of the comment field value
     * @since 1.16
     */
    public CommentSource getCommentSource() {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "73936405-8e31-446c-9b7a-a32883aec036");
        return commentSource;
    }

    /**
     * Sets the source of the comment field value.
     * @param commentSource source of the comment field value
     * @since 1.16
     */
    public void setCommentSource(CommentSource commentSource) {
        writeline("/home/ubuntu/results/coverage/ZipArchiveEntry/ZipArchiveEntry_2_10.coverage", "4ebf1bf8-cb57-4777-b50b-357b7a7f4bcd");
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
