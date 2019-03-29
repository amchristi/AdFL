package org.apache.commons.compress.archivers.zip;

import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import java.util.zip.ZipException;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;
import org.apache.commons.compress.compressors.deflate64.Deflate64CompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import static org.apache.commons.compress.archivers.zip.ZipConstants.DWORD;
import static org.apache.commons.compress.archivers.zip.ZipConstants.SHORT;
import static org.apache.commons.compress.archivers.zip.ZipConstants.WORD;
import static org.apache.commons.compress.archivers.zip.ZipConstants.ZIP64_MAGIC;
import static org.apache.commons.compress.archivers.zip.ZipConstants.ZIP64_MAGIC_SHORT;
import java.io.*;

/**
 * Replacement for <code>java.util.ZipFile</code>.
 *
 * <p>This class adds support for file name encodings other than UTF-8
 * (which is required to work on ZIP files created by native zip tools
 * and is able to skip a preamble like the one found in self
 * extracting archives.  Furthermore it returns instances of
 * <code>org.apache.commons.compress.archivers.zip.ZipArchiveEntry</code>
 * instead of <code>java.util.zip.ZipEntry</code>.</p>
 *
 * <p>It doesn't extend <code>java.util.zip.ZipFile</code> as it would
 * have to reimplement all methods anyway.  Like
 * <code>java.util.ZipFile</code>, it uses SeekableByteChannel under the
 * covers and supports compressed and uncompressed entries.  As of
 * Apache Commons Compress 1.3 it also transparently supports Zip64
 * extensions and thus individual entries and archives larger than 4
 * GB or with more than 65536 entries.</p>
 *
 * <p>The method signatures mimic the ones of
 * <code>java.util.zip.ZipFile</code>, with a couple of exceptions:
 *
 * <ul>
 *   <li>There is no getName method.</li>
 *   <li>entries has been renamed to getEntries.</li>
 *   <li>getEntries and getEntry return
 *   <code>org.apache.commons.compress.archivers.zip.ZipArchiveEntry</code>
 *   instances.</li>
 *   <li>close is allowed to throw IOException.</li>
 * </ul>
 *
 */
public class ZipFile implements Closeable {

    private static final int HASH_SIZE = 509;

    static final int NIBLET_MASK = 0x0f;

    static final int BYTE_SHIFT = 8;

    private static final int POS_0 = 0;

    private static final int POS_1 = 1;

    private static final int POS_2 = 2;

    private static final int POS_3 = 3;

    /**
     * List of entries in the order they appear inside the central
     * directory.
     */
    private final List<ZipArchiveEntry> entries = new LinkedList<ZipArchiveEntry>();

    /**
     * Maps String to list of ZipArchiveEntrys, name -> actual entries.
     */
    private final Map<String, LinkedList<ZipArchiveEntry>> nameMap = new HashMap<String, LinkedList<ZipArchiveEntry>>(HASH_SIZE);

    /**
     * The encoding to use for filenames and the file comment.
     *
     * <p>For a list of possible values see <a
     * href="http://java.sun.com/j2se/1.5.0/docs/guide/intl/encoding.doc.html">http://java.sun.com/j2se/1.5.0/docs/guide/intl/encoding.doc.html</a>.
     * Defaults to UTF-8.</p>
     */
    private final String encoding;

    /**
     * The zip encoding to use for filenames and the file comment.
     */
    private final ZipEncoding zipEncoding;

    /**
     * File name of actual source.
     */
    private final String archiveName;

    /**
     * The actual data source.
     */
    private final SeekableByteChannel archive;

    /**
     * Whether to look for and use Unicode extra fields.
     */
    private final boolean useUnicodeExtraFields;

    /**
     * Whether the file is closed.
     */
    private volatile boolean closed = true;

    private final byte[] dwordBuf = new byte[DWORD];

    private final byte[] wordBuf = new byte[WORD];

    private final byte[] cfhBuf = new byte[CFH_LEN];

    private final byte[] shortBuf = new byte[SHORT];

    private final ByteBuffer dwordBbuf = ByteBuffer.wrap(dwordBuf);

    private final ByteBuffer wordBbuf = ByteBuffer.wrap(wordBuf);

    private final ByteBuffer cfhBbuf = ByteBuffer.wrap(cfhBuf);

    /**
     * Opens the given file for reading, assuming "UTF8" for file names.
     *
     * @param f the archive.
     *
     * @throws IOException if an error occurs while reading the file.
     */
    public ZipFile(final File f) throws IOException {
        this(f, ZipEncodingHelper.UTF8);
    }

    /**
     * Opens the given file for reading, assuming "UTF8".
     *
     * @param name name of the archive.
     *
     * @throws IOException if an error occurs while reading the file.
     */
    public ZipFile(final String name) throws IOException {
        this(new File(name), ZipEncodingHelper.UTF8);
    }

    /**
     * Opens the given file for reading, assuming the specified
     * encoding for file names, scanning unicode extra fields.
     *
     * @param name name of the archive.
     * @param encoding the encoding to use for file names, use null
     * for the platform's default encoding
     *
     * @throws IOException if an error occurs while reading the file.
     */
    public ZipFile(final String name, final String encoding) throws IOException {
        this(new File(name), encoding, true);
    }

    /**
     * Opens the given file for reading, assuming the specified
     * encoding for file names and scanning for unicode extra fields.
     *
     * @param f the archive.
     * @param encoding the encoding to use for file names, use null
     * for the platform's default encoding
     *
     * @throws IOException if an error occurs while reading the file.
     */
    public ZipFile(final File f, final String encoding) throws IOException {
        this(f, encoding, true);
    }

    /**
     * Opens the given file for reading, assuming the specified
     * encoding for file names.
     *
     * @param f the archive.
     * @param encoding the encoding to use for file names, use null
     * for the platform's default encoding
     * @param useUnicodeExtraFields whether to use InfoZIP Unicode
     * Extra Fields (if present) to set the file names.
     *
     * @throws IOException if an error occurs while reading the file.
     */
    public ZipFile(final File f, final String encoding, final boolean useUnicodeExtraFields) throws IOException {
        this(Files.newByteChannel(f.toPath(), EnumSet.of(StandardOpenOption.READ)), f.getAbsolutePath(), encoding, useUnicodeExtraFields, true);
    }

    /**
     * Opens the given channel for reading, assuming "UTF8" for file names.
     *
     * <p>{@link
     * org.apache.commons.compress.utils.SeekableInMemoryByteChannel}
     * allows you to read from an in-memory archive.</p>
     *
     * @param channel the archive.
     *
     * @throws IOException if an error occurs while reading the file.
     * @since 1.13
     */
    public ZipFile(final SeekableByteChannel channel) throws IOException {
        this(channel, "unknown archive", ZipEncodingHelper.UTF8, true);
    }

    /**
     * Opens the given channel for reading, assuming the specified
     * encoding for file names.
     *
     * <p>{@link
     * org.apache.commons.compress.utils.SeekableInMemoryByteChannel}
     * allows you to read from an in-memory archive.</p>
     *
     * @param channel the archive.
     * @param encoding the encoding to use for file names, use null
     * for the platform's default encoding
     *
     * @throws IOException if an error occurs while reading the file.
     * @since 1.13
     */
    public ZipFile(final SeekableByteChannel channel, final String encoding) throws IOException {
        this(channel, "unknown archive", encoding, true);
    }

    /**
     * Opens the given channel for reading, assuming the specified
     * encoding for file names.
     *
     * <p>{@link
     * org.apache.commons.compress.utils.SeekableInMemoryByteChannel}
     * allows you to read from an in-memory archive.</p>
     *
     * @param channel the archive.
     * @param archiveName name of the archive, used for error messages only.
     * @param encoding the encoding to use for file names, use null
     * for the platform's default encoding
     * @param useUnicodeExtraFields whether to use InfoZIP Unicode
     * Extra Fields (if present) to set the file names.
     *
     * @throws IOException if an error occurs while reading the file.
     * @since 1.13
     */
    public ZipFile(final SeekableByteChannel channel, final String archiveName, final String encoding, final boolean useUnicodeExtraFields) throws IOException {
        this(channel, archiveName, encoding, useUnicodeExtraFields, false);
    }

    private ZipFile(final SeekableByteChannel channel, final String archiveName, final String encoding, final boolean useUnicodeExtraFields, final boolean closeOnError) throws IOException {
        this.archiveName = archiveName;
        this.encoding = encoding;
        this.zipEncoding = ZipEncodingHelper.getZipEncoding(encoding);
        this.useUnicodeExtraFields = useUnicodeExtraFields;
        archive = channel;
        boolean success = false;
        try {
            final Map<ZipArchiveEntry, NameAndComment> entriesWithoutUTF8Flag = populateFromCentralDirectory();
            resolveLocalFileHeaderData(entriesWithoutUTF8Flag);
            success = true;
        } finally {
            closed = !success;
            if (!success && closeOnError) {
                IOUtils.closeQuietly(archive);
            }
        }
    }

    /**
     * The encoding to use for filenames and the file comment.
     *
     * @return null if using the platform's default character encoding.
     */
    public String getEncoding() {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "49878f5c-79f8-43df-8dff-ee68463038b8");
        return encoding;
    }

    /**
     * Closes the archive.
     * @throws IOException if an error occurs closing the archive.
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "5fad9bbb-dea9-4ef6-aca6-fd2710e29a0a");
        closed = true;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "40d70739-ddb6-4ecd-8988-9a3d3377dc9f");
        archive.close();
    }

    /**
     * close a zipfile quietly; throw no io fault, do nothing
     * on a null parameter
     * @param zipfile file to close, can be null
     */
    public static void closeQuietly(final ZipFile zipfile) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2df3faf0-ee2d-47c4-a045-243f2412e867");
        IOUtils.closeQuietly(zipfile);
    }

    /**
     * Returns all entries.
     *
     * <p>Entries will be returned in the same order they appear
     * within the archive's central directory.</p>
     *
     * @return all entries as {@link ZipArchiveEntry} instances
     */
    public Enumeration<ZipArchiveEntry> getEntries() {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0be9b114-4ae8-468a-a61b-9901f787c950");
        return Collections.enumeration(entries);
    }

    /**
     * Returns all entries in physical order.
     *
     * <p>Entries will be returned in the same order their contents
     * appear within the archive.</p>
     *
     * @return all entries as {@link ZipArchiveEntry} instances
     *
     * @since 1.1
     */
    public Enumeration<ZipArchiveEntry> getEntriesInPhysicalOrder() {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1eb564ae-ba4d-4375-9162-0b25b1a2c5e6");
        final ZipArchiveEntry[] allEntries = entries.toArray(new ZipArchiveEntry[entries.size()]);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "16af2ce7-cffe-4a3a-aded-f33a2c00b9cb");
        Arrays.sort(allEntries, offsetComparator);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "225d8a2e-328b-42d7-9235-67d4139bfe49");
        return Collections.enumeration(Arrays.asList(allEntries));
    }

    /**
     * Returns a named entry - or {@code null} if no entry by
     * that name exists.
     *
     * <p>If multiple entries with the same name exist the first entry
     * in the archive's central directory by that name is
     * returned.</p>
     *
     * @param name name of the entry.
     * @return the ZipArchiveEntry corresponding to the given name - or
     * {@code null} if not present.
     */
    public ZipArchiveEntry getEntry(final String name) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e1edb5c6-f96b-4c8a-8146-acca5e4a7be7");
        final LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c5eef34a-23c3-4403-8350-6fb3f9e5a11b");
        return entriesOfThatName != null ? entriesOfThatName.getFirst() : null;
    }

    /**
     * Returns all named entries in the same order they appear within
     * the archive's central directory.
     *
     * @param name name of the entry.
     * @return the Iterable&lt;ZipArchiveEntry&gt; corresponding to the
     * given name
     * @since 1.6
     */
    public Iterable<ZipArchiveEntry> getEntries(final String name) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "709c2203-db86-4e58-8de4-dcf535a8439c");
        final List<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3aeb6b1a-ec9c-4206-a044-f8b18f90be70");
        return entriesOfThatName != null ? entriesOfThatName : Collections.<ZipArchiveEntry>emptyList();
    }

    /**
     * Returns all named entries in the same order their contents
     * appear within the archive.
     *
     * @param name name of the entry.
     * @return the Iterable&lt;ZipArchiveEntry&gt; corresponding to the
     * given name
     * @since 1.6
     */
    public Iterable<ZipArchiveEntry> getEntriesInPhysicalOrder(final String name) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "28eaf651-3eb3-4ef2-86a6-a1a8e9c978a3");
        ZipArchiveEntry[] entriesOfThatName = new ZipArchiveEntry[0];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e92b6bd9-497c-4626-8273-579adc93e0a9");
        if (nameMap.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "626f35d4-cfda-4122-b94b-75e3821f0b8d");
            entriesOfThatName = nameMap.get(name).toArray(entriesOfThatName);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "086c0f55-8c46-42aa-a2ec-50409fe30053");
            Arrays.sort(entriesOfThatName, offsetComparator);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a38ab7af-2882-4a6e-8931-b3da8c8e14c3");
        return Arrays.asList(entriesOfThatName);
    }

    /**
     * Whether this class is able to read the given entry.
     *
     * <p>May return false if it is set up to use encryption or a
     * compression method that hasn't been implemented yet.</p>
     * @since 1.1
     * @param ze the entry
     * @return whether this class is able to read the given entry.
     */
    public boolean canReadEntryData(final ZipArchiveEntry ze) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6d93b440-9b14-46f7-91f5-c689d53891b0");
        return ZipUtil.canHandleEntryData(ze);
    }

    /**
     * Expose the raw stream of the archive entry (compressed form).
     *
     * <p>This method does not relate to how/if we understand the payload in the
     * stream, since we really only intend to move it on to somewhere else.</p>
     *
     * @param ze The entry to get the stream for
     * @return The raw input stream containing (possibly) compressed data.
     * @since 1.11
     */
    public InputStream getRawInputStream(final ZipArchiveEntry ze) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0cda8bb9-68a2-422d-92ce-d7b72c1e6809");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "da00a569-5ea0-4665-afba-4d530226761c");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a317b405-4104-4db2-b9bf-73bfd7709350");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "92221822-60c0-481b-8b89-2efde747aae9");
        return createBoundedInputStream(start, ze.getCompressedSize());
    }

    /**
     * Transfer selected entries from this zipfile to a given #ZipArchiveOutputStream.
     * Compression and all other attributes will be as in this file.
     * <p>This method transfers entries based on the central directory of the zip file.</p>
     *
     * @param target The zipArchiveOutputStream to write the entries to
     * @param predicate A predicate that selects which entries to write
     * @throws IOException on error
     */
    public void copyRawEntries(final ZipArchiveOutputStream target, final ZipArchiveEntryPredicate predicate) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "64ec165b-73d2-4f6b-aca6-9bf2b8796167");
        final Enumeration<ZipArchiveEntry> src = getEntriesInPhysicalOrder();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "54f84b12-6b98-4eee-b16d-9820c4b51e9b");
        while (src.hasMoreElements()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1fe59c93-a68b-435f-892b-844bb7f83f02");
            final ZipArchiveEntry entry = src.nextElement();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4a5a82b6-ab4f-403f-9f7a-caedbb8f0e55");
            if (predicate.test(entry)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "04d88c69-e901-4b69-b6f8-601dda2e776a");
                target.addRawArchiveEntry(entry, getRawInputStream(entry));
            }
        }
    }

    /**
     * Returns an InputStream for reading the contents of the given entry.
     *
     * @param ze the entry to get the stream for.
     * @return a stream to read the entry from.
     * @throws IOException if unable to create an input stream from the zipentry
     * @throws ZipException if the zipentry uses an unsupported feature
     */
    public InputStream getInputStream(final ZipArchiveEntry ze) throws IOException, ZipException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f708ff90-317e-42b8-a74c-d0ecf3af0d2a");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "46a0c16c-8755-4a4e-b27e-4d42a1e7d3d1");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "862416af-bed4-475f-a2f0-5d1cc4e43217");
        ZipUtil.checkRequestedFeatures(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a58b9d98-dc63-4089-bd3e-bdca90639a58");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "047ef797-d59b-43f7-bc47-1c3eb8ac7906");
        final BoundedInputStream bis = createBoundedInputStream(start, ze.getCompressedSize());
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a580e90a-9217-4f9d-b35c-22e53253006d");
        switch(ZipMethod.getMethodByCode(ze.getMethod())) {
            case STORED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2d35eadf-3262-4bbb-b0da-6983c964a907");
                return bis;
            case UNSHRINKING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "93a5babf-a068-4c9f-bbfa-1b5b7a8ecee6");
                return new UnshrinkingInputStream(bis);
            case IMPLODING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "398a6809-2532-4cef-8963-3b2b8d27ab44");
                return new ExplodingInputStream(ze.getGeneralPurposeBit().getSlidingDictionarySize(), ze.getGeneralPurposeBit().getNumberOfShannonFanoTrees(), new BufferedInputStream(bis));
            case DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "156dc3b5-db15-42c8-8c13-91c1c9e6a436");
                bis.addDummy();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9b3d47c6-1b0e-4130-a07a-68153bbc7910");
                final Inflater inflater = new Inflater(true);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0325a9ca-ec23-4ab7-90a3-0ffd1507c7c8");
                return new InflaterInputStream(bis, inflater) {

                    @Override
                    public void close() throws IOException {
                        try {
                            super.close();
                        } finally {
                            inflater.end();
                        }
                    }
                };
            case BZIP2:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "cc0fb07d-8203-497c-a035-285d38467687");
                return new BZip2CompressorInputStream(bis);
            case ENHANCED_DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "09de428f-8de8-472e-9ff7-545c8857ca7d");
                return new Deflate64CompressorInputStream(bis);
            default:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6b1a9ca6-df7c-43af-9b78-ed9149e4e0bb");
                throw new ZipException("Found unsupported compression method " + ze.getMethod());
        }
    }

    /**
     * <p>
     * Convenience method to return the entry's content as a String if isUnixSymlink()
     * returns true for it, otherwise returns null.
     * </p>
     *
     * <p>This method assumes the symbolic link's file name uses the
     * same encoding that as been specified for this ZipFile.</p>
     *
     * @param entry ZipArchiveEntry object that represents the symbolic link
     * @return entry's content as a String
     * @throws IOException problem with content's input stream
     * @since 1.5
     */
    public String getUnixSymlink(final ZipArchiveEntry entry) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2a495052-2d88-40f6-9ca6-c08707253de1");
        if (entry != null && entry.isUnixSymlink()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "420f1ffe-a142-487f-b589-79caf32155c9");
            try {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "491f1b29-cb24-44dd-8b3c-5f97132ddcbb");
                InputStream in = getInputStream(entry);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4a2b8c7b-809a-49f4-a5d2-b7d58f3a4790");
                return zipEncoding.decode(IOUtils.toByteArray(in));
            } catch (Exception ex) {
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ee2b0f13-5417-4ff9-96d3-dd1b7b42568e");
        return null;
    }

    /**
     * Ensures that the close method of this zipfile is called when
     * there are no more references to it.
     * @see #close()
     */
    @Override
    protected void finalize() throws Throwable {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "01aee37b-beca-4d5c-bfc6-780ca5437c7e");
        try {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2a08bbd1-9309-45d6-a9f8-31f74e73a4df");
            if (!closed) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3cca089b-4f90-4ffb-a95a-a55214abb3c9");
                System.err.println("Cleaning up unclosed ZipFile for archive " + archiveName);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3db8340d-5c17-4499-aa1c-d55242629c44");
                close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3032718f-7f7b-4fdc-a4b9-7e88bf1abd7c");
            super.finalize();
        }
    }

    /**
     * Length of a "central directory" entry structure without file
     * name, extra fields or comment.
     */
    private static final int CFH_LEN = SHORT + SHORT + SHORT + SHORT + SHORT + SHORT + WORD + WORD + WORD + SHORT + SHORT + SHORT + SHORT + SHORT + WORD + WORD;

    private static final long CFH_SIG = ZipLong.getValue(ZipArchiveOutputStream.CFH_SIG);

    /**
     * Reads the central directory of the given archive and populates
     * the internal tables with ZipArchiveEntry instances.
     *
     * <p>The ZipArchiveEntrys will know all data that can be obtained from
     * the central directory alone, but not the data that requires the
     * local file header or additional data to be read.</p>
     *
     * @return a map of zipentries that didn't have the language
     * encoding flag set when read.
     */
    private Map<ZipArchiveEntry, NameAndComment> populateFromCentralDirectory() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "19d135c7-9c0e-443a-b551-823e4b753ca9");
        final HashMap<ZipArchiveEntry, NameAndComment> noUTF8Flag = new HashMap<ZipArchiveEntry, NameAndComment>();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3c2e6acc-d1ae-465b-b80a-7fd12fcbe424");
        positionAtCentralDirectory();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "5fda7885-0bf3-423a-9a10-cb8e8bcc18e3");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c7071757-5c67-4d35-95ac-4880ec0aa77b");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ac757081-4898-4b92-afe2-47957ff019a4");
        long sig = ZipLong.getValue(wordBuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "37a93cfd-4804-4ddf-b72c-88852a5722ad");
        if (sig != CFH_SIG && startsWithLocalFileHeader()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "7e9f434e-0b9e-4260-bdc5-efe08a3bd790");
            throw new IOException("central directory is empty, can't expand" + " corrupt archive.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "03105cdb-3865-495c-a671-13361a1bf988");
        while (sig == CFH_SIG) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "cd285d13-58d2-4423-9d4f-4230c8058a1a");
            readCentralDirectoryEntry(noUTF8Flag);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6be05f1d-1b5a-4df9-b99f-2f4961cc619b");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d0ba5645-7226-4f87-9a2b-0775b78c62f8");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "243be86a-948f-4ca4-9ad7-a5ce1a9197f1");
            sig = ZipLong.getValue(wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "eac4ad14-940a-44c7-bc03-c92bd32d4f07");
        return noUTF8Flag;
    }

    /**
     * Reads an individual entry of the central directory, creats an
     * ZipArchiveEntry from it and adds it to the global maps.
     *
     * @param noUTF8Flag map used to collect entries that don't have
     * their UTF-8 flag set and whose name will be set by data read
     * from the local file header later.  The current entry may be
     * added to this map.
     */
    private void readCentralDirectoryEntry(final Map<ZipArchiveEntry, NameAndComment> noUTF8Flag) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "85649737-b97f-422e-91d5-36f0c165c6d3");
        cfhBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "872c1fad-757e-4b85-8fca-49eebcdb92be");
        IOUtils.readFully(archive, cfhBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "59af46b8-d6f4-4c7f-b86b-c035771bec7f");
        int off = 0;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0669f2a9-91eb-4dbd-8be1-caedaf3f9790");
        final Entry ze = new Entry();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fef68a5e-81cf-419b-8302-dca1bb9e6e5a");
        final int versionMadeBy = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f4c7ac39-a412-46da-9626-b61c565a6eaf");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b989bb68-e83c-4a51-ada3-3e56f21892dd");
        ze.setVersionMadeBy(versionMadeBy);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1c2edd30-aeb1-4b54-9696-fd7fc5175a29");
        ze.setPlatform((versionMadeBy >> BYTE_SHIFT) & NIBLET_MASK);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "eaa90182-fa80-4110-9c2f-f3b5a8933b69");
        ze.setVersionRequired(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ff205d4e-2978-4a15-b20f-e1c35b1db388");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d4383e4f-7e96-4e51-b329-d40fce47f605");
        final GeneralPurposeBit gpFlag = GeneralPurposeBit.parse(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0922bdfb-f531-4ab7-92b1-e346292150ac");
        final boolean hasUTF8Flag = gpFlag.usesUTF8ForNames();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4c46e18a-5ad6-4009-a523-478ac17152ee");
        final ZipEncoding entryEncoding = hasUTF8Flag ? ZipEncodingHelper.UTF8_ZIP_ENCODING : zipEncoding;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "28bf563e-fc8e-4365-a4da-563894cece1a");
        if (hasUTF8Flag) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "feab99e7-3178-4180-afec-956736eec189");
            ze.setNameSource(ZipArchiveEntry.NameSource.NAME_WITH_EFS_FLAG);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "bf2ef63f-9417-41bc-8c15-2dabd21dec7d");
        ze.setGeneralPurposeBit(gpFlag);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6677b72d-f285-4475-b931-5836e9666aae");
        ze.setRawFlag(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4ccfedd4-96ef-4da6-93da-bb506ec3620b");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "193e02cb-f61c-4444-84a7-8b0070d5b17f");
        ze.setMethod(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ef972a7c-8c07-4681-9a73-ce592afad87a");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "7daebe84-ad25-4d2b-b062-7d0a96b69cd7");
        final long time = ZipUtil.dosToJavaTime(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4f9625f2-eb02-4110-8411-77978b3236bc");
        ze.setTime(time);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f4fca4f4-48ae-47ac-8066-6900611f105e");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1161c7c2-61a0-4800-9821-90a256d2d547");
        ze.setCrc(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "62b3b18d-b866-49a8-af61-879546fbb393");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c4a002e4-9ce0-4831-a5fe-923c9021129b");
        ze.setCompressedSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e6731bc5-fe09-41fe-8cba-2a503f0f2211");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "96175833-487e-4951-a37b-0a476b3c3c95");
        ze.setSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "11dd7977-f962-4709-93d8-f7b89dd7ad34");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4a6e71ab-ee6b-4f6f-a59c-23f50e719616");
        final int fileNameLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3fe4ab4a-11bb-4e46-b9f9-fdb0ed90af46");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ee9d6f7a-0f19-4a80-9880-2b7cc60d5057");
        final int extraLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9fdfc2a7-d1bc-4f41-93f5-7d201cdd1451");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d2943106-69e8-4176-9eac-950f5c0fba8e");
        final int commentLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e39270e5-7570-4d02-9677-a30c8214044a");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e6f3c6e2-1ed7-4eb5-87fc-aa0301e4b183");
        final int diskStart = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d2d05c70-38f6-4582-8f9a-bb56e68c9254");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "04841b48-4a1b-4697-b36e-c88372c79df4");
        ze.setInternalAttributes(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0af07781-d99e-4dd4-82f2-26df28af0b39");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f0fc77a3-13b1-420e-b9bd-45108bf754fa");
        ze.setExternalAttributes(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a2abd6ce-404a-4a38-9e23-b92d55232f37");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6e54e795-6915-40f2-a854-24ea27a1e47b");
        final byte[] fileName = new byte[fileNameLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "992bad86-bf5e-480a-8a07-4e9024b44d54");
        IOUtils.readFully(archive, ByteBuffer.wrap(fileName));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "99b1cfc0-4db0-45a7-a09f-694429d2a29f");
        ze.setName(entryEncoding.decode(fileName), fileName);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f54d2dfd-a7ad-4316-b1a3-babace5445f4");
        ze.setLocalHeaderOffset(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9ecfcbb0-882a-46c3-9704-ab56e8518b06");
        entries.add(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4585644d-8409-4da4-a00e-1050d037492e");
        final byte[] cdExtraData = new byte[extraLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fc1f5c76-c77f-436d-a724-ce01be8d96bb");
        IOUtils.readFully(archive, ByteBuffer.wrap(cdExtraData));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fbb41373-7d17-43f9-925c-a0c69d7dda5a");
        ze.setCentralDirectoryExtra(cdExtraData);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9898fcd9-f640-4e54-981f-8d1c1b32372e");
        setSizesAndOffsetFromZip64Extra(ze, diskStart);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b2918b61-b1e1-4a4f-9895-82806310c6e7");
        final byte[] comment = new byte[commentLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1e351183-53cc-4d0a-a0f2-4467c50fa1d0");
        IOUtils.readFully(archive, ByteBuffer.wrap(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "11d553bc-3b93-4162-bb00-a67551dcbd66");
        ze.setComment(entryEncoding.decode(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "aa74b23b-2ba1-4119-bdfc-c269d80535ef");
        if (!hasUTF8Flag && useUnicodeExtraFields) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "51c31680-001f-4d0c-b980-f3ad5be91cb2");
            noUTF8Flag.put(ze, new NameAndComment(fileName, comment));
        }
    }

    /**
     * If the entry holds a Zip64 extended information extra field,
     * read sizes from there if the entry's sizes are set to
     * 0xFFFFFFFFF, do the same for the offset of the local file
     * header.
     *
     * <p>Ensures the Zip64 extra either knows both compressed and
     * uncompressed size or neither of both as the internal logic in
     * ExtraFieldUtils forces the field to create local header data
     * even if they are never used - and here a field with only one
     * size would be invalid.</p>
     */
    private void setSizesAndOffsetFromZip64Extra(final ZipArchiveEntry ze, final int diskStart) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "998bd308-ade7-4ece-90f1-bb62a5851eac");
        final Zip64ExtendedInformationExtraField z64 = (Zip64ExtendedInformationExtraField) ze.getExtraField(Zip64ExtendedInformationExtraField.HEADER_ID);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b61835be-beae-42d0-a4ab-bc62dfaba45b");
        if (z64 != null) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6e6e718a-f635-4ba1-856c-9821be91ed3e");
            final boolean hasUncompressedSize = ze.getSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4b0c4426-96fe-474c-9934-a175da18bdfa");
            final boolean hasCompressedSize = ze.getCompressedSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ca9a42db-1238-4d10-af0c-fd96bff71f07");
            final boolean hasRelativeHeaderOffset = ze.getLocalHeaderOffset() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d3bcdec6-bc66-4cb2-965e-287fa5088d36");
            z64.reparseCentralDirectoryData(hasUncompressedSize, hasCompressedSize, hasRelativeHeaderOffset, diskStart == ZIP64_MAGIC_SHORT);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "95de01a1-22ec-42c6-8c5f-b8fdf0c43caa");
            if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f33ffc44-a663-4c06-a4f0-73ddd7a3ea75");
                ze.setSize(z64.getSize().getLongValue());
            } else if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6238e144-74e6-4a61-9ede-085d67e62d2f");
                z64.setSize(new ZipEightByteInteger(ze.getSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e9c7847c-92d3-4e18-9f00-440480068334");
            if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3ebcc956-5a45-4786-b7a6-308852a01cc7");
                ze.setCompressedSize(z64.getCompressedSize().getLongValue());
            } else if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "14cdd42e-8f4d-41de-a239-a8251010889c");
                z64.setCompressedSize(new ZipEightByteInteger(ze.getCompressedSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "023a8c85-05e2-434b-bc58-4fef6f7c7d75");
            if (hasRelativeHeaderOffset) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e9a76313-6b25-4850-b2f9-d7bdf4597091");
                ze.setLocalHeaderOffset(z64.getRelativeHeaderOffset().getLongValue());
            }
        }
    }

    /**
     * Length of the "End of central directory record" - which is
     * supposed to be the last structure of the archive - without file
     * comment.
     */
    static final int MIN_EOCD_SIZE = WORD + SHORT + SHORT + SHORT + SHORT + WORD + WORD + SHORT;

    /**
     * Maximum length of the "End of central directory record" with a
     * file comment.
     */
    private static final int MAX_EOCD_SIZE = MIN_EOCD_SIZE + ZIP64_MAGIC_SHORT;

    /**
     * Offset of the field that holds the location of the first
     * central directory entry inside the "End of central directory
     * record" relative to the start of the "End of central directory
     * record".
     */
    private static final int CFD_LOCATOR_OFFSET = WORD + SHORT + SHORT + SHORT + SHORT + WORD;

    /**
     * Length of the "Zip64 end of central directory locator" - which
     * should be right in front of the "end of central directory
     * record" if one is present at all.
     */
    private static final int ZIP64_EOCDL_LENGTH = WORD + WORD + DWORD + WORD;

    /**
     * Offset of the field that holds the location of the "Zip64 end
     * of central directory record" inside the "Zip64 end of central
     * directory locator" relative to the start of the "Zip64 end of
     * central directory locator".
     */
    private static final int ZIP64_EOCDL_LOCATOR_OFFSET = WORD + WORD;

    /**
     * Offset of the field that holds the location of the first
     * central directory entry inside the "Zip64 end of central
     * directory record" relative to the start of the "Zip64 end of
     * central directory record".
     */
    private static final int ZIP64_EOCD_CFD_LOCATOR_OFFSET = WORD + DWORD + SHORT + SHORT + WORD + WORD + DWORD + DWORD + DWORD;

    /**
     * Searches for either the &quot;Zip64 end of central directory
     * locator&quot; or the &quot;End of central dir record&quot;, parses
     * it and positions the stream at the first central directory
     * record.
     */
    private void positionAtCentralDirectory() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "81874c20-4405-47b8-912d-858223b9dc4a");
        positionAtEndOfCentralDirectoryRecord();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c8bb5170-4f4e-4a4b-8645-d95fc76ac2b4");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "7f55f155-5418-4e12-9772-6c130381f925");
        final boolean searchedForZip64EOCD = archive.position() > ZIP64_EOCDL_LENGTH;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c8f6dfd2-9992-43c6-a73b-5da97e8c2a5a");
        if (searchedForZip64EOCD) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f94ce98b-26d2-4569-8f3b-8e637fe211d7");
            archive.position(archive.position() - ZIP64_EOCDL_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "82a95a9a-b0b6-44a3-882d-0e69381133e5");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2e96012b-dc1b-4303-87d6-1da7c5041951");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "44712df9-f5c9-4f56-996e-08846860cb9e");
            found = Arrays.equals(ZipArchiveOutputStream.ZIP64_EOCD_LOC_SIG, wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "91d1c628-2f80-40ac-8f1f-f23360e3703e");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "efffd712-34ad-42df-a628-2500373d2e8b");
            if (searchedForZip64EOCD) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "11645b8e-4284-4bb8-98f0-7e149e0373f5");
                skipBytes(ZIP64_EOCDL_LENGTH - WORD);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d15b3f0f-e03f-47af-b0d1-d98b2149a57f");
            positionAtCentralDirectory32();
        } else {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ba73fe27-c437-4493-b764-ae414ce9c6cd");
            positionAtCentralDirectory64();
        }
    }

    /**
     * Parses the &quot;Zip64 end of central directory locator&quot;,
     * finds the &quot;Zip64 end of central directory record&quot; using the
     * parsed information, parses that and positions the stream at the
     * first central directory record.
     *
     * Expects stream to be positioned right behind the &quot;Zip64
     * end of central directory locator&quot;'s signature.
     */
    private void positionAtCentralDirectory64() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "30323389-cb58-4563-a5f8-37c79b2a8ca7");
        skipBytes(ZIP64_EOCDL_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "02d25454-2d58-4d63-8596-513c89c8db51");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9ad7174d-57b5-4cd2-82eb-c149cce0734c");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1d0010e6-6d23-467e-add7-a963d46a4f40");
        archive.position(ZipEightByteInteger.getLongValue(dwordBuf));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a1621159-c12c-400e-b0a8-4b45d2ee2c62");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fb97c34d-f149-494b-8918-3e750295fd33");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "a99b50be-b592-4ef4-bf81-4133f235b79c");
        if (!Arrays.equals(wordBuf, ZipArchiveOutputStream.ZIP64_EOCD_SIG)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6343489f-551c-4b0e-a0d9-8e08c0162f1b");
            throw new ZipException("archive's ZIP64 end of central " + "directory locator is corrupt.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fe21f179-d8be-4968-af25-e5caa01817a0");
        skipBytes(ZIP64_EOCD_CFD_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ff6a36d2-0376-4af2-abac-ad77522aead8");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "cab0ce80-52a3-4020-b198-c2cf78214bf1");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "96cdd4ab-94ac-4e12-ad1e-d98e6126fc92");
        archive.position(ZipEightByteInteger.getLongValue(dwordBuf));
    }

    /**
     * Parses the &quot;End of central dir record&quot; and positions
     * the stream at the first central directory record.
     *
     * Expects stream to be positioned at the beginning of the
     * &quot;End of central dir record&quot;.
     */
    private void positionAtCentralDirectory32() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "95ba8cb6-3a43-4da0-acad-f86f8b46d106");
        skipBytes(CFD_LOCATOR_OFFSET);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "68bbb651-e13a-42a6-a32e-7235ec5e4e74");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3de8257a-c883-45c2-9804-d298cf7b2ee6");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "960e31c8-fc2d-4ee3-ae59-694d4827203f");
        archive.position(ZipLong.getValue(wordBuf));
    }

    /**
     * Searches for the and positions the stream at the start of the
     * &quot;End of central dir record&quot;.
     */
    private void positionAtEndOfCentralDirectoryRecord() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "51f459de-f478-44db-808d-e8872e547e45");
        final boolean found = tryToLocateSignature(MIN_EOCD_SIZE, MAX_EOCD_SIZE, ZipArchiveOutputStream.EOCD_SIG);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "851e5f30-aeee-4db0-8f95-74551b63ee96");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9d598056-449f-464f-b2d2-e1fd2c450cbe");
            throw new ZipException("archive is not a ZIP archive");
        }
    }

    /**
     * Searches the archive backwards from minDistance to maxDistance
     * for the given signature, positions the RandomaccessFile right
     * at the signature if it has been found.
     */
    private boolean tryToLocateSignature(final long minDistanceFromEnd, final long maxDistanceFromEnd, final byte[] sig) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b0dcaac1-967a-48fd-b8ae-a9fae10cd480");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "40182fdf-cba8-48a2-ba2a-ec97a2699a19");
        long off = archive.size() - minDistanceFromEnd;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "8666e032-1258-4209-a052-4d3dec7c4b1f");
        final long stopSearching = Math.max(0L, archive.size() - maxDistanceFromEnd);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ded6559f-ebb9-4a22-91f6-76cc805c86f7");
        if (off >= 0) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fb8ef761-ebfe-4e45-91e6-1d1621e8a4e0");
            for (; off >= stopSearching; off--) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "dac8d7c1-f07b-4909-8c8a-e30d55a23093");
                archive.position(off);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b5b5d75e-ae5f-434a-a83c-32da7f4230b2");
                try {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1163f8b4-205d-47d7-b50d-007816b7be38");
                    wordBbuf.rewind();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fbace44b-6840-492d-ab87-580c60de18c8");
                    IOUtils.readFully(archive, wordBbuf);
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1eb33601-1bf1-443e-b9af-efc3f61a5b44");
                    wordBbuf.flip();
                } catch (EOFException ex) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e2d93645-09ad-4998-a95e-cc8ab52e4d4a");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c829678f-d32d-4f70-9fc6-0fe438be37c1");
                int curr = wordBbuf.get();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "e48da5a7-2935-4f61-8891-14f9c5d97918");
                if (curr == sig[POS_0]) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c6b92716-8266-4399-8fe9-aecee57e518a");
                    curr = wordBbuf.get();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1a2427a0-0d91-405a-bbb6-f2c42a96a220");
                    if (curr == sig[POS_1]) {
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2a69a268-07c1-48b6-8904-0ab576b30e94");
                        curr = wordBbuf.get();
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "bb8acb67-7586-4b2b-8cac-f74c2c7a92c0");
                        if (curr == sig[POS_2]) {
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "f77367f7-fd25-420f-9306-ee1d2515385d");
                            curr = wordBbuf.get();
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4b85db6f-d10c-41eb-99a3-3847b3237b87");
                            if (curr == sig[POS_3]) {
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6b019fcf-f488-49cc-a089-15028b06e24e");
                                found = true;
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1c64e351-6181-42e2-a398-24a4d2fce888");
                                break;
                            }
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "082c7bd0-fa8e-4875-9b12-d7fef087228e");
        if (found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fc02e9e1-722d-4a16-8013-bdabeacaadbf");
            archive.position(off);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "06bbc011-1703-4bc1-a576-0cb352ac4c49");
        return found;
    }

    /**
     * Skips the given number of bytes or throws an EOFException if
     * skipping failed.
     */
    private void skipBytes(final int count) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "cd4b736a-c5c6-4b2d-8b0f-2e1ffb2e513f");
        long currentPosition = archive.position();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fb9c2663-59ca-49db-a7b2-8fa177965d4f");
        long newPosition = currentPosition + count;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "7734063e-cfbb-4eba-a2a1-a1609cfb6e25");
        if (newPosition > archive.size()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "35afa1cb-c972-4849-ac64-db1041d270e6");
            throw new EOFException();
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "192697cc-7c13-4a98-918e-cee5b81bbe00");
        archive.position(newPosition);
    }

    /**
     * Number of bytes in local file header up to the &quot;length of
     * filename&quot; entry.
     */
    private static final long LFH_OFFSET_FOR_FILENAME_LENGTH = WORD + SHORT + SHORT + SHORT + SHORT + SHORT + WORD + WORD + (long) WORD;

    /**
     * Walks through all recorded entries and adds the data available
     * from the local file header.
     *
     * <p>Also records the offsets for the data to read from the
     * entries.</p>
     */
    private void resolveLocalFileHeaderData(final Map<ZipArchiveEntry, NameAndComment> entriesWithoutUTF8Flag) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b2185010-a928-4dc0-8fd6-afd5ad52c197");
        for (final ZipArchiveEntry zipArchiveEntry : entries) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "c7bded6d-1e73-4cd4-8c8d-f4b18d9439cc");
            final Entry ze = (Entry) zipArchiveEntry;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "19773de0-06d9-4867-91f7-8967b79fdba8");
            final long offset = ze.getLocalHeaderOffset();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9da55f54-7bfa-4ede-b9b8-7fea58ea79ea");
            archive.position(offset + LFH_OFFSET_FOR_FILENAME_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d716496a-4fe2-45d6-bbe5-43fce5946b28");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6e0e7946-1ff0-4e67-92c5-ee8b29849e69");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "3f5b5239-82cb-41a4-a5e5-b2b04847d21f");
            wordBbuf.flip();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b999756c-098f-47f7-8573-73a7d9849b8f");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "24076748-fc5e-417d-83b1-2e17a3763923");
            final int fileNameLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "492a577b-22c2-430f-890b-7b9ebbf2eb42");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "bc81d292-6313-4ef6-80bc-80924a48e053");
            final int extraFieldLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "6cf87cbc-8ac2-4109-a233-d6cbea8e357c");
            skipBytes(fileNameLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "aa03d616-87ea-4f79-891c-1bf8802ede6d");
            final byte[] localExtraData = new byte[extraFieldLen];
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "bbb373f6-63a9-46ab-8bdf-c155d5182b97");
            IOUtils.readFully(archive, ByteBuffer.wrap(localExtraData));
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "fd5203d2-2b51-42e0-9a93-514fd9cc9eba");
            ze.setExtra(localExtraData);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2f0e7fda-9c7f-48bf-8d38-5be9e35f4d05");
            ze.setDataOffset(offset + LFH_OFFSET_FOR_FILENAME_LENGTH + SHORT + SHORT + fileNameLen + extraFieldLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9400ba5c-b600-467c-96c0-c08c0beab281");
            ze.setStreamContiguous(true);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "4e944a2e-20e5-42db-9078-7b8b1e14ead9");
            if (entriesWithoutUTF8Flag.containsKey(ze)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "5a048082-e696-4e98-a3bc-2a80b6d80c53");
                final NameAndComment nc = entriesWithoutUTF8Flag.get(ze);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "1d364d4d-fb35-42e1-a8cc-121d656f574b");
                ZipUtil.setNameAndCommentFromExtraFields(ze, nc.name, nc.comment);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "ba84809e-82ea-465d-b5b2-3cbedbe13df5");
            final String name = ze.getName();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "b7a5402b-ff68-401c-83f9-6ba9f3846abc");
            LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "8b109569-a9f5-4119-a3c7-3abfb0a0806c");
            if (entriesOfThatName == null) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2acccaa8-54a6-4bb7-b026-4bd9b05a6b92");
                entriesOfThatName = new LinkedList<ZipArchiveEntry>();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "2b078b78-143d-457e-9499-9276d68e7858");
                nameMap.put(name, entriesOfThatName);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "0a2ecb66-70ac-4499-8fc1-34791b8d35a8");
            entriesOfThatName.addLast(ze);
        }
    }

    /**
     * Checks whether the archive starts with a LFH.  If it doesn't,
     * it may be an empty archive.
     */
    private boolean startsWithLocalFileHeader() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "05450a28-eaae-4048-8caa-86190bda615c");
        archive.position(0);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "24d67a88-75b9-4b45-b856-b7b66b082a83");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "9c077cd3-5125-4aa0-82ba-8cba165d6d92");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "d362be09-2eb3-4b83-9a99-5e4c2bcbe6b6");
        return Arrays.equals(wordBuf, ZipArchiveOutputStream.LFH_SIG);
    }

    /**
     * Creates new BoundedInputStream, according to implementation of
     * underlying archive channel.
     */
    private BoundedInputStream createBoundedInputStream(long start, long remaining) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_2_10.coverage", "5c56b2a5-d138-42cc-b458-c131fc42489c");
        return archive instanceof FileChannel ? new BoundedFileChannelInputStream(start, remaining) : new BoundedInputStream(start, remaining);
    }

    /**
     * InputStream that delegates requests to the underlying
     * SeekableByteChannel, making sure that only bytes from a certain
     * range can be read.
     */
    private class BoundedInputStream extends InputStream {

        private ByteBuffer singleByteBuffer;

        private final long end;

        private long loc;

        private boolean addDummy = false;

        BoundedInputStream(final long start, final long remaining) {
            this.end = start + remaining;
            if (this.end < start) {
                throw new IllegalArgumentException("Invalid length of stream at offset=" + start + ", length=" + remaining);
            }
            loc = start;
        }

        @Override
        public synchronized int read() throws IOException {
            if (loc >= end) {
                if (loc == end && addDummy) {
                    addDummy = false;
                    return 0;
                }
                return -1;
            }
            if (singleByteBuffer == null) {
                singleByteBuffer = ByteBuffer.allocate(1);
            } else {
                singleByteBuffer.rewind();
            }
            int read = read(loc, singleByteBuffer);
            if (read < 0) {
                return read;
            }
            loc++;
            return singleByteBuffer.get() & 0xff;
        }

        @Override
        public synchronized int read(final byte[] b, final int off, int len) throws IOException {
            if (len <= 0) {
                return 0;
            }
            if (len > end - loc) {
                if (loc >= end) {
                    if (loc == end && addDummy) {
                        addDummy = false;
                        b[off] = 0;
                        return 1;
                    }
                    return -1;
                }
                len = (int) (end - loc);
            }
            ByteBuffer buf;
            buf = ByteBuffer.wrap(b, off, len);
            int ret = read(loc, buf);
            if (ret > 0) {
                loc += ret;
                return ret;
            }
            return ret;
        }

        protected int read(long pos, ByteBuffer buf) throws IOException {
            int read;
            synchronized (archive) {
                archive.position(pos);
                read = archive.read(buf);
            }
            buf.flip();
            return read;
        }

        synchronized void addDummy() {
            this.addDummy = true;
        }
    }

    /**
     * Lock-free implementation of BoundedInputStream. The
     * implementation uses positioned reads on the underlying archive
     * file channel and therefore performs significantly faster in
     * concurrent environment.
     */
    private class BoundedFileChannelInputStream extends BoundedInputStream {

        private final FileChannel archive;

        BoundedFileChannelInputStream(final long start, final long remaining) {
            super(start, remaining);
            archive = (FileChannel) ZipFile.this.archive;
        }

        @Override
        protected int read(long pos, ByteBuffer buf) throws IOException {
            int read = archive.read(buf, pos);
            buf.flip();
            return read;
        }
    }

    private static final class NameAndComment {

        private final byte[] name;

        private final byte[] comment;

        private NameAndComment(final byte[] name, final byte[] comment) {
            this.name = name;
            this.comment = comment;
        }
    }

    /**
     * Compares two ZipArchiveEntries based on their offset within the archive.
     *
     * <p>Won't return any meaningful results if one of the entries
     * isn't part of the archive at all.</p>
     *
     * @since 1.1
     */
    private final Comparator<ZipArchiveEntry> offsetComparator = new Comparator<ZipArchiveEntry>() {

        @Override
        public int compare(final ZipArchiveEntry e1, final ZipArchiveEntry e2) {
            if (e1 == e2) {
                return 0;
            }
            final Entry ent1 = e1 instanceof Entry ? (Entry) e1 : null;
            final Entry ent2 = e2 instanceof Entry ? (Entry) e2 : null;
            if (ent1 == null) {
                return 1;
            }
            if (ent2 == null) {
                return -1;
            }
            final long val = (ent1.getLocalHeaderOffset() - ent2.getLocalHeaderOffset());
            return val == 0 ? 0 : val < 0 ? -1 : +1;
        }
    };

    /**
     * Extends ZipArchiveEntry to store the offset within the archive.
     */
    private static class Entry extends ZipArchiveEntry {

        Entry() {
        }

        @Override
        public int hashCode() {
            return 3 * super.hashCode() + (int) getLocalHeaderOffset() + (int) (getLocalHeaderOffset() >> 32);
        }

        @Override
        public boolean equals(final Object other) {
            if (super.equals(other)) {
                final Entry otherEntry = (Entry) other;
                return getLocalHeaderOffset() == otherEntry.getLocalHeaderOffset() && getDataOffset() == otherEntry.getDataOffset();
            }
            return false;
        }
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
