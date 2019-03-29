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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "17b13095-d7fe-4a4b-ae3f-da7e8eaa24c7");
        return encoding;
    }

    /**
     * Closes the archive.
     * @throws IOException if an error occurs closing the archive.
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "8c8de79d-d012-472b-b750-5543d96fb228");
        closed = true;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b4e8d8a5-1ad6-4345-87a8-31118520044d");
        archive.close();
    }

    /**
     * close a zipfile quietly; throw no io fault, do nothing
     * on a null parameter
     * @param zipfile file to close, can be null
     */
    public static void closeQuietly(final ZipFile zipfile) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fb5d7285-010b-4676-83b0-4393fd3f79a9");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fd5e89fd-360d-4d6f-bec4-bb5cae96b687");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a8f91de1-8cc5-4e24-a6f9-9a515725edd0");
        final ZipArchiveEntry[] allEntries = entries.toArray(new ZipArchiveEntry[entries.size()]);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fe639b43-9d70-4d0d-ad55-d3db5f440501");
        Arrays.sort(allEntries, offsetComparator);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "49fae66a-840a-4b8d-b61b-4b11eb5fedad");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "0ac788ff-6af8-403a-912f-b3c23f4a225b");
        final LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "13cc99a1-4d07-4a91-b94b-32cc1ef931f5");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4e024b4b-632a-45f9-b00b-13fa6f0ace28");
        final List<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "1cab7e8e-55c4-428d-9178-35d8da8fe13c");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "208ddcf9-e120-4b79-bdb2-8427ffcda164");
        ZipArchiveEntry[] entriesOfThatName = new ZipArchiveEntry[0];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5779ff94-7f1f-4f3e-a198-6806ce100247");
        if (nameMap.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bbf83eca-e989-4051-9582-edcdee53fb6d");
            entriesOfThatName = nameMap.get(name).toArray(entriesOfThatName);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bc2f7131-a040-4a48-b197-8e0506c8776e");
            Arrays.sort(entriesOfThatName, offsetComparator);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "1a4d096b-5cd4-4789-8e7b-a8367646ace1");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e5d33b43-4c1c-4a33-90ac-16b501dec66e");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f22404c0-97d9-47ad-8113-06f072b4172f");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5e6e6ca6-3778-40af-981c-4327a8ba3c39");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ac29030b-7c8f-4a18-9193-3bae144472c9");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4a66fc02-940f-4daf-be1c-499c327ae453");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "00db2f97-657e-40ea-a646-6085a3369629");
        final Enumeration<ZipArchiveEntry> src = getEntriesInPhysicalOrder();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4a38d900-4165-4063-8d74-1bda84f1e8dc");
        while (src.hasMoreElements()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b0bc4f7c-9519-4836-95b2-620112f845f2");
            final ZipArchiveEntry entry = src.nextElement();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e2d75b30-a46b-4c5c-8461-abd60d02b352");
            if (predicate.test(entry)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "77be41c2-18e2-46f5-8d3e-a7ebe4c4b4b1");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f21d9a3a-3125-4391-99d7-8f48636d7706");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "710f60d7-1673-459d-8d0e-9867a0e0bfe0");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a514fedb-423b-48ec-8c49-89af794098d6");
        ZipUtil.checkRequestedFeatures(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e01fbea6-b110-4fbe-9d92-1c0b8cad998b");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3772ca91-7cb5-4508-b109-b6a51b4afb94");
        final BoundedInputStream bis = createBoundedInputStream(start, ze.getCompressedSize());
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "23a8f1f8-a9c1-47cf-aab7-f905aaba6b29");
        switch(ZipMethod.getMethodByCode(ze.getMethod())) {
            case STORED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "2fe3d33d-b848-4486-b495-be4f9a6dbbeb");
                return bis;
            case UNSHRINKING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "71ff015a-2a9a-4c44-be65-feae62bcc6a4");
                return new UnshrinkingInputStream(bis);
            case IMPLODING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c7ad3d3f-a44a-466f-b696-33aba515876f");
                return new ExplodingInputStream(ze.getGeneralPurposeBit().getSlidingDictionarySize(), ze.getGeneralPurposeBit().getNumberOfShannonFanoTrees(), new BufferedInputStream(bis));
            case DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "9c3eaea5-75ae-4cb1-a548-6826c4ebacec");
                bis.addDummy();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "eef160ca-ad72-4761-a2f0-c44c9dd0460d");
                final Inflater inflater = new Inflater(true);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3e4a09d1-9042-43ef-8232-7e7e723b51cd");
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
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7fe51888-7eed-4959-8498-569c70aa925c");
                return new BZip2CompressorInputStream(bis);
            case ENHANCED_DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "cfbaaf55-bf1f-47ec-9d28-7357ae74f00c");
                return new Deflate64CompressorInputStream(bis);
            default:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "9745b0a6-d93e-4fd0-a81e-a42602dedf0b");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "dfd71b7b-c2a4-4e15-8bfc-bf75997a3153");
        if (entry != null && entry.isUnixSymlink()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "81560100-e8e1-4394-b919-fb245d4fed8f");
            try {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "42a2ea3f-8217-470c-8602-3d9d5893a412");
                InputStream in = getInputStream(entry);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e806f2d0-65f4-4942-81cb-b84c36f4e992");
                return zipEncoding.decode(IOUtils.toByteArray(in));
            } catch (Exception ex) {
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "372a5f2e-e42c-43a9-9264-0d7a9e046f14");
        return null;
    }

    /**
     * Ensures that the close method of this zipfile is called when
     * there are no more references to it.
     * @see #close()
     */
    @Override
    protected void finalize() throws Throwable {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d3264243-492a-49bc-9a5c-6265e81a3793");
        try {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ea0207aa-f15f-44dd-9bfb-35aff0b9d03c");
            if (!closed) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6c9d355f-2d62-4e13-9073-5a1129e617bb");
                System.err.println("Cleaning up unclosed ZipFile for archive " + archiveName);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f89cf3b1-5ca9-49b9-b2a0-860d7f659bda");
                close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bb261518-00b9-4c29-81f2-f82ceff272bd");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "22c9e6fa-1c33-4a40-a41c-2f813b89edc4");
        final HashMap<ZipArchiveEntry, NameAndComment> noUTF8Flag = new HashMap<ZipArchiveEntry, NameAndComment>();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "05590fc4-b42f-4aec-973c-d018465d1f92");
        positionAtCentralDirectory();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "77b89ebd-a59a-44bc-ab60-2ea667058427");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "430bb4ff-dac5-4289-a8d0-68e55734ed6d");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b3b87bd2-b93b-464a-9313-89642d62dc11");
        long sig = ZipLong.getValue(wordBuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "22b4e7ef-e8ed-4045-a508-2d2080be9fa6");
        if (sig != CFH_SIG && startsWithLocalFileHeader()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "32d5a90f-773a-41c1-8387-6770bd23e677");
            throw new IOException("central directory is empty, can't expand" + " corrupt archive.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4b75b1e2-67f8-4350-b4da-a0d30dc5fbb4");
        while (sig == CFH_SIG) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6021f5c2-2ca3-4954-89e9-0bbc1b1b5371");
            readCentralDirectoryEntry(noUTF8Flag);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "07685903-538e-4d15-9afd-7ad0a1761c67");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fa47e1df-8683-4e35-929c-e0509b07a53e");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a877bbe6-b1c7-42bb-b72a-e58555fefb87");
            sig = ZipLong.getValue(wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c81d0d17-db04-4331-955a-bad9ab85ffe3");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5391c64f-00bd-485f-8fdc-961847d3613d");
        cfhBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "265c3da1-6e46-43e5-8d48-11bcaf52ee60");
        IOUtils.readFully(archive, cfhBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "12a042cd-23b9-4a83-93db-e331a76a63d6");
        int off = 0;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "cc4a0f9a-2093-45b1-80e4-a5218fc6cc86");
        final Entry ze = new Entry();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b826a0ce-4b47-4fb3-bbe2-740049a3faf8");
        final int versionMadeBy = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7440ea52-2881-42bf-b7f8-e31ce3072b07");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6ea85e13-f95e-48cb-9a6b-093cb78607cb");
        ze.setVersionMadeBy(versionMadeBy);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "38bdc195-004f-4db0-a0e0-0f8d3d434074");
        ze.setPlatform((versionMadeBy >> BYTE_SHIFT) & NIBLET_MASK);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "23320d55-3716-4bf0-8646-ba37dde7a635");
        ze.setVersionRequired(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3f7f5173-e5ef-4dbb-b910-82554491ba9b");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d747bbc2-8056-471e-82e8-16593a9fd469");
        final GeneralPurposeBit gpFlag = GeneralPurposeBit.parse(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "af14559f-1ebe-4bb9-b976-96647171f5f2");
        final boolean hasUTF8Flag = gpFlag.usesUTF8ForNames();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "8228f002-d30b-40f4-825c-41e77bccf42d");
        final ZipEncoding entryEncoding = hasUTF8Flag ? ZipEncodingHelper.UTF8_ZIP_ENCODING : zipEncoding;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e5942542-7440-4c59-9564-5b5e3c8d6dae");
        if (hasUTF8Flag) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "2d6cbc30-2717-4e34-8f9f-d03e9dcd92b7");
            ze.setNameSource(ZipArchiveEntry.NameSource.NAME_WITH_EFS_FLAG);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "432c0535-6e48-4515-9c4a-fa97d6c3bdec");
        ze.setGeneralPurposeBit(gpFlag);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "8dc7431f-5312-4629-bae5-4853be86f080");
        ze.setRawFlag(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ea4dd1dc-a6d3-4b27-91ee-bbad793ee908");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c83312de-4cfc-4264-bb82-8680f5162b7b");
        ze.setMethod(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "57348131-c502-43bf-9e7b-6fcc7638410a");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a211d7ba-9954-49c3-a426-3fac14b43ab4");
        final long time = ZipUtil.dosToJavaTime(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "461b5419-411b-4bda-8b73-82386ad6203a");
        ze.setTime(time);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5e21ea13-0b19-4191-b89c-1e4340f84199");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f3e09705-6917-478d-a978-64fcab6cc00f");
        ze.setCrc(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3c296036-606a-4f7c-9e0f-66c17e3685c3");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5605d7af-1ac5-4372-bb06-bc6cfcba720c");
        ze.setCompressedSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4686cede-a39b-4d4e-ad7d-8653a464b3ec");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "98fcf973-ccab-4f7a-a448-6fe32ed1ab2c");
        ze.setSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "382220e8-69f1-4be4-bd95-7589d9592e69");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "84a2171d-5455-457f-a5c4-6eb2c174528f");
        final int fileNameLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3a1cadca-5f71-40da-8fbc-2ed8bf876d1a");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "715eb9eb-8291-4927-95a6-ceea613fbc8c");
        final int extraLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e269abdc-f2b4-42be-848b-ffc5fe44dde4");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4e5b5c55-858a-4465-8abd-80bbaa2812e8");
        final int commentLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "75c94ba1-5789-477f-b052-56bb4d9e42aa");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "330212e6-3ae0-4514-b89b-9ff6f83268c0");
        final int diskStart = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "045928db-80df-4577-bae0-e080ad45a494");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "31f523ff-bfca-4fb8-8be9-ac6578541f36");
        ze.setInternalAttributes(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bd89afde-d0ed-4718-acd3-ac14eba0ee2e");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "cada8ddc-8c11-4221-a44a-4483810ca25a");
        ze.setExternalAttributes(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "716a4aa4-7f2f-4e83-8ae9-43ff0cb82c4c");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "939ff4bc-969a-4996-b311-3c9ae9d08d1c");
        final byte[] fileName = new byte[fileNameLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "dbb038e9-4e1c-4c02-8468-a1964f55a21e");
        IOUtils.readFully(archive, ByteBuffer.wrap(fileName));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "1f74fbb5-3280-422a-a00c-6bf08a96dc80");
        ze.setName(entryEncoding.decode(fileName), fileName);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "647c3f13-99ce-4d13-a7ad-6f426830d804");
        ze.setLocalHeaderOffset(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "db92cb89-418e-4494-a00e-3125b414f212");
        entries.add(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ad57d942-c916-43d3-b4b2-ab4025236e10");
        final byte[] cdExtraData = new byte[extraLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3194e151-5e03-4f9a-a821-500280cf3b9d");
        IOUtils.readFully(archive, ByteBuffer.wrap(cdExtraData));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a55eb1c3-ae24-48db-89b9-19e915e48447");
        ze.setCentralDirectoryExtra(cdExtraData);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "04e003ea-4229-4995-b438-e8f3fdf4b5b3");
        setSizesAndOffsetFromZip64Extra(ze, diskStart);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "782d2e6e-a126-4a46-aa9f-4da5cd11a1bf");
        final byte[] comment = new byte[commentLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a7ea9449-c0e4-4539-93f5-59f6a25e3fa5");
        IOUtils.readFully(archive, ByteBuffer.wrap(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "44617433-e94f-462f-b79f-00ae5c3d5c75");
        ze.setComment(entryEncoding.decode(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "17e1cbac-4e56-41bb-b3b0-9a91e8d2d6af");
        if (!hasUTF8Flag && useUnicodeExtraFields) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "0d3f88fc-bcc4-442a-b98d-7edee65f7e13");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4bebf92e-14dc-4555-b253-6722bbe1308b");
        final Zip64ExtendedInformationExtraField z64 = (Zip64ExtendedInformationExtraField) ze.getExtraField(Zip64ExtendedInformationExtraField.HEADER_ID);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7d192911-8a25-41de-951b-c7ebc4a3b4c2");
        if (z64 != null) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5d77dbf0-a394-46e5-ad3e-3a5d4bfc45d2");
            final boolean hasUncompressedSize = ze.getSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "dbb0b8d7-30bd-4151-b69c-4135109f05d7");
            final boolean hasCompressedSize = ze.getCompressedSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "70258294-237c-4500-90bf-9f09b73da270");
            final boolean hasRelativeHeaderOffset = ze.getLocalHeaderOffset() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a3664ef8-080b-4137-ad47-46d007213d4a");
            z64.reparseCentralDirectoryData(hasUncompressedSize, hasCompressedSize, hasRelativeHeaderOffset, diskStart == ZIP64_MAGIC_SHORT);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "81361a62-72fc-4a11-b940-1f43459b09bb");
            if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a6546736-f063-4d2e-a1c2-9abaf4e02c70");
                ze.setSize(z64.getSize().getLongValue());
            } else if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7e304573-f550-45dc-b565-6746f155be66");
                z64.setSize(new ZipEightByteInteger(ze.getSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "94114887-1b02-4306-8b0c-b9c8564f0e2c");
            if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ba9e35c1-c9ac-4dd4-8e54-b713a6bac8c1");
                ze.setCompressedSize(z64.getCompressedSize().getLongValue());
            } else if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c4ae9293-a83d-4d11-93b6-9ba69b1e3643");
                z64.setCompressedSize(new ZipEightByteInteger(ze.getCompressedSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "047331ea-8200-4e2a-85f0-8684eb782622");
            if (hasRelativeHeaderOffset) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a57df1ba-e856-4dda-ba27-521cbdb80b3e");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "dc44a1c6-281d-48ce-8d43-cacb852f0913");
        positionAtEndOfCentralDirectoryRecord();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6585c329-b6d9-44dd-9bfd-d0192f245fc0");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ee81695c-f6b4-457b-9434-6b111436874c");
        final boolean searchedForZip64EOCD = archive.position() > ZIP64_EOCDL_LENGTH;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7571cef3-51fa-42ae-bdd7-e17f5c421de8");
        if (searchedForZip64EOCD) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3ad13958-9cee-4cc9-bd02-66f45bd3d1d8");
            archive.position(archive.position() - ZIP64_EOCDL_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fc812687-c99c-456f-83b5-43e130836db7");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a7df38bf-4ef3-4815-b40a-e5b273e6bea0");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "60c10df8-b4bc-4707-83dc-b52b7a7e04f0");
            found = Arrays.equals(ZipArchiveOutputStream.ZIP64_EOCD_LOC_SIG, wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "357931fe-e314-46fc-9d3d-2cf27f45654a");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "0d4e2867-8dd1-48ba-9dd1-36c1f1caa138");
            if (searchedForZip64EOCD) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "62a8a1cc-758e-4058-9240-1238ab678732");
                skipBytes(ZIP64_EOCDL_LENGTH - WORD);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6ef9f05a-6a43-4aaa-8e13-5f9e00ecf3f4");
            positionAtCentralDirectory32();
        } else {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "27f3a4fc-98a6-40fc-aa7c-080ec092945e");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f52dda4a-1599-4948-aceb-2532f8e68030");
        skipBytes(ZIP64_EOCDL_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f4bacc07-f7a3-4f6b-9074-3711245b0bed");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f3af8923-a2b4-4e5a-afd0-b13654f134d2");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "511f5b80-6d56-41ce-8765-87692a36ebb3");
        archive.position(ZipEightByteInteger.getLongValue(dwordBuf));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "1ead81f1-b43c-4e3c-8e76-687b331f7c76");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "afaf494b-6c6d-4d3e-a1df-cc48397ddbd8");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c11389bb-200d-473b-b809-fcd70003500c");
        if (!Arrays.equals(wordBuf, ZipArchiveOutputStream.ZIP64_EOCD_SIG)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a5d1e3bb-be94-44f5-b104-030fdc747e2a");
            throw new ZipException("archive's ZIP64 end of central " + "directory locator is corrupt.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6ba99934-7a33-4262-ace1-9b670b57f639");
        skipBytes(ZIP64_EOCD_CFD_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fb630b2a-d0e4-4e32-b763-f53d607bc7fe");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "558821ae-e247-49b3-8b38-1726b1064ac1");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d7de0c2f-833f-44e9-a5f5-931ba1056aa8");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "585616f8-beb8-4d7b-882e-959ba4dbb3c4");
        skipBytes(CFD_LOCATOR_OFFSET);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c27629cb-4886-49c4-b195-31e1cd6e54ca");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5857d5fa-2049-4d89-8b54-bc3e9173f78e");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bdcc8e43-b855-47a5-836c-88358da15307");
        archive.position(ZipLong.getValue(wordBuf));
    }

    /**
     * Searches for the and positions the stream at the start of the
     * &quot;End of central dir record&quot;.
     */
    private void positionAtEndOfCentralDirectoryRecord() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "03254b81-f5ff-459d-8b4e-933d905a0583");
        final boolean found = tryToLocateSignature(MIN_EOCD_SIZE, MAX_EOCD_SIZE, ZipArchiveOutputStream.EOCD_SIG);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c640cb4d-c701-4a8b-b2fe-cd6c15761666");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c7808098-7b41-442b-b992-6b1142a951b4");
            throw new ZipException("archive is not a ZIP archive");
        }
    }

    /**
     * Searches the archive backwards from minDistance to maxDistance
     * for the given signature, positions the RandomaccessFile right
     * at the signature if it has been found.
     */
    private boolean tryToLocateSignature(final long minDistanceFromEnd, final long maxDistanceFromEnd, final byte[] sig) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "5eafd8af-881f-4bad-81ad-d9fe9d1e7968");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3c9f4939-9078-42af-a451-3b749263214e");
        long off = archive.size() - minDistanceFromEnd;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "334d36a1-360c-44ab-8188-10c9f176a823");
        final long stopSearching = Math.max(0L, archive.size() - maxDistanceFromEnd);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c2175b47-7503-477e-bee7-3adff3854209");
        if (off >= 0) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d1b57414-f187-40c2-8080-eb71d8bff70f");
            for (; off >= stopSearching; off--) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "cf9ee259-0db8-4dbb-8d5c-a867e4de8f5d");
                archive.position(off);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7a8956f0-7874-4369-9b97-39034c2e29a5");
                try {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "880e30d5-6219-476a-9561-4ad6b974dbb2");
                    wordBbuf.rewind();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "cf693399-f812-445b-8f2a-a026fdbe7b5a");
                    IOUtils.readFully(archive, wordBbuf);
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "657c1896-5793-4a1c-abf2-e6e2b89ae5b3");
                    wordBbuf.flip();
                } catch (EOFException ex) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c81a60d1-b069-424b-a575-0c4ede033380");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b981119b-16aa-40df-b274-9299ef516093");
                int curr = wordBbuf.get();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f46741ba-2d7a-4a95-981a-87a57197dc00");
                if (curr == sig[POS_0]) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "c78dce77-805d-4eb8-9c69-21d412cb6e3c");
                    curr = wordBbuf.get();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "2b20f0ed-5952-4361-8f6f-82b7d9f4984c");
                    if (curr == sig[POS_1]) {
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "feb3ef3f-f0ed-424c-9507-ed282532d101");
                        curr = wordBbuf.get();
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "65ecd89b-618a-42b7-b8ee-c2468ffdcb7a");
                        if (curr == sig[POS_2]) {
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "db2ee30f-e1c6-403f-b23a-8811c7e226ae");
                            curr = wordBbuf.get();
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d2eacff5-0f90-4c54-91e1-ddad8123fd64");
                            if (curr == sig[POS_3]) {
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "91e3d8c7-8f9d-4b0b-a36d-aa16c305dd0c");
                                found = true;
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "2c228ec3-3bbd-46b5-98f1-4860c4babbfc");
                                break;
                            }
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7ddd7f93-9652-4ec9-a86b-fddd9eaf01c2");
        if (found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d4dce39b-16c4-4310-a98c-8d108c463329");
            archive.position(off);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b894ec85-7fb9-4bb1-b909-8541d43e128a");
        return found;
    }

    /**
     * Skips the given number of bytes or throws an EOFException if
     * skipping failed.
     */
    private void skipBytes(final int count) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e826f597-6577-4998-87e9-27b4672f148a");
        long currentPosition = archive.position();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "40881dbc-d042-44a8-8b2e-36aed0a30419");
        long newPosition = currentPosition + count;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "1fc3ec31-8521-465b-9efe-acc44bd4a3be");
        if (newPosition > archive.size()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e121bf1b-8ead-4305-a20a-1b581044d8e7");
            throw new EOFException();
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "cc6693a4-b104-4319-893c-6d4a714e42dc");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "ce51c6a4-1629-498e-930f-38b79ab9d48a");
        for (final ZipArchiveEntry zipArchiveEntry : entries) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f6d0cc54-65e2-4f6a-a75b-008defd0375d");
            final Entry ze = (Entry) zipArchiveEntry;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bc61327a-bd62-4692-90da-897df21b1ddc");
            final long offset = ze.getLocalHeaderOffset();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e761205e-7818-4c94-91bd-7557911844ac");
            archive.position(offset + LFH_OFFSET_FOR_FILENAME_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "a6f58266-dd8f-402b-af2c-bf23586ba606");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "e82b030c-9e6d-4e34-aeae-dd9967c3e326");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "8f98850a-7c1a-4b14-8f55-ac8590458683");
            wordBbuf.flip();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "24c7842b-e5a0-4b30-a2ea-63150f3de289");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d0b8a0f6-a9a9-4eb4-82a0-9858d4049ea7");
            final int fileNameLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "fc080d54-4810-4cb7-a8d7-578d2aa2f8f0");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b7189122-9307-40d6-b28e-b3b6219daaaa");
            final int extraFieldLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d15f005a-06b6-45cd-8a4f-cfdf1b6d552a");
            skipBytes(fileNameLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "bcd05a3e-63e5-455b-bbed-1944932c3e80");
            final byte[] localExtraData = new byte[extraFieldLen];
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b5e50450-b06c-41d8-8c1d-8e0ae153679f");
            IOUtils.readFully(archive, ByteBuffer.wrap(localExtraData));
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "70445a81-505b-470a-9b3b-7b6432aff40f");
            ze.setExtra(localExtraData);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "4d9c33d4-af8a-4327-8745-88909a3b4003");
            ze.setDataOffset(offset + LFH_OFFSET_FOR_FILENAME_LENGTH + SHORT + SHORT + fileNameLen + extraFieldLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "7409a6db-93d4-4e71-b13c-30be71537658");
            ze.setStreamContiguous(true);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "9a582a57-228c-4669-979c-d87f04ae96a5");
            if (entriesWithoutUTF8Flag.containsKey(ze)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "f027f2f3-f3de-484a-9444-e9195bc5aac6");
                final NameAndComment nc = entriesWithoutUTF8Flag.get(ze);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "2fd7b3e3-1865-4bc1-ada9-3c9e5bc8a201");
                ZipUtil.setNameAndCommentFromExtraFields(ze, nc.name, nc.comment);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "d401cdf6-c9d1-4c60-a089-e9570bff280b");
            final String name = ze.getName();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "47c33bf6-cacd-413e-b947-7ae456fd4ec6");
            LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "70f6e071-e8a1-4b2b-99a7-6698db0eb2ea");
            if (entriesOfThatName == null) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "6bc197d0-2f25-429c-8b55-24d45d893c15");
                entriesOfThatName = new LinkedList<ZipArchiveEntry>();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3504bc4f-7578-40d3-902b-1859b6b83321");
                nameMap.put(name, entriesOfThatName);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "b0ae5ade-4b3b-4dac-8458-d91be873a001");
            entriesOfThatName.addLast(ze);
        }
    }

    /**
     * Checks whether the archive starts with a LFH.  If it doesn't,
     * it may be an empty archive.
     */
    private boolean startsWithLocalFileHeader() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "88fef3cd-4067-4769-9585-565025596d47");
        archive.position(0);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "0b918276-abf0-4392-b196-0fc46a3e95c6");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "2c89e035-4f01-4855-b00f-593178b8609a");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "3e7d9886-a65c-4e8a-aa30-07e366e4ef05");
        return Arrays.equals(wordBuf, ZipArchiveOutputStream.LFH_SIG);
    }

    /**
     * Creates new BoundedInputStream, according to implementation of
     * underlying archive channel.
     */
    private BoundedInputStream createBoundedInputStream(long start, long remaining) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_5_10.coverage", "71c3aa77-d751-4b80-b475-bd5a0eb01d5e");
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
