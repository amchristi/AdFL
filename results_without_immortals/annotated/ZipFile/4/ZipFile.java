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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "f798bded-6363-40bf-afd6-0eb86e95aca0");
        return encoding;
    }

    /**
     * Closes the archive.
     * @throws IOException if an error occurs closing the archive.
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0abde39a-4e00-4476-8709-dcc505a0bad6");
        closed = true;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "f7430975-77ed-4835-aac2-340705a4a16a");
        archive.close();
    }

    /**
     * close a zipfile quietly; throw no io fault, do nothing
     * on a null parameter
     * @param zipfile file to close, can be null
     */
    public static void closeQuietly(final ZipFile zipfile) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6ee09c9a-d228-447f-acb1-7e17d04eca8c");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "56380e57-39f1-4970-9f7a-4546e1c36577");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "272d7454-4c3d-4aba-a8ad-9c4331a6d380");
        final ZipArchiveEntry[] allEntries = entries.toArray(new ZipArchiveEntry[entries.size()]);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2d148cde-d4b3-4834-a786-eefafad96f08");
        Arrays.sort(allEntries, offsetComparator);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c3b5e9a1-0462-4dd3-b22d-3ab39992df9c");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "ba6b0278-3778-49d7-9d57-68f6ee3498d7");
        final LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7c750860-fec3-4777-8efe-e2cbc607a689");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c2501f26-6ea4-4c3a-9faa-2f22b65a0fff");
        final List<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d5caafa1-31a0-426e-9266-1e23df3e91a2");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "cae3f2b2-5752-4330-b3ff-32675270f345");
        ZipArchiveEntry[] entriesOfThatName = new ZipArchiveEntry[0];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d646218b-ce32-408a-9952-732d59bfa327");
        if (nameMap.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "57be3c91-9c5a-4025-b834-a00febcddc81");
            entriesOfThatName = nameMap.get(name).toArray(entriesOfThatName);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5ed1ca5f-53e1-4ad9-9581-671c4a48e136");
            Arrays.sort(entriesOfThatName, offsetComparator);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "083e81df-975f-4dc2-9a5b-d7652a50c50e");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a816e493-c797-45ed-8576-b699d1497fc2");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a5de70ba-0536-4a16-ad87-d8ea236557ec");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5f3b6e1d-9042-4d22-a6b3-bf9db5ec9127");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "669c8858-1d56-4f47-9a08-39a318db6c20");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e3896056-6902-4358-84e8-337cdbba3fae");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5cb73a8d-06dd-44d4-84ab-6c5e42b84a66");
        final Enumeration<ZipArchiveEntry> src = getEntriesInPhysicalOrder();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "be363c4c-f74a-441f-8012-ad108086ccdf");
        while (src.hasMoreElements()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "bddc9dea-57a1-4665-89de-75035477567e");
            final ZipArchiveEntry entry = src.nextElement();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "b3e4c20b-a4b3-4868-b9ac-d804fc184270");
            if (predicate.test(entry)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "876eb4ff-f694-4322-a36f-ed344b4ea5c2");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e8da1c42-b412-4c2f-87ad-fc8da4fa5d33");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c5edfa9a-6b77-4a68-9776-d7dd0ec9c3c4");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9d69b989-1a27-4856-9a13-d2235321794c");
        ZipUtil.checkRequestedFeatures(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "eca283d7-862a-4513-80ef-f7d6cec97517");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1f81e2b4-ef2d-4d34-9ca9-46492e1c8cc9");
        final BoundedInputStream bis = createBoundedInputStream(start, ze.getCompressedSize());
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9aaadf74-162c-43b1-9b3f-186786bef3ab");
        switch(ZipMethod.getMethodByCode(ze.getMethod())) {
            case STORED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "df305694-69ad-4300-b260-bf0e79dd09b3");
                return bis;
            case UNSHRINKING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "4e550d0d-c571-4407-8c70-5a782ec896d5");
                return new UnshrinkingInputStream(bis);
            case IMPLODING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2956a23f-bf7e-408e-8791-a29410ff0e4d");
                return new ExplodingInputStream(ze.getGeneralPurposeBit().getSlidingDictionarySize(), ze.getGeneralPurposeBit().getNumberOfShannonFanoTrees(), new BufferedInputStream(bis));
            case DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8250d8ad-edc5-4cea-8363-0bb6357daf7f");
                bis.addDummy();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1e08b3b2-fde1-473d-a66b-73e4fed4de5c");
                final Inflater inflater = new Inflater(true);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "795fe9aa-1fc8-459c-afeb-cd038fb8eb4d");
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
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a1b2113c-367f-43b2-9490-63ecb81b967f");
                return new BZip2CompressorInputStream(bis);
            case ENHANCED_DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e26a0915-79e9-4be1-bdb6-8512d591f39f");
                return new Deflate64CompressorInputStream(bis);
            default:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "ba71a398-811a-450d-82db-94a06910cd46");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5290bca5-c978-4003-9aed-48036be0aaad");
        if (entry != null && entry.isUnixSymlink()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2a0a37d3-63d8-40d8-ae19-237ff51d8f62");
            try {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1954b887-bb40-45fb-a2e0-be7be3d26cdc");
                InputStream in = getInputStream(entry);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "ef60220a-a294-48cf-8ee2-a86387701b14");
                return zipEncoding.decode(IOUtils.toByteArray(in));
            } catch (Exception ex) {
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "93270fd2-5aae-405c-bc6f-23dfe9ca00a6");
        return null;
    }

    /**
     * Ensures that the close method of this zipfile is called when
     * there are no more references to it.
     * @see #close()
     */
    @Override
    protected void finalize() throws Throwable {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "962b4e5b-712d-455f-8957-d1b4bcf399d8");
        try {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "45de8a25-d5aa-4c0f-968d-a8614850def5");
            if (!closed) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7ee4b3b2-a979-4f8c-8775-2126234334d4");
                System.err.println("Cleaning up unclosed ZipFile for archive " + archiveName);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1a0816f3-a2cc-47cb-a079-1c12fb8667c1");
                close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "39465fc5-f653-4922-8b3c-dd835dc2e6ad");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c7db741d-af0b-40cd-b121-70dac3ba3df8");
        final HashMap<ZipArchiveEntry, NameAndComment> noUTF8Flag = new HashMap<ZipArchiveEntry, NameAndComment>();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0556167c-8ce7-48bb-8160-336e11d50c71");
        positionAtCentralDirectory();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "df0a4f28-1e23-438a-8977-a761894e0a2c");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0a915324-fd57-492c-b837-79ebef25a115");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a2ded0fd-3a0c-439d-8f17-743543a1a410");
        long sig = ZipLong.getValue(wordBuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "720c29f8-c0a5-4819-a366-9d611f568b66");
        if (sig != CFH_SIG && startsWithLocalFileHeader()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "efd2cbc8-262e-4884-9060-c40504a640bf");
            throw new IOException("central directory is empty, can't expand" + " corrupt archive.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d0da4301-ef9e-4aae-b963-32f1229b5ce4");
        while (sig == CFH_SIG) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a41cf146-83b4-48fb-a8c7-f179492fa97f");
            readCentralDirectoryEntry(noUTF8Flag);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "36d08628-96e8-4c24-9839-605f102f03c2");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "718866b3-0d0b-4bc3-9006-9caabb780934");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c984ca9a-4f46-49b2-8a81-7f4f9cd91a3c");
            sig = ZipLong.getValue(wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "121bd596-5185-497e-aceb-0703361b3e50");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0099a78f-53dc-4fb6-834e-7c9067953bd3");
        cfhBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e5e4927a-e951-4ff0-a880-39e948853415");
        IOUtils.readFully(archive, cfhBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5edefe98-b2d2-4d43-8e2b-bac10af0a860");
        int off = 0;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6380f08d-52c4-4ad9-af57-6dac19101a93");
        final Entry ze = new Entry();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9f0d99bc-0181-4a94-8066-51fc33d0b4be");
        final int versionMadeBy = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "12323289-edbd-4365-bd5e-5eae38b83ea8");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a91adca2-8633-43c2-89e6-d334b6d47b4a");
        ze.setVersionMadeBy(versionMadeBy);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "45767c50-b98c-4fe0-8cef-4ae512f27a61");
        ze.setPlatform((versionMadeBy >> BYTE_SHIFT) & NIBLET_MASK);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "aa887d63-b561-4fb8-894b-7e08def2ee96");
        ze.setVersionRequired(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1de5d702-d761-4848-b673-35628d6be1b5");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "31046092-2245-4097-8a75-4548d4eec72f");
        final GeneralPurposeBit gpFlag = GeneralPurposeBit.parse(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "47a11493-fe6f-437e-ad18-18c5931f0be1");
        final boolean hasUTF8Flag = gpFlag.usesUTF8ForNames();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "15024701-8a4e-414e-ab1f-630f6c979438");
        final ZipEncoding entryEncoding = hasUTF8Flag ? ZipEncodingHelper.UTF8_ZIP_ENCODING : zipEncoding;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "3674143f-72d8-4d65-be80-825b3f8d9e54");
        if (hasUTF8Flag) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8eeee636-57f7-40f6-822f-50d32abfb59d");
            ze.setNameSource(ZipArchiveEntry.NameSource.NAME_WITH_EFS_FLAG);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e2b045ef-6e39-413d-8394-cb057b246f49");
        ze.setGeneralPurposeBit(gpFlag);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "439888cf-5bda-4e00-986c-17762e782c80");
        ze.setRawFlag(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a99983e6-86f6-4d58-9578-d9c66fe98764");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9fa8562c-0397-455a-8465-820e56efbf33");
        ze.setMethod(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0c234952-0d3c-45c8-bfaf-375711c2302a");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "142270f0-bfdf-4df9-bb11-4bcfbeb60cab");
        final long time = ZipUtil.dosToJavaTime(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0ae2b9e5-5263-4f27-b8c1-57b596843e4c");
        ze.setTime(time);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d7420308-d5b5-434f-9e78-aaa6ed5467c1");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "4581ed3c-72ce-4439-a001-08a9111a77e3");
        ze.setCrc(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "b18a6fbc-c45a-4a22-93dc-d59bbf13e7eb");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2ce180bb-1f9a-461c-b213-1706f8a41477");
        ze.setCompressedSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "17849ba2-a107-4ac9-a92a-913e623db4f7");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "33144107-b8b1-4788-b34a-d2d261f81264");
        ze.setSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "50772430-a908-436c-9f66-55c4fe122a87");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "b98fcbcf-5ee5-4aa6-bbd3-4830da78bfe1");
        final int fileNameLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e04e3054-0491-4656-84a2-bdbb36b98287");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a7f0e13c-df9b-4abb-ad66-939e5a0d74a7");
        final int extraLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a765f7c0-8aa4-47c6-bb02-5f9781814edb");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "59d7970d-12c1-4e53-8181-7d63505542c6");
        final int commentLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "b0bacedb-312a-4f93-88dd-93c367394a9a");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "cf2ffe4a-ca5c-43a3-ab7b-e946c0c6ab9a");
        final int diskStart = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "942b94d5-f72f-415c-913a-e0ebdf8a37a4");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7f95276e-1449-414e-bc22-c5306f2a1834");
        ze.setInternalAttributes(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "12150d63-888a-4e7c-ad73-d3b205d43dad");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9659dfee-ab26-4af3-9532-ed7d6092da14");
        ze.setExternalAttributes(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "92001320-7ac3-418c-8759-b4b5e0084848");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6fe725c2-f59a-4ffa-8aae-167420eeb02d");
        final byte[] fileName = new byte[fileNameLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a3685d1f-655b-43fc-ba54-5b2154344bcc");
        IOUtils.readFully(archive, ByteBuffer.wrap(fileName));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c9343144-ead0-4c7d-9d3b-f807020390d4");
        ze.setName(entryEncoding.decode(fileName), fileName);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5f28afee-f443-4b94-8243-55e225808158");
        ze.setLocalHeaderOffset(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7e994d2e-0356-445a-b96f-6d513fa937fc");
        entries.add(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9de0cfe3-d726-49db-b7f8-bc2344e93e99");
        final byte[] cdExtraData = new byte[extraLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2341b22a-0d91-42f0-8532-b435b5766d86");
        IOUtils.readFully(archive, ByteBuffer.wrap(cdExtraData));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "eba1bcc4-55b6-40d0-ad32-3e60c6d358da");
        ze.setCentralDirectoryExtra(cdExtraData);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d86d3449-38b1-4e55-8560-127a9ec97eaf");
        setSizesAndOffsetFromZip64Extra(ze, diskStart);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "29ba752c-dff5-412d-9fb5-12bd127961e2");
        final byte[] comment = new byte[commentLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "774fc84d-1d8c-4d23-999a-768e1a073280");
        IOUtils.readFully(archive, ByteBuffer.wrap(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2923779e-8e16-4e6e-a382-93536a088dbe");
        ze.setComment(entryEncoding.decode(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "89764f64-d0e1-4f96-8d93-5852ec92eb2e");
        if (!hasUTF8Flag && useUnicodeExtraFields) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "db58af4a-95b9-450b-8732-da45a0a6d529");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a32952b2-55cf-441b-9e2e-9aa673976051");
        final Zip64ExtendedInformationExtraField z64 = (Zip64ExtendedInformationExtraField) ze.getExtraField(Zip64ExtendedInformationExtraField.HEADER_ID);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "693c374d-ab98-40f3-869c-edd17782e91c");
        if (z64 != null) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7e00aaa7-f4fa-45fa-834a-c547a9b5ff24");
            final boolean hasUncompressedSize = ze.getSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2f419736-3cec-4ee7-b2b6-2309d8f335b0");
            final boolean hasCompressedSize = ze.getCompressedSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d20cd0e1-b527-410c-ae0c-b519ff5c385c");
            final boolean hasRelativeHeaderOffset = ze.getLocalHeaderOffset() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e4c15410-e1ad-4088-9034-6f17249b0119");
            z64.reparseCentralDirectoryData(hasUncompressedSize, hasCompressedSize, hasRelativeHeaderOffset, diskStart == ZIP64_MAGIC_SHORT);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "40392d45-4c01-4107-8295-f67f0ea02fae");
            if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c21308b5-0daa-476d-9612-18c99ee3be44");
                ze.setSize(z64.getSize().getLongValue());
            } else if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d351734c-d2a3-4024-875b-d42deb939adb");
                z64.setSize(new ZipEightByteInteger(ze.getSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1d272d38-d1d5-4545-859a-048a495e80bf");
            if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8013c021-d64d-4009-9b15-e4507c5c907f");
                ze.setCompressedSize(z64.getCompressedSize().getLongValue());
            } else if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "72f110c6-b4c9-442e-a2fb-58cbe6b85dbd");
                z64.setCompressedSize(new ZipEightByteInteger(ze.getCompressedSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9fc49154-7777-4728-aef8-98bb2ff205c6");
            if (hasRelativeHeaderOffset) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "453892cf-0a1f-485e-8574-5a5118da80ff");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c253a55b-3898-4546-bddb-c5b75519d9d8");
        positionAtEndOfCentralDirectoryRecord();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0d4187ad-1402-46ff-9472-3dbe289f017c");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "80745d4f-82dd-4043-b508-7162489cea87");
        final boolean searchedForZip64EOCD = archive.position() > ZIP64_EOCDL_LENGTH;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "deaeb201-14d8-4116-95bd-f20575dbd678");
        if (searchedForZip64EOCD) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "dc8d900d-0fe9-4ce0-a683-470b0ea837e7");
            archive.position(archive.position() - ZIP64_EOCDL_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "eebfce93-d96b-46f8-8a83-ee64be0bfcf4");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c554c478-2f7a-4e29-8c1f-ecbb761c624a");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "913c601e-9714-45ee-8944-097ac049a3fe");
            found = Arrays.equals(ZipArchiveOutputStream.ZIP64_EOCD_LOC_SIG, wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8f545e36-9313-4b27-ae07-86af452d4139");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "282ba903-920a-44a4-b3b9-672514927438");
            if (searchedForZip64EOCD) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6e86f2f6-e8e8-4497-b2a6-75fce22afcaf");
                skipBytes(ZIP64_EOCDL_LENGTH - WORD);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "666c0697-330f-4c9c-9833-d3eac4b40ec2");
            positionAtCentralDirectory32();
        } else {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "b5d48833-25a9-4721-81ae-420b45215bed");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "fb65fd11-01d5-4694-99f7-7200ab1de370");
        skipBytes(ZIP64_EOCDL_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "3308333a-677f-4862-a63b-aff7c63c3456");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "465b0dc3-639d-4b1e-9b32-826626467af7");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "704040db-f887-4751-9363-0eb5ce9a08b5");
        archive.position(ZipEightByteInteger.getLongValue(dwordBuf));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2c003988-6525-4d67-9110-bb369615ccb4");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "3f15ec42-c4e5-41cd-bf70-915e4506d27b");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7efefdc8-8f35-4a53-b101-f273c57bc999");
        if (!Arrays.equals(wordBuf, ZipArchiveOutputStream.ZIP64_EOCD_SIG)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6e91e133-833b-4a64-89f2-ef5aafc8177b");
            throw new ZipException("archive's ZIP64 end of central " + "directory locator is corrupt.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "10de122c-1339-4de6-a0ac-92d588c3b139");
        skipBytes(ZIP64_EOCD_CFD_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9195ec2b-d45a-4c99-a614-4e174882f36f");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a457a312-573c-481f-9bcf-9d48608e0148");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9992b5ae-8d67-4220-83a6-209e2735416b");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "bb63f5a3-9283-44c8-a620-b86c2fda6853");
        skipBytes(CFD_LOCATOR_OFFSET);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e6c6285b-7e0b-4e48-be32-14e658784b7d");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "069c93bc-71ac-424e-a3a0-ac7aa145f6a0");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c27f3897-88ad-47c8-a641-ce27108eba66");
        archive.position(ZipLong.getValue(wordBuf));
    }

    /**
     * Searches for the and positions the stream at the start of the
     * &quot;End of central dir record&quot;.
     */
    private void positionAtEndOfCentralDirectoryRecord() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "4e5bfed7-4b7f-4b72-98ba-96647192d060");
        final boolean found = tryToLocateSignature(MIN_EOCD_SIZE, MAX_EOCD_SIZE, ZipArchiveOutputStream.EOCD_SIG);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "f6e2b5fb-fd37-4652-854d-1815436d6b60");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "dbefcccc-e9ba-4b85-bd0a-771805b5511c");
            throw new ZipException("archive is not a ZIP archive");
        }
    }

    /**
     * Searches the archive backwards from minDistance to maxDistance
     * for the given signature, positions the RandomaccessFile right
     * at the signature if it has been found.
     */
    private boolean tryToLocateSignature(final long minDistanceFromEnd, final long maxDistanceFromEnd, final byte[] sig) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "58b285df-a2a0-49e5-8ab0-f5e23b81ac4b");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "66972f6d-bf6f-40f5-88a5-334ef428d61b");
        long off = archive.size() - minDistanceFromEnd;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7d70a328-2934-423a-aee6-aaad09604a3e");
        final long stopSearching = Math.max(0L, archive.size() - maxDistanceFromEnd);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2250a266-cf8e-4f0e-80c8-aa6f0c5c5590");
        if (off >= 0) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "69d25acb-239d-44f4-bd95-b339b90e8eed");
            for (; off >= stopSearching; off--) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "46daef0c-2bb3-4725-a0f2-e6a7a1582437");
                archive.position(off);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "17f8f19b-5e64-4ba1-970d-82f77cf826d7");
                try {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1029d4fc-25a4-46b0-8a76-e9014724c85a");
                    wordBbuf.rewind();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1e70d65b-3bfd-4559-8956-8cf86e0f9b3a");
                    IOUtils.readFully(archive, wordBbuf);
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "43ee1693-bcdc-478a-afd5-f8807a4f3fee");
                    wordBbuf.flip();
                } catch (EOFException ex) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "3c1d89ae-9033-4363-9e16-97a3ab7dad37");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "75e246cf-80e7-4452-8ddd-58f2a1c9f5df");
                int curr = wordBbuf.get();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "79c37b4f-b3cc-4578-9373-41fb10457ec9");
                if (curr == sig[POS_0]) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "0af17687-a252-4a4d-b5f8-e6ea34f31856");
                    curr = wordBbuf.get();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "4ce1f1e6-6f7e-441d-a51b-cfe875e161d3");
                    if (curr == sig[POS_1]) {
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "5422c039-51e1-4e6f-a901-ca73fd2bb36c");
                        curr = wordBbuf.get();
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "23a10340-d2da-4805-aa26-f0f0740456ac");
                        if (curr == sig[POS_2]) {
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1d882805-feac-42b0-ab83-285e4ce756c8");
                            curr = wordBbuf.get();
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "40dc5354-0d34-4680-bb1a-61eab4b7ab2d");
                            if (curr == sig[POS_3]) {
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a239fc87-3c87-4b3a-9017-1db0f8f75df9");
                                found = true;
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a6c92b20-833b-4a23-8473-9815e434e38d");
                                break;
                            }
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6acee44c-9d70-42bc-abef-3262498b8658");
        if (found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8ce4b98e-d7df-4924-ba2d-a16240fbca75");
            archive.position(off);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9eeaf376-3929-4813-9587-54f3625c9aef");
        return found;
    }

    /**
     * Skips the given number of bytes or throws an EOFException if
     * skipping failed.
     */
    private void skipBytes(final int count) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "66cabba6-6997-4ff8-9101-319290559640");
        long currentPosition = archive.position();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "618b54d2-7935-458c-ac92-537b7c4c2a98");
        long newPosition = currentPosition + count;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8283852b-d9a9-4c8c-920b-92d8a940fb4d");
        if (newPosition > archive.size()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "ec6afbff-c395-4d4e-a706-f402c366b352");
            throw new EOFException();
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "d69aa710-2f95-4299-ae59-b1593341877b");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7edeb10b-43f4-4739-9cef-68cc7899f407");
        for (final ZipArchiveEntry zipArchiveEntry : entries) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "13356e34-1c6b-48eb-a404-aa9f816de79a");
            final Entry ze = (Entry) zipArchiveEntry;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "61a77381-f661-4929-b2fd-d82ac8c49e34");
            final long offset = ze.getLocalHeaderOffset();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "7cf889e3-4d7e-4b26-8ec1-b669525cdc65");
            archive.position(offset + LFH_OFFSET_FOR_FILENAME_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "f82a0d14-03ad-4e4a-8691-3af83d2d6070");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "9a073b70-3643-4240-8e19-26e6d724f78f");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "367b9f0b-150f-4a14-8c11-e78dee35ee62");
            wordBbuf.flip();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "6c124b2b-e7cd-402d-a1b3-cee14ae33944");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "8b7bb429-fb13-4b8b-b795-cf59b6d17737");
            final int fileNameLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "33c98c2b-03f3-4373-9e21-345574dc9071");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "090ed2f3-7bd7-4af5-94c4-4eae8a81132f");
            final int extraFieldLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "71afac48-8734-45bc-987c-035a077ca93a");
            skipBytes(fileNameLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "45cb460b-73b9-4755-b7f7-7a65c9c0362e");
            final byte[] localExtraData = new byte[extraFieldLen];
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "1666d53b-858d-43fa-aa28-dc8fde8b400b");
            IOUtils.readFully(archive, ByteBuffer.wrap(localExtraData));
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "65dede48-f4cc-40a4-a905-debeee4f1b36");
            ze.setExtra(localExtraData);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "58f58e27-3b12-4241-b06b-4221df230716");
            ze.setDataOffset(offset + LFH_OFFSET_FOR_FILENAME_LENGTH + SHORT + SHORT + fileNameLen + extraFieldLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "ee8125c9-e30d-4d08-b853-754458c54c1c");
            ze.setStreamContiguous(true);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "f4008fb4-f43c-40dc-8d94-a8055fffaac4");
            if (entriesWithoutUTF8Flag.containsKey(ze)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "eaa63374-fb65-4038-802f-4cd5615921f2");
                final NameAndComment nc = entriesWithoutUTF8Flag.get(ze);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2fccd4af-cb62-419c-bd18-5620b4744504");
                ZipUtil.setNameAndCommentFromExtraFields(ze, nc.name, nc.comment);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "67af2d14-2cd9-4938-82fb-a519088ff600");
            final String name = ze.getName();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c93c56bd-8993-43a2-92fe-493814f31ce2");
            LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c9429209-f2e8-4121-aad0-ce4be277701d");
            if (entriesOfThatName == null) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "a3551af2-3cfc-4690-9be6-8e106396321b");
                entriesOfThatName = new LinkedList<ZipArchiveEntry>();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2b753348-6c6b-4573-99f9-f6ae6ca5e329");
                nameMap.put(name, entriesOfThatName);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "85cd403a-11af-4b47-9638-457d631589ae");
            entriesOfThatName.addLast(ze);
        }
    }

    /**
     * Checks whether the archive starts with a LFH.  If it doesn't,
     * it may be an empty archive.
     */
    private boolean startsWithLocalFileHeader() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "e75fe017-9e25-439c-93f0-4d77b0c020e0");
        archive.position(0);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "c1e98a78-4f88-4f62-930b-25a438434dff");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "ecc977fd-9a0f-4c60-9d31-e1cbb955f52b");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "2cbf32b6-5ab9-42ad-9711-49ab0e1924d8");
        return Arrays.equals(wordBuf, ZipArchiveOutputStream.LFH_SIG);
    }

    /**
     * Creates new BoundedInputStream, according to implementation of
     * underlying archive channel.
     */
    private BoundedInputStream createBoundedInputStream(long start, long remaining) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_4_10.coverage", "025fcb5a-a5d2-4f7b-ba76-d08432708412");
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
