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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "eb7a124e-2d81-444b-a983-d7034216f02d");
        return encoding;
    }

    /**
     * Closes the archive.
     * @throws IOException if an error occurs closing the archive.
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8a5339ee-4ce7-4db3-a617-0d3fff2f783f");
        closed = true;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e1b768ec-7d09-4b16-88f1-2b6daedfff21");
        archive.close();
    }

    /**
     * close a zipfile quietly; throw no io fault, do nothing
     * on a null parameter
     * @param zipfile file to close, can be null
     */
    public static void closeQuietly(final ZipFile zipfile) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "03c50fc4-f4a0-41a3-803f-b7739ffeee79");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a0a04d61-b7c5-464f-b3bb-85d8c7615e46");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "269743ee-0252-4362-b956-0d2de39d1bbd");
        final ZipArchiveEntry[] allEntries = entries.toArray(new ZipArchiveEntry[entries.size()]);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "27f2ce05-5d60-4ad6-9407-dc53cd7d1b10");
        Arrays.sort(allEntries, offsetComparator);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "9a442625-7bd6-43dd-bc1d-5c133fe8f125");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "de512806-54f8-4bcb-b8a6-f20c1fb81ee5");
        final LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "339d0205-0724-4194-a0ba-f4862d3c78e4");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4b452135-f387-4d32-810f-2c0d17cc655a");
        final List<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "fa0294ee-a609-4eb0-942a-c72fcaa9cd9c");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "60c48891-f335-4443-bc18-d97bca14dffe");
        ZipArchiveEntry[] entriesOfThatName = new ZipArchiveEntry[0];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "0b8d7426-16ce-4563-94f7-1b45d4910a2c");
        if (nameMap.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a1980322-20d5-4d7e-aa92-b65728d70de9");
            entriesOfThatName = nameMap.get(name).toArray(entriesOfThatName);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7b9abe1e-bb55-4152-8d2a-922e3f49742d");
            Arrays.sort(entriesOfThatName, offsetComparator);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "2534b4b9-9913-4cf2-95da-b34e15644b3d");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8043a99b-ddff-4e23-856a-787e2607b6f6");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a5d43d04-b54c-4586-b7d6-3ef111d2a9a9");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "88a9e369-0455-4cb4-bff9-bc0aede8b1fa");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "38ead7af-1a23-4e9d-b4c3-d2f85bc8146e");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "27669db1-b1a8-4f33-81b7-e4491b0cdf14");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "5239e2db-8ee8-4a18-a050-f00cf2b5fb14");
        final Enumeration<ZipArchiveEntry> src = getEntriesInPhysicalOrder();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "65914276-2c55-414c-85af-2dbf2e8f5c5a");
        while (src.hasMoreElements()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "32ecdc1d-41d7-496b-98f6-213e7c3dfe70");
            final ZipArchiveEntry entry = src.nextElement();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "95d7ea9d-038c-45a0-a0ca-40f11a4ed122");
            if (predicate.test(entry)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4ebbc612-8c1f-4081-9f7d-e864971d4bde");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "831b47f4-1c7a-4e5b-bf2a-2eddafa1d5a3");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c89e1290-1d95-4078-818e-f17e52fadc55");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "085e9e5d-4e98-4377-91ee-1de2ae57e22d");
        ZipUtil.checkRequestedFeatures(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8d146fe7-2092-45e5-9c7f-4bfb17b7ba9a");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "dce2e843-12af-4211-872a-1291d0fe3bf4");
        final BoundedInputStream bis = createBoundedInputStream(start, ze.getCompressedSize());
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a32a3b3f-5a82-4230-b527-47e3c08655b2");
        switch(ZipMethod.getMethodByCode(ze.getMethod())) {
            case STORED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7bbc46de-da03-4501-89a5-971191b5b3f2");
                return bis;
            case UNSHRINKING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "465699ca-ef49-4c50-b431-c8da7441a376");
                return new UnshrinkingInputStream(bis);
            case IMPLODING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b202dcc7-6d88-48d2-88c7-3770f471f6d3");
                return new ExplodingInputStream(ze.getGeneralPurposeBit().getSlidingDictionarySize(), ze.getGeneralPurposeBit().getNumberOfShannonFanoTrees(), new BufferedInputStream(bis));
            case DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7b493fce-d52d-4937-aba9-0e535f6ce82b");
                bis.addDummy();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c934d940-527d-42a2-af70-8ace137fcca0");
                final Inflater inflater = new Inflater(true);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "2c194a46-f193-444a-9d77-41f89b0d3236");
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
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c3bdcbb2-5171-4443-ac04-77a6c2864c33");
                return new BZip2CompressorInputStream(bis);
            case ENHANCED_DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "f7660af8-b232-44ad-8c28-37dcddac58fa");
                return new Deflate64CompressorInputStream(bis);
            default:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "60f1b85b-e4b7-4b90-be1e-06f114e28ad0");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6af094a8-4655-4f19-8424-212abfa3eb8d");
        if (entry != null && entry.isUnixSymlink()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "51227a60-3de5-41ac-a66f-5d3d0fd7868c");
            try {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8d076479-8620-4706-8bb1-b2a0386cea65");
                InputStream in = getInputStream(entry);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4404ecad-5bfb-414d-a647-452f38841636");
                return zipEncoding.decode(IOUtils.toByteArray(in));
            } catch (Exception ex) {
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "f0604588-f9c6-482e-a0f7-09eaad719143");
        return null;
    }

    /**
     * Ensures that the close method of this zipfile is called when
     * there are no more references to it.
     * @see #close()
     */
    @Override
    protected void finalize() throws Throwable {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "23a22c52-6484-4312-bf24-7fa00fa3ddd7");
        try {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a4b31413-2129-48e2-9ad2-5c7a4100a5d0");
            if (!closed) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "314d874c-ad61-49a7-ab2a-3e97323dcea1");
                System.err.println("Cleaning up unclosed ZipFile for archive " + archiveName);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "92aeeee8-cf4e-4f33-b2c5-b527a409c42d");
                close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "39a88f10-83d0-484d-9085-554a9f48225f");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "303fd594-d062-410a-84ae-31cfbe1f01a8");
        final HashMap<ZipArchiveEntry, NameAndComment> noUTF8Flag = new HashMap<ZipArchiveEntry, NameAndComment>();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d135d18d-8ba9-4151-b321-fdf09682e3db");
        positionAtCentralDirectory();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e0fe612f-fb49-4b8e-b0bb-ebf49af6d6f3");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b47f0b37-2e85-43e8-8ff2-e179221b5934");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "0ef582ac-ceb1-454d-ab35-70c453e8127c");
        long sig = ZipLong.getValue(wordBuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "f6da84b9-8e7c-4fe4-96a0-2d3b6f78fa6e");
        if (sig != CFH_SIG && startsWithLocalFileHeader()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "707ef202-0732-49e9-9afd-ffbd60a79509");
            throw new IOException("central directory is empty, can't expand" + " corrupt archive.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "be2070ae-2244-44e7-b159-f64ec017aee2");
        while (sig == CFH_SIG) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4616fe4a-2c26-4df7-80f0-473391ee7198");
            readCentralDirectoryEntry(noUTF8Flag);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6b527ee6-231e-41b8-96df-d6a389f3e110");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "49dc7846-12a1-453b-a2d0-ecb6e496242c");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "03c43176-4ac5-4a0c-b200-bf12e6dbea1e");
            sig = ZipLong.getValue(wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6aaf2ba9-c7de-4c43-98e6-b93d9c53f4db");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "50871f45-a1f0-4d8a-b31a-5adeae0d8395");
        cfhBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3da25271-2513-44e5-9cfc-22d7712358a6");
        IOUtils.readFully(archive, cfhBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d213c504-7084-4852-b240-f541b4ecbce8");
        int off = 0;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "16015a19-ff6f-4095-a9a0-ecbbc06aec4f");
        final Entry ze = new Entry();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a2e7e44c-afca-4b4d-9844-096e4550c49b");
        final int versionMadeBy = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "95e44c15-36e1-44b0-8514-a676942fe25d");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "eac5bb4a-1073-498d-86bd-25ee9849bcb7");
        ze.setVersionMadeBy(versionMadeBy);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "45335878-2e86-4a24-891f-916246568c70");
        ze.setPlatform((versionMadeBy >> BYTE_SHIFT) & NIBLET_MASK);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b2c84266-111b-48d0-9c35-cf5962d173fb");
        ze.setVersionRequired(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "998ade85-114c-41ae-9ae2-ca3ed715817f");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7f63a7dc-a8d2-407c-b509-051b47e20b6e");
        final GeneralPurposeBit gpFlag = GeneralPurposeBit.parse(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d72753d8-aaba-487a-958a-4fb0f6aa5205");
        final boolean hasUTF8Flag = gpFlag.usesUTF8ForNames();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b651534c-abc6-4754-a6f6-22a114fcdb6b");
        final ZipEncoding entryEncoding = hasUTF8Flag ? ZipEncodingHelper.UTF8_ZIP_ENCODING : zipEncoding;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "232ea909-9779-4cdf-8453-b7c3ac7173a4");
        if (hasUTF8Flag) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d5784809-54c0-46f9-bd0e-7410394aa07e");
            ze.setNameSource(ZipArchiveEntry.NameSource.NAME_WITH_EFS_FLAG);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "0d9ac3a3-7a33-40fa-b03c-d03222722555");
        ze.setGeneralPurposeBit(gpFlag);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "1eedf834-d5f4-4efb-abd3-88750ed1d994");
        ze.setRawFlag(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "45f33115-d17b-4e2c-8923-12d0da485b0d");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "51f41abc-67c6-4330-8c36-d5792a710901");
        ze.setMethod(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "5a5f610b-67d2-4995-9c89-af1360f717c7");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "227f7f43-56e5-49f9-aa3a-d51de5c2227b");
        final long time = ZipUtil.dosToJavaTime(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e2753055-63c6-4748-b849-e1b54b8fe977");
        ze.setTime(time);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "caea203d-9a72-4975-90eb-9a0beca8aea5");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8f5f7da3-50d4-441d-a489-e10c150f5550");
        ze.setCrc(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6073411f-bdd7-40a9-85f4-2480d5e184fb");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e32dbe48-d7bd-489b-836e-e60f0ee9700e");
        ze.setCompressedSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "57ba00c7-ec91-442b-ad63-7c7d23272022");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "31e89833-caae-4364-a7f9-decb5471c2d6");
        ze.setSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "05a79c5c-06d5-4e73-9945-aafc2a7b7662");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6c399db2-a1b3-4f81-bbc4-3e1ceaf1d63f");
        final int fileNameLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "9c2220c2-db79-4864-be8b-adcb1b54c377");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "05f16c4e-2443-4e73-bc2e-a1319f0c5c2a");
        final int extraLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c5367e44-4fa2-4cdd-9a50-21b3bbe4bc51");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b6485797-f400-4348-9633-423d87987d7f");
        final int commentLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "1f1783e8-23fc-4f30-a1af-f9bd743d6eb3");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "ea122123-c2c3-4be7-a148-cd91bd6ee408");
        final int diskStart = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d98d5029-3774-4a21-bc1a-5eb74fc22e49");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "29a0f556-ce0a-4d34-886a-982e480f581e");
        ze.setInternalAttributes(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e3cda693-8d47-4b27-9ed5-a952aaf26363");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "41f678c7-a155-4166-884c-feae8d5e5b21");
        ze.setExternalAttributes(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "5e1d673e-cb4e-4993-9487-a95442b783be");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "f933fc81-1f4e-47b6-a3e4-f6610288d132");
        final byte[] fileName = new byte[fileNameLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3800467b-81f9-4bbd-a6d9-619781504d0d");
        IOUtils.readFully(archive, ByteBuffer.wrap(fileName));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a3a916e1-c55d-4092-919b-96133952f398");
        ze.setName(entryEncoding.decode(fileName), fileName);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7e028bd3-e01a-490c-a645-103a8dd10f3c");
        ze.setLocalHeaderOffset(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "bb0ab9c6-0019-408f-a3cb-23e417379c94");
        entries.add(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "416b619d-c0f3-48b9-925a-7e9c331e83e7");
        final byte[] cdExtraData = new byte[extraLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6397ac6e-ce8b-4355-866b-c7a7fcffd3b2");
        IOUtils.readFully(archive, ByteBuffer.wrap(cdExtraData));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "615165ec-3a31-4e1e-a2b4-10fd3c1ada93");
        ze.setCentralDirectoryExtra(cdExtraData);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7ec87a97-bb64-4956-9bea-67268b4e2080");
        setSizesAndOffsetFromZip64Extra(ze, diskStart);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b264df04-644f-47de-b0f9-ad5929edf625");
        final byte[] comment = new byte[commentLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "70d35269-a88b-449e-97a0-cc54df188c23");
        IOUtils.readFully(archive, ByteBuffer.wrap(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "01518c7f-612d-4381-810e-ea267ebf21ce");
        ze.setComment(entryEncoding.decode(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "865d8003-f16e-4a6b-a210-0895cce9fa17");
        if (!hasUTF8Flag && useUnicodeExtraFields) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "71735834-3fd4-47be-8ae6-268a1a434ece");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a90ea9fb-84ca-4f47-bdcc-3c234313471c");
        final Zip64ExtendedInformationExtraField z64 = (Zip64ExtendedInformationExtraField) ze.getExtraField(Zip64ExtendedInformationExtraField.HEADER_ID);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "280356ac-9142-4f58-8e29-6ba178a1fba9");
        if (z64 != null) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e6904e83-baf1-44f1-9eb5-58d01ef607f4");
            final boolean hasUncompressedSize = ze.getSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e6fb55f2-342a-4166-b3ce-6b6b4b2585fc");
            final boolean hasCompressedSize = ze.getCompressedSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "ba92d07e-2144-4252-9e3f-5cfff73a56d2");
            final boolean hasRelativeHeaderOffset = ze.getLocalHeaderOffset() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "618f0df0-167f-438d-9d42-b68523721033");
            z64.reparseCentralDirectoryData(hasUncompressedSize, hasCompressedSize, hasRelativeHeaderOffset, diskStart == ZIP64_MAGIC_SHORT);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "0217ae20-7285-49a3-9ce7-9557aeebcae3");
            if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "fde505d9-2fb7-448b-b802-208ecc8771d9");
                ze.setSize(z64.getSize().getLongValue());
            } else if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "1a9d5fa9-b6ae-48dd-bd96-d70a126da398");
                z64.setSize(new ZipEightByteInteger(ze.getSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "830808aa-116e-45c4-ac88-e81c6e1652b7");
            if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8b71127e-2007-414d-9dce-26055f9b174c");
                ze.setCompressedSize(z64.getCompressedSize().getLongValue());
            } else if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "9eabc896-e510-4402-b69d-5c435580b916");
                z64.setCompressedSize(new ZipEightByteInteger(ze.getCompressedSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "dd477215-f85c-466f-b279-5d34afc72d3b");
            if (hasRelativeHeaderOffset) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c7755458-2564-4bbb-bce1-a51998d55c12");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "af8ffff3-ea72-4c07-91c9-5165b01bab1f");
        positionAtEndOfCentralDirectoryRecord();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "14cf11c0-800d-492e-9c74-bb3a9502973c");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "cb46a3f9-3d00-4aec-b706-2944aa149682");
        final boolean searchedForZip64EOCD = archive.position() > ZIP64_EOCDL_LENGTH;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b7e8531d-80af-44c5-a798-416a8bbd40e6");
        if (searchedForZip64EOCD) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a3e7e8dd-f2b5-46c9-903d-4d510a3e4bf8");
            archive.position(archive.position() - ZIP64_EOCDL_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "30fb55ef-5b3e-4ba4-aab1-989fd7cb119e");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "bc445e8e-5f29-4533-8638-4e9cf050dde9");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "2796b4c8-1d19-44a8-8639-f9ca3dde066d");
            found = Arrays.equals(ZipArchiveOutputStream.ZIP64_EOCD_LOC_SIG, wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8367fc44-4dce-4f33-aead-5fe0ba1ddaa4");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c39d3e4e-301a-4093-9f2d-fc6c51e27a8d");
            if (searchedForZip64EOCD) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d578cd86-60df-4219-86b0-03ae4b143fd8");
                skipBytes(ZIP64_EOCDL_LENGTH - WORD);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "06b1edfb-f1b1-451f-a173-26a5bc1e1f69");
            positionAtCentralDirectory32();
        } else {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "6d7a9039-8fc9-45c7-85e0-fb26dded7021");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "69c2708a-5ab8-44eb-8e61-3faca0d435fb");
        skipBytes(ZIP64_EOCDL_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7fe8e114-76b6-4dcb-9317-81bf03a9cef0");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "82831aff-1c03-4e7c-9f15-eab690ea3894");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8cae1478-da09-438e-ae87-772b4b42526b");
        archive.position(ZipEightByteInteger.getLongValue(dwordBuf));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "95b16aee-c310-4430-990a-2d7cd074ca23");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d5b5530e-50cc-4b23-896b-cfdfe22a50c6");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b7ddf50e-e43c-4590-bfa4-2d01e5fffced");
        if (!Arrays.equals(wordBuf, ZipArchiveOutputStream.ZIP64_EOCD_SIG)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "da30345e-3404-44dc-ae7e-33f88585cea7");
            throw new ZipException("archive's ZIP64 end of central " + "directory locator is corrupt.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "5055ef75-4b99-4e3e-8748-c9b3e52b61ec");
        skipBytes(ZIP64_EOCD_CFD_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "eca9bb48-dcc9-4714-8464-3fb052cc3f64");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "bfe25e61-468e-4fbf-9054-ce06228a04b0");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "ec7ea5ce-eefb-4741-8e8c-54bb0abbf569");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "e1528c3d-a37d-492c-a814-25a4e0687321");
        skipBytes(CFD_LOCATOR_OFFSET);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8d63d333-949d-41bd-961d-72aecff3d1ca");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c4069641-565b-4a67-8cf1-05527d4af164");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7e203498-0431-4e68-bacf-f2b8149f0ab4");
        archive.position(ZipLong.getValue(wordBuf));
    }

    /**
     * Searches for the and positions the stream at the start of the
     * &quot;End of central dir record&quot;.
     */
    private void positionAtEndOfCentralDirectoryRecord() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d568c38a-46ac-4043-b26f-35afaf4aa2a2");
        final boolean found = tryToLocateSignature(MIN_EOCD_SIZE, MAX_EOCD_SIZE, ZipArchiveOutputStream.EOCD_SIG);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d0f7a38f-4a03-4d16-82e8-6697f9cd3301");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4d2054fb-2be9-4ab4-aaca-b8fc3ff268c6");
            throw new ZipException("archive is not a ZIP archive");
        }
    }

    /**
     * Searches the archive backwards from minDistance to maxDistance
     * for the given signature, positions the RandomaccessFile right
     * at the signature if it has been found.
     */
    private boolean tryToLocateSignature(final long minDistanceFromEnd, final long maxDistanceFromEnd, final byte[] sig) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "1785c78e-274a-4f3f-a9a7-a087587f520b");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "01f456e3-ce81-458d-ab1f-71a3c79d10b9");
        long off = archive.size() - minDistanceFromEnd;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "0940996b-1ef0-45a2-b126-17b590825e61");
        final long stopSearching = Math.max(0L, archive.size() - maxDistanceFromEnd);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "214de409-09aa-4daf-8553-6ae3e435f112");
        if (off >= 0) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "85e55b8a-1687-47b4-95c2-8fcdf01d4997");
            for (; off >= stopSearching; off--) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a15626c2-d09b-4b8f-b5a4-7871f8be79de");
                archive.position(off);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3189d3cd-1b8a-4720-8cf5-5f6e87b662e6");
                try {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "38e75c01-2245-4f04-b93c-2ff0ed5516e9");
                    wordBbuf.rewind();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "61bffddd-ee96-4200-bea9-d34a937fc085");
                    IOUtils.readFully(archive, wordBbuf);
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "763da1e7-e4b6-4803-96cf-503373c814fe");
                    wordBbuf.flip();
                } catch (EOFException ex) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "62d37d58-9cfc-43f6-8ae2-c1d7b3b59a0c");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3c987a70-736f-446f-a8c8-e39e0fdc7cde");
                int curr = wordBbuf.get();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a979012c-aa88-42ae-ad1e-32004937e710");
                if (curr == sig[POS_0]) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "ee7889e0-4459-4942-a4a5-483260e004ee");
                    curr = wordBbuf.get();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b1f26ff4-999c-4e48-b8a6-1211b5e081fd");
                    if (curr == sig[POS_1]) {
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c99f1e8e-b204-4eb7-9061-524ff6e9b62c");
                        curr = wordBbuf.get();
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a3c25bb9-8397-43b3-8158-4523f3078fcc");
                        if (curr == sig[POS_2]) {
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8eb16663-8bb1-40ec-a268-050a3f97fc77");
                            curr = wordBbuf.get();
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "fd5d3e96-31c1-4da1-8ed4-a56e7192d41e");
                            if (curr == sig[POS_3]) {
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c53f9f76-676b-440f-bca4-aae4de6384e1");
                                found = true;
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "54b9c696-ec04-4cc0-a3b4-ca8cbe93de32");
                                break;
                            }
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3915ac6c-47c3-4d8d-85b9-0690b0134ea7");
        if (found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4dbd0de4-ed21-413b-bd2f-7ff44dfc5471");
            archive.position(off);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "882b89ad-8572-4458-8dc6-dd53090a1ab4");
        return found;
    }

    /**
     * Skips the given number of bytes or throws an EOFException if
     * skipping failed.
     */
    private void skipBytes(final int count) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "119095bb-2bc1-4ab9-bda7-5b923a83a857");
        long currentPosition = archive.position();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3f81ef1e-44d7-442d-b8b2-4b69ab2e472b");
        long newPosition = currentPosition + count;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "ba9168b6-f8ec-4ea1-8938-fed846778d47");
        if (newPosition > archive.size()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "1a508647-00df-4c9b-aa3a-3175b942bbf4");
            throw new EOFException();
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "63abbf00-9617-4e15-b394-129d03d2843b");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "442f123c-dc95-45e5-8586-0a26233238ca");
        for (final ZipArchiveEntry zipArchiveEntry : entries) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "72ca6df2-04a2-4bfa-82a8-e1b172f46277");
            final Entry ze = (Entry) zipArchiveEntry;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7324192a-ee30-49d4-955c-d8f139ef1e39");
            final long offset = ze.getLocalHeaderOffset();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "189dc8f0-0411-47bd-a09e-0da8c72f5b5f");
            archive.position(offset + LFH_OFFSET_FOR_FILENAME_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "cda23cd6-a5f9-43ba-8245-5ee7482be29d");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "734a140b-517c-4339-93fc-59d8b16c009e");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "1f4e6313-230f-4b84-85e7-35a6583ee2bc");
            wordBbuf.flip();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "dc05725e-47d3-4a1e-bc8e-2216aa13eec2");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "a90fcd74-ce00-4b22-a966-6a71215997cf");
            final int fileNameLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "bb3d38d7-bb26-41e1-8aa0-7ea09e1a41a6");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "7b31ccff-bee5-4c05-8c2d-f0078c013e22");
            final int extraFieldLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b445b085-a948-45b9-afea-e130c0efcc96");
            skipBytes(fileNameLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "88fef409-94dd-4c0d-a859-1f6fd8dd5d2d");
            final byte[] localExtraData = new byte[extraFieldLen];
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "beea296e-d1de-4c6a-9520-2a35a993d1a2");
            IOUtils.readFully(archive, ByteBuffer.wrap(localExtraData));
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b1e4072a-6373-4328-b482-bc0b8fb79b87");
            ze.setExtra(localExtraData);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "24422b01-1018-487c-aa2d-da9c89f9823d");
            ze.setDataOffset(offset + LFH_OFFSET_FOR_FILENAME_LENGTH + SHORT + SHORT + fileNameLen + extraFieldLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "ff420ca3-944c-4d65-9eec-3d4d1df0d147");
            ze.setStreamContiguous(true);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "5a65fc28-da4a-4d33-bc5f-5285d84b52dc");
            if (entriesWithoutUTF8Flag.containsKey(ze)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "038ba70e-ecf5-4a75-9b79-eb129a3a06c2");
                final NameAndComment nc = entriesWithoutUTF8Flag.get(ze);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "3c222a13-610c-498f-b6dc-c6020d679da5");
                ZipUtil.setNameAndCommentFromExtraFields(ze, nc.name, nc.comment);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "71b69d11-ad41-426b-a1df-02c7b04543bc");
            final String name = ze.getName();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "68fbfcb8-5711-4819-80eb-b0c94bbfeb24");
            LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b8b77f0d-4987-4590-a7a4-8fb9ef5cf959");
            if (entriesOfThatName == null) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "c634915a-77e8-4f97-956b-f9a47b86ebef");
                entriesOfThatName = new LinkedList<ZipArchiveEntry>();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "2f276fa9-15e3-4e9c-b121-995c3155ce3b");
                nameMap.put(name, entriesOfThatName);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "8ad1f234-e684-4422-a0cc-e1781e4cd56d");
            entriesOfThatName.addLast(ze);
        }
    }

    /**
     * Checks whether the archive starts with a LFH.  If it doesn't,
     * it may be an empty archive.
     */
    private boolean startsWithLocalFileHeader() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "4f7f88b8-2c27-480a-a206-6b368217804e");
        archive.position(0);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "d379c7d3-611b-42b9-992b-4dee0ebf9507");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "b8d3ab11-a60c-4201-99fa-691430679302");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "0a46f82d-dfd5-40ee-b03b-eed8e37b8406");
        return Arrays.equals(wordBuf, ZipArchiveOutputStream.LFH_SIG);
    }

    /**
     * Creates new BoundedInputStream, according to implementation of
     * underlying archive channel.
     */
    private BoundedInputStream createBoundedInputStream(long start, long remaining) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_1_10.coverage", "18b2a657-e2e8-4229-bd23-b9f5d40f0a72");
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
