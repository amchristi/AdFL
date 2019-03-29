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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "0bcb3e45-6ceb-4c51-82d4-4d84fda852d2");
        return encoding;
    }

    /**
     * Closes the archive.
     * @throws IOException if an error occurs closing the archive.
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "24053693-2c93-4936-930f-4bfd44d94233");
        closed = true;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "ed1e41c9-ebea-464e-a572-1bc7083110ff");
        archive.close();
    }

    /**
     * close a zipfile quietly; throw no io fault, do nothing
     * on a null parameter
     * @param zipfile file to close, can be null
     */
    public static void closeQuietly(final ZipFile zipfile) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "83c00f04-7922-48c4-b3bc-35cb84a0bf04");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d4376b76-df5c-480c-a93d-eb169b82efd8");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "413e7111-2be2-40d0-ba8e-9c3bbf98ae62");
        final ZipArchiveEntry[] allEntries = entries.toArray(new ZipArchiveEntry[entries.size()]);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "7453b02f-06db-4eed-8fc6-10efa41ae406");
        Arrays.sort(allEntries, offsetComparator);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a9bb28b4-5e26-4ca7-92c5-442fe38ddc14");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a5b3ac6c-918f-4490-8ae6-80a136595d50");
        final LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "35250c80-6276-4816-9c8e-0a02a1cc99a5");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "f6eb718b-fbb0-4edf-8e1f-9a72804a3b99");
        final List<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2ecc76db-5bf4-4bf4-81cb-05073c184ab8");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5938f4ea-3fbb-4ff4-a42b-521cd7d73fec");
        ZipArchiveEntry[] entriesOfThatName = new ZipArchiveEntry[0];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "6e891734-1f1a-436c-8abb-c5d023e9969a");
        if (nameMap.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e374c371-49c4-4b84-90a3-6e6217e27635");
            entriesOfThatName = nameMap.get(name).toArray(entriesOfThatName);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8cf302ef-6291-4d78-a63f-bb5fe7cab3b8");
            Arrays.sort(entriesOfThatName, offsetComparator);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "1ec2baeb-7469-4b37-9926-16c835d383db");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "4bfaa276-af45-4aeb-802b-683dfc747a35");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c41a5b57-be86-4845-bb82-ead6f0ff8512");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8edef66e-1a06-43cd-91d3-e56750278baf");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2bd4049a-48a4-43ef-a57e-65bad45f9b4a");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b6b69b8d-bcbf-4cf3-be88-ac5df9508f3f");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "ef5dd93f-2e2a-4984-860c-1cec28fa2d92");
        final Enumeration<ZipArchiveEntry> src = getEntriesInPhysicalOrder();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "883227a4-376c-4b5e-b06a-806fe84bc15c");
        while (src.hasMoreElements()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "fa0791d8-40cb-4bfe-8569-e388044aa34b");
            final ZipArchiveEntry entry = src.nextElement();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "eb252c88-9d3b-43f8-b657-4f5e8cb5f6af");
            if (predicate.test(entry)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "567bc240-0875-49b1-be79-3a24d30e5e06");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b1c247f6-84c8-43e6-b714-0e15f6f1d682");
        if (!(ze instanceof Entry)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "4d5f3125-8bc5-4523-b5f3-72e773c4b7b8");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "6ceadcd2-2e01-460a-960a-bc5bef27d0ce");
        ZipUtil.checkRequestedFeatures(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "542cb299-f21d-450e-b71d-22a0eb6d1fc7");
        final long start = ze.getDataOffset();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "952f4860-abc9-42ce-85eb-e59c57a9a610");
        final BoundedInputStream bis = createBoundedInputStream(start, ze.getCompressedSize());
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d1d1d813-2b7f-4561-9488-b7cab5f70f0a");
        switch(ZipMethod.getMethodByCode(ze.getMethod())) {
            case STORED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "71426ca9-c969-471e-88ae-51fa640a9d22");
                return bis;
            case UNSHRINKING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "affc0354-1eb3-4c14-95c4-1914bb74e42b");
                return new UnshrinkingInputStream(bis);
            case IMPLODING:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "02f740b6-6f08-41e2-b400-0a109a6069ee");
                return new ExplodingInputStream(ze.getGeneralPurposeBit().getSlidingDictionarySize(), ze.getGeneralPurposeBit().getNumberOfShannonFanoTrees(), new BufferedInputStream(bis));
            case DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "07e5b09c-851c-40bc-900b-952bebf7d662");
                bis.addDummy();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3f6ed8c9-68f9-4bde-96cc-c27256f366f1");
                final Inflater inflater = new Inflater(true);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "dec2e434-fb9b-4100-90c7-e8c5b79fb19d");
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
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "86d3d967-8498-4def-ac1f-0d5946f7eceb");
                return new BZip2CompressorInputStream(bis);
            case ENHANCED_DEFLATED:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3e93c2d8-0341-41f6-845b-27bc9ce2c190");
                return new Deflate64CompressorInputStream(bis);
            default:
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e6d0c587-09c1-4f68-8e0b-435a64d2a034");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "f2bcffcc-2cbb-47b5-867b-50ae47fab439");
        if (entry != null && entry.isUnixSymlink()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c25583cc-8455-4bb7-878c-f6a28d1003d1");
            try {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "f6c2c9d2-d98e-4c4e-a3e7-7331e189b0e1");
                InputStream in = getInputStream(entry);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "f0124602-6f99-45a4-a4c6-fee902edf6fa");
                return zipEncoding.decode(IOUtils.toByteArray(in));
            } catch (Exception ex) {
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c32eba45-ee7d-4ab5-8fd4-c685b3cf3d7a");
        return null;
    }

    /**
     * Ensures that the close method of this zipfile is called when
     * there are no more references to it.
     * @see #close()
     */
    @Override
    protected void finalize() throws Throwable {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b46746ac-cf3b-4ca4-a1dc-1b865c02094b");
        try {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "fd04555b-6f76-403e-9b77-eb89598ebaa0");
            if (!closed) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "54a1821a-3465-4e62-8400-4f3eebba7c3d");
                System.err.println("Cleaning up unclosed ZipFile for archive " + archiveName);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8faeebd7-edc3-43bd-a86e-1b6564b77445");
                close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "f9eece10-861c-443b-bcd5-91adc99ead2c");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "ef92274d-47a7-4545-ae3e-ddff48cc3de5");
        final HashMap<ZipArchiveEntry, NameAndComment> noUTF8Flag = new HashMap<ZipArchiveEntry, NameAndComment>();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "671f8068-75a3-47af-ba14-74fd829ca818");
        positionAtCentralDirectory();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d02af3b6-68e2-4ed9-a7d2-394c17b2295f");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c986f942-221d-4678-ae6e-7aa1645b2fd5");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "57c79d25-8b23-4194-9b60-9f2be897b940");
        long sig = ZipLong.getValue(wordBuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "65746c03-27ce-4497-80be-82c3b38e8e75");
        if (sig != CFH_SIG && startsWithLocalFileHeader()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "42a474e3-5356-4e9a-abc8-316a97bc9a97");
            throw new IOException("central directory is empty, can't expand" + " corrupt archive.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "f4cadd69-53d4-4c6a-9402-519efbc42ab6");
        while (sig == CFH_SIG) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3af286d2-4e21-42cf-ba14-02f7379229d4");
            readCentralDirectoryEntry(noUTF8Flag);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8e507bff-3f89-4524-bbd3-98693cd139d2");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "9fb3abfb-9f56-40ef-8bc6-e5a64ccb3521");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2761bb31-1494-4f88-8f88-482697dd7901");
            sig = ZipLong.getValue(wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8538eaf1-3ac6-422b-8cff-6d4ced0c0db0");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "4fe5be7f-23ab-4d8f-a54e-d6fcbc9e94d2");
        cfhBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e8599109-ce2b-4d39-b317-8c077674bafd");
        IOUtils.readFully(archive, cfhBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "06ebb901-2ea2-460d-b0e3-2f487bbeb1ac");
        int off = 0;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "136ddef7-776b-4dfb-881b-ac4312160764");
        final Entry ze = new Entry();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "89d0d2d9-c370-4bd9-b8ef-ff4fd8c2ec62");
        final int versionMadeBy = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "ff705f4e-6d0b-435e-9ac6-3bc10aa4010f");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "85c71bd8-57fd-4fbf-bcda-fed429eb5fe8");
        ze.setVersionMadeBy(versionMadeBy);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3a0c2fdd-e125-4176-937a-47c34d0a0c09");
        ze.setPlatform((versionMadeBy >> BYTE_SHIFT) & NIBLET_MASK);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5c2570ed-fc83-48a8-9932-29ea4d4e13f1");
        ze.setVersionRequired(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "6f316700-1645-471a-aa2a-03b864dfe759");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b60a38da-8d05-4512-a3d9-7efc5cc2d5cd");
        final GeneralPurposeBit gpFlag = GeneralPurposeBit.parse(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "64dc0528-36db-4859-8c0d-2a300b0df2c8");
        final boolean hasUTF8Flag = gpFlag.usesUTF8ForNames();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3cd1a2cf-7c0e-4619-8bc3-0227c9ca8a05");
        final ZipEncoding entryEncoding = hasUTF8Flag ? ZipEncodingHelper.UTF8_ZIP_ENCODING : zipEncoding;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "746b4e67-e1ed-4f3e-ad54-5352ec314bdd");
        if (hasUTF8Flag) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8015377c-bcfd-4db4-b5c9-0f010f84188f");
            ze.setNameSource(ZipArchiveEntry.NameSource.NAME_WITH_EFS_FLAG);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d4f6fde6-a527-4ae6-b261-b2812669caf0");
        ze.setGeneralPurposeBit(gpFlag);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "83aaa36f-2519-449e-b6a9-18fbc74948e6");
        ze.setRawFlag(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d929d5ce-84f1-48ac-b4b1-82fb4d522f60");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8cf60bc4-dec6-4b99-8184-39c25828c357");
        ze.setMethod(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e32fd80f-50e4-4f1f-b38b-4cd2dd3e66d1");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "1f058eeb-5583-4f16-b306-fed2d3d71b03");
        final long time = ZipUtil.dosToJavaTime(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5f6aa819-998d-40bc-9be0-48262eca137e");
        ze.setTime(time);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "674d67ef-5b59-4ce2-aac6-02a714c18a44");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "023f6624-32af-426c-85fe-5a2741f46b86");
        ze.setCrc(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c222f685-29f7-47c1-aa18-def25002f755");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2cd5fd0c-b8b8-4bc8-be04-3cb08e55c661");
        ze.setCompressedSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "49bdf2df-2b92-4465-8ee3-f2dfa45c300f");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a59501e2-d628-4504-8aea-b446ec2e6b99");
        ze.setSize(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "1a5b3350-9232-4d24-b68d-52e63c1eb40f");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c188ec52-dc7d-4f02-91db-439bb76380fd");
        final int fileNameLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b584b61a-e0dc-4fee-9248-a762a099a0aa");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "55497597-3563-449f-92b2-12e94189960a");
        final int extraLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "cf2a1135-8f3f-4247-bd75-58ec2ce0efcf");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c410916f-2bf2-4ae8-8b7f-72c1ec9c2365");
        final int commentLen = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3a748cad-cdbd-4070-983a-9b94843e36f8");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "292a35d0-c768-4eb1-b05f-2020e65b43c7");
        final int diskStart = ZipShort.getValue(cfhBuf, off);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "7529f48d-ee5d-4166-a287-6c24293643b8");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "26903084-0ade-418a-a21b-4f79322a94c5");
        ze.setInternalAttributes(ZipShort.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "0e96e1c9-8424-4ae3-933f-5e8dec480206");
        off += SHORT;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "82d8bb8f-f13b-4fe1-9fbb-61a4aa4e1c2f");
        ze.setExternalAttributes(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a4d920e3-1df9-4179-9c61-2869e32fcf45");
        off += WORD;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "85569009-0519-4005-8a55-08b79252eb0c");
        final byte[] fileName = new byte[fileNameLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "046911a6-9b01-45b0-97ba-4580975038d0");
        IOUtils.readFully(archive, ByteBuffer.wrap(fileName));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "aa434d1c-9551-4181-b53f-9b2631ec12ff");
        ze.setName(entryEncoding.decode(fileName), fileName);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b549c8f5-edba-42b8-8c9d-59387a40d025");
        ze.setLocalHeaderOffset(ZipLong.getValue(cfhBuf, off));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "943e7c8f-05c0-4e1f-8f78-a924ecd0de2d");
        entries.add(ze);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "4ab564fe-1205-4aa0-b829-e9bb427641f4");
        final byte[] cdExtraData = new byte[extraLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e62a6b30-352b-4872-8e9e-85d57264e462");
        IOUtils.readFully(archive, ByteBuffer.wrap(cdExtraData));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "20bd51d1-b395-4955-8c33-b64c9ed89cc7");
        ze.setCentralDirectoryExtra(cdExtraData);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "94887733-7a37-4caa-97db-a7d16527996c");
        setSizesAndOffsetFromZip64Extra(ze, diskStart);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5169cb82-9aee-4580-b0bf-789a246381b6");
        final byte[] comment = new byte[commentLen];
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d6ce25cf-afb9-4da4-8238-d78444ac0706");
        IOUtils.readFully(archive, ByteBuffer.wrap(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b22cf7bf-9ff4-4839-ba86-64d8221b50fa");
        ze.setComment(entryEncoding.decode(comment));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d87659ce-697f-4c96-9e04-878a1d1279c4");
        if (!hasUTF8Flag && useUnicodeExtraFields) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c63ad8b0-ed0b-43c8-8a19-ee67754fa636");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "6c03741e-89a0-466c-adb8-367e788dab21");
        final Zip64ExtendedInformationExtraField z64 = (Zip64ExtendedInformationExtraField) ze.getExtraField(Zip64ExtendedInformationExtraField.HEADER_ID);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "1c5abc35-3ef5-449d-afee-ae587abf60d9");
        if (z64 != null) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a1f9b393-52d7-4a13-961f-c34988adb4ef");
            final boolean hasUncompressedSize = ze.getSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "77b19631-87bb-49ce-9148-91946e1bc75b");
            final boolean hasCompressedSize = ze.getCompressedSize() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2c277fa6-a958-4797-b9b8-b305a1e93f29");
            final boolean hasRelativeHeaderOffset = ze.getLocalHeaderOffset() == ZIP64_MAGIC;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "58922f85-bdb8-4edc-a5e7-db128df902eb");
            z64.reparseCentralDirectoryData(hasUncompressedSize, hasCompressedSize, hasRelativeHeaderOffset, diskStart == ZIP64_MAGIC_SHORT);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "040dd8d7-2ad2-418f-9fe2-f70055cc8ea2");
            if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c2cb8dbd-85bf-41bd-abae-72bc2e7f298a");
                ze.setSize(z64.getSize().getLongValue());
            } else if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3ee2f8fa-ef25-408c-9f4a-0a69c2d83350");
                z64.setSize(new ZipEightByteInteger(ze.getSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "9c5961a6-b8b5-4441-8a25-bafeb3acaee4");
            if (hasCompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d62fa6ff-b72e-4d19-901a-fe5d474cb676");
                ze.setCompressedSize(z64.getCompressedSize().getLongValue());
            } else if (hasUncompressedSize) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "7108748a-ab6d-4900-a773-bc512d683b9a");
                z64.setCompressedSize(new ZipEightByteInteger(ze.getCompressedSize()));
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "7862e7b0-ef3a-4d7d-a626-3385bd1af457");
            if (hasRelativeHeaderOffset) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e947a87e-a583-4211-8276-41945a3314cc");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "12b67c36-f4a6-42a4-9516-4b0dc59f835c");
        positionAtEndOfCentralDirectoryRecord();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "9389201a-ba82-4ba5-a3d6-263ae8014d69");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "23c5b7bd-5a02-40ff-9fe7-fb23b2f7bd31");
        final boolean searchedForZip64EOCD = archive.position() > ZIP64_EOCDL_LENGTH;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "06e052dc-0b79-4ca6-820d-c77321fc7a6d");
        if (searchedForZip64EOCD) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b337f9a4-e414-4762-98ea-dcd0ce280564");
            archive.position(archive.position() - ZIP64_EOCDL_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a5fe53ce-8e42-4d76-918e-cf630e6d493a");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a2d436e0-00cf-4901-86dd-a05feeed099b");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "26f2260a-f241-46b5-b892-adb34a6719de");
            found = Arrays.equals(ZipArchiveOutputStream.ZIP64_EOCD_LOC_SIG, wordBuf);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "682196bc-b241-4211-92a0-f7ebc2954eef");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "1735498a-ecd4-442c-9fe5-96dd269de08a");
            if (searchedForZip64EOCD) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "364e355c-80f8-4a26-991a-feb813cfc1f8");
                skipBytes(ZIP64_EOCDL_LENGTH - WORD);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "17a205ab-68ca-4c32-a6bc-6eacf29e04d1");
            positionAtCentralDirectory32();
        } else {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "0844f374-52b2-47f3-9efa-ed0fe8681b84");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c5dbd992-2d7a-431a-ac10-d73f850b8cff");
        skipBytes(ZIP64_EOCDL_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "44ec8166-e788-41e5-aade-6bbc44802c11");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "064b5c90-364c-43ac-9495-945c0014255d");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3c956aee-6272-42b8-bbe9-02b81a28d0a0");
        archive.position(ZipEightByteInteger.getLongValue(dwordBuf));
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "7481d2b8-121d-4074-a7c7-5ed7926dfb87");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "dc7cc712-0025-4148-bedb-abc08c0bb749");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "bd600f9d-88a6-4bec-85fe-b310ab03b147");
        if (!Arrays.equals(wordBuf, ZipArchiveOutputStream.ZIP64_EOCD_SIG)) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "91236f0c-9f21-47e3-9aca-9dce64770683");
            throw new ZipException("archive's ZIP64 end of central " + "directory locator is corrupt.");
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2b988f84-8ec6-4ace-856f-f35e3084333f");
        skipBytes(ZIP64_EOCD_CFD_LOCATOR_OFFSET - WORD);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "86230b13-441b-44e2-9fe0-dde95f5eaa84");
        dwordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c4609319-608d-4452-9f02-3a27537d7567");
        IOUtils.readFully(archive, dwordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "272bdb04-deb1-47d4-b79b-72a52d636a7b");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e69641b1-730f-410f-93b9-e5b66ceb6fd2");
        skipBytes(CFD_LOCATOR_OFFSET);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "9a71017f-d279-45be-883c-1511dcf7966a");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "cd1486bb-0003-4197-b5fc-8ba85b29dbec");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "2b9442ed-fa48-4d0d-ae68-95abd6332b3c");
        archive.position(ZipLong.getValue(wordBuf));
    }

    /**
     * Searches for the and positions the stream at the start of the
     * &quot;End of central dir record&quot;.
     */
    private void positionAtEndOfCentralDirectoryRecord() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e42e27bb-1039-4837-a570-1e5d98f41375");
        final boolean found = tryToLocateSignature(MIN_EOCD_SIZE, MAX_EOCD_SIZE, ZipArchiveOutputStream.EOCD_SIG);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e85c8620-ad90-40a3-81d9-e800744c7f5c");
        if (!found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "cf8ccce3-f3d7-40f5-bc08-4b7a6f79a8c5");
            throw new ZipException("archive is not a ZIP archive");
        }
    }

    /**
     * Searches the archive backwards from minDistance to maxDistance
     * for the given signature, positions the RandomaccessFile right
     * at the signature if it has been found.
     */
    private boolean tryToLocateSignature(final long minDistanceFromEnd, final long maxDistanceFromEnd, final byte[] sig) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a2e63ca7-059e-4b8d-a97b-8a9f526c7d36");
        boolean found = false;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "7fce06cf-5c51-4a08-a0b1-8c0cadcd96f6");
        long off = archive.size() - minDistanceFromEnd;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e88ce673-5d44-44b8-9a8f-ca2358785719");
        final long stopSearching = Math.max(0L, archive.size() - maxDistanceFromEnd);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a0dddf2b-39e8-4598-b81e-aa56284289c9");
        if (off >= 0) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "19dee3fa-cd2d-4959-b9b8-5c2f2c517272");
            for (; off >= stopSearching; off--) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "fd8c57c7-c673-4ccb-978f-61f7d9df53d9");
                archive.position(off);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "165a38c7-d9d5-461f-8a16-40099423663f");
                try {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "1422ad04-7e6f-41a3-adac-72522da349ac");
                    wordBbuf.rewind();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "3fbaf62a-9a76-4380-8a25-023131355a80");
                    IOUtils.readFully(archive, wordBbuf);
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a0ec70b5-7989-455a-a88d-cbc03eda9279");
                    wordBbuf.flip();
                } catch (EOFException ex) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d95d3094-017a-4a9e-8513-efc7bbaab7d3");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8dad3e5b-a2df-435b-b069-a3e412214074");
                int curr = wordBbuf.get();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "355d59d6-d9e0-4a4e-9618-80286eb2a8eb");
                if (curr == sig[POS_0]) {
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "02f069e2-8409-43c2-837d-913ddabede5d");
                    curr = wordBbuf.get();
                    writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "257f68e2-8d86-48b2-afe7-1610e7e99b0f");
                    if (curr == sig[POS_1]) {
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5afab377-2c4e-4920-a4e4-c49a887181c1");
                        curr = wordBbuf.get();
                        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b0e19597-c8d7-4513-bf59-22a13cb9dffd");
                        if (curr == sig[POS_2]) {
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "70e568a2-afba-431f-9e8b-d829b09afd18");
                            curr = wordBbuf.get();
                            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "be6d4716-8fbe-433e-ae19-b09559b230b7");
                            if (curr == sig[POS_3]) {
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "bbe9d86b-45c4-45d9-a0cc-8866ca0c0aad");
                                found = true;
                                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "976a66b5-b841-4e5c-8982-1955af4ac6e8");
                                break;
                            }
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "e4d16b8e-3bb5-41c8-9256-38712a23d805");
        if (found) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "93798894-a8d9-4668-b7cc-b5c06a5d102c");
            archive.position(off);
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "90a1b5cb-65e9-46d5-9643-1a579b63f807");
        return found;
    }

    /**
     * Skips the given number of bytes or throws an EOFException if
     * skipping failed.
     */
    private void skipBytes(final int count) throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8e34f575-f52c-48ae-8b95-e0b473b9980b");
        long currentPosition = archive.position();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "22b39fd2-262a-4763-b0f8-276dedd31d04");
        long newPosition = currentPosition + count;
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "14ea4043-9477-44e3-9eed-7a86125ec38f");
        if (newPosition > archive.size()) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "557c231a-6429-4999-90ef-06fea72ffba8");
            throw new EOFException();
        }
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c483877a-5939-4fb7-a765-584ba535326d");
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
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "91789152-5328-491b-a03b-404a8b406ae4");
        for (final ZipArchiveEntry zipArchiveEntry : entries) {
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "9389521b-dfd2-4aca-823b-75411af04f40");
            final Entry ze = (Entry) zipArchiveEntry;
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "21fa0010-68cc-4f2e-ba7e-ac006efb51e7");
            final long offset = ze.getLocalHeaderOffset();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "aa66bd4a-eda1-4341-bfcb-492b3cd0fb19");
            archive.position(offset + LFH_OFFSET_FOR_FILENAME_LENGTH);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "722cba7f-ba68-4f25-b46a-979ae87ef235");
            wordBbuf.rewind();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "a6888d59-bc2e-4fc5-a805-e4c3c2eea236");
            IOUtils.readFully(archive, wordBbuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8f006788-b030-437f-9c05-d35414b83f8a");
            wordBbuf.flip();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5fa76d36-c0a2-40ae-afc1-fab85e8566d7");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "4dc1c64a-7028-45ef-8152-bfd567fe35f2");
            final int fileNameLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "13918a99-85ce-4c27-8df6-c6e7b44929d5");
            wordBbuf.get(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c8461ff2-8f4d-46c5-abc7-04cad7c7c3ef");
            final int extraFieldLen = ZipShort.getValue(shortBuf);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "cf209a13-66b6-463d-9302-6e01dbf6318d");
            skipBytes(fileNameLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "342f1606-ff49-4a2d-8766-55a24693f856");
            final byte[] localExtraData = new byte[extraFieldLen];
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5e1b3b8a-d2fe-4db3-856b-d952d2c2deba");
            IOUtils.readFully(archive, ByteBuffer.wrap(localExtraData));
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c7406d7a-14ed-489d-9083-73f2c2636977");
            ze.setExtra(localExtraData);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "589e9e60-8da2-44f8-af5e-01644174e03c");
            ze.setDataOffset(offset + LFH_OFFSET_FOR_FILENAME_LENGTH + SHORT + SHORT + fileNameLen + extraFieldLen);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "19dd6be8-0f8a-4fa0-b7ea-08b3a75249c4");
            ze.setStreamContiguous(true);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8660e48c-370f-454d-a9c7-e059506f419f");
            if (entriesWithoutUTF8Flag.containsKey(ze)) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "cfc434fe-4163-46ad-be7f-268e4b957377");
                final NameAndComment nc = entriesWithoutUTF8Flag.get(ze);
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "35f62398-7070-4dcb-b9d8-3a7aea4819cb");
                ZipUtil.setNameAndCommentFromExtraFields(ze, nc.name, nc.comment);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d0917ad3-3398-48f1-850c-f103e79bec51");
            final String name = ze.getName();
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "382dd4fb-c254-4b88-bbca-2d024db2def8");
            LinkedList<ZipArchiveEntry> entriesOfThatName = nameMap.get(name);
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d2630ece-0053-472d-8e1b-dd3883950cdb");
            if (entriesOfThatName == null) {
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "8f4b2fac-259f-4105-902f-0f62d29340c3");
                entriesOfThatName = new LinkedList<ZipArchiveEntry>();
                writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "55a2115f-087a-4147-93ba-ff9e4344e447");
                nameMap.put(name, entriesOfThatName);
            }
            writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "05cf4714-1747-47d0-a8c1-3de51249b59d");
            entriesOfThatName.addLast(ze);
        }
    }

    /**
     * Checks whether the archive starts with a LFH.  If it doesn't,
     * it may be an empty archive.
     */
    private boolean startsWithLocalFileHeader() throws IOException {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "c132af4e-730d-434b-8cd8-2ade10d2d7d9");
        archive.position(0);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "d83905cf-09e2-4dfc-a841-26f99a172903");
        wordBbuf.rewind();
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "b7a8d1ec-5212-4011-871b-19e09f8da7f8");
        IOUtils.readFully(archive, wordBbuf);
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "56a4d831-01a1-4617-a56d-ec7abcc63a80");
        return Arrays.equals(wordBuf, ZipArchiveOutputStream.LFH_SIG);
    }

    /**
     * Creates new BoundedInputStream, according to implementation of
     * underlying archive channel.
     */
    private BoundedInputStream createBoundedInputStream(long start, long remaining) {
        writeline("/home/ubuntu/results/coverage/ZipFile/ZipFile_3_10.coverage", "5bf07d38-76be-4972-965c-32b42603b85e");
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
