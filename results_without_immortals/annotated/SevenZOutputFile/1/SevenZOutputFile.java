package org.apache.commons.compress.archivers.sevenz;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.zip.CRC32;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.utils.CountingOutputStream;
import java.io.*;

/**
 * Writes a 7z file.
 * @since 1.6
 */
public class SevenZOutputFile implements Closeable {

    private final SeekableByteChannel channel;

    private final List<SevenZArchiveEntry> files = new ArrayList<SevenZArchiveEntry>();

    private int numNonEmptyStreams = 0;

    private final CRC32 crc32 = new CRC32();

    private final CRC32 compressedCrc32 = new CRC32();

    private long fileBytesWritten = 0;

    private boolean finished = false;

    private CountingOutputStream currentOutputStream;

    private CountingOutputStream[] additionalCountingStreams;

    private Iterable<? extends SevenZMethodConfiguration> contentMethods = Collections.singletonList(new SevenZMethodConfiguration(SevenZMethod.LZMA2));

    private final Map<SevenZArchiveEntry, long[]> additionalSizes = new HashMap<SevenZArchiveEntry, long[]>();

    /**
     * Opens file to write a 7z archive to.
     *
     * @param filename the file to write to
     * @throws IOException if opening the file fails
     */
    public SevenZOutputFile(final File filename) throws IOException {
        this(Files.newByteChannel(filename.toPath(), EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)));
    }

    /**
     * Prepares channel to write a 7z archive to.
     *
     * <p>{@link
     * org.apache.commons.compress.utils.SeekableInMemoryByteChannel}
     * allows you to write to an in-memory archive.</p>
     *
     * @param channel the channel to write to
     * @throws IOException if the channel cannot be positioned properly
     * @since 1.13
     */
    public SevenZOutputFile(final SeekableByteChannel channel) throws IOException {
        this.channel = channel;
        channel.position(SevenZFile.SIGNATURE_HEADER_SIZE);
    }

    /**
     * Sets the default compression method to use for entry contents - the
     * default is LZMA2.
     *
     * <p>Currently only {@link SevenZMethod#COPY}, {@link
     * SevenZMethod#LZMA2}, {@link SevenZMethod#BZIP2} and {@link
     * SevenZMethod#DEFLATE} are supported.</p>
     *
     * <p>This is a short form for passing a single-element iterable
     * to {@link #setContentMethods}.</p>
     * @param method the default compression method
     */
    public void setContentCompression(final SevenZMethod method) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "dbaad18d-4ded-4c15-8808-6ec488d21f28");
        setContentMethods(Collections.singletonList(new SevenZMethodConfiguration(method)));
    }

    /**
     * Sets the default (compression) methods to use for entry contents - the
     * default is LZMA2.
     *
     * <p>Currently only {@link SevenZMethod#COPY}, {@link
     * SevenZMethod#LZMA2}, {@link SevenZMethod#BZIP2} and {@link
     * SevenZMethod#DEFLATE} are supported.</p>
     *
     * <p>The methods will be consulted in iteration order to create
     * the final output.</p>
     *
     * @since 1.8
     * @param methods the default (compression) methods
     */
    public void setContentMethods(final Iterable<? extends SevenZMethodConfiguration> methods) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4c78e17b-4778-49c8-8fd0-c5e99c8e769c");
        this.contentMethods = reverse(methods);
    }

    /**
     * Closes the archive, calling {@link #finish} if necessary.
     *
     * @throws IOException on error
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "348f1d3b-f58d-42e7-8346-ac69d08e83c7");
        if (!finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "02a8cd05-0bab-483c-aa6f-66d7aaf5fd27");
            finish();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ec35e5eb-a169-4451-b2aa-33c8d8bd47da");
        channel.close();
    }

    /**
     * Create an archive entry using the inputFile and entryName provided.
     *
     * @param inputFile file to create an entry from
     * @param entryName the name to use
     * @return the ArchiveEntry set up with details from the file
     *
     * @throws IOException on error
     */
    public SevenZArchiveEntry createArchiveEntry(final File inputFile, final String entryName) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ac725321-a2ee-48c5-8cfa-d777aede833f");
        final SevenZArchiveEntry entry = new SevenZArchiveEntry();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f25f5eaf-4afc-4824-b3fc-f8e9321b3c05");
        entry.setDirectory(inputFile.isDirectory());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ebed90fb-c0a5-4d64-bc43-18b45dbdc52b");
        entry.setName(entryName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "04cc114f-cf32-47eb-be2e-cd22ccb71b45");
        entry.setLastModifiedDate(new Date(inputFile.lastModified()));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "79075114-2e4f-4d99-821e-dca4409bf6f0");
        return entry;
    }

    /**
     * Records an archive entry to add.
     *
     * The caller must then write the content to the archive and call
     * {@link #closeArchiveEntry()} to complete the process.
     *
     * @param archiveEntry describes the entry
     * @throws IOException on error
     */
    public void putArchiveEntry(final ArchiveEntry archiveEntry) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8b6bb489-4899-4fa3-90c5-12a94530848a");
        final SevenZArchiveEntry entry = (SevenZArchiveEntry) archiveEntry;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8737ac06-cb58-450f-b628-36a855557014");
        files.add(entry);
    }

    /**
     * Closes the archive entry.
     * @throws IOException on error
     */
    public void closeArchiveEntry() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "821e699b-fb96-437b-8900-2b03dee02ad0");
        if (currentOutputStream != null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "668be74b-a835-4807-a938-431d0dc2f44c");
            currentOutputStream.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2d8249a5-a153-462f-834d-14d3dedae934");
            currentOutputStream.close();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a359cfba-7bf2-4721-9325-7d6f548e8f43");
        final SevenZArchiveEntry entry = files.get(files.size() - 1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ebcdcdfc-a063-4422-8f5c-e5d95163e624");
        if (fileBytesWritten > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "467c8d60-93b2-469e-9fdb-dacde1e4c93d");
            entry.setHasStream(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e7a9b6f6-f3bb-4680-9754-05ae2600214e");
            ++numNonEmptyStreams;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "cc12ee7c-828e-44bb-ad1e-6ed385e2f422");
            entry.setSize(currentOutputStream.getBytesWritten());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "997d9d8b-0ed1-4175-8a5a-560428fcdcd2");
            entry.setCompressedSize(fileBytesWritten);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "99adbeb8-eddd-43b6-b9e4-07c0f189f3d5");
            entry.setCrcValue(crc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "331650fa-0e8b-4b0a-ad29-1ad4977f0e8f");
            entry.setCompressedCrcValue(compressedCrc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "edf5b86c-75d4-4333-bf63-b081dcaea363");
            entry.setHasCrc(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "42b5498c-906e-48a6-967f-96f4a9f4c307");
            if (additionalCountingStreams != null) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6142251d-8d38-497b-9bf3-eb7509082c3c");
                final long[] sizes = new long[additionalCountingStreams.length];
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b9a00f4d-f01d-4e86-91e6-0718d00209e7");
                for (int i = 0; i < additionalCountingStreams.length; i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "92355255-c2f3-4de7-b143-7f4b00b6018b");
                    sizes[i] = additionalCountingStreams[i].getBytesWritten();
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5b2ceb2d-3d1d-435d-83c9-1fccd7d85893");
                additionalSizes.put(entry, sizes);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "efa6d09f-bc23-4ee5-9cfc-e01e45af92aa");
            entry.setHasStream(false);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a40eac26-0eec-4782-916f-c73e73c81e54");
            entry.setSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a4c1d91e-8be4-4ba8-a5c2-ff990f0d9c93");
            entry.setCompressedSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f63c608d-fa66-4269-b315-a973f360e7f3");
            entry.setHasCrc(false);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4134ca69-e32f-4f31-b982-15bd73a78ab2");
        currentOutputStream = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a71ab88b-d7c1-4037-921c-42884f6bb502");
        additionalCountingStreams = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8bbb6155-2f64-47c8-b15d-850ee1747bc6");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e0fb5514-3580-4d3a-a441-66125b91a9f9");
        compressedCrc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3ebdd8fa-1764-4551-b443-bb38d76fb25b");
        fileBytesWritten = 0;
    }

    /**
     * Writes a byte to the current archive entry.
     * @param b The byte to be written.
     * @throws IOException on error
     */
    public void write(final int b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9cfa4f3e-2fa2-43e4-8a95-d01355d53ba3");
        getCurrentOutputStream().write(b);
    }

    /**
     * Writes a byte array to the current archive entry.
     * @param b The byte array to be written.
     * @throws IOException on error
     */
    public void write(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "21ef76f7-a1d7-4e18-a179-e9872245874b");
        write(b, 0, b.length);
    }

    /**
     * Writes part of a byte array to the current archive entry.
     * @param b The byte array to be written.
     * @param off offset into the array to start writing from
     * @param len number of bytes to write
     * @throws IOException on error
     */
    public void write(final byte[] b, final int off, final int len) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ca399210-9984-4e4a-9674-4bb2f158efe0");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "dc641ab4-4961-4272-b0e1-276fe5baa979");
            getCurrentOutputStream().write(b, off, len);
        }
    }

    /**
     * Finishes the addition of entries to this archive, without closing it.
     *
     * @throws IOException if archive is already closed.
     */
    public void finish() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e21a849c-efb0-4884-9d8a-556d955207b9");
        if (finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1315cf15-5d1f-46f3-80b2-06b4c6058b3c");
            throw new IOException("This archive has already been finished");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1b9b61c0-a6e6-4c31-8fbe-18dcb9e5ecd0");
        finished = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8a20ecee-10b8-46f2-afc3-6b44635d0f84");
        final long headerPosition = channel.position();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f230ea86-697e-46e7-9c16-bf9e98cc1edb");
        final ByteArrayOutputStream headerBaos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "74eba11f-3d41-4f90-a265-eddc83613ec1");
        final DataOutputStream header = new DataOutputStream(headerBaos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7100ae11-abdb-4af0-84b5-0fb4869518a8");
        writeHeader(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3e223029-48b3-4080-b0be-3bf8d9f5e7e6");
        header.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5d2b23d7-c0b6-4013-94a6-98be8fe4c35e");
        final byte[] headerBytes = headerBaos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "fbd86e2d-724f-49a2-96a1-d83a6c98cf5b");
        channel.write(ByteBuffer.wrap(headerBytes));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3e30ce27-f989-4b6c-86d3-7bd4bb7e85b8");
        final CRC32 crc32 = new CRC32();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a54bf7b3-8cb7-41ba-8617-5768c068cae1");
        crc32.update(headerBytes);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "15d201ba-27f3-46db-887a-79014e4fedc2");
        ByteBuffer bb = ByteBuffer.allocate(SevenZFile.sevenZSignature.length + 2 + 4 + 8 + 8 + 4).order(ByteOrder.LITTLE_ENDIAN);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5bbec063-bd6e-4e42-b180-a2b9fd1b9ac0");
        channel.position(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "dc058d8d-4491-4599-8713-f4a0fc22e897");
        bb.put(SevenZFile.sevenZSignature);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "fef97c27-26d7-4e04-a3dc-4ae282bdbf78");
        bb.put((byte) 0).put((byte) 2);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5193b8ab-c2db-4f9f-aa4a-50db20281339");
        bb.putInt(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "cd881a4e-4b49-4a24-a7cf-b4476cb019a2");
        bb.putLong(headerPosition - SevenZFile.SIGNATURE_HEADER_SIZE).putLong(0xffffFFFFL & headerBytes.length).putInt((int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7e7c99ba-96db-42da-bca8-f5b4d711bbd9");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "26bd2094-db10-49d1-8f71-7a72d7bca967");
        crc32.update(bb.array(), SevenZFile.sevenZSignature.length + 6, 20);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "65625a9d-bc84-4ab5-a50b-f9b368080b56");
        bb.putInt(SevenZFile.sevenZSignature.length + 2, (int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8a2f17f0-2c17-4b06-8b19-564c2d4b0dd2");
        bb.flip();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5c69c3c5-f2ac-4078-8286-f709fdd11533");
        channel.write(bb);
    }

    private OutputStream getCurrentOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5bcc30c2-4193-48a7-af3e-42ac2832f7f1");
        if (currentOutputStream == null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "10e2987b-039c-440c-8797-27c0f4562743");
            currentOutputStream = setupFileOutputStream();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4c7ff9aa-09bd-466b-abec-c23ead05097c");
        return currentOutputStream;
    }

    private CountingOutputStream setupFileOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9df29093-7982-4991-9da1-f22e86305f52");
        if (files.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "441d8b62-8a64-40c5-ac97-1cba878f5c6c");
            throw new IllegalStateException("No current 7z entry");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "dbf8c051-2a47-45e4-942f-951164554254");
        OutputStream out = new OutputStreamWrapper();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2f33b6b3-abf0-437d-b672-6971cf3c7c31");
        final ArrayList<CountingOutputStream> moreStreams = new ArrayList<CountingOutputStream>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "eafa422d-b5fd-4205-b059-e0f22f0b1b64");
        boolean first = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "39e6b031-786e-48bb-9c7f-d4320975dc42");
        for (final SevenZMethodConfiguration m : getContentMethods(files.get(files.size() - 1))) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9a35e8ac-5550-48ea-8189-0243bd3c5a0a");
            if (!first) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "35973748-6da6-45cd-be06-9343cead4f7f");
                final CountingOutputStream cos = new CountingOutputStream(out);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "226f414a-3869-4537-811f-7e432288b014");
                moreStreams.add(cos);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ba662e17-9256-44f4-aea9-44b88bcea593");
                out = cos;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f2306948-b99a-4bba-a542-adc14083f201");
            out = Coders.addEncoder(out, m.getMethod(), m.getOptions());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d2bd1b1a-55e1-4b3f-8d4f-1176577beb37");
            first = false;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0e70cadf-2d38-4ee8-8ad2-7ffcafd44856");
        if (!moreStreams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b1704008-2cd6-4e54-8f41-a88ac1dc171a");
            additionalCountingStreams = moreStreams.toArray(new CountingOutputStream[moreStreams.size()]);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a460ecd8-9a00-4af4-896a-a2e64c4f8593");
        return new CountingOutputStream(out) {

            @Override
            public void write(final int b) throws IOException {
                super.write(b);
                crc32.update(b);
            }

            @Override
            public void write(final byte[] b) throws IOException {
                super.write(b);
                crc32.update(b);
            }

            @Override
            public void write(final byte[] b, final int off, final int len) throws IOException {
                super.write(b, off, len);
                crc32.update(b, off, len);
            }
        };
    }

    private Iterable<? extends SevenZMethodConfiguration> getContentMethods(final SevenZArchiveEntry entry) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a190865d-ef0d-491e-b8db-674f260e6dc8");
        final Iterable<? extends SevenZMethodConfiguration> ms = entry.getContentMethods();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "84ae8616-0047-4afb-b09a-e4ba7aa81a46");
        return ms == null ? contentMethods : ms;
    }

    private void writeHeader(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6165e110-80b2-499a-b149-d869ed924f89");
        header.write(NID.kHeader);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "bbfb9888-ff0b-480d-b422-93d382330434");
        header.write(NID.kMainStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1d7bfce2-9458-46d5-ad45-1a178597fd03");
        writeStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b46c5423-384f-4111-96ad-a8c357ff8aa5");
        writeFilesInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b3b51db5-14ae-45c9-9cda-74a05ea787cf");
        header.write(NID.kEnd);
    }

    private void writeStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a0e1a06a-ecda-480b-9137-07a3e3eb999b");
        if (numNonEmptyStreams > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "05246a9f-a94e-492d-94b8-7808bc18fb39");
            writePackInfo(header);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "970b50b4-3a7d-4fd2-a9e6-578dd9382b52");
            writeUnpackInfo(header);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8cb88d14-8e1d-4abf-9b62-1c1c8217125e");
        writeSubStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "61227997-9203-49ae-bd85-3c41ecab2632");
        header.write(NID.kEnd);
    }

    private void writePackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "40791294-f944-4ee9-9fdc-d1a4cd94ebae");
        header.write(NID.kPackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1c0a1a5e-2334-4ac1-8d97-cff9965891e1");
        writeUint64(header, 0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "11089e20-5e33-4f57-bc3c-fb87a7e6fc23");
        writeUint64(header, 0xffffFFFFL & numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "88fa4af5-07a7-4012-b823-009b47bcce86");
        header.write(NID.kSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d94fb197-416e-4054-bbb3-47b325ebad1b");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "40c0c2ac-5d7d-4192-a53c-7f19c943f055");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "09ef0ec3-c59a-49d7-aa3b-a34c62b06076");
                writeUint64(header, entry.getCompressedSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "abe62a85-a03c-4f97-896e-48591887ffb9");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a658ccd9-a36c-4be2-ac7e-b0216e9b1c4d");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e1da9187-47b3-4ea9-b682-c060832634b7");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6f49e73a-98a9-4408-8770-653428a8fca6");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a2f9fafd-aa34-4ef7-a967-0de8505a003c");
                header.writeInt(Integer.reverseBytes((int) entry.getCompressedCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d976d049-21d2-4628-9223-8b0531949eae");
        header.write(NID.kEnd);
    }

    private void writeUnpackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f77fc971-8ab4-4dfe-bec2-079f0ff1d5ff");
        header.write(NID.kUnpackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "001808e3-94ab-4a0e-85e8-145b1a7b0c26");
        header.write(NID.kFolder);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c8e37989-e6e8-48bf-abd6-8923ba3a04e2");
        writeUint64(header, numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "228f8d3f-c92d-48e5-8090-a9d02526570f");
        header.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "279a3062-36e0-4c04-be1c-d8b1e78ec0c6");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "17acfe4f-f23c-4512-ac03-37f8b7cf24f0");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "06ecff9a-760f-4888-a535-81535916c2b9");
                writeFolder(header, entry);
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3cf5e946-5c9c-4b6c-8eaf-848768155e5b");
        header.write(NID.kCodersUnpackSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6c5a49d5-4791-449a-b8ba-aeef4a5246fd");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0cda9f06-0b86-4ffc-a1c9-272410559900");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "df063938-4dd1-48ae-b61e-db4ab5bcc203");
                final long[] moreSizes = additionalSizes.get(entry);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "08da5d4b-4f60-4a92-8dd0-62850c9f8c9c");
                if (moreSizes != null) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b9bf2ed0-ab1a-4026-bb89-d15d2d96a224");
                    for (final long s : moreSizes) {
                        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a7206f6a-dc16-4b12-8e75-ad433513012b");
                        writeUint64(header, s);
                    }
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "793d8120-1585-4468-9943-368d234c3457");
                writeUint64(header, entry.getSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4e72a041-e978-49c8-a851-f449351128e2");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "227fd3ed-c413-444c-ae9a-fc12f2123d54");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "602f642d-830a-4e8d-9069-ba5cec6ab43f");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "86f775b6-0ff3-4bf1-9977-94c03229807e");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "61e14968-4170-4e18-aa1b-ac9bc1f865fe");
                header.writeInt(Integer.reverseBytes((int) entry.getCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f745cafb-a04e-4962-8733-2eaef0b34384");
        header.write(NID.kEnd);
    }

    private void writeFolder(final DataOutput header, final SevenZArchiveEntry entry) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3ff6921e-a69d-468a-bbc8-a43c269d3580");
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f9f375c3-b7fe-4b95-b4a5-aa96c925afbb");
        int numCoders = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4abfa6a1-fa0b-4ce5-a95a-4145343f5c25");
        for (final SevenZMethodConfiguration m : getContentMethods(entry)) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1104b943-c57f-46b4-ae9c-ff6dad9811f7");
            numCoders++;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4ebdda8f-1917-42c0-ab55-2f0240727960");
            writeSingleCodec(m, bos);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a2364f59-76d1-4ecb-9c5d-defbf236667a");
        writeUint64(header, numCoders);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1481e714-2473-4a98-be90-f630530218d5");
        header.write(bos.toByteArray());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b1b8359a-c873-4e30-8861-72c68a0e4259");
        for (long i = 0; i < numCoders - 1; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "27503ba2-eb25-4b25-8cf0-c3ce0dcc8484");
            writeUint64(header, i + 1);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9058f5d2-423d-40ec-aed2-151597fd87a0");
            writeUint64(header, i);
        }
    }

    private void writeSingleCodec(final SevenZMethodConfiguration m, final OutputStream bos) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e491215f-7680-4f02-a9f5-8434146335e1");
        final byte[] id = m.getMethod().getId();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "51abaa1d-879e-4e5b-9257-1c0f20ffae2f");
        final byte[] properties = Coders.findByMethod(m.getMethod()).getOptionsAsProperties(m.getOptions());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5b3607f3-75c5-41ea-bc20-83ed0e5a760c");
        int codecFlags = id.length;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1eed7733-8d77-4fba-9b3d-08c32162c94a");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5651be11-f41a-4696-964d-28fa3df0770e");
            codecFlags |= 0x20;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "16121e54-c5e1-439d-9acb-90eee83c61bc");
        bos.write(codecFlags);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "72448367-c785-4fec-a64d-b769b6c0d1a7");
        bos.write(id);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "68a45116-08a9-485e-84f7-6839aba4c606");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ba01486d-3779-4e81-9bab-47db154e38dc");
            bos.write(properties.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f2a959f6-17e6-4ee3-a6d7-ef6229f7147f");
            bos.write(properties);
        }
    }

    private void writeSubStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0c9938c2-720b-4bb0-8b0b-2f03b7ffd3e5");
        header.write(NID.kSubStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "bdc596cb-00b5-43e1-b61d-1ea0598c4fa3");
        header.write(NID.kEnd);
    }

    private void writeFilesInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6ea4e2da-fdc0-4ffc-b86c-b6029525cd58");
        header.write(NID.kFilesInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9af4e1e6-eaa9-4f56-bbd3-3ece29e7c544");
        writeUint64(header, files.size());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "87332fe1-7951-48c9-80e8-15aad4fd27fe");
        writeFileEmptyStreams(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a22ea392-e979-4c6d-b3ed-44010f7cae3a");
        writeFileEmptyFiles(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b68cdb6e-82db-43da-ae1e-9b830bb71527");
        writeFileAntiItems(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "14493885-7d45-470a-9b25-7f803856821f");
        writeFileNames(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "235183a0-3c39-463c-8517-4cb3c5dcddce");
        writeFileCTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c83e6065-5110-4d39-8bb7-e0b6b686b1bc");
        writeFileATimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4de0690f-b178-4132-875c-e33a308bc541");
        writeFileMTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "26c2c3e4-9f7d-45f1-8726-499bd1c1f2d2");
        writeFileWindowsAttributes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ca53d39a-8276-448e-9825-2903cb7f9652");
        header.write(NID.kEnd);
    }

    private void writeFileEmptyStreams(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d0432629-836d-4304-bd1b-3ee714d4827e");
        boolean hasEmptyStreams = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8e4a6a83-7b95-49ba-beac-4462504f208f");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "799dca3d-0c58-4927-8b3b-e6cd316edacb");
            if (!entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "90bcfb5f-9310-4fee-a10f-f1715d576dba");
                hasEmptyStreams = true;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8a6da5b7-8847-4278-9e0d-21dde0a6e1ac");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "26e91874-8f9d-4602-a24c-97f57eba39c2");
        if (hasEmptyStreams) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "48795dcd-dfa0-4da8-b1f9-5f58bc647225");
            header.write(NID.kEmptyStream);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "47651603-8a11-4534-acbc-71983c466293");
            final BitSet emptyStreams = new BitSet(files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e9adcf9c-bec4-4e67-8a4c-53b5a07cea20");
            for (int i = 0; i < files.size(); i++) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4f9478c0-2bd8-461d-b858-65817bb88116");
                emptyStreams.set(i, !files.get(i).hasStream());
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9cf4dd88-7061-4a29-8439-3f963108bd32");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "748d2aa6-c013-421c-bf4a-2d4a150d011f");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5b85c403-3785-4de4-bb9a-41f4a33fe5e3");
            writeBits(out, emptyStreams, files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5432df5a-21db-4898-a189-1e3c6490ef85");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "222ad70a-ee78-4a27-b2e4-d23131beccc3");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c9ee2be3-20e0-45be-8319-53b448c8895c");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e2509be9-1342-4750-a68c-9faf6d409e5e");
            header.write(contents);
        }
    }

    private void writeFileEmptyFiles(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "fbdb6771-1fbb-4506-9d5d-b4357bb95d53");
        boolean hasEmptyFiles = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "960564cb-3695-4889-8a8b-6d1b4204303b");
        int emptyStreamCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "15a64f74-0858-4025-8d8f-6d6912a3deba");
        final BitSet emptyFiles = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "96f93873-bf45-4ecb-9d6b-b4692dca9d7b");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "12ff615c-80a1-4de8-b58d-e281421b9a9b");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2d15851e-5676-4f22-aa17-09e422995f39");
                final boolean isDir = file1.isDirectory();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6fd1f634-3a5d-4095-aa34-75c7dc7af228");
                emptyFiles.set(emptyStreamCounter++, !isDir);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e7d186a5-b58e-4249-8f25-1ab1b2d60ebb");
                hasEmptyFiles |= !isDir;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7de58633-469d-4869-bee9-351a03dd3689");
        if (hasEmptyFiles) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a858fb49-ce2c-481f-8e2b-8eed45c3536f");
            header.write(NID.kEmptyFile);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0243d83f-3bab-4350-875e-146f090b92b4");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "60c3c9a5-76f7-4530-af5b-495ec4e4ff7c");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "056cbc15-ce54-4e3f-9bbe-db2fba93e87c");
            writeBits(out, emptyFiles, emptyStreamCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "bb6b9581-a754-4afc-abd2-1bf3c15de646");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "84f100d0-65af-4de8-9475-e02f2bd31085");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e44cdd6e-0a70-4624-addd-361b15d881c5");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "72827643-a339-4dc5-8f57-aba0ecf95f34");
            header.write(contents);
        }
    }

    private void writeFileAntiItems(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "73e07de5-f9d8-4ac1-94d6-a0be41bd1d1c");
        boolean hasAntiItems = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "74ed9b9c-b74e-444d-af63-7708b887909c");
        final BitSet antiItems = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "061c20f0-9529-419a-8799-508493dfed5f");
        int antiItemCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a333b3fb-ebaa-43e6-80c6-4912c1930e06");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "25ab8946-9286-4c63-9b4b-3f0abe73d18f");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "842ae58a-093b-4e70-8b9f-76a11a0283bf");
                final boolean isAnti = file1.isAntiItem();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c67fe9bc-ded6-4434-84be-7a9e6c9f3c78");
                antiItems.set(antiItemCounter++, isAnti);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "fc9c2ff1-8436-4e7c-af1b-b63748727a0f");
                hasAntiItems |= isAnti;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "93efb387-91b6-4725-90c8-432b64affceb");
        if (hasAntiItems) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "534afe48-66e8-47b1-8d42-bcc7f0e5b462");
            header.write(NID.kAnti);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "86d232e5-fb9d-459e-9329-8d3c521987dc");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d9311faf-602e-4d8c-8cfe-86af65b15869");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0d2c450e-2e72-42b9-a864-81189fac78a7");
            writeBits(out, antiItems, antiItemCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "003ffdb8-c1ae-47dd-b9ca-ca1f0ad52d64");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "241745f0-7fb1-4445-9ae0-35a0e1a07eec");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "73096893-8660-42b1-b1f6-a8cf75cf6c79");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "dd66dbe9-1eaf-472b-a7b8-323df76eb433");
            header.write(contents);
        }
    }

    private void writeFileNames(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7dc616f9-16cd-425e-bc4b-5eb384619ee6");
        header.write(NID.kName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "76ee5cfb-73a5-4ddd-b04f-61be9bf34a07");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "694fef9a-8990-4b57-aaac-672b852b87ae");
        final DataOutputStream out = new DataOutputStream(baos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f8bc223f-82b4-4130-9d7d-d41f6303eb6c");
        out.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9342ca36-17e8-4c88-b019-05b9d407d106");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "74f32365-f7f2-49dc-a6ff-cbf8c3bff4d8");
            out.write(entry.getName().getBytes("UTF-16LE"));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "619424f7-db6e-486c-8262-a5f7751c7011");
            out.writeShort(0);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "46d9880c-1fe0-4b49-ae0a-f8786c714c14");
        out.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0084297d-e771-4e34-937b-7ce724c42fb0");
        final byte[] contents = baos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "55184d1e-9542-4b50-9d54-8c60107746a0");
        writeUint64(header, contents.length);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "87c33a5d-5e2c-4f6a-b16b-5578a1855daf");
        header.write(contents);
    }

    private void writeFileCTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "240e0be2-6185-4527-9f73-4b5db0497567");
        int numCreationDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b448f19e-7512-43e5-9240-bdad543072a2");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a402e875-c352-412b-8442-3a8325433942");
            if (entry.getHasCreationDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6ab38fae-bc19-4eb6-8794-b8127a236e5d");
                ++numCreationDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d766059b-7792-43f3-9824-f7beef9ee8cc");
        if (numCreationDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a9904f32-51bf-4e04-9b04-a2b852a96e2a");
            header.write(NID.kCTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ea41ecf6-2ff1-4faa-8ec9-44137933f63f");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7ff2f9e0-d348-4ac6-990d-86ee8e6f8d7e");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "081f43ff-ff02-4de8-b930-d3b483927635");
            if (numCreationDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7c6c68c9-a479-4c34-8556-b4fce046e117");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "60aa2c3d-3c85-4341-ac27-1f3c6b0a99ca");
                final BitSet cTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f17c4b5e-37e9-40de-800b-e4fd00b70627");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a953f420-100c-43e6-827a-8788a8523883");
                    cTimes.set(i, files.get(i).getHasCreationDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "22288d7c-f4b9-4b4f-9eb6-1c843b81e3c3");
                writeBits(out, cTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9b776c38-8de2-4feb-b71a-6c4f3ad8b362");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6d2b845a-e509-4779-8894-31bbe7208621");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9b5dfa2c-7aac-4c99-8020-41fe3270a764");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8e873cf5-d9b4-4eb5-af4f-1cfb279edcb1");
                if (entry.getHasCreationDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "0b123c87-c227-4c7f-ac70-aaa91d61d6e6");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getCreationDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e7310b79-64f9-4463-b4be-80260eea3c98");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a7bd5444-2a19-46b4-8e1d-a46d8add0af1");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2050c670-c402-49b3-a0d9-38b88364df63");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b9c83d5f-0f10-4fd4-89be-d035971e239d");
            header.write(contents);
        }
    }

    private void writeFileATimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1cc3eaa4-489a-4f27-bdaa-de46b26e1fa9");
        int numAccessDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f12ccd8e-d2e7-496a-ba69-853a10e7ed77");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f92a4cd4-e4c6-466d-921c-5e3a632cabb6");
            if (entry.getHasAccessDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "233f98c0-8f63-4721-9a67-cac4a3adf634");
                ++numAccessDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b85b8d45-af23-4412-836d-2cd2dccb7a3e");
        if (numAccessDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3d313a9a-461e-484f-b5cf-e80d311b1c3a");
            header.write(NID.kATime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c2ce88f4-a4e8-4488-8389-fabaedf17f5f");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "86eecf3c-8875-450b-bab0-cc82612bf0cf");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "edbf9c88-497e-4ea0-ac6f-6cd2d8af1dbd");
            if (numAccessDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d34033e0-7a68-4c7e-96ce-b8d170fee724");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "771029f7-98bf-45dd-93d3-5844b2dd53cd");
                final BitSet aTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "fe1ac600-d71c-4f57-b9bf-26213ee5080d");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4bdd33ac-3e8b-4453-be90-6fda913e0528");
                    aTimes.set(i, files.get(i).getHasAccessDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f22f980f-2072-4366-881b-c311589fc0ae");
                writeBits(out, aTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "71b0f981-6559-4354-b61e-106af20ddf1d");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1cbc21fc-00d0-466a-a089-52bbc9fbf6c5");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4f6eeecc-01a3-4263-82f6-9fc20f791e0c");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "bf9a7398-b540-4e2b-a315-d8e60aa824c2");
                if (entry.getHasAccessDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9c35b8b9-8bbf-4f00-9fdf-728beda2294b");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getAccessDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4f8a7482-71e8-4d5f-8351-6a6ad2f39197");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f0eaebb6-39c7-497e-9ba3-0b8942d35101");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "70acb71a-00a7-4381-ab72-5e19b3db844d");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8b262f64-8bd6-4ee9-a6e4-0b0aba3679b9");
            header.write(contents);
        }
    }

    private void writeFileMTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3ca463f6-73dc-45a0-9a1d-60827f6fb929");
        int numLastModifiedDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4b1044af-1985-4f71-b99b-28ef154a41fd");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a9fecbe8-e8e4-4c8b-a247-cbd569d82d56");
            if (entry.getHasLastModifiedDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "bae2add1-22b6-4369-8fbb-6ed0f84abc05");
                ++numLastModifiedDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ebc7ba50-5858-492d-9b5b-7142630fea8c");
        if (numLastModifiedDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b51503cb-a620-4058-b7d1-225a315236ee");
            header.write(NID.kMTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4a3ef5ba-3791-4dea-980a-c38e44ffe6a3");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7ad5ed3f-111b-41fa-ba0e-667f0ff722b1");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "22ee8880-527b-4bce-8542-77b458cee338");
            if (numLastModifiedDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "347a5c58-6161-46c8-a7ae-9a24c9ef67c6");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "21bf18fa-a1ce-4f1b-a2c5-690f74d75b1d");
                final BitSet mTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "87c8d6ea-ae3b-4e19-9085-f3dd186844f2");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "4bdfc021-f340-4f4e-a1db-9a2259c124c9");
                    mTimes.set(i, files.get(i).getHasLastModifiedDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9277bd28-af19-4e8c-8b78-027a97096c5a");
                writeBits(out, mTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1a88651f-fc0e-44b1-acf8-bce4b45ff3b3");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ca0c86be-3486-406b-8195-a18562732ce8");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c9b89951-1eed-4164-8bab-495ae7a1c661");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2052e01d-5ad5-4a1f-aee9-55905d716077");
                if (entry.getHasLastModifiedDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "df6b5ad0-ab16-4793-a6be-5e9e4fcd723a");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getLastModifiedDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "eb37ca6a-bbc2-438d-9f0a-38437ef0062a");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "205e86d7-1cde-4140-862a-a65cd74e4a48");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "da7a437d-c3ff-42bc-b5ee-5cc18707c9f3");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "58a83f52-f287-468d-90b7-bb50fee120bf");
            header.write(contents);
        }
    }

    private void writeFileWindowsAttributes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6b9acf06-65ad-4438-a730-98b0f93b9e72");
        int numWindowsAttributes = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "1f23def9-b5c2-4db3-9b7e-0618c55a5aa8");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "077b9fdf-bbdf-438e-b386-9bf36c34cd74");
            if (entry.getHasWindowsAttributes()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "07399a4d-1b82-42c6-bbbd-3613d49173cb");
                ++numWindowsAttributes;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "23cdf57e-1d9a-4a5b-b3db-3dd8603e3629");
        if (numWindowsAttributes > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d8828d11-b108-490a-b9b7-530f63dcc258");
            header.write(NID.kWinAttributes);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c73c46a9-0115-45c3-9e30-6389cec6a338");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d9ca7057-ad98-4842-9dcd-bdb8cd001799");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f0acf64f-8bc1-4839-85bf-89e26224542e");
            if (numWindowsAttributes != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "01397e93-4aba-4ad4-9d57-69a98934b460");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "00ded049-04c3-4384-8223-c19d657f7467");
                final BitSet attributes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "5e9a7b9b-44dd-45aa-88a4-555509106bc5");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "7349a20e-a8cd-4b81-a06a-8e0c94a95887");
                    attributes.set(i, files.get(i).getHasWindowsAttributes());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "f69264f9-0926-4b1f-857c-48c006767710");
                writeBits(out, attributes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "72571791-4bf2-4286-868f-18c87dd83f61");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "59c3e523-a5d9-42e1-b306-5ff0ce98423a");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "e246ef43-46a2-422b-907c-7be4dea8041e");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ec2fe2a1-e7ba-4d6a-9641-39420ada323f");
                if (entry.getHasWindowsAttributes()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "6dc877b6-9d11-4ab2-a160-e5d37e3203d8");
                    out.writeInt(Integer.reverseBytes(entry.getWindowsAttributes()));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "89a1cd34-e2a2-4ec2-ade4-b4e8458e7ad9");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "804aba08-e605-457f-9368-7b89db2e410f");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "358c8694-c64d-426b-b60c-3b27c1a388fb");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ad326bde-ea26-4e84-9804-72669f98c34c");
            header.write(contents);
        }
    }

    private void writeUint64(final DataOutput header, long value) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "30a56609-89d9-4019-9f3d-7c079f68e086");
        int firstByte = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "602b15eb-3255-4630-9b63-6ee2e6267f73");
        int mask = 0x80;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a9e219c4-b5d8-4499-a723-be0e190e52af");
        int i;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ffdac702-3c59-4a42-896c-6e37b5f5fd1f");
        for (i = 0; i < 8; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "683a2a56-8757-4ec0-a306-7f86396bf22e");
            if (value < ((1L << (7 * (i + 1))))) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ffc8867f-90ef-452b-abba-6c9693a393cb");
                firstByte |= (value >>> (8 * i));
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "02e73de0-a648-4e31-a410-8c2a7132edf5");
                break;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d52c1be1-6315-429d-95d0-aa1736108290");
            firstByte |= mask;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "d94f973e-098d-40f7-b4de-67aaa80e0dbf");
            mask >>>= 1;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "cd785a9f-ab0f-4c46-9a4a-d5ef175ea618");
        header.write(firstByte);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "c0cb6c46-c04f-4da4-9d56-94f5583c229a");
        for (; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3e27c077-97bb-4eae-8b5a-5ed5edb122e4");
            header.write((int) (0xff & value));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8ce4909f-75fe-4bdd-a7d4-e7381d107132");
            value >>>= 8;
        }
    }

    private void writeBits(final DataOutput header, final BitSet bits, final int length) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "ffcd00ff-ea2d-470f-af44-7811be44f597");
        int cache = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9bff2459-c2b9-41ec-91e0-8ceb67935f38");
        int shift = 7;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "72f153f6-7742-45f6-bb8c-47a1f3ba0b50");
        for (int i = 0; i < length; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2e4d26ea-6ce9-4572-88be-80207cd4bdbf");
            cache |= ((bits.get(i) ? 1 : 0) << shift);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "dd77b784-1afc-44a3-9bc7-699c4f0d76f7");
            if (--shift < 0) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9b218415-9c34-47ab-af7b-c6d42663bdbe");
                header.write(cache);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "3941d8b9-49e2-4c01-b6b8-0ff6eaba914b");
                shift = 7;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "947a469c-b0c3-48bf-b690-5990531aede0");
                cache = 0;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "9746b5f7-8d07-4f15-8224-3db7c836826d");
        if (shift != 7) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "a05ce368-d274-4d1f-9d09-65e4ee3c08f6");
            header.write(cache);
        }
    }

    private static <T> Iterable<T> reverse(final Iterable<T> i) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "8301556a-7458-4ec3-8939-6bbe2e1a3fda");
        final LinkedList<T> l = new LinkedList<T>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "b75adabe-ba3e-4c4e-96f6-3a1c9bd870b0");
        for (final T t : i) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "43d7ea37-6d6b-47a1-92d2-021612e172f8");
            l.addFirst(t);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_1_10.coverage", "2516211a-dc7a-42cd-88fd-c1f5a65d9b5a");
        return l;
    }

    private class OutputStreamWrapper extends OutputStream {

        private static final int BUF_SIZE = 8192;

        private final ByteBuffer buffer = ByteBuffer.allocate(BUF_SIZE);

        @Override
        public void write(final int b) throws IOException {
            buffer.clear();
            buffer.put((byte) b).flip();
            channel.write(buffer);
            compressedCrc32.update(b);
            fileBytesWritten++;
        }

        @Override
        public void write(final byte[] b) throws IOException {
            OutputStreamWrapper.this.write(b, 0, b.length);
        }

        @Override
        public void write(final byte[] b, final int off, final int len) throws IOException {
            if (len > BUF_SIZE) {
                channel.write(ByteBuffer.wrap(b, off, len));
            } else {
                buffer.clear();
                buffer.put(b, off, len).flip();
                channel.write(buffer);
            }
            compressedCrc32.update(b, off, len);
            fileBytesWritten += len;
        }

        @Override
        public void flush() throws IOException {
        }

        @Override
        public void close() throws IOException {
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
