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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "2346b7b4-f27d-4a04-b6a2-767391f028d3");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0eff9700-928a-4b81-92a7-0f43bef19538");
        this.contentMethods = reverse(methods);
    }

    /**
     * Closes the archive, calling {@link #finish} if necessary.
     *
     * @throws IOException on error
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a7507dc8-91e3-4386-a246-0cf6fbfb2b04");
        if (!finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "518d2fad-618b-4dbf-83b8-f195a3e1d4af");
            finish();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1801fdc7-6e96-4170-a734-0783b3489756");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d1f5d7a7-62b8-4e72-bd18-d8fd16bf6067");
        final SevenZArchiveEntry entry = new SevenZArchiveEntry();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4e224ae1-4c94-4bf9-807e-e9db591a2440");
        entry.setDirectory(inputFile.isDirectory());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e907e6cf-347e-4c0b-9635-9f9e97ed1635");
        entry.setName(entryName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ddb87ad8-ebe5-4a40-a999-9aa623c8febe");
        entry.setLastModifiedDate(new Date(inputFile.lastModified()));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ea05253d-d268-4a0c-aab8-bf7c46eea14b");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "81e0c68d-3085-43a6-8825-6797cb2ff5c2");
        final SevenZArchiveEntry entry = (SevenZArchiveEntry) archiveEntry;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "93498041-9112-4cdd-b7fd-23046ba31aad");
        files.add(entry);
    }

    /**
     * Closes the archive entry.
     * @throws IOException on error
     */
    public void closeArchiveEntry() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "714505ef-eb6c-44b5-a24c-e230e4e227c4");
        if (currentOutputStream != null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "907c95c3-1e82-49ce-aef8-14cb5294da41");
            currentOutputStream.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "278be299-67cc-4a8a-8d70-a5c65d2753ca");
            currentOutputStream.close();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3a4aaedb-0536-46b2-af55-75d03e28a305");
        final SevenZArchiveEntry entry = files.get(files.size() - 1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8f954e83-a203-4a46-849b-30d413158201");
        if (fileBytesWritten > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e98c9482-7d7c-4de4-bbe7-f9c9a6de82ad");
            entry.setHasStream(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3220d866-0334-454b-b2aa-16ae3cd892d9");
            ++numNonEmptyStreams;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8f97342f-2da8-472b-abd1-35784e1c1d77");
            entry.setSize(currentOutputStream.getBytesWritten());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7e242fcb-0ecd-46a0-b9ee-d89cd02d26d2");
            entry.setCompressedSize(fileBytesWritten);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c3433ece-096d-4bf2-9ef8-e99122bb2b5a");
            entry.setCrcValue(crc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3259e855-5586-405d-a12a-83bebc04ea93");
            entry.setCompressedCrcValue(compressedCrc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "09214e43-9e7d-47a5-82ee-28cccd3b1f26");
            entry.setHasCrc(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "198d2b46-ea89-4891-845f-d69993b8ca9d");
            if (additionalCountingStreams != null) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "31d30156-e49c-41b6-89b9-2817550ec668");
                final long[] sizes = new long[additionalCountingStreams.length];
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ccb12b04-4f5c-4870-8ebb-3720ed4fdda2");
                for (int i = 0; i < additionalCountingStreams.length; i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3b7bc6ac-f99a-43e8-9dea-941ae39d4371");
                    sizes[i] = additionalCountingStreams[i].getBytesWritten();
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "cf5d5b8b-810d-49d8-a6e7-75d8b6e86575");
                additionalSizes.put(entry, sizes);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4ce60f8e-7fb1-41fc-a2e9-2eb9d55fe9b0");
            entry.setHasStream(false);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0d85a645-0b13-47b8-9b7a-2138d53cd1c1");
            entry.setSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7e095b0e-c496-4fb7-908c-3bba81a1f439");
            entry.setCompressedSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b07763e9-4341-4fab-a1a4-73bca7aafca4");
            entry.setHasCrc(false);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "57d7ede0-cd81-48ab-a329-906c7e27b35c");
        currentOutputStream = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7b621a26-134b-4eba-a11f-63fb8f09c1a3");
        additionalCountingStreams = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6e95e2ce-dfbd-4bda-bee5-c760c6185b3d");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "044433c9-aa89-4bb9-9414-835635fa730e");
        compressedCrc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bf80fee1-921e-4b68-a410-83cd37be6b6e");
        fileBytesWritten = 0;
    }

    /**
     * Writes a byte to the current archive entry.
     * @param b The byte to be written.
     * @throws IOException on error
     */
    public void write(final int b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "715d00bd-becd-4cc9-a126-f1ca6ae39e5b");
        getCurrentOutputStream().write(b);
    }

    /**
     * Writes a byte array to the current archive entry.
     * @param b The byte array to be written.
     * @throws IOException on error
     */
    public void write(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "da646bf0-b4d2-4d38-b991-25497722cab7");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ce6847e3-cbc2-4cf9-8061-59900dcb315d");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "07f7a188-74b0-41f3-865c-2db7f1c40d4f");
            getCurrentOutputStream().write(b, off, len);
        }
    }

    /**
     * Finishes the addition of entries to this archive, without closing it.
     *
     * @throws IOException if archive is already closed.
     */
    public void finish() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bfa04a50-6ca6-4eeb-9b7a-a7d86fdd3c8e");
        if (finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1b3dfd38-7c64-4935-af91-669ba92206ea");
            throw new IOException("This archive has already been finished");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f2bed679-84eb-46f4-8bfd-36c234f9b733");
        finished = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "977759a8-69df-43bb-8373-690b43f9f67b");
        final long headerPosition = channel.position();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "67349eeb-d0c9-4090-b67b-77eda0315a4c");
        final ByteArrayOutputStream headerBaos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4bd80b36-3fa5-408d-8849-3ec104134fd7");
        final DataOutputStream header = new DataOutputStream(headerBaos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "220a15c3-2e1a-47ee-903b-3986eb499a0b");
        writeHeader(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "cc36c6cd-1ad7-48ee-9a98-1a0ddf91c02f");
        header.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ec3d25cc-b057-4787-9fc6-de5c974e22db");
        final byte[] headerBytes = headerBaos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "438553bb-edb8-4046-8564-4df23837a8d2");
        channel.write(ByteBuffer.wrap(headerBytes));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "101aca9f-fcf3-45c6-9a61-9f0b991e4940");
        final CRC32 crc32 = new CRC32();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a3aeb158-2aab-40c0-b5a0-75feac50bc5b");
        crc32.update(headerBytes);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "08abb9af-8e73-4291-9b3b-9fa22831ea61");
        ByteBuffer bb = ByteBuffer.allocate(SevenZFile.sevenZSignature.length + 2 + 4 + 8 + 8 + 4).order(ByteOrder.LITTLE_ENDIAN);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a455d926-ce88-46c7-b855-f691f5ade6fc");
        channel.position(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "aed5027f-ca64-4a59-8d49-4236bdb309c3");
        bb.put(SevenZFile.sevenZSignature);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bd3ed933-e067-4a58-a1c1-786fab1e9e81");
        bb.put((byte) 0).put((byte) 2);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "956d5583-c06a-4446-8896-b6fa1fb0f7a4");
        bb.putInt(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "17ea27d5-dc52-4ae0-bc7d-36c9278e2a05");
        bb.putLong(headerPosition - SevenZFile.SIGNATURE_HEADER_SIZE).putLong(0xffffFFFFL & headerBytes.length).putInt((int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c6bf18ae-fd83-49fe-9413-2980908d219b");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d1c1858e-6508-440e-82ae-f7d1395cd379");
        crc32.update(bb.array(), SevenZFile.sevenZSignature.length + 6, 20);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "764727e9-5ede-4d11-a0c4-75b75af5852c");
        bb.putInt(SevenZFile.sevenZSignature.length + 2, (int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "02e0f95a-2845-45b6-820f-81934562e47f");
        bb.flip();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d9bca9ac-a10f-4ae9-adad-3fae85253319");
        channel.write(bb);
    }

    private OutputStream getCurrentOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "804b1939-f6bf-4e6b-aeec-7066b1ab221a");
        if (currentOutputStream == null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d696788d-b1e1-4476-83c5-d90f141eaba3");
            currentOutputStream = setupFileOutputStream();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "424c7b38-ba4e-4734-9436-3ac3f79f8164");
        return currentOutputStream;
    }

    private CountingOutputStream setupFileOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c43bf499-fc99-4dd1-b869-03f1ab42e9a1");
        if (files.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b3bf4d3a-0edf-4f68-b815-5901ba57326a");
            throw new IllegalStateException("No current 7z entry");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b05b975a-d7d9-48a2-bde2-0b70d6ad37e3");
        OutputStream out = new OutputStreamWrapper();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5fdfa758-3c55-4a10-a9f5-10e55f2426ce");
        final ArrayList<CountingOutputStream> moreStreams = new ArrayList<CountingOutputStream>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "80fc0356-128c-41fd-9fd3-28442b6c7e21");
        boolean first = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "cec0a411-a820-4a00-abd9-da5b69a99702");
        for (final SevenZMethodConfiguration m : getContentMethods(files.get(files.size() - 1))) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d9be13c3-a03c-46e2-ab27-ea398153f1a5");
            if (!first) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3f7c2725-9e71-4e03-82f6-f4e628919e69");
                final CountingOutputStream cos = new CountingOutputStream(out);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "153b6c92-b02e-43b1-8b35-a47ff992134b");
                moreStreams.add(cos);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "aa039fe9-cdfd-413d-93ce-597b3ae6b2e2");
                out = cos;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9a67c9a4-2ac0-4185-9ab0-f85a232f8804");
            out = Coders.addEncoder(out, m.getMethod(), m.getOptions());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a86d5ae7-2f69-4516-bf86-88f38daa2c02");
            first = false;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7d4e5122-0478-4e51-b447-199f55cd95f3");
        if (!moreStreams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9ed20c52-1a7b-4b00-ae8f-4cd85a6b4cfe");
            additionalCountingStreams = moreStreams.toArray(new CountingOutputStream[moreStreams.size()]);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ee423c35-a527-48a8-ad71-338f0e0a5e45");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "42314450-ef4d-44cd-b3c4-a9a44a323c96");
        final Iterable<? extends SevenZMethodConfiguration> ms = entry.getContentMethods();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f74a2348-0a7e-4fb8-b1e3-812f24fcda62");
        return ms == null ? contentMethods : ms;
    }

    private void writeHeader(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a2d96409-0c47-49fe-802d-f6deee767b44");
        header.write(NID.kHeader);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "80210820-edc5-4220-9725-cbe329e0ccc5");
        header.write(NID.kMainStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "558c8b4f-5bfb-4419-b144-31a6a9dbd851");
        writeStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "86662139-b63c-4b7a-b2ac-013d22df60ab");
        writeFilesInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f8f8c8e7-486f-44d6-878f-b17f3f78d0d3");
        header.write(NID.kEnd);
    }

    private void writeStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b7f76a39-edc3-42db-b297-23b5c77c32c7");
        if (numNonEmptyStreams > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3d2dafd5-3aae-46ae-b439-5a975ca73ffa");
            writePackInfo(header);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4dcb693e-115e-406d-83cf-6be3a827776a");
            writeUnpackInfo(header);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5e36248e-8363-465c-8b1a-a4643f7ad618");
        writeSubStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "99448e9a-4e44-4de3-b45d-d0f332e9c4a9");
        header.write(NID.kEnd);
    }

    private void writePackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e7a2cd8d-0bc5-495a-a2f4-27a73d998216");
        header.write(NID.kPackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fba1a09f-9a3a-4bb5-8f54-f97488d7aaee");
        writeUint64(header, 0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "83881b5b-9d03-4e8f-957c-fabb12ea126f");
        writeUint64(header, 0xffffFFFFL & numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fdfcdf53-1a4c-4b3a-9c1d-7df9565fdf4c");
        header.write(NID.kSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "54658645-cd58-4eaf-8f97-af55e3be8b6f");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d3bad828-048c-46ae-ba50-f56d38b1e5ce");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ba8bcc89-8d6b-4615-8ee5-22f6e027634c");
                writeUint64(header, entry.getCompressedSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1bfdb4e0-a88f-4b67-95bb-8804a9c9c65f");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0fd9af0e-5d6d-41e1-82f5-3eec119de3cc");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0c31f5ec-944b-43f2-8de5-94db28d332e8");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ac1a0adf-d0b3-46c4-b48f-f9a033bec52a");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "53db614c-10cf-4a90-96e5-147b82304b5c");
                header.writeInt(Integer.reverseBytes((int) entry.getCompressedCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4deaf4b3-58f2-45fd-bcd9-581615f2a3c9");
        header.write(NID.kEnd);
    }

    private void writeUnpackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "880f160f-f069-47dd-bafb-a048679cf00d");
        header.write(NID.kUnpackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "df40c6f3-a0b6-4a5e-8d64-81acd120beae");
        header.write(NID.kFolder);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e3c6a4e5-6077-4a50-b90f-e67f874a8a3b");
        writeUint64(header, numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5d2f27bb-d7cd-4ef8-9f80-7ba256297eda");
        header.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a3395d44-0a18-4172-9f5b-5321c703bed3");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "27616a74-16e8-4f0d-9070-f442f099b327");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6138c41e-0b13-46bb-b708-fdb2988575d8");
                writeFolder(header, entry);
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fb19c981-ffd5-447b-860a-fa0541ae0b8c");
        header.write(NID.kCodersUnpackSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "17c1d974-3393-40b3-9eb0-e99681411505");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ca6ffa80-d463-4c1b-bd62-a48653acdf54");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7fb23795-b09b-49d8-9142-25dc2cc3e11a");
                final long[] moreSizes = additionalSizes.get(entry);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "10f43feb-4d78-4ef1-b240-c991b6c846a0");
                if (moreSizes != null) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c4201a6f-7b34-442c-a228-7757dbfbf2a9");
                    for (final long s : moreSizes) {
                        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "67ff0905-9948-4ad3-b4f5-56b0da0b632b");
                        writeUint64(header, s);
                    }
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f917ea87-0f3c-43d5-9639-3b070cbfd032");
                writeUint64(header, entry.getSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f69d46b0-2ecf-4599-bfb9-f3b4e92717ca");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b27c3f07-29ee-455a-b356-049fe4d17ea2");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1e2d9011-2bc0-47c4-a8bb-26715933201e");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a080599b-94c5-4be1-ad59-6bc9cc6e2a13");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "68880b51-e036-45c2-8d6e-e42333c5862b");
                header.writeInt(Integer.reverseBytes((int) entry.getCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fd31553d-dfe6-479c-a7d6-eb1c49ff338e");
        header.write(NID.kEnd);
    }

    private void writeFolder(final DataOutput header, final SevenZArchiveEntry entry) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d7ffd6a1-f9e3-4116-87e1-e52358f6d522");
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5b712c97-8ee3-4424-b4e7-f85fd70d1d1e");
        int numCoders = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3f6ae4e0-24a9-4c89-af84-23fc7d64d439");
        for (final SevenZMethodConfiguration m : getContentMethods(entry)) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4cb72667-ea4e-469d-9495-cf39868427a5");
            numCoders++;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "cb3a24a4-5c3b-4f06-adeb-20c525768eba");
            writeSingleCodec(m, bos);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "cf321cca-3062-4354-93fd-87bd698d486f");
        writeUint64(header, numCoders);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "cbe34b74-5d02-419a-ade7-40e9c0a8e98e");
        header.write(bos.toByteArray());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1a47c231-0b21-4b04-a748-658d2c4bb29e");
        for (long i = 0; i < numCoders - 1; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f64f46a5-8ddf-483f-b6ed-b08fbd74e0ff");
            writeUint64(header, i + 1);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9736bce7-6a91-4cde-87d2-2392e035fbfa");
            writeUint64(header, i);
        }
    }

    private void writeSingleCodec(final SevenZMethodConfiguration m, final OutputStream bos) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9c72992b-a2a6-4b07-984a-c26fa2e0e62a");
        final byte[] id = m.getMethod().getId();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6856aa20-f987-4e80-b521-4d928187d6cf");
        final byte[] properties = Coders.findByMethod(m.getMethod()).getOptionsAsProperties(m.getOptions());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "413163fe-d2bf-45c6-94e1-1399d5a6c05d");
        int codecFlags = id.length;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "46d8f038-f8a8-4eaf-857d-800982b78e45");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "87e9bc4f-8613-49ba-afa3-96683b088782");
            codecFlags |= 0x20;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f94aef1b-6263-4dd2-a1fb-89a034f83003");
        bos.write(codecFlags);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "20a21655-cd1b-49f1-bc5e-bf1cd366d554");
        bos.write(id);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "02b48662-097f-4623-ade4-1c9a69686c39");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "22464c3d-fad3-482e-a019-213b29e89229");
            bos.write(properties.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ed465bf7-fbbf-4a15-8618-a3f1c3fac373");
            bos.write(properties);
        }
    }

    private void writeSubStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f84ec953-8e2b-41e0-ac9c-7f2c6c0fb053");
        header.write(NID.kSubStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "13c64f87-05c8-4a34-a67e-01a3cc35fd13");
        header.write(NID.kEnd);
    }

    private void writeFilesInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "90c714fa-6a82-463d-9874-4c8e3ffebf80");
        header.write(NID.kFilesInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ab1f7ea5-6571-4feb-b5f2-c8a1e7d3761b");
        writeUint64(header, files.size());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "673ee6c9-ceee-4eaa-bd6e-e6971bc6b783");
        writeFileEmptyStreams(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "35696512-fe0b-45e4-ade3-ff32e642f4a7");
        writeFileEmptyFiles(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1e851839-565c-4b1e-a860-37264b7f3eb0");
        writeFileAntiItems(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e44a9f17-6a5b-4a74-8847-d1d9fefa9b43");
        writeFileNames(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b262d219-af4b-45bc-9dd8-8f3e9fd08482");
        writeFileCTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8446a57d-349b-4719-9990-90c972875613");
        writeFileATimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bcc59bd1-5a11-4e43-8004-f84fb903b8b2");
        writeFileMTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "43f3253c-1325-47e4-bfa2-f75678963be4");
        writeFileWindowsAttributes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d1aeb3c5-8a00-4aa1-839f-2100a12dbd58");
        header.write(NID.kEnd);
    }

    private void writeFileEmptyStreams(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9963e76e-9fd5-424c-a1db-671bc8fd3041");
        boolean hasEmptyStreams = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6c688423-0b38-4379-b780-f1ba1001561b");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "88686a3b-56c6-4124-bd95-f106fe9e2118");
            if (!entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "798b4df6-20a0-4190-9811-29ed52829ba7");
                hasEmptyStreams = true;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7a188f7b-a85d-4b58-8a04-ad054fd5830f");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c68d72d7-81c4-4f46-a255-11219a5b1e01");
        if (hasEmptyStreams) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "aa73ae1d-5c8c-41e4-bf7f-b9a85f2d5462");
            header.write(NID.kEmptyStream);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "3967bf75-01bd-4e2e-9bd0-3f644e2162d4");
            final BitSet emptyStreams = new BitSet(files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "63538fa6-6e31-4cc1-988a-85a85f509430");
            for (int i = 0; i < files.size(); i++) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "79e09030-200f-4f78-b052-09b6cc90b19c");
                emptyStreams.set(i, !files.get(i).hasStream());
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d14974f3-7afd-45fb-940e-3b8c59e20181");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e2c20d57-ca21-44c6-8ebf-a95be8a6de74");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d42dc796-5848-4f71-a6e9-11a1e5c8b352");
            writeBits(out, emptyStreams, files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5bb26717-b808-40ee-a7a9-392e9ee4846e");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a548289d-1a15-4457-b7f7-a7c357ee5bc6");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0ceb6225-f325-47a6-9fcf-8613113bdfdc");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1434aae6-ce01-4108-8400-b99adc5db2ad");
            header.write(contents);
        }
    }

    private void writeFileEmptyFiles(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "88faf764-4983-43cf-a8f6-eb88899cb6a0");
        boolean hasEmptyFiles = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bc7b841d-6c77-4fe9-8068-0fda23543915");
        int emptyStreamCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "36317a10-113d-4002-b9fa-dde3be3c33ca");
        final BitSet emptyFiles = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "29050f64-194d-47aa-a834-953e136d1039");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f10e938b-f2f0-40fe-93d2-14393152f163");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6edff709-a982-4964-bff2-042e7cac1d70");
                final boolean isDir = file1.isDirectory();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a3c43ced-ec8d-444d-a054-a3fce8d7b723");
                emptyFiles.set(emptyStreamCounter++, !isDir);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7f13263c-2958-4049-9e1d-b4f01d2e500d");
                hasEmptyFiles |= !isDir;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "164ffb95-d956-4a83-bd7c-77478a4d1ced");
        if (hasEmptyFiles) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "be3760c6-4deb-400c-b644-6653e95320bf");
            header.write(NID.kEmptyFile);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0cdfa57d-36f6-41ed-bef7-a3eba6835ec9");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6f5c85e0-18fe-470a-be14-36452c156d20");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e7620b16-18c4-459f-8f2a-6ef92ccf95d8");
            writeBits(out, emptyFiles, emptyStreamCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a73d36c8-88f3-43d1-b476-a0065880c689");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8fc6afde-2b0b-402d-8090-47d096012db7");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a7210212-61a4-4114-94d7-4a2f35216542");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a67b1d41-d8ce-4d8d-9997-9ed610d5a46a");
            header.write(contents);
        }
    }

    private void writeFileAntiItems(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "33e6f9dd-0dde-4e04-83c4-2913235952a5");
        boolean hasAntiItems = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "36351ce7-af22-4472-b005-5f646363608d");
        final BitSet antiItems = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9d7c149e-5014-47ac-a2e8-6c81000ee0b4");
        int antiItemCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5e3bb27d-0eb1-4fd3-a504-eafa952cfac3");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6f08bea7-712b-4079-b9bd-b1e95654dcdf");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "993eb684-0a36-4915-91e3-8eda2c24340e");
                final boolean isAnti = file1.isAntiItem();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "be9e9b53-ec3a-4bfc-a735-17ee6a827f71");
                antiItems.set(antiItemCounter++, isAnti);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8f63e676-034e-4dfb-8dce-a0afc8ff46c4");
                hasAntiItems |= isAnti;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1335fabd-0cdc-4f50-9cfc-6833ed33849d");
        if (hasAntiItems) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e3452d1b-b896-474b-93c0-3e62e89da175");
            header.write(NID.kAnti);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "62f47c4b-3020-4d89-ac6a-6d8b2d4e703d");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "529977a7-43f9-472d-a9ee-5ef7abda5e40");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b648bc38-b130-4cb7-b9f9-90e987b3e412");
            writeBits(out, antiItems, antiItemCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0d9ed4c9-cb5f-43bd-8e95-2df83cd630ee");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8ce88f2f-7789-468b-a196-e74e204e7541");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "65c871c7-b079-4128-afa2-416e679ee99d");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f6c535a8-4146-4ecd-87b1-f66a40e341d9");
            header.write(contents);
        }
    }

    private void writeFileNames(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "338cc519-f25e-4c96-b419-d91665d3794d");
        header.write(NID.kName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "74be40b0-2d02-477c-aff9-cf5e6cc17798");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d20b305f-01af-4ad3-a1d7-00e5ffb95714");
        final DataOutputStream out = new DataOutputStream(baos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "94fdc5e6-d839-41a5-b818-b16255d99724");
        out.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "77f00f11-058b-44da-8494-54d12580a70d");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "48e21026-3904-474d-8916-351d3b20a6bf");
            out.write(entry.getName().getBytes("UTF-16LE"));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c5947fb1-51b5-4083-b9b1-ff390af7951c");
            out.writeShort(0);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c314f492-0cfd-4f44-831e-64675da353ea");
        out.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fe36d4c8-6f1d-4cb0-acd0-26c8a1a967d6");
        final byte[] contents = baos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f3bb61d6-4402-46d4-992a-26d93d460a1f");
        writeUint64(header, contents.length);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fb62b941-83aa-4c7c-acb1-a873d27789e7");
        header.write(contents);
    }

    private void writeFileCTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "df798c5e-c049-4f86-ab3b-35f74dc300d2");
        int numCreationDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e2a0e876-34db-4c38-b6ce-7fced159b3a2");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ae7124d6-4589-4c14-ad60-c9dbe5949d13");
            if (entry.getHasCreationDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7d609844-bd84-41b4-a173-e4dfd13ec84b");
                ++numCreationDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bbc7640a-48ff-48f5-b128-ed9bc601f994");
        if (numCreationDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9dfedf84-5076-40a0-81bd-9d66579be75a");
            header.write(NID.kCTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ad19d479-3a33-4502-b4e9-d48441d482c1");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "74909479-7075-4055-a6c2-00816333d7f5");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c25a442c-ab9f-4944-b80b-8abbecf2f096");
            if (numCreationDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ac6d11f3-5235-4f6f-bd71-a9989bf52a23");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "932f923f-2d36-4c69-b6ae-2197de9e5ae5");
                final BitSet cTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5487ac88-0a09-44a8-9b66-4583ffd532ae");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "dc095c06-6e31-427b-ae9a-effeea71f6ab");
                    cTimes.set(i, files.get(i).getHasCreationDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fc4ecd6c-a8d7-4a48-9479-3e522776912a");
                writeBits(out, cTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8547591b-cd1f-4115-b7e9-59bdee86f56e");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e29cd360-53ee-48bc-a9fa-5046223b8916");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4c69a515-44c6-4f84-a95d-79525daf1071");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "99dc3e3c-bcbd-47fb-8d3a-31da95f5cc1d");
                if (entry.getHasCreationDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0cd7fb51-7927-4380-bffd-45be9d9febc7");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getCreationDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "365f29e3-00a2-419b-a061-abada10f800c");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0d446af9-d1a9-4544-a6e3-7d435cdfbf86");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5039a51d-c550-4ae1-8c4e-7db8028945e6");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9afddf3c-9c38-486d-adfd-88c62d26acb1");
            header.write(contents);
        }
    }

    private void writeFileATimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "161f66db-2195-4465-af2c-1e09d6934975");
        int numAccessDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "caebca16-d86c-4f72-80cf-51d99f6ef0dd");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fd2986ef-a775-454f-b03e-a4aca1bf7d19");
            if (entry.getHasAccessDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e01745d0-a7a7-40d2-8808-b5e872cb79fc");
                ++numAccessDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d4fefddf-1f83-42c6-9000-5694f527c0ce");
        if (numAccessDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "67418e0a-60ab-47b3-bf61-5c0a500ae48d");
            header.write(NID.kATime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a0777447-acaa-48b3-ab04-d17fa9aefbe4");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "14ad83f3-4603-4963-b6dd-c47ab5560ddc");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "eaa718a4-99c9-4c21-943c-2378eda49b9b");
            if (numAccessDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f49a37e4-b69f-4897-8030-5185b9331b3a");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "905f079b-5733-4269-a71e-c214db2cb716");
                final BitSet aTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "210115c2-7676-40f8-b258-ec573eef7f92");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "694536cb-3339-481d-a8a5-e7cad84dadaa");
                    aTimes.set(i, files.get(i).getHasAccessDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "343ce225-ba89-4c7d-a7c2-c4c16e0b595d");
                writeBits(out, aTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1fbbfce0-259c-44f6-88db-2dd10b19a8f9");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "72a00d96-ac60-482e-af85-2944572dc899");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ae195bd8-0b75-4358-a622-c7515a79e246");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "ba4884a9-bfa8-416b-86ac-46971b40c28d");
                if (entry.getHasAccessDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "79ebb8c7-b856-40ba-8a42-00e36abd3c96");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getAccessDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d4fcbe3e-d037-4d1b-871e-ce9d38e38bbd");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "0e86bcc5-1cc8-4dc8-ba60-6b76cb3f2e33");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4357bb60-4fff-44c6-afe7-c983e49b04f6");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "54dc6a0e-2bca-45f7-a5a5-cff85eee4519");
            header.write(contents);
        }
    }

    private void writeFileMTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e84f7eea-7bdc-4926-bbd9-09799ad5ef1b");
        int numLastModifiedDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c253842b-dc1f-4e67-993b-da31566bff1a");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fb1bcc0e-2170-4f11-bbf4-d3922a25a653");
            if (entry.getHasLastModifiedDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4ca79bf5-864e-4722-ad78-9d2659e09c7b");
                ++numLastModifiedDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1230579a-4ca2-4ea7-99cb-49ac42749578");
        if (numLastModifiedDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7b31a6d9-407f-48c7-a3ab-66219b77317b");
            header.write(NID.kMTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6bf56140-90a1-418f-99ea-e96f64e597fb");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d1ebb714-a7a2-4737-bb1f-201cf2b24471");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "2c6b3f64-d778-440a-88bf-bdfea66414c1");
            if (numLastModifiedDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "01de0ba3-a1e1-4676-aeae-278907397c1d");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fce2322f-45b6-4aef-b1a4-7426d7925b78");
                final BitSet mTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "edb189f5-ac73-4db3-9e50-9a958255fce5");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f8731e23-fd05-4e0e-96d3-e923d89e690c");
                    mTimes.set(i, files.get(i).getHasLastModifiedDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c7825705-ecd2-4fa3-9c10-590e67380b6b");
                writeBits(out, mTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "f2330a9d-bd6d-4135-b746-334adafdd889");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "fe4ed921-a579-4b43-a216-f35970a2b346");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "02b7aa27-8e64-43d8-bee4-cd6e2fa22e2d");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1cd38d01-f344-401a-bed2-42b10a9bd931");
                if (entry.getHasLastModifiedDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "95978fbe-c9e0-4981-939a-06f45ab4c2f6");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getLastModifiedDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6c75e3e1-15de-4773-941e-90b1ab066fef");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "7ba64856-8a6e-47aa-adbc-b6149e745784");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6b89ab1d-8f7e-43c1-b5bd-a99b2ac2307b");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8cea556f-7010-4c85-b549-e5f8a4c0a47a");
            header.write(contents);
        }
    }

    private void writeFileWindowsAttributes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "a42e2bed-da43-4f2a-81fa-4df1cf91cb31");
        int numWindowsAttributes = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5110e246-db13-4481-bd5a-8c513b1cfd61");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b73aa6bf-49a4-4610-9d32-e0a7a2e81f93");
            if (entry.getHasWindowsAttributes()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4feea62e-2127-476a-8831-1f702bd2b208");
                ++numWindowsAttributes;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8a4c716a-3d08-452d-ae83-171156039112");
        if (numWindowsAttributes > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e0994770-726e-4762-9eeb-97ea4f8f3ab4");
            header.write(NID.kWinAttributes);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e12a3d21-d23b-49c3-8192-d2a4ef53dc15");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "1288586a-b1ff-4a11-8295-e76f4d7117db");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4bd902b7-dd64-4f75-a80c-a75d7f19af82");
            if (numWindowsAttributes != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "92642e44-cb43-45f4-90e4-ba1473da73d8");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "30de4794-c8ea-47c6-9c13-ddfa595555fb");
                final BitSet attributes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "483359e7-f103-4bd1-ac73-0efd6e358966");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "2bbac395-3714-4671-b8c5-70e9c2e903b2");
                    attributes.set(i, files.get(i).getHasWindowsAttributes());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "20fbb8ea-089c-417a-8a20-2f098956693a");
                writeBits(out, attributes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e45cfadb-16fc-4263-ac8a-d397afabdbaf");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5ce1d869-2efe-4a98-b5ce-130237e7b562");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "53919515-bc55-4bea-aca3-5d8dcafd4e77");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "85a0e089-511c-444c-ab9a-1f048c720f40");
                if (entry.getHasWindowsAttributes()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "2957f81f-cedd-4069-a3ea-8a4a2086230e");
                    out.writeInt(Integer.reverseBytes(entry.getWindowsAttributes()));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6ab4f090-1c60-4932-815f-290f17624c3f");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "33f5182f-834a-481b-80ca-e25282ed651a");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b35a9983-48e5-4664-ab17-dcc555c1ed18");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "031a3839-57bf-40dd-8f26-9c83a14fcad2");
            header.write(contents);
        }
    }

    private void writeUint64(final DataOutput header, long value) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "9a6401c7-7b32-411e-9a01-ae3dd49f41ea");
        int firstByte = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "bd9127d1-41e3-43a2-b64c-d34d3fa1a9e2");
        int mask = 0x80;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5dd7adb4-0fce-499d-800f-b703f8bbde9f");
        int i;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d67865a8-1d3e-444d-ab52-70fa677d2f63");
        for (i = 0; i < 8; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "c4c0279c-e640-4334-97d9-0476a392bd5e");
            if (value < ((1L << (7 * (i + 1))))) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e41eaa48-c209-4dad-be43-2e37af6e6c19");
                firstByte |= (value >>> (8 * i));
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6841dbc9-484a-4e2b-9ee8-cdefb409230d");
                break;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5c79bbd0-a579-4aa8-8ecb-185d641a6bec");
            firstByte |= mask;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4c773986-2512-4b32-b019-31a5851d1eac");
            mask >>>= 1;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "db09c45c-f1dc-48a9-8025-9718019d5404");
        header.write(firstByte);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "75d84555-e822-4359-9d80-ab1d01a6f1f7");
        for (; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "18e8d9de-cfe8-4844-9d4d-b3048878564c");
            header.write((int) (0xff & value));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "305e1aa5-b678-4bce-aba4-373a5ecf8a86");
            value >>>= 8;
        }
    }

    private void writeBits(final DataOutput header, final BitSet bits, final int length) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "5c51ca66-e726-4be8-99f2-633f3f111db9");
        int cache = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8539e619-273e-4c30-8312-777180d23edf");
        int shift = 7;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "6974ff49-65d8-4539-affe-e3bc606d3fdd");
        for (int i = 0; i < length; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "e89e7e4e-bd2c-4f85-b779-df489945628f");
            cache |= ((bits.get(i) ? 1 : 0) << shift);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "55fb0b29-4629-47e1-a225-2d7d1c17095d");
            if (--shift < 0) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "2da60311-d615-483f-ac55-dbe68bc44fba");
                header.write(cache);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "8e6b4fcb-9fb2-44e6-8152-6ad8a78b8ef2");
                shift = 7;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "38c2c336-55b4-45d7-8bfe-c834c211ca3d");
                cache = 0;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "b1ac8b9e-817b-43c0-aae2-9395dbccfa6c");
        if (shift != 7) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "43e9d756-84ce-44c6-ba98-aca3cc3eefdb");
            header.write(cache);
        }
    }

    private static <T> Iterable<T> reverse(final Iterable<T> i) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "d42826d8-45c7-4cbd-8d16-fd0c374e9744");
        final LinkedList<T> l = new LinkedList<T>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "2afd2b2d-b65e-499a-a2a2-56c6881a5605");
        for (final T t : i) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "49cc1937-d054-40ba-a379-9e5214cb5e29");
            l.addFirst(t);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_5_10.coverage", "4f46a042-4217-407f-b4b8-15559955a6a1");
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
