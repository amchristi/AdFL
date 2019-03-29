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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6299d4c4-b45f-4975-88f6-f5d469e3f4c8");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "538416d9-62c3-41cf-ad6e-85aff6509dfb");
        this.contentMethods = reverse(methods);
    }

    /**
     * Closes the archive, calling {@link #finish} if necessary.
     *
     * @throws IOException on error
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2a3b53ee-3c25-4b4e-b741-6420452bfc14");
        if (!finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7944d887-c79a-48ee-bfcf-00632fd927f2");
            finish();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2a052035-351b-4e04-992d-7410243f7f78");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9d79000f-1713-4180-9920-55624cc8a591");
        final SevenZArchiveEntry entry = new SevenZArchiveEntry();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "60d7c473-d239-4e94-8b6c-83fb4505ec7c");
        entry.setDirectory(inputFile.isDirectory());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "add46c75-7e6c-4791-835e-beda787b1f2a");
        entry.setName(entryName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "57292027-3307-40c2-9d81-9cdefe2e39ea");
        entry.setLastModifiedDate(new Date(inputFile.lastModified()));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a74f35ed-8deb-4c66-aaa0-26c0671e88eb");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4965400d-af91-4d8b-8d11-2f9e64fd9dac");
        final SevenZArchiveEntry entry = (SevenZArchiveEntry) archiveEntry;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4d61eafc-0b4d-440f-a332-d7260c8a6489");
        files.add(entry);
    }

    /**
     * Closes the archive entry.
     * @throws IOException on error
     */
    public void closeArchiveEntry() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a1d464f0-cedf-493e-93d6-3449b1ce24d5");
        if (currentOutputStream != null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "63c5d826-55c3-4592-b721-4cb04100fd98");
            currentOutputStream.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c6505204-546d-4191-9e93-bab9a9fe943e");
            currentOutputStream.close();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "bf41bff9-759b-44a1-9b68-1f93221aab55");
        final SevenZArchiveEntry entry = files.get(files.size() - 1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9c9d38e6-2630-4698-9d18-8844227f9915");
        if (fileBytesWritten > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0671df25-71a7-47cb-ac21-f6e8069b3d91");
            entry.setHasStream(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b90edf91-b1ff-4046-ae00-455e8317389e");
            ++numNonEmptyStreams;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "34662a64-d5b3-4ea6-8e51-dc0906ce4348");
            entry.setSize(currentOutputStream.getBytesWritten());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "91d35930-d76a-4012-9ca6-3c96f6bb082d");
            entry.setCompressedSize(fileBytesWritten);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "25117ac8-c3c8-4e4a-9c1f-2a0da1de02ed");
            entry.setCrcValue(crc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "92aadeb1-487c-4067-8fe7-f6deb2297a42");
            entry.setCompressedCrcValue(compressedCrc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0ed9fbef-7b89-4a56-85e4-0fa54905daf5");
            entry.setHasCrc(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "047adc47-abfe-451c-82ea-28478d292a20");
            if (additionalCountingStreams != null) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7c1ce14b-e53e-42a8-b97e-44089ad28378");
                final long[] sizes = new long[additionalCountingStreams.length];
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b6b6a133-7af3-4ff8-8a2a-0f46cea02045");
                for (int i = 0; i < additionalCountingStreams.length; i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "311eb461-6aff-4aaa-90ee-06c45171895b");
                    sizes[i] = additionalCountingStreams[i].getBytesWritten();
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "368a562d-36c7-426f-a854-5f9dd6d15081");
                additionalSizes.put(entry, sizes);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9e2e1718-0c83-4005-8930-15aa8e6f0c07");
            entry.setHasStream(false);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "18d872fd-bf3c-42f0-848a-f506809e91b4");
            entry.setSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "984f0563-17cd-4901-b84f-e6d628071f9a");
            entry.setCompressedSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d4a4e35d-626f-4a73-a081-34839b1c6d77");
            entry.setHasCrc(false);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9abc72fb-9a20-476e-a845-551a3b8ea60f");
        currentOutputStream = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4d4bfb6a-c499-4907-8b98-eb0b8a25531f");
        additionalCountingStreams = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "fe3d640c-2044-4373-93c0-611b4dead482");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e0b19f50-9597-47ed-aecf-b3a271f36289");
        compressedCrc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "60880341-191b-428c-a41b-b6e239a4551a");
        fileBytesWritten = 0;
    }

    /**
     * Writes a byte to the current archive entry.
     * @param b The byte to be written.
     * @throws IOException on error
     */
    public void write(final int b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ae8aacd4-cdb9-441b-8cd7-948c75d68fe6");
        getCurrentOutputStream().write(b);
    }

    /**
     * Writes a byte array to the current archive entry.
     * @param b The byte array to be written.
     * @throws IOException on error
     */
    public void write(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "89b2857c-e023-495c-bf98-85858068311b");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d70e1a81-577d-4299-814f-91227f2019fb");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a303e40c-cc8d-48df-bd85-729d1f906b95");
            getCurrentOutputStream().write(b, off, len);
        }
    }

    /**
     * Finishes the addition of entries to this archive, without closing it.
     *
     * @throws IOException if archive is already closed.
     */
    public void finish() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f0efd9bc-3f71-4fe1-923a-6454f59d116c");
        if (finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "22027377-ccf9-4a9a-9fb8-273fccfa037b");
            throw new IOException("This archive has already been finished");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f10e02b8-bb54-43d0-b5c6-f6e8b8613660");
        finished = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f150e17a-7731-4531-93c0-691ae960a543");
        final long headerPosition = channel.position();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "cf9d54db-890c-4a72-a8ee-e877b6be6589");
        final ByteArrayOutputStream headerBaos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b6542698-a049-4b55-8c2a-92633d66f64a");
        final DataOutputStream header = new DataOutputStream(headerBaos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a13bafa0-d1bb-42d9-86ab-189c504b1f11");
        writeHeader(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a191c31a-447e-40d4-afac-a2fab5db2733");
        header.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f05de7b8-2c2a-436c-827b-d9875c27ee01");
        final byte[] headerBytes = headerBaos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "32874284-4420-4610-8a3a-a20c4f8d73e4");
        channel.write(ByteBuffer.wrap(headerBytes));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6c24cf97-3ada-4d32-b050-aa6ec9a77d71");
        final CRC32 crc32 = new CRC32();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d9441bb4-d989-48a1-baab-fe2dc243f551");
        crc32.update(headerBytes);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ce99f212-8cfb-4cdf-b530-743bc67c7b7e");
        ByteBuffer bb = ByteBuffer.allocate(SevenZFile.sevenZSignature.length + 2 + 4 + 8 + 8 + 4).order(ByteOrder.LITTLE_ENDIAN);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c49f30f9-d4f6-44bc-b9d3-c57659d021b3");
        channel.position(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1c19210d-9bd5-4469-a350-a13284472654");
        bb.put(SevenZFile.sevenZSignature);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "074a3230-6db3-4825-9116-c335827c46e3");
        bb.put((byte) 0).put((byte) 2);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e4485330-d5af-4e7c-8ebc-991fd57d0765");
        bb.putInt(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "15c330ef-a654-4e3d-8faa-13dbe608044f");
        bb.putLong(headerPosition - SevenZFile.SIGNATURE_HEADER_SIZE).putLong(0xffffFFFFL & headerBytes.length).putInt((int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1871ef1b-b73a-4d70-b0fb-ff1f6ee301fc");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f0a7ed4b-e141-4859-b698-f4146e50e1f8");
        crc32.update(bb.array(), SevenZFile.sevenZSignature.length + 6, 20);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c15ce73e-fc1d-4709-a30e-064a24486317");
        bb.putInt(SevenZFile.sevenZSignature.length + 2, (int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b63bee1a-f58c-4423-bd5f-7f4e07b6fbf9");
        bb.flip();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "68214cf6-4d2b-41d8-b7b3-52827d33ea96");
        channel.write(bb);
    }

    private OutputStream getCurrentOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e32ba591-514c-43d2-8419-73ef724e8de4");
        if (currentOutputStream == null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d3a2ab2d-8994-47d2-ae25-cdb548210670");
            currentOutputStream = setupFileOutputStream();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7ea70c8f-bfb5-44ea-8b54-2ceedec4b666");
        return currentOutputStream;
    }

    private CountingOutputStream setupFileOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "013d5d8a-2eae-453a-b2fb-3284ff7d4e59");
        if (files.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "eb7314d4-a50b-41d9-92b8-02b5129c08f9");
            throw new IllegalStateException("No current 7z entry");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "bbc4d2cc-6a34-446f-933b-1751592bcb19");
        OutputStream out = new OutputStreamWrapper();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "60a42513-0bcc-4cc5-ac9e-13f4afc94d24");
        final ArrayList<CountingOutputStream> moreStreams = new ArrayList<CountingOutputStream>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e28ff568-d309-4da0-9e58-bac991f6a87e");
        boolean first = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "68c18ff9-bdc1-47cb-9b86-716902743dbd");
        for (final SevenZMethodConfiguration m : getContentMethods(files.get(files.size() - 1))) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9b51eb2a-b179-459e-b195-50706121170b");
            if (!first) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3edfab15-f249-40ff-a194-92a51437f6e8");
                final CountingOutputStream cos = new CountingOutputStream(out);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3e5e6884-3820-4c4b-9981-aed508927320");
                moreStreams.add(cos);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "41e62c2b-c46d-45f5-8879-2c530cc1499e");
                out = cos;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "8125a7e6-c518-4a31-ba7a-12d74fa713a3");
            out = Coders.addEncoder(out, m.getMethod(), m.getOptions());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1e4ccf8b-2bac-4f2c-ae00-d00bf960976a");
            first = false;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0dad5ec9-9223-404c-93b6-09152d77db12");
        if (!moreStreams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5b009a83-43c4-408b-b81d-67619f3a90cc");
            additionalCountingStreams = moreStreams.toArray(new CountingOutputStream[moreStreams.size()]);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e600e248-4546-4ff1-9887-d1dc972e20c9");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "860e4d87-61d9-44a5-8333-b6b82020fc3c");
        final Iterable<? extends SevenZMethodConfiguration> ms = entry.getContentMethods();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c5cd1f62-6687-4255-bcb7-621fcce5872c");
        return ms == null ? contentMethods : ms;
    }

    private void writeHeader(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ffcf857f-17b2-4485-927b-54e53a7f4440");
        header.write(NID.kHeader);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0b4d9d55-4851-43a0-867c-c2ba8546ae58");
        header.write(NID.kMainStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0b93bd8c-f667-499c-8cfd-2a4dffd860e3");
        writeStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "29948975-ab4c-4777-bc15-d24e1cff6955");
        writeFilesInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "57e09aab-655e-42b3-979b-89e882ce1716");
        header.write(NID.kEnd);
    }

    private void writeStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4bf21a56-d9ae-4b9a-ac67-5760d7761779");
        if (numNonEmptyStreams > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "8188861a-2b5a-4ece-abb0-69011e621d99");
            writePackInfo(header);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f4583610-1fa6-4d92-8beb-6707873046ba");
            writeUnpackInfo(header);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "91b251fe-303a-428d-87a2-f6fba0c80412");
        writeSubStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7e9c77ca-6908-4039-8559-732336188181");
        header.write(NID.kEnd);
    }

    private void writePackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "08070784-0fa8-46ce-a79a-826b6d222b92");
        header.write(NID.kPackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "40a5ec4e-853e-4a66-84b8-330396506fc6");
        writeUint64(header, 0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "453e292b-79ba-4646-b1f1-cf21c09483b0");
        writeUint64(header, 0xffffFFFFL & numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7fd19c74-e90e-4ac3-8925-3ccd85e3d7fd");
        header.write(NID.kSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "18d16d0d-8940-44fe-b2b0-d89655f7e505");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "33a77ce5-3383-4e13-a881-456115ef30fe");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "77c5b9da-a2f1-4995-999d-78fa856ddf3e");
                writeUint64(header, entry.getCompressedSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a9f68cbe-535d-41e5-bbc5-883b4ae40d30");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "246b4df9-3c0b-4c3a-92d0-27e47ec91dea");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b8a49439-2f8a-4f2b-ad05-0dc99037238e");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "bad39671-63c1-4a7a-8410-63b6875e242d");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "05ebd359-0424-443d-a9aa-b05d14d18b2e");
                header.writeInt(Integer.reverseBytes((int) entry.getCompressedCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "384a938f-df6a-4e65-8c64-b592c7f087e7");
        header.write(NID.kEnd);
    }

    private void writeUnpackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1826604d-669e-4f82-b496-0cc05f8d85e2");
        header.write(NID.kUnpackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ef6d3766-ff5c-46d2-9c3b-8c304beb81fa");
        header.write(NID.kFolder);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "628e2e4f-334f-4c99-82ff-eb6387459006");
        writeUint64(header, numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c328513c-acae-43b5-9e97-60a73520a295");
        header.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0feaf949-ad21-4115-9450-223a4257003e");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "089ecbfb-0705-4f7c-ad62-4bc0fae4a676");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "efde70d7-d1f6-4492-9390-7c1b12a1a66a");
                writeFolder(header, entry);
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "00d720c3-8f18-499a-b00b-bf554dbc4a76");
        header.write(NID.kCodersUnpackSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "009cc615-8f81-4884-97fb-639368564b12");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b70dcd35-9390-48c5-a4ab-057998ca831b");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0414decb-5862-4fc1-9a90-a90c73255c18");
                final long[] moreSizes = additionalSizes.get(entry);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "36f847a7-ce67-48d3-bf44-500c2d97cf18");
                if (moreSizes != null) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4a1c5576-1822-4f39-8f4b-782da0a1dad5");
                    for (final long s : moreSizes) {
                        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0abce808-fd0e-49e2-9c7d-e1a2e2630016");
                        writeUint64(header, s);
                    }
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5275318d-f97a-4c00-9c94-c4141c0a7b86");
                writeUint64(header, entry.getSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6382b3d0-ffb4-471a-9fc6-a27261a4ab17");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4cff8bcb-c6e9-492c-8ae7-b80df674dada");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9b0f1cfa-1869-4d37-950d-a3c5a2c6cd79");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a970d320-87ad-44e8-bb08-f7dcc790613c");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "cb09bda2-95c7-4abf-afa5-7b74d8c0a58a");
                header.writeInt(Integer.reverseBytes((int) entry.getCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0a505984-0336-4322-9b60-acbd95b16b92");
        header.write(NID.kEnd);
    }

    private void writeFolder(final DataOutput header, final SevenZArchiveEntry entry) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ff797d3a-2e86-45dc-8744-bd81a7cda1db");
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0bcd17c2-fad8-4823-921a-0d6a0d7f2d2c");
        int numCoders = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7dadb569-053a-4326-bf4c-31059902f206");
        for (final SevenZMethodConfiguration m : getContentMethods(entry)) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "28a11fbd-cfd3-419f-b68c-8614a28e06c9");
            numCoders++;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "09c4d758-d46a-4594-bc9f-19c7910064b0");
            writeSingleCodec(m, bos);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "32ee9bfa-8c64-4e1f-9946-83798cac7dda");
        writeUint64(header, numCoders);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c23e6f6a-afef-4aa6-b720-d5af82d128d2");
        header.write(bos.toByteArray());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9bd47820-ef21-48a9-aa02-f46892ab4491");
        for (long i = 0; i < numCoders - 1; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e96b73da-c02c-4b59-8dfe-a8ef77d892de");
            writeUint64(header, i + 1);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a5898bc8-f3d1-4a26-9ece-e0e743434c98");
            writeUint64(header, i);
        }
    }

    private void writeSingleCodec(final SevenZMethodConfiguration m, final OutputStream bos) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7a864221-2970-415e-8013-7c53ab725b60");
        final byte[] id = m.getMethod().getId();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c29a1d5d-00cb-49f7-b71b-0d18302f4372");
        final byte[] properties = Coders.findByMethod(m.getMethod()).getOptionsAsProperties(m.getOptions());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4a1908ea-6b03-43e4-bf4b-1766f901818c");
        int codecFlags = id.length;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e48cc3d0-0525-4708-8c5f-adbd148f75fe");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e20cb0e3-8559-49dd-9be9-e3fc208c2ab8");
            codecFlags |= 0x20;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a3aae7b4-c272-412a-8849-e88a980d9635");
        bos.write(codecFlags);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2013c3ed-1016-4fd7-8b36-4b1aba151965");
        bos.write(id);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0658e4b3-b3ef-4470-9f1e-93bbde760939");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b94a53bd-5c72-442f-bfde-10ca3c607a37");
            bos.write(properties.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5982f8fe-aca6-43b9-8e9e-2098486f04ff");
            bos.write(properties);
        }
    }

    private void writeSubStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9cf070fb-1401-4e52-b489-2088c7c8e7bb");
        header.write(NID.kSubStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "dc8c76b4-fdd8-442a-b559-55386b97d2f9");
        header.write(NID.kEnd);
    }

    private void writeFilesInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5b027b3f-2d32-44d4-9cf2-750cf5a28082");
        header.write(NID.kFilesInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "02e73ad9-0472-495f-9f06-1864ed3ce0ec");
        writeUint64(header, files.size());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "401b78e9-e4c2-470c-bed5-58914df7fe05");
        writeFileEmptyStreams(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9fa1cfe3-3dd9-4172-94d6-d42c1db83acc");
        writeFileEmptyFiles(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "336100ba-0500-4625-a830-7e9e917af37d");
        writeFileAntiItems(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f61afc41-6491-4442-bcde-0c918ca6a549");
        writeFileNames(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "85a96d39-8a35-448f-ac05-11f9bd381eda");
        writeFileCTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f1d3dc97-300f-4414-ac9d-0d1ea2417bd0");
        writeFileATimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "886d800f-1f36-407a-83fa-a8621f02820f");
        writeFileMTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5743c0e9-9b6b-4abc-85de-4305a65a8e61");
        writeFileWindowsAttributes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3e5b1537-b231-4cc8-8627-0f477a6f1677");
        header.write(NID.kEnd);
    }

    private void writeFileEmptyStreams(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "8de738a7-6ddf-417f-b5e6-47b1eb71100d");
        boolean hasEmptyStreams = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5e8e96d4-8504-4850-b20a-e47c580fae00");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "fca6ce36-17e8-476b-920e-9b1947b69872");
            if (!entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2b17e8fb-4df6-447c-bc06-4eaea7bc1511");
                hasEmptyStreams = true;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3d053716-7aa1-46cc-9c09-f50703f03413");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7f9c83aa-3911-4a24-8e2d-ecc072ea8d79");
        if (hasEmptyStreams) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "62bd82b3-b199-425c-a9b8-b4856a9fe65a");
            header.write(NID.kEmptyStream);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "49e14efe-4eb5-4b5c-a4ca-38d5de3c9cdd");
            final BitSet emptyStreams = new BitSet(files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a9a2b88a-e436-4d71-b1af-08f102507013");
            for (int i = 0; i < files.size(); i++) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a3e13223-9fc6-4398-8b09-0c8cfc89f275");
                emptyStreams.set(i, !files.get(i).hasStream());
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e0078ce1-e186-4494-9dc1-9aa8762744d4");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a01c2afa-a3f5-493a-a931-2aff4982bb3d");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6d4c070b-1bb3-4f5e-8716-294877c15c4e");
            writeBits(out, emptyStreams, files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "33314a44-b476-4344-839f-819f4e4431e8");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "71a57c82-8feb-4863-b692-626452fae869");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "76faef1c-70f0-4b71-a016-05ffcf25273b");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "10da0b17-7841-440c-b222-d2f8f3424487");
            header.write(contents);
        }
    }

    private void writeFileEmptyFiles(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5f21ab3b-63f3-4547-9945-19535f62a3a8");
        boolean hasEmptyFiles = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e01fc9ec-9fdb-4bd8-8453-654140569b08");
        int emptyStreamCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d25edfff-1edf-4d78-ab2a-45408ee546a0");
        final BitSet emptyFiles = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "47c22a28-3569-4af3-812f-e0b24a0d6ac8");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "74002aa0-b81a-459f-89cc-616ff3ec1e25");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "273e822c-2ef3-4d75-9f6e-dd8986e1339f");
                final boolean isDir = file1.isDirectory();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "18a3a074-42f0-45ef-8334-b0c4109782d2");
                emptyFiles.set(emptyStreamCounter++, !isDir);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "319fa1a5-d7e9-4e77-8e8c-dbce81c778a9");
                hasEmptyFiles |= !isDir;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5c4b815b-ca04-4056-b85e-ed6e9063fbf5");
        if (hasEmptyFiles) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5aff2e06-ff56-49aa-924d-4adbb8ef1da0");
            header.write(NID.kEmptyFile);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e1e08a95-cfd4-48ae-b5d9-be8982e7a7fa");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d5dbee87-15ae-4a57-b6c0-9bde021db8c3");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "433f529e-4e8f-4494-a6ff-afda9b8a3d7c");
            writeBits(out, emptyFiles, emptyStreamCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "90d59f10-c1c9-43fa-90b0-1278bad89f00");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d46cb386-bcea-4a1c-aff0-bfe40d8fdabf");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "60a9e1da-187b-441e-89ba-85e5a4f9b892");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b178c7ca-81b0-4324-98f8-2a641173b0ae");
            header.write(contents);
        }
    }

    private void writeFileAntiItems(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c556e3e4-d47a-45c5-aefd-758d08457aec");
        boolean hasAntiItems = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6cc1f3b7-39ea-47cc-80ad-b503eebb70b6");
        final BitSet antiItems = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "96d33d97-54d6-4290-b86a-6654a23fc507");
        int antiItemCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "28cd4c45-0a25-4d30-b164-8f6dc700bc90");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "14279a93-d972-44f8-8087-4350e7a9de75");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d8d3ce43-a2bc-4e71-90e3-bc83eb09abc6");
                final boolean isAnti = file1.isAntiItem();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "93bb9178-e273-466a-b4a3-f18c79bd66d9");
                antiItems.set(antiItemCounter++, isAnti);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7b7554ea-9753-44bc-9914-1039ed2d5deb");
                hasAntiItems |= isAnti;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "61767762-a348-4372-a550-7b39b05f53c9");
        if (hasAntiItems) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "78e74b80-c336-4df7-b546-c739a5d39b95");
            header.write(NID.kAnti);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1de83925-ca9a-4484-96ff-2a9819b7bd7b");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "48fd9323-370c-4067-becd-a6ead9fe0a68");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "331454f0-f100-47a1-87c1-ab9bf73c2d72");
            writeBits(out, antiItems, antiItemCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "111064d0-260d-42a3-b164-1afc4134b97d");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e350e1f6-f3d0-4f1e-9073-e3eddd10a406");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "486d4083-b6ff-4179-94c4-5a1604e1bc7d");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b7d8f59f-6931-4e30-b2a9-aa522b34ef51");
            header.write(contents);
        }
    }

    private void writeFileNames(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c95d0168-43e3-497f-8f13-b655eb200dc0");
        header.write(NID.kName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f916d3bf-5c73-407d-8dab-343cc3208840");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1c3ed5ab-4264-44db-9e47-61a76312d323");
        final DataOutputStream out = new DataOutputStream(baos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "af68d71f-051b-4eb0-b446-5e3349fb7207");
        out.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9fea0e05-e1d1-432b-8069-61fbfafb1c61");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a65f4d8d-da4a-43d1-ab54-a85e81a93c85");
            out.write(entry.getName().getBytes("UTF-16LE"));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "55772c5a-cf91-4a3e-a2ad-1bc49af447e0");
            out.writeShort(0);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d96a29a2-b9ce-4cdd-9e97-a90c5cb41ebb");
        out.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9221a253-ecdb-4df1-9f93-29e8cc6d5611");
        final byte[] contents = baos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7dc86d28-ecc1-4c6c-9fd2-db5ca92dce8f");
        writeUint64(header, contents.length);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "73836466-7660-4043-911e-03301ad403ef");
        header.write(contents);
    }

    private void writeFileCTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "8176a182-45ce-44ea-86b5-08b5d9248073");
        int numCreationDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0ff32a02-fa10-41e9-b0f2-26b2c75d85e3");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c7d296ad-e4d5-4ce9-a16c-3e3da7d08ab1");
            if (entry.getHasCreationDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0ff832a8-783e-4082-8b93-bdcaf515d97b");
                ++numCreationDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9ed9b451-c3ee-4a20-af19-b157818469c5");
        if (numCreationDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "24acd697-0dbd-4112-bfc2-b64755d57ddc");
            header.write(NID.kCTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c860e3d6-d7ab-4728-ba00-be37e0505df5");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "36bcced0-8bc8-423a-a0a5-38c21fc0a013");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ab506a64-a328-456e-ae9a-a3a758d0bcff");
            if (numCreationDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d801d239-2159-40da-be74-5715767a7cb7");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e472c7eb-7332-42e1-bdfa-acc56b1d8e68");
                final BitSet cTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "800f91cd-7815-4932-9631-3b4b8f7acc0d");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "af4588e9-b911-443c-90d3-be431c66676d");
                    cTimes.set(i, files.get(i).getHasCreationDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "517f40e6-2884-4382-8b47-71844f01124d");
                writeBits(out, cTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1873e364-5ab4-43a1-b590-dbb62525059a");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "034c6c8d-029f-41be-a6b3-8d317e2f4a42");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0605892e-854f-4ebf-a1d2-df924d8a1d7b");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "73714a91-15b7-479b-be9e-ff9b23e8c2ee");
                if (entry.getHasCreationDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "884d8ed6-8887-40c9-84b2-21993467a461");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getCreationDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0920cdec-6ca9-4ab4-a557-259a25b4697c");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "66d0dcdd-b09e-4c26-bc12-28574fba81ea");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a169dd6d-d52c-4e63-bf8d-d7c95a5d31ef");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0f9b6705-9cd2-4df4-9062-df94b66d6952");
            header.write(contents);
        }
    }

    private void writeFileATimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0b9d4ec3-ab81-4b00-a10a-d3612a2bcd05");
        int numAccessDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d42718f3-8a50-40d7-a284-3bdbd1e50289");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "0d872ca0-0ea3-402f-aa46-848f83a92806");
            if (entry.getHasAccessDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e9a777c2-629f-498b-b4ba-d9f2f9491cf5");
                ++numAccessDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e8e28238-aa17-4ecf-91ac-5f5f7c287714");
        if (numAccessDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c345ffa6-85df-4f74-8d11-ba1c6fd19fdd");
            header.write(NID.kATime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "8a72a8cb-b48f-46d7-8913-737944ab80a0");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6f9bf670-aff2-4175-a99e-6efb5f92e8db");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "db706317-8da4-4c81-9b65-565de271da62");
            if (numAccessDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "128a0c56-1976-4b70-87cb-ab0111173c5d");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "85437cff-db18-4289-b557-cf58d1a9885d");
                final BitSet aTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "14afb613-08c6-4bb5-93bd-19bdf79a8d9c");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ffe92d62-5e98-4c54-8633-12cf63b5d322");
                    aTimes.set(i, files.get(i).getHasAccessDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "782b7ce7-dea7-4e0a-b295-a358f2065a6d");
                writeBits(out, aTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "64e3bf89-602a-4ee7-ae23-71c071df81f4");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9345ad1c-8401-448b-a73e-6e8d5860292c");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a6533893-8415-49bd-90f5-231c41a4c804");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c5440e84-a138-445e-9c6d-742137f1206a");
                if (entry.getHasAccessDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "008c13e8-7ae0-45ac-80ad-42673710ff46");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getAccessDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d231a83e-a0f4-4883-a2f4-b71a4fa6b92d");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "91cf5ad8-1072-460a-983c-a40d0aca35bc");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4a4c4b39-8272-4711-b6c4-182230373446");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5d4c9499-9e00-4306-8858-8eaaaa766a92");
            header.write(contents);
        }
    }

    private void writeFileMTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6767fe80-407e-4d73-8ec6-0bac0aff295c");
        int numLastModifiedDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1efd1a44-df07-4c1c-9215-9d3b48564167");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "bb0cf7a4-07b4-4c38-bb99-b55a56b2581b");
            if (entry.getHasLastModifiedDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7fc92b39-178c-4657-81e3-041a8f06f00c");
                ++numLastModifiedDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "01dea5ab-5c27-45ef-b749-23b05f08d5cd");
        if (numLastModifiedDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "23d005c4-f133-4ce1-bb01-5fb41eeb84c7");
            header.write(NID.kMTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "157fcb98-053f-432e-8563-13b0a920b43a");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "302eb646-16ae-4408-9498-53077114f815");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4b1b410c-59b5-4778-8a93-c41f435a884d");
            if (numLastModifiedDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2b56f7ab-3be1-48af-a11f-50f37d654629");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "4ae328dc-cd59-44d3-9bad-82ae0540ec48");
                final BitSet mTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "86054375-26d4-4c74-9bc4-88507e0683d4");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2f86ca3f-4fe0-4e25-90da-91f96e49bfb9");
                    mTimes.set(i, files.get(i).getHasLastModifiedDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "90ed08bf-f646-4139-ab71-9069350cacb3");
                writeBits(out, mTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c92af044-885f-4a96-b747-fbb21e36803e");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6b53c194-21e2-441d-9e86-b84645e0d522");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "04281673-fdf1-40e0-b620-6a9d18d181e4");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6ea7e9d1-5c1a-4b50-a308-cfd7cb2c6d8b");
                if (entry.getHasLastModifiedDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "21d9546b-eecd-402a-be3d-942f843c5be4");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getLastModifiedDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "d4dee772-24d1-43e3-828c-7fad9a2bc09f");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "ecb6f937-2ae3-4a34-9c95-4bb74e4334c8");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "11014601-5c0d-4d49-baaf-f90a4815c983");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "18b5e6e3-5b0b-4061-9911-2433f3be62a7");
            header.write(contents);
        }
    }

    private void writeFileWindowsAttributes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "40b9acde-1456-456a-90b6-efd27f70c38e");
        int numWindowsAttributes = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "6065b6f5-9cbe-4ba9-b830-88cb8765c19d");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "fde76614-928c-46b9-98e0-934d3021e3ce");
            if (entry.getHasWindowsAttributes()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "64d682c0-fc0c-48bc-b9cd-0f823446132e");
                ++numWindowsAttributes;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "fd744c00-5c09-4627-b2a9-53795ac198e3");
        if (numWindowsAttributes > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "400ac370-497f-4272-acaa-2c2d48b6f0e8");
            header.write(NID.kWinAttributes);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f35ae7c1-db2e-4a98-b550-7314052f0e27");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "43910971-89c5-4ae2-8705-6d85aa9fbfcd");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "08a4723d-4b23-40ec-a0ce-8331dd96cda3");
            if (numWindowsAttributes != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2401c160-84a1-401a-958b-2a640ee6daf8");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c80fd79f-57eb-4ccb-b47a-34ef429510db");
                final BitSet attributes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a5047510-1fcf-4a51-b0b9-3af6ae4052bf");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "2105521c-2255-4c19-b32d-76a16686ccc7");
                    attributes.set(i, files.get(i).getHasWindowsAttributes());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e751292f-deff-43cf-bbf5-68c5be7ef22f");
                writeBits(out, attributes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1895a3f6-ece9-4691-a0e2-283613461b8d");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "deba7584-30fe-4166-adad-8fd703e30df7");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "543303fc-afc0-4880-826a-50b89fbb447d");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "9b81b0e0-e55d-40cc-81c5-d96d267e55d1");
                if (entry.getHasWindowsAttributes()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "5ba38fad-ef6b-419b-bfa7-fbff41a99624");
                    out.writeInt(Integer.reverseBytes(entry.getWindowsAttributes()));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3d0607f6-7a80-476a-afda-eb35f1f2a323");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "452c132a-c396-4777-8577-76fe51e81bb0");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "27f81a81-a7ac-4dee-8458-0f99e82a782c");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c1c3028b-30cc-4fc6-b979-bceef95ff218");
            header.write(contents);
        }
    }

    private void writeUint64(final DataOutput header, long value) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "57c0e922-9454-4ec8-b847-bdce5eab18d4");
        int firstByte = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "c13ac27f-0f2a-493b-9e80-daa2c29c4009");
        int mask = 0x80;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "8f2caba0-5d37-4621-9ad3-cade7b8fc2d8");
        int i;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "934df3e7-a401-4dd5-aba7-25df5b16acfe");
        for (i = 0; i < 8; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "69c91d7e-310d-4238-9ef9-b701e47ad628");
            if (value < ((1L << (7 * (i + 1))))) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "765b2341-c887-4d34-b379-0dab670d886c");
                firstByte |= (value >>> (8 * i));
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "07c31b7b-814f-4fa3-8b69-063c413c7938");
                break;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "867ed165-e5e8-4b12-96d9-b44663926c80");
            firstByte |= mask;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "69aa5f61-98eb-4100-bb82-bcad92c85ec1");
            mask >>>= 1;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "24f9cb88-83a8-4ccd-aaf9-069f413b458f");
        header.write(firstByte);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "20cee554-deb3-43bb-be3c-f3cc4153af38");
        for (; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "001a1d5f-53ca-4a52-877a-04c0119eb29c");
            header.write((int) (0xff & value));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b9df2c92-9aa2-42f0-baa1-86c37f3b422e");
            value >>>= 8;
        }
    }

    private void writeBits(final DataOutput header, final BitSet bits, final int length) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "1876f81b-5b1f-4da4-8c4e-3646f7c26262");
        int cache = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3340dd85-2637-44ed-895a-3df5b5f7795a");
        int shift = 7;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "f3cbe715-7381-4c7b-8dfb-970fef3f1872");
        for (int i = 0; i < length; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "b31ae7f8-8063-4200-9d3f-d559a8fc86b3");
            cache |= ((bits.get(i) ? 1 : 0) << shift);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a7a5ffe6-88c5-4cb4-942d-8ec0727442b9");
            if (--shift < 0) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "e184cc70-4333-48a6-addc-d3251667ca2a");
                header.write(cache);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "3a2df72e-9f6f-4b20-89dc-b61c7226b494");
                shift = 7;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "43e99679-916e-41ae-b5d5-038c600e8d8c");
                cache = 0;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "fac50f3a-41b8-4a05-8dcc-6ddbc47364ea");
        if (shift != 7) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "a73d0899-63d1-41d8-bf88-d285807eb0a0");
            header.write(cache);
        }
    }

    private static <T> Iterable<T> reverse(final Iterable<T> i) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "7c32f555-5443-4fc6-b66d-ebdad60fa6bd");
        final LinkedList<T> l = new LinkedList<T>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "159a42a2-8ea0-4d55-a5d0-ec8c29c4adea");
        for (final T t : i) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "80466bba-535e-49e3-8ecd-2f5379cc4cc0");
            l.addFirst(t);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_4_10.coverage", "58686f43-b65d-45a2-a966-ef797de1b0a2");
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
