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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ad9631df-6d03-4791-8257-533cb99bc9e5");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "12726190-2bb6-4ef1-a7d8-9f7ef254cad3");
        this.contentMethods = reverse(methods);
    }

    /**
     * Closes the archive, calling {@link #finish} if necessary.
     *
     * @throws IOException on error
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3e74e6e5-58d3-4ce7-aebc-01354563063c");
        if (!finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "87f94737-bbd0-446c-b303-adc0939ad5fd");
            finish();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fdbb40c5-91b2-49a2-a9cb-e8b9970525e0");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "63bac7af-27a0-4041-8dd8-2ea9b9e6db70");
        final SevenZArchiveEntry entry = new SevenZArchiveEntry();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "42c2021d-fd85-4e1b-8468-7049371aebe8");
        entry.setDirectory(inputFile.isDirectory());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4258a6cb-e2c1-43c1-8131-0d4255f905ea");
        entry.setName(entryName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "dce85301-630f-4c9a-bc62-5da5aa322057");
        entry.setLastModifiedDate(new Date(inputFile.lastModified()));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ee99bc36-aac2-44da-8c56-cc00e45b1eb7");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ea5071ba-78a9-4184-8fcd-bb02188cc3dc");
        final SevenZArchiveEntry entry = (SevenZArchiveEntry) archiveEntry;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0838e3fe-d36e-4a96-86e9-8948187ef409");
        files.add(entry);
    }

    /**
     * Closes the archive entry.
     * @throws IOException on error
     */
    public void closeArchiveEntry() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c854c090-e54c-4fc6-a6ad-5d96568b4e5a");
        if (currentOutputStream != null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b7a6f411-6dd0-499c-959a-6177098fdc74");
            currentOutputStream.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "445576a1-9838-4ed7-8b60-46fea40fd030");
            currentOutputStream.close();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ceafadce-f9ce-4cfa-b78e-5ef7f17526d6");
        final SevenZArchiveEntry entry = files.get(files.size() - 1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9cbf02bf-04c7-4d7b-8c1e-11b9dac1c146");
        if (fileBytesWritten > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "17c174f3-7637-4b53-9742-80975aedd4cd");
            entry.setHasStream(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "580d9fca-86db-47e5-8500-c5be6e329c3e");
            ++numNonEmptyStreams;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6bbd9eb5-eb3e-444e-bfd7-76a9f0e561cd");
            entry.setSize(currentOutputStream.getBytesWritten());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "eae597c9-fb6a-4e74-83f2-9ca5c3d02af0");
            entry.setCompressedSize(fileBytesWritten);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "af411334-1ed5-4b04-a887-4aab0e14b8fc");
            entry.setCrcValue(crc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "eb15f0c2-c215-483c-a118-b51b99db4e12");
            entry.setCompressedCrcValue(compressedCrc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e8919b1c-b94a-4a79-a920-59806a373d2c");
            entry.setHasCrc(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ad3769fb-ba5e-407a-b95c-846cd1e3012e");
            if (additionalCountingStreams != null) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "97570ab4-f711-41c2-a8a6-5450fd069b22");
                final long[] sizes = new long[additionalCountingStreams.length];
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7dc2e407-aed6-47ce-96d9-4c2799d9cdd8");
                for (int i = 0; i < additionalCountingStreams.length; i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a35f96af-452e-4671-8bb9-e1ee4c2e8bb8");
                    sizes[i] = additionalCountingStreams[i].getBytesWritten();
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "5747b6da-af06-451d-a92f-af3862925d1c");
                additionalSizes.put(entry, sizes);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "86b5f5b6-b6a7-4f2a-ba03-33796064ebfe");
            entry.setHasStream(false);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "25e1993e-9f8f-49f2-858f-6ddfa3618ef7");
            entry.setSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "99311771-cca4-4615-8fc7-3af661ad6253");
            entry.setCompressedSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4fa3b683-fb52-43fb-b256-b76dc2e28a87");
            entry.setHasCrc(false);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2afc91d5-fd2d-404b-a1f4-183e05e163b7");
        currentOutputStream = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "77ce79c5-66b7-498d-b5e2-6af9c4fc29b8");
        additionalCountingStreams = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3360169c-58a3-4230-a764-7f473f72fc90");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8eceee42-eebf-46d5-8fd5-c739a0b0daf6");
        compressedCrc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "87843531-503e-4390-93c3-2637991ac3e9");
        fileBytesWritten = 0;
    }

    /**
     * Writes a byte to the current archive entry.
     * @param b The byte to be written.
     * @throws IOException on error
     */
    public void write(final int b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ddc7de17-4dee-40ee-b2e5-c00054ecf42e");
        getCurrentOutputStream().write(b);
    }

    /**
     * Writes a byte array to the current archive entry.
     * @param b The byte array to be written.
     * @throws IOException on error
     */
    public void write(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e801ef79-c637-4579-97fb-7e6ef9ad215b");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "01ff6e25-3d1d-487f-aee3-e3689d295b54");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "cbcdea3e-b9bc-4529-9b76-06ad94138383");
            getCurrentOutputStream().write(b, off, len);
        }
    }

    /**
     * Finishes the addition of entries to this archive, without closing it.
     *
     * @throws IOException if archive is already closed.
     */
    public void finish() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1ee1cd16-ae37-427c-bf9e-789f26e28013");
        if (finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "21d7e28c-7d80-4951-9f79-f7c092b21328");
            throw new IOException("This archive has already been finished");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "74a70c77-b4ca-44be-9f9d-982629949413");
        finished = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fb1bbc6d-d3d0-47a9-bce5-4ab6e8821e1f");
        final long headerPosition = channel.position();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a7e2f596-ab22-4bfc-af83-159be1f8c4f7");
        final ByteArrayOutputStream headerBaos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d5ebd817-0991-4ae6-9e4d-95a97d775c21");
        final DataOutputStream header = new DataOutputStream(headerBaos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b575b04a-f045-4c98-a568-47f4a9221b35");
        writeHeader(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6a717db7-6ab9-432c-bb3e-9ad356079139");
        header.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6044102d-c019-4f50-93e1-56119fe5ad8e");
        final byte[] headerBytes = headerBaos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a60f2725-726f-4a5c-a752-d48ea719cc00");
        channel.write(ByteBuffer.wrap(headerBytes));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "300d48f2-5c4d-4c8a-b952-978b69a8e6bc");
        final CRC32 crc32 = new CRC32();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0194b19e-6794-4415-bcb4-90c73bbf9d21");
        crc32.update(headerBytes);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6471424b-d22f-4fa1-ba10-6df51fce1e2f");
        ByteBuffer bb = ByteBuffer.allocate(SevenZFile.sevenZSignature.length + 2 + 4 + 8 + 8 + 4).order(ByteOrder.LITTLE_ENDIAN);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e6d5b9cd-f537-488a-b41a-4d00537327d9");
        channel.position(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2ec8df9a-8b8c-4c8b-ba56-c6b191abe725");
        bb.put(SevenZFile.sevenZSignature);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fdd08aa9-0958-478f-abbf-d21c161b9d57");
        bb.put((byte) 0).put((byte) 2);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1b2c8c67-275b-4a1b-b67a-3c2e1840d53a");
        bb.putInt(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "5b20e6da-2ca4-413b-be82-bfe3b8b1fdf9");
        bb.putLong(headerPosition - SevenZFile.SIGNATURE_HEADER_SIZE).putLong(0xffffFFFFL & headerBytes.length).putInt((int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3faf3d6d-9752-47b0-a424-6aa5f7ad9984");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e8331f53-b0a9-497a-b0b3-c48234d479eb");
        crc32.update(bb.array(), SevenZFile.sevenZSignature.length + 6, 20);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1c01d732-b8d0-448d-a12f-bf5311376c96");
        bb.putInt(SevenZFile.sevenZSignature.length + 2, (int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0fd9cb53-a7e2-4ce7-ae31-9e23892da02a");
        bb.flip();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "14310e1d-a254-4961-a563-838408810fc3");
        channel.write(bb);
    }

    private OutputStream getCurrentOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a78e9b99-854c-4a1c-a71f-c1f087902061");
        if (currentOutputStream == null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e5042699-7b71-48c0-b6e7-20fda9ae4574");
            currentOutputStream = setupFileOutputStream();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c13e276a-2df3-4449-9b8d-63daf2025234");
        return currentOutputStream;
    }

    private CountingOutputStream setupFileOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "11426bbd-63c8-4523-acbd-fee77532e515");
        if (files.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "36dcf707-53dc-4ed5-b35f-eb5d03ab1457");
            throw new IllegalStateException("No current 7z entry");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "39b22c5e-13c1-4773-8d23-0851a3b7c333");
        OutputStream out = new OutputStreamWrapper();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6982a1e1-05cf-4b45-9f37-c8c3ac89a388");
        final ArrayList<CountingOutputStream> moreStreams = new ArrayList<CountingOutputStream>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6dbba896-5c33-4f99-84de-ab46bfbd1033");
        boolean first = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d9f49f6a-2d2c-4356-a083-d51a04db11ab");
        for (final SevenZMethodConfiguration m : getContentMethods(files.get(files.size() - 1))) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "68720399-0372-477c-853e-c7f0c4bb799d");
            if (!first) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fb42dbed-05eb-4cf3-a98b-1ee0e3bb03d3");
                final CountingOutputStream cos = new CountingOutputStream(out);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "69b525d1-e5b4-4036-9e09-b1f55b9d35d9");
                moreStreams.add(cos);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "97362a09-7a7b-4293-a790-d6166bd0ed7f");
                out = cos;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "afd2e149-3828-4d1b-b4c8-c5013030685a");
            out = Coders.addEncoder(out, m.getMethod(), m.getOptions());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e319926e-f155-426f-9963-9557533ff8d5");
            first = false;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0119c35e-a936-496c-b9d6-430e767031c9");
        if (!moreStreams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "42b6fcc6-3249-482f-a85e-b349a9df80a6");
            additionalCountingStreams = moreStreams.toArray(new CountingOutputStream[moreStreams.size()]);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a4e5faa1-3f6d-4270-9785-4245c428a665");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "eaba7611-6c29-494b-ae6f-3eb0dca98e7e");
        final Iterable<? extends SevenZMethodConfiguration> ms = entry.getContentMethods();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "91086b5e-24ba-4bd6-aca1-baebb04de9a8");
        return ms == null ? contentMethods : ms;
    }

    private void writeHeader(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "07cc4f92-8ce7-4fa3-a2b9-ff69ea8434d1");
        header.write(NID.kHeader);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e372d75d-a60f-41aa-8f5b-f695d6ee94e7");
        header.write(NID.kMainStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c11fe8f1-a5ba-4389-9f45-760ea115d6d5");
        writeStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "02f7eee3-cdc4-4fc7-9060-c6f1aec5689d");
        writeFilesInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8f3e1afd-c491-47b6-a915-cc117db99cad");
        header.write(NID.kEnd);
    }

    private void writeStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3e629d38-9469-410a-92d0-73ae640df36f");
        if (numNonEmptyStreams > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c04fb814-6fd4-4c61-ac6f-723309311a39");
            writePackInfo(header);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "13ca6dcb-9599-4f0b-b7b7-d5907d614cf9");
            writeUnpackInfo(header);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e1e468cf-f059-45c4-aa31-2f5eb4f1369a");
        writeSubStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "71b07004-750e-4a5c-b55e-0edf5fda9184");
        header.write(NID.kEnd);
    }

    private void writePackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "841aa3cf-646a-4ec3-b79f-e9885249bae1");
        header.write(NID.kPackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3023cae9-bf40-4a17-8415-0ee553f0b0f2");
        writeUint64(header, 0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "cfba8161-b5b2-4fab-ad92-a29dcfece343");
        writeUint64(header, 0xffffFFFFL & numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "cce05a03-53eb-417a-8ab7-e893801883ac");
        header.write(NID.kSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "16ca7577-c447-4b14-b833-a720dae8970a");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6c981741-adbc-4a52-8d0b-b1991b2cf023");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "da174bba-ca26-4fc8-a7bb-2d9fa88855a1");
                writeUint64(header, entry.getCompressedSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e78927ae-78b2-44ce-a339-98ad133565bb");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "41c0d4c1-95a3-4ceb-9b5a-1d6e4da12f7a");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "213a7de2-d690-4338-9785-698e84ed25b2");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ecfe649b-7b01-4894-826d-5b9d84080fad");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ada41663-2571-4824-b917-d7cccfffa44f");
                header.writeInt(Integer.reverseBytes((int) entry.getCompressedCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8499e16f-ff58-475b-9dad-9e127dbc3857");
        header.write(NID.kEnd);
    }

    private void writeUnpackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4fa3d434-7dc2-489c-a517-330c9e23dc9f");
        header.write(NID.kUnpackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0fe503cc-5d38-4a97-9a3c-51239f8ecacd");
        header.write(NID.kFolder);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "71bd7dbd-3c11-4b05-ae39-75ce4e71f412");
        writeUint64(header, numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4da9d93f-6609-49cf-a453-fd23f18a975c");
        header.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9da7d26c-9732-48f5-b2e3-2f2accdbacb7");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "592ea0bc-7779-4b69-8a82-5f18c0bacb21");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "5d2129f3-90d9-421d-a321-18eb3aeb5db6");
                writeFolder(header, entry);
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "084ec9b0-7f4e-4e0c-bec4-e6c62224d76e");
        header.write(NID.kCodersUnpackSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "bd84e8c6-ef97-40a0-ba15-bd184f5733dc");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "851b8b4c-7075-457e-9b07-48636c1e13b8");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "70cdb580-0533-4faf-adb2-1c88fb626afc");
                final long[] moreSizes = additionalSizes.get(entry);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "99e1e1c4-9832-4703-b869-f2bb602f1586");
                if (moreSizes != null) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "27b10f2b-a2f8-4994-861f-d390be417808");
                    for (final long s : moreSizes) {
                        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "67cd517c-94c6-46dc-95cd-fff5ac1dd81c");
                        writeUint64(header, s);
                    }
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "43c0d9ce-72f2-4096-8738-2b5b626eaa59");
                writeUint64(header, entry.getSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "83c3ef37-10c4-45b9-b490-c6968ad0cbc1");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0bfde75e-2d22-404b-987b-b4c498e74dc2");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "416b1c9d-4785-4f83-9986-db331611c8a8");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "066c6559-7a9e-46c6-80cc-b715ab85a327");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fa4001eb-af67-499b-8e08-c119817f4f3a");
                header.writeInt(Integer.reverseBytes((int) entry.getCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d5b57cf7-3878-4dd8-b0f6-e4a905a6e171");
        header.write(NID.kEnd);
    }

    private void writeFolder(final DataOutput header, final SevenZArchiveEntry entry) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ad594bcf-d139-4084-9e8e-8d550d48340f");
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "472505b3-b6e1-46fe-9acd-7b57c7134871");
        int numCoders = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "5aecf85f-f74c-4111-bd97-4f740394883f");
        for (final SevenZMethodConfiguration m : getContentMethods(entry)) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a8bc7b5a-582e-4707-9ef8-5e3e2b9ee17f");
            numCoders++;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "71b92e8a-de45-4d9f-8ebc-b12a4be123c4");
            writeSingleCodec(m, bos);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9f2193b3-d37b-455b-ae6e-9418d5071ec2");
        writeUint64(header, numCoders);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d865794b-0273-4ba7-b1f7-56aee7af0fe0");
        header.write(bos.toByteArray());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a5ce914c-776c-4d97-8753-b84cba701b32");
        for (long i = 0; i < numCoders - 1; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "bbb2521d-11a1-4666-b4cd-35a2a58ef78a");
            writeUint64(header, i + 1);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "91c9b4b9-b4cd-4edf-9b94-aaa89b7e55b1");
            writeUint64(header, i);
        }
    }

    private void writeSingleCodec(final SevenZMethodConfiguration m, final OutputStream bos) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "04d22d5d-29b2-4118-b32e-0fbfd20429dd");
        final byte[] id = m.getMethod().getId();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c34e73f7-379d-4250-be43-5e56d7d6f878");
        final byte[] properties = Coders.findByMethod(m.getMethod()).getOptionsAsProperties(m.getOptions());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3d35aa3d-5183-4b71-8cb4-d1b036aa6e60");
        int codecFlags = id.length;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2f46c4e8-fb1a-4083-ad05-145c1fab06ff");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2491264a-ad6f-4400-af93-4f7f01dc98b7");
            codecFlags |= 0x20;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "93b3f9d1-0e37-4eaf-9cae-15c4e1ce6dc0");
        bos.write(codecFlags);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "cf72ccda-0c32-476f-989a-e861469c0cdc");
        bos.write(id);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "03cec547-7639-4e69-9f2d-d54274560ebe");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9006a8bd-70a4-4cc3-9026-6e03905b0e91");
            bos.write(properties.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7f6ef8d2-4982-42e5-8ef9-52661a815574");
            bos.write(properties);
        }
    }

    private void writeSubStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1fc53308-968a-4d63-a560-02f6185006b0");
        header.write(NID.kSubStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1f0280c5-a271-4ea9-bcf8-0851bf391319");
        header.write(NID.kEnd);
    }

    private void writeFilesInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "cd31b7e3-f86a-480a-8ac0-d0ad92ba441c");
        header.write(NID.kFilesInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9b883e9b-433f-4db5-915c-1e0cfa3850e6");
        writeUint64(header, files.size());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8157187c-438c-4571-a1d0-f2affe403db0");
        writeFileEmptyStreams(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ad1bd1fb-c26b-4b93-a6e8-4e363a4dd1d0");
        writeFileEmptyFiles(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0cd72094-544d-4f86-a38c-c10dd51d3141");
        writeFileAntiItems(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a1a3db95-b244-47a3-bb6c-e27dab320394");
        writeFileNames(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8a26bde8-b19f-4871-84ce-8fb4e7152f4b");
        writeFileCTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "246bceff-631f-421f-86aa-78701f7a71e1");
        writeFileATimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ec7d6020-f885-4c85-94e4-f8f41603aadc");
        writeFileMTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "44873068-4641-483a-9527-3e882be3202d");
        writeFileWindowsAttributes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2665c579-5707-4573-9c5b-ff09e460b211");
        header.write(NID.kEnd);
    }

    private void writeFileEmptyStreams(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "dfb4d8c9-a3e8-449c-8754-21de9cbe691e");
        boolean hasEmptyStreams = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a2289216-e911-45c6-b1a6-65d49c2dde1c");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e28ced26-1b4d-41fd-91b2-1e8fee8cca17");
            if (!entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d7a882cb-7492-422b-9270-20440e50fdfe");
                hasEmptyStreams = true;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4d4cdcec-2b59-49c8-9a66-e23dfaadda3d");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "93a28778-cae5-4aff-8d91-41f75e33c478");
        if (hasEmptyStreams) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b7b7d6e2-08bf-46ad-bed1-e8ca9a6eaabf");
            header.write(NID.kEmptyStream);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ae542f70-1a92-46f5-a233-14866df5dbc3");
            final BitSet emptyStreams = new BitSet(files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9e3a3245-7fcd-4d29-aa0e-1a009e5d41ad");
            for (int i = 0; i < files.size(); i++) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a323ead0-97c6-41f2-97d4-d28c46451c83");
                emptyStreams.set(i, !files.get(i).hasStream());
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "41b9e67f-d056-49b6-be20-456efddc7baf");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9f39292a-a044-4e37-8509-710efe456372");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d4c8d06d-3e9f-436c-971c-d3482b268153");
            writeBits(out, emptyStreams, files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e63c6b43-564d-4fda-96f9-133cdc21475a");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "f2f20a5d-6bb7-42f8-96c7-571f5a8cc1e8");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9c55bec0-b78d-410f-a5b4-3e3474178054");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "036d52f9-9cde-4be6-b003-3a8f4fd97ba0");
            header.write(contents);
        }
    }

    private void writeFileEmptyFiles(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "bb286ceb-d247-4f5d-94b3-fa157320097b");
        boolean hasEmptyFiles = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4e7dd7a7-efb9-405a-ad5a-e890fdc75e9d");
        int emptyStreamCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "85071cde-8db8-44ae-b504-0b473cb42e2c");
        final BitSet emptyFiles = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "393238a3-9a9c-40e3-897e-02401927d629");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e5fd50c0-332b-4994-a18b-73d702a5faaf");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "33b5b240-2efd-4677-a8f8-132dde920dea");
                final boolean isDir = file1.isDirectory();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a8634ea1-22e6-49d6-8872-f085ebd85821");
                emptyFiles.set(emptyStreamCounter++, !isDir);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9310ff90-13df-48e5-a863-76454757a0d7");
                hasEmptyFiles |= !isDir;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "717d75a2-9cf6-44ab-ba22-1eeeb12761bd");
        if (hasEmptyFiles) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3d89e06b-7d1d-4078-a849-4825c74c5bc3");
            header.write(NID.kEmptyFile);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "382ff84e-03b6-401f-9aac-457cb5a93cef");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7766fa9c-c8e3-4ad9-8e94-b0cea634bfcb");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b550a9a4-a967-4472-9fb3-d79500596939");
            writeBits(out, emptyFiles, emptyStreamCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "09d8a41d-e099-43a9-b261-db92d0ca108e");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "285db106-8899-4025-9e73-3d1661e093f1");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d0032fd4-7189-4200-8c9c-ef37bde53519");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "789b3d4e-5efc-44e0-ba6a-0c0788a58da9");
            header.write(contents);
        }
    }

    private void writeFileAntiItems(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "dd47c126-9ec2-455e-9c63-288c7e416504");
        boolean hasAntiItems = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "72a67164-32e9-4ac4-8637-c71f6960f6b2");
        final BitSet antiItems = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "74e984a9-613f-462d-9e0e-ab41fa771d21");
        int antiItemCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "08964f84-2da3-4b83-a71e-3089eaaef440");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "99ac8a68-686e-4083-9653-8016930548bc");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "363b47d7-8d2a-416d-b76d-bf7430bdf2fa");
                final boolean isAnti = file1.isAntiItem();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7f8d889a-143e-45de-ab4a-741215662023");
                antiItems.set(antiItemCounter++, isAnti);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fa63e889-60d4-4715-830e-1c88b845750e");
                hasAntiItems |= isAnti;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "5c71dd99-4d80-45cd-b4f9-26e6df2851a4");
        if (hasAntiItems) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "36245701-07e5-4893-b7db-efdf86580178");
            header.write(NID.kAnti);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ecf5d59f-f962-4023-ac0b-112928a9ca86");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d4ecc1e6-19a2-4212-9238-6456060896a3");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "240b6c4e-eeb3-4c14-801c-225dff5cf85a");
            writeBits(out, antiItems, antiItemCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "eaf95bb1-a360-46eb-826b-185662103875");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "79ef4225-401c-44d1-b9a7-b7f650601003");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e3d10c2c-ef1a-46b6-96ea-f615cbdcf0b2");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "dfa0f8fa-0a36-493d-82c5-70c48abbc2b1");
            header.write(contents);
        }
    }

    private void writeFileNames(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "30d54792-cff9-455a-97b7-844bdf10429f");
        header.write(NID.kName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b57eefd7-2446-4197-999f-69698daf651b");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "483cc4ae-8c9c-4d1e-9f24-81b0412bfc0e");
        final DataOutputStream out = new DataOutputStream(baos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "13b47656-7715-47d6-8273-aa57d4cf8fca");
        out.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "70843794-4730-43a6-bbd5-feda7a336a67");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d314d17f-7617-4ddb-9680-288d91d5a668");
            out.write(entry.getName().getBytes("UTF-16LE"));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3e53e64b-e286-454b-98f0-68793495e6b0");
            out.writeShort(0);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "62da4beb-1e15-40a9-8add-83ff9cfad8a7");
        out.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9b4cff25-aa58-4afd-a0f4-fe85ff70add0");
        final byte[] contents = baos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "442d8940-d3e4-4683-a594-74b9e61868ce");
        writeUint64(header, contents.length);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3f385115-9a83-4bbb-a748-ca332c096f45");
        header.write(contents);
    }

    private void writeFileCTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "27dbb123-2015-4aa3-a0fd-0d911adffa51");
        int numCreationDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "70a19e19-799b-4a91-b6ec-f1915c63d0ef");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "076cfc5e-b267-4b1d-b20c-aa0d8fb56a06");
            if (entry.getHasCreationDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a939b60d-2d88-4217-b7eb-8808f25e5567");
                ++numCreationDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1acb5f97-21da-4650-9f55-f9d2947b7fd8");
        if (numCreationDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0f603e0c-28af-492c-b4ae-af1efdf92ef2");
            header.write(NID.kCTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d1b2a56f-04a1-4df1-a316-dd313581f76e");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d0ecaf4e-8508-4775-bf15-9082b4bf5482");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "78bd8b9a-4319-4528-b9cf-32c635973dc1");
            if (numCreationDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fd140262-e5b7-4e25-bf13-90a7f29ab00a");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "672dde48-4ea2-4feb-9540-fdf15478bd91");
                final BitSet cTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "73ac8fc6-1156-4fcc-ab87-033a80c4c046");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "f1b59886-c3b9-4e64-93a2-08c612320c1c");
                    cTimes.set(i, files.get(i).getHasCreationDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7eb498f9-b751-4a82-afc1-24daff18c268");
                writeBits(out, cTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "cc85e2d7-544d-49be-abbb-f489061e42b4");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2df9c15f-bb74-496a-8bb5-d4365e90bf8e");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c91af939-2400-491f-be7a-66cb1c7788c0");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "82432da8-48ed-49af-8402-c9b798cb6322");
                if (entry.getHasCreationDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "84c07503-45ea-4ed0-8acf-dbad99cf2178");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getCreationDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a33a994f-fb34-464b-96ae-37abb0ed5ebb");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6af6b7ae-98e5-4d38-8c7a-fae021dc1895");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "92f8fd2d-a3cf-41e7-991a-7eab5fc863f6");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "252d302f-6478-4d48-a528-593abcc413ff");
            header.write(contents);
        }
    }

    private void writeFileATimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ac2d8875-3533-4393-82ba-fe76568f59ca");
        int numAccessDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "97bf30b7-b015-4727-a2c7-614a5848ecb3");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "29d99e96-55e6-4dc6-93d3-0f10e09c8ec1");
            if (entry.getHasAccessDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "f116c309-9981-46ab-8342-d62ba999f87e");
                ++numAccessDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "dad7dd9f-750f-4662-ba96-d8602ecb2cd5");
        if (numAccessDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "707e8d0d-f395-4341-b875-94a2de05d802");
            header.write(NID.kATime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "22297c37-1565-4a98-9255-a21f1d89c247");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6542558b-2d9f-4e5d-bb6e-3b805103ce4a");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ff5d9de3-f4e8-4689-9bec-a1e13e605dfd");
            if (numAccessDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "bca0f7f9-5b56-4cfc-893c-da8fd73ba49a");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e3493ace-b0ca-418a-bd64-8ef55756bfc5");
                final BitSet aTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1e6c121f-c551-4e32-ae23-aefc4226aa18");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8c152c24-d145-450f-8f18-e901f09ea6b4");
                    aTimes.set(i, files.get(i).getHasAccessDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0f988b37-333f-4fa7-b22e-c78a8274e9b0");
                writeBits(out, aTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "23ee3b2f-af96-4b59-a1cc-190300e070c6");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3c6d646a-b396-40ec-af9e-b61e1ed8d577");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "bc57fbfe-9e22-4f5e-b7e7-30c3cb12036a");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9a8dcddc-4cf6-497f-9b61-a26ae4e90b20");
                if (entry.getHasAccessDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "06ebd64e-12fd-4f4f-af7a-a4d7db9b2677");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getAccessDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "fcda8247-70b5-4010-919f-8078e25df64f");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "71e2a211-010c-4b25-abc6-8f7e32a30568");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "e6afdaa6-65f1-4947-a8d2-b6ae0b691b45");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "413aff16-3601-45ec-ac07-cd6f86e659cc");
            header.write(contents);
        }
    }

    private void writeFileMTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "eb7e2bc5-bcad-4259-ac5a-392e5f5f9036");
        int numLastModifiedDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "025b41a3-d447-4711-b8d5-c41cc04bb3de");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "235eb814-bb39-4c96-b8a7-26c2c8408b7c");
            if (entry.getHasLastModifiedDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "89af470c-eeb5-453a-a8e4-237d151e90c3");
                ++numLastModifiedDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "17c3c6f1-c6e8-4296-b3d4-2b10b1d0e314");
        if (numLastModifiedDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "df69d02b-7302-4ac7-b596-4803a2315920");
            header.write(NID.kMTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8277ed20-b63a-455b-8f2b-c202cec558f2");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2f29f224-1586-4d02-b049-63dbcb4a8624");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3fe42db0-ef90-4841-9133-ae07b84d856a");
            if (numLastModifiedDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d9269770-f6d0-4318-a689-be753949abd1");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "12ed87a7-f6e5-460d-98fd-39198a0cabb0");
                final BitSet mTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8a9ddd87-b6b6-4e7d-86b2-cef6ca472ce5");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "759f88c6-88a5-49f4-875f-8d5487c7664d");
                    mTimes.set(i, files.get(i).getHasLastModifiedDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b165d082-d1e5-4b9b-a33c-ba6a9b571752");
                writeBits(out, mTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "87a76f7d-00b4-42ad-b5e0-dc5965aed6cd");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7b7d7917-8cb3-4425-bb2a-bfb64191ce59");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "58128b7e-068c-4b92-b6b5-59b8a2b9a6c4");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3dc9f5a7-41e0-464b-ba6d-042723de4843");
                if (entry.getHasLastModifiedDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3a74067f-741b-47f5-bf69-e677cf5c226f");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getLastModifiedDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d6d32537-891a-4456-b78b-7717d5724404");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "237c3962-ab04-466c-ba4d-51f7bc46c433");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "f494fb40-ad0f-4240-9f5d-e96a9a3e6c17");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a72b1a43-bb5f-42a1-94d7-c18a14e843b2");
            header.write(contents);
        }
    }

    private void writeFileWindowsAttributes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "11b329a4-7782-423a-91b1-ea25d999cd58");
        int numWindowsAttributes = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "6d22aa81-e917-4463-83b3-510874e8bf70");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "34481508-4dc3-4c52-9aa7-42f79c3e37ee");
            if (entry.getHasWindowsAttributes()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "9020dbb3-b383-40a8-a070-e1cc690020b3");
                ++numWindowsAttributes;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "774fb5ca-63b7-43b3-af11-4ea506958877");
        if (numWindowsAttributes > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7ff2874d-bb67-4230-8282-dc41d6ce5c68");
            header.write(NID.kWinAttributes);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d184e03a-15b1-4d83-856e-ebc228e5fb87");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "22cc3d87-a5d2-4368-8bf6-8e4e77bb3fe4");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d40ea83f-47ca-47bc-aa2b-060f07d14456");
            if (numWindowsAttributes != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a3c5416f-e1c6-42b9-9830-2ab3aa41f3a2");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "7a7bfc6d-ba79-41e3-8fd9-b51e77eef37e");
                final BitSet attributes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "2238975c-3c7d-4ca5-9631-91f890b1fe35");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "683dd37d-6137-41ca-a72e-4406850f17e4");
                    attributes.set(i, files.get(i).getHasWindowsAttributes());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "662ba381-5457-407a-97d3-108d8073fd0d");
                writeBits(out, attributes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "663bf5b8-ef7e-4ad3-9c36-32adedcbae1c");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0228075b-5906-45bc-b919-3c958e75b47a");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8512ec1a-ea7f-47de-9fbc-0a7f68d47127");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ba31ad4d-e6ca-483f-b67e-8c601aecab24");
                if (entry.getHasWindowsAttributes()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "4b4bbce2-ed0c-417a-89ae-f958b5262b77");
                    out.writeInt(Integer.reverseBytes(entry.getWindowsAttributes()));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "07151b5a-bb46-4ce2-aaee-22ddb3013bdf");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "57b3ac46-2e55-4e15-8746-95abec4959f6");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8891d077-c065-4328-b97f-e695ebcdf387");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "506c15b5-bdc4-439a-be17-9d2610528cd0");
            header.write(contents);
        }
    }

    private void writeUint64(final DataOutput header, long value) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "19131a95-9368-4f06-9ddd-11d6a0194225");
        int firstByte = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "a4f0f082-6773-475b-ac5c-7ed78ef26480");
        int mask = 0x80;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "182fd275-f003-468f-ada3-08f7df6e28f2");
        int i;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "bc3863a9-b466-43c3-b34d-af6d433cec2c");
        for (i = 0; i < 8; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "3cf3a360-6429-4760-95cd-b85c036a97a0");
            if (value < ((1L << (7 * (i + 1))))) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b701f8da-2431-4a35-aaf3-15e858e20acf");
                firstByte |= (value >>> (8 * i));
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "75e1e666-d151-4a12-84b3-612a660312c9");
                break;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "8d9f223d-5637-4b27-bd1b-c383885fad04");
            firstByte |= mask;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "680755e3-b4ed-4d81-89d6-24b97d032025");
            mask >>>= 1;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "94841a08-6a73-4bf7-9399-976ee788f73f");
        header.write(firstByte);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0a45200d-1415-4117-b999-4d7d5d82484e");
        for (; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c8c20ede-05c1-4696-9925-9aea2f2786e6");
            header.write((int) (0xff & value));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "acdab437-6a2d-4920-be61-cb437733ab14");
            value >>>= 8;
        }
    }

    private void writeBits(final DataOutput header, final BitSet bits, final int length) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d6ac28ed-7f87-4216-a076-3ab00f5921f8");
        int cache = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0621f0c0-3d79-4293-888e-4defbfa2be57");
        int shift = 7;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "dec5b77d-e8af-4d34-8359-31cc1d00ba74");
        for (int i = 0; i < length; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "d2aab93d-234b-47b3-b7c4-43237df52281");
            cache |= ((bits.get(i) ? 1 : 0) << shift);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "baa8c576-2d39-4826-82c4-406f80dd910e");
            if (--shift < 0) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "caa0f749-7da3-4e35-9869-e0703db9e3c3");
                header.write(cache);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "16759e21-815d-4569-8c49-f491747d95a9");
                shift = 7;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "0c341c3d-ed0f-4c88-b9ef-44aa2dc6a45f");
                cache = 0;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "54c59534-6f46-4075-b0aa-6dafb37b5243");
        if (shift != 7) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "c8774157-70e4-4641-a7df-d807bd093c16");
            header.write(cache);
        }
    }

    private static <T> Iterable<T> reverse(final Iterable<T> i) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "af2bf5eb-2723-46b3-bf58-eb5104b4c6f3");
        final LinkedList<T> l = new LinkedList<T>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "1420d398-8e90-4f9a-9782-69fac24bb74b");
        for (final T t : i) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "ddee20f5-8ca1-4773-8359-fb684386e425");
            l.addFirst(t);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_2_10.coverage", "b0fbd7bd-2c61-46a4-b115-d31a675e8089");
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
