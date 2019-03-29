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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "50ceac7e-1ace-4746-aa0a-0962545043ca");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d11f8eb6-c0c6-42fa-bdaa-11e0fe5f6341");
        this.contentMethods = reverse(methods);
    }

    /**
     * Closes the archive, calling {@link #finish} if necessary.
     *
     * @throws IOException on error
     */
    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b5be01c6-a7b6-413b-9214-874d3c370896");
        if (!finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c7e81150-653b-499e-a879-de7f945c1316");
            finish();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d27c880d-dcf6-4ba8-b4aa-a0bec5b2b478");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "219dcb2f-ed93-426d-8ed4-7a48fdc9abb8");
        final SevenZArchiveEntry entry = new SevenZArchiveEntry();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "682eac72-56c0-4629-b6f0-a6052717650b");
        entry.setDirectory(inputFile.isDirectory());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c2c1577d-1d90-43ec-abb0-48d80539c83e");
        entry.setName(entryName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cab38e2a-3030-4950-bd71-639daae08736");
        entry.setLastModifiedDate(new Date(inputFile.lastModified()));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cf8eefb9-ba2b-4cf4-ab13-fe0eabe5a5aa");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cec8b685-6dbe-4cfe-b90f-6b1ae9660dad");
        final SevenZArchiveEntry entry = (SevenZArchiveEntry) archiveEntry;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d0534508-a904-44b1-b0d3-63f09a2467d9");
        files.add(entry);
    }

    /**
     * Closes the archive entry.
     * @throws IOException on error
     */
    public void closeArchiveEntry() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3c5f7a64-db03-4a70-a888-3899045cf734");
        if (currentOutputStream != null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f556bea1-5a8a-45ef-b805-13fbd656e4e4");
            currentOutputStream.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b62ebda3-0134-47f3-99ca-0eadb9bbda1d");
            currentOutputStream.close();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5672356b-32b0-4338-b56c-619e19ea72f0");
        final SevenZArchiveEntry entry = files.get(files.size() - 1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ec06a8d2-a01f-4569-a5cf-a2b63a350615");
        if (fileBytesWritten > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "68f9cfcf-83bb-4d1a-9d26-9821f6e63fe1");
            entry.setHasStream(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "615d4e3f-b846-42bc-be36-eb1d775c257e");
            ++numNonEmptyStreams;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "472f1b20-60e5-4364-958a-8e79cd412bb8");
            entry.setSize(currentOutputStream.getBytesWritten());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "8683cc0f-21f0-417d-b36c-160f15fe53cc");
            entry.setCompressedSize(fileBytesWritten);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "8aa65151-c691-4192-b08e-581ee17027d3");
            entry.setCrcValue(crc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0dfc13c0-bce1-4a36-a577-ed37047d14e2");
            entry.setCompressedCrcValue(compressedCrc32.getValue());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d618f07f-5162-4e29-b1dd-c831105cffa7");
            entry.setHasCrc(true);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "753edbb1-ee07-4cf0-b318-6a3b9e7b2b7f");
            if (additionalCountingStreams != null) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "78097cda-32e9-47c2-8bde-b7b9201b1de2");
                final long[] sizes = new long[additionalCountingStreams.length];
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a2979ab7-ff6c-47dd-8c46-c657fe9d6490");
                for (int i = 0; i < additionalCountingStreams.length; i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1e22f0ae-0902-45c2-9e50-f85297a860c9");
                    sizes[i] = additionalCountingStreams[i].getBytesWritten();
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "92625057-d3a1-46f4-bdd4-832928340478");
                additionalSizes.put(entry, sizes);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "38ffe28f-fb77-4037-9ec6-c9c527193eaa");
            entry.setHasStream(false);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e1f731b4-c398-483a-a013-8899ff794bc1");
            entry.setSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "95295af7-d6c0-48a4-b841-ce82e29dfc55");
            entry.setCompressedSize(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ddc7d01b-10ff-4acb-bd67-acd83c38b8f4");
            entry.setHasCrc(false);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3f33f86b-1ea7-4e94-89da-290fdf478346");
        currentOutputStream = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5328fc56-e1a7-4db2-8a05-f2ac87f8e434");
        additionalCountingStreams = null;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "26d6d6f8-1e00-49b8-ad53-dd00b17e0981");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9253d0a8-9864-45a8-ae59-1334d7aa7395");
        compressedCrc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "609f31c3-f708-492d-ba40-8cc1bbf5a2a8");
        fileBytesWritten = 0;
    }

    /**
     * Writes a byte to the current archive entry.
     * @param b The byte to be written.
     * @throws IOException on error
     */
    public void write(final int b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2799a47c-e627-406b-a719-be29d810896e");
        getCurrentOutputStream().write(b);
    }

    /**
     * Writes a byte array to the current archive entry.
     * @param b The byte array to be written.
     * @throws IOException on error
     */
    public void write(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5f8b0388-7076-46bb-a47d-45715d3e1cb7");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9aa6524e-d31f-4164-bc45-b2f772c3897a");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9d11b547-2f84-481d-8358-1f8fd824cb10");
            getCurrentOutputStream().write(b, off, len);
        }
    }

    /**
     * Finishes the addition of entries to this archive, without closing it.
     *
     * @throws IOException if archive is already closed.
     */
    public void finish() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6e5d0324-4f1e-4e12-a9a1-b2c3fec66cc2");
        if (finished) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e339c6b2-e3c7-4cfc-8ece-2d5a541f19f4");
            throw new IOException("This archive has already been finished");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a94860df-670e-45ef-9936-ebaf731e12e7");
        finished = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "56b1b693-6c90-46f4-98dd-ebf0890c9852");
        final long headerPosition = channel.position();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b659ea6d-3845-4ceb-ab38-4af12f03e587");
        final ByteArrayOutputStream headerBaos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cde956b2-3df7-4595-a1c4-c520b8963338");
        final DataOutputStream header = new DataOutputStream(headerBaos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a587c284-89ff-4839-9188-77b5923b2f0d");
        writeHeader(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "48d5a911-71d5-4d77-86e5-4f8f83246045");
        header.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "837a381d-898c-4de0-b878-d4c14a20926f");
        final byte[] headerBytes = headerBaos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2a978219-8871-4759-a04f-a8a1e630cba2");
        channel.write(ByteBuffer.wrap(headerBytes));
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e9e39f3f-dfef-4335-a54f-4f8e2cc7a907");
        final CRC32 crc32 = new CRC32();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4314e523-7a99-4017-9a1f-b28b652709e8");
        crc32.update(headerBytes);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f4aeb210-78c5-449e-83ba-dbc08fc2330e");
        ByteBuffer bb = ByteBuffer.allocate(SevenZFile.sevenZSignature.length + 2 + 4 + 8 + 8 + 4).order(ByteOrder.LITTLE_ENDIAN);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3dd9122c-27a9-4a01-9dd8-088ff861b406");
        channel.position(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b2fc9cb1-093d-4b22-8e47-1c80083e2330");
        bb.put(SevenZFile.sevenZSignature);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "87635cf2-f970-4239-bfa2-14712227da30");
        bb.put((byte) 0).put((byte) 2);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "510ecf67-d2e4-4ca1-9a62-5c9052d7f3a6");
        bb.putInt(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "99efec2f-1dc3-4426-97ee-b33d50676f59");
        bb.putLong(headerPosition - SevenZFile.SIGNATURE_HEADER_SIZE).putLong(0xffffFFFFL & headerBytes.length).putInt((int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7fae3272-4b97-4fd0-b0e2-55a852d8da70");
        crc32.reset();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c5681617-c233-47ea-8801-0512be74c07a");
        crc32.update(bb.array(), SevenZFile.sevenZSignature.length + 6, 20);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6a449f9f-a9ee-4519-979a-3c64dd7fbc81");
        bb.putInt(SevenZFile.sevenZSignature.length + 2, (int) crc32.getValue());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4b62c315-f58d-4e2b-8e98-7bde14bf417f");
        bb.flip();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "8d67fafb-03d7-42ca-8804-6d2a81075ed6");
        channel.write(bb);
    }

    private OutputStream getCurrentOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "851e0f12-ad1b-4555-ab5a-8a42194e8d80");
        if (currentOutputStream == null) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b9e1d940-7406-4fbd-bf3e-241c4ee2f19d");
            currentOutputStream = setupFileOutputStream();
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "81e6cc17-5fa9-4dbf-865b-68a9fcd6ec7a");
        return currentOutputStream;
    }

    private CountingOutputStream setupFileOutputStream() throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b48bd1f7-89f8-48f0-b690-bbed7087f6ab");
        if (files.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1a401e08-a039-44c4-90c0-b98cac1a41c4");
            throw new IllegalStateException("No current 7z entry");
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3b0d9585-ffff-484b-b42c-106b7962af48");
        OutputStream out = new OutputStreamWrapper();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a454eacc-1c47-4649-b568-6b68433ec1bc");
        final ArrayList<CountingOutputStream> moreStreams = new ArrayList<CountingOutputStream>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "989900ab-ef63-4734-8c2c-306395c91fea");
        boolean first = true;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7073dd38-1774-4715-8e7f-f64c00bd153e");
        for (final SevenZMethodConfiguration m : getContentMethods(files.get(files.size() - 1))) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5700dec5-e874-4bba-81a6-144c48714a6a");
            if (!first) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9ebd376c-f20d-4bae-86ca-fbad4996b7eb");
                final CountingOutputStream cos = new CountingOutputStream(out);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a65069f5-f38d-4dc4-b839-a49a993ae538");
                moreStreams.add(cos);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "971675ab-4bd1-4a19-a75c-c93d5f8461a8");
                out = cos;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e0d8d051-1f1c-43ad-9537-ed6c23cd5e46");
            out = Coders.addEncoder(out, m.getMethod(), m.getOptions());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "78e9364c-789d-43cf-8968-798f534dc436");
            first = false;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b587d629-39aa-457d-aad3-d1c57f065904");
        if (!moreStreams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "75e34893-c3db-47b4-a859-8966b44e780f");
            additionalCountingStreams = moreStreams.toArray(new CountingOutputStream[moreStreams.size()]);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e2a77d59-9a2b-4e6e-9955-9d0d7f357b93");
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
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3e788f32-df3a-4c2d-9d48-8671a47a03cd");
        final Iterable<? extends SevenZMethodConfiguration> ms = entry.getContentMethods();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4211d0ed-fac6-40f9-b4d0-7c495d09993f");
        return ms == null ? contentMethods : ms;
    }

    private void writeHeader(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "dc0f585d-2427-4d87-90d8-98882a651a67");
        header.write(NID.kHeader);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "979dd18e-f7dc-4e2e-9bd6-5ab7bee38276");
        header.write(NID.kMainStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2b620932-2a06-4532-b798-88bc150ab108");
        writeStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f561c455-2901-40f5-ad9b-7cd8d28ae0bc");
        writeFilesInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4ef29198-7a50-4f16-9e65-eae9f21f3cd5");
        header.write(NID.kEnd);
    }

    private void writeStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3ce5c89f-0172-4fe5-a44f-1d5257d5a282");
        if (numNonEmptyStreams > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "965f5c0e-8711-4496-b9b3-43018d3655fa");
            writePackInfo(header);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7a76f155-48f0-4754-b0c9-70478ae806ed");
            writeUnpackInfo(header);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9ef0ac8a-b34f-40c3-bc27-cbc325e18590");
        writeSubStreamsInfo(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "dd229b27-855c-436e-acd4-0bb53428b974");
        header.write(NID.kEnd);
    }

    private void writePackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2ba141bb-bae6-4d8e-b1a2-d397f79d742d");
        header.write(NID.kPackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e80da1fe-5203-4bd6-a69c-ee16d63400f4");
        writeUint64(header, 0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a4a63672-6832-4319-8786-080932cb5caa");
        writeUint64(header, 0xffffFFFFL & numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "67d4b859-5650-4754-835f-04a4dc384ef3");
        header.write(NID.kSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7028e384-ee92-4ef1-b818-42949de70bec");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "76bfcb4e-3fed-4020-8353-b2590b3cb3a4");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "701ff499-94e4-4f3f-8058-060cab266c7a");
                writeUint64(header, entry.getCompressedSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1832faab-1dbf-4aaa-9899-d852567d376d");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "51ac9795-9131-498d-841b-ecc0c5ebe05f");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "51c4feb1-eebc-4bb9-ad57-cdbef08ebecb");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "468c7595-38af-4900-a1e0-c0a6334799ad");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ed344c12-348f-42f3-96ca-4bb69f9aafeb");
                header.writeInt(Integer.reverseBytes((int) entry.getCompressedCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "fc3cf418-a036-4eb1-ba83-ef0bdbe28d8f");
        header.write(NID.kEnd);
    }

    private void writeUnpackInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "be8e4fe4-23dd-426c-b829-8ac61e14947c");
        header.write(NID.kUnpackInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "bbe5e5bb-b277-403c-9c95-707d0f503f67");
        header.write(NID.kFolder);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "eed01ede-8317-48c9-a782-1773f5b4643e");
        writeUint64(header, numNonEmptyStreams);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7949f484-fc5a-49d2-b394-9c8bb8d1a74f");
        header.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3e963adb-67f1-4034-bc8a-6ec52a14a240");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a963c1ce-b4f0-4833-bd7a-2ff76a6de8eb");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c7112493-efa5-4273-8b48-6a39a6635560");
                writeFolder(header, entry);
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "af83e6ba-e23c-456b-ada8-df4c4362b365");
        header.write(NID.kCodersUnpackSize);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d1abf5f0-3fb4-407b-b4c6-213dcf070ea7");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d76fd2de-83c3-4d29-b560-3d2848bae2d4");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "556dcbe1-b34e-4973-ae59-8f3ceb93a27b");
                final long[] moreSizes = additionalSizes.get(entry);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "32153c3b-ed99-40f4-8270-176312483c3f");
                if (moreSizes != null) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c9dacfe2-81a5-4ad2-9c6d-9ea774c4e203");
                    for (final long s : moreSizes) {
                        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1e8dc763-931c-4e61-9ef2-80efef748f11");
                        writeUint64(header, s);
                    }
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "42ed3d6d-1c1c-4e01-adb5-96180afe6c47");
                writeUint64(header, entry.getSize());
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "8715096e-81c2-4634-9f28-e160007731f9");
        header.write(NID.kCRC);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4acc254b-b3d5-43a7-a741-841dec898d2f");
        header.write(1);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "dd038e6b-7b02-4b50-8f73-e6e9c28e3673");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3510f761-60c0-495d-be78-fbd0d326ac07");
            if (entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "88e607b3-a2e0-4487-8da4-d1c4f64b48e8");
                header.writeInt(Integer.reverseBytes((int) entry.getCrcValue()));
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "26bb477d-3024-464c-b3fe-1be326449322");
        header.write(NID.kEnd);
    }

    private void writeFolder(final DataOutput header, final SevenZArchiveEntry entry) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0f7f7608-e50e-4361-b7d7-3dad7f88dd07");
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9a7a9ced-3527-40cf-b921-6440c339c489");
        int numCoders = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0816e781-c674-4533-a376-87bf14fbd789");
        for (final SevenZMethodConfiguration m : getContentMethods(entry)) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c32ec51b-279b-423e-a238-19a3ff6997ef");
            numCoders++;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d242024c-02d5-4109-87b1-642a4d8e98cd");
            writeSingleCodec(m, bos);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "57fc9d23-8f09-4640-9aee-e28dec2c4d67");
        writeUint64(header, numCoders);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b9f6fdf8-aa6f-40cd-bd4b-a1d95fb62c07");
        header.write(bos.toByteArray());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "dbda12cb-e97f-42fa-bf14-fb67522ae59f");
        for (long i = 0; i < numCoders - 1; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "83140fe0-4999-4a71-bedd-9dadb132184b");
            writeUint64(header, i + 1);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "34b8c642-798d-4511-983e-f83953a360db");
            writeUint64(header, i);
        }
    }

    private void writeSingleCodec(final SevenZMethodConfiguration m, final OutputStream bos) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a847009f-fc29-4beb-bab6-52d6fdfd7fdf");
        final byte[] id = m.getMethod().getId();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a859fc2c-4504-4e31-9eb3-1903f763425d");
        final byte[] properties = Coders.findByMethod(m.getMethod()).getOptionsAsProperties(m.getOptions());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b1a5b82f-c53b-49d0-8579-f456b26a0458");
        int codecFlags = id.length;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "792591ad-8ab6-4f7f-a7b9-d53a6204dca8");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9efb031a-9aba-438c-8bd2-3752a52335b2");
            codecFlags |= 0x20;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "54d7917d-2e4d-4aa9-9390-31a2883c317e");
        bos.write(codecFlags);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e6e91caf-0b62-420c-95cf-b25049dc905d");
        bos.write(id);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "43062b13-09fe-4f7f-bc10-ed9a2b12dd5d");
        if (properties.length > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "73188f78-2789-4e6f-8ddb-0befd76f8999");
            bos.write(properties.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0473553f-03ba-42ff-831f-2cf696ad9fbb");
            bos.write(properties);
        }
    }

    private void writeSubStreamsInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "21fd8447-d1a2-4ba0-9f7b-a94635555bd7");
        header.write(NID.kSubStreamsInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "598003e3-db08-42ea-a61f-b24a7afdfbb5");
        header.write(NID.kEnd);
    }

    private void writeFilesInfo(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5dc9603e-8ac7-4099-a1e8-75386bfc09f4");
        header.write(NID.kFilesInfo);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "517164d0-b8d2-47bb-b6f9-0a52bb6f0609");
        writeUint64(header, files.size());
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0e775bd1-44d2-404a-94b6-d03a37a290f9");
        writeFileEmptyStreams(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "8e8f0047-021d-4c5f-97c1-137f00683265");
        writeFileEmptyFiles(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "504d295d-c0db-43ff-90b7-d128fc50c57a");
        writeFileAntiItems(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7c078c08-5511-4d11-be26-239deba77b5e");
        writeFileNames(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6172a8d7-b0dc-4e88-8661-17ea7ca68249");
        writeFileCTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ec4cf6b6-a43f-4a0a-b6a7-bdafe4cd6209");
        writeFileATimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0c14fab3-eecd-487c-8605-34f707b8c9bd");
        writeFileMTimes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b03cec02-362c-4dd8-b36f-eb024252b369");
        writeFileWindowsAttributes(header);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "57af8b3d-d82f-4520-a290-2db02c8ab75c");
        header.write(NID.kEnd);
    }

    private void writeFileEmptyStreams(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "269f2149-1634-4786-82cd-63e3784be258");
        boolean hasEmptyStreams = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "619fcde1-cc0a-4752-8b20-aa8241c60196");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "147cb6f4-2f5b-4688-9c70-5ed7a4f21443");
            if (!entry.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6221c0f1-6b87-439e-a8d9-c28960f0dc2f");
                hasEmptyStreams = true;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b0f24cef-2fd7-42a0-876b-774e06097b70");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9e04b21c-ae51-4f1a-b58a-bf7cc82aac92");
        if (hasEmptyStreams) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "fbe72d52-9c4d-448d-a75b-0fc07e70467b");
            header.write(NID.kEmptyStream);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "03b25e50-8a16-4145-beb6-b75e77c9d9a8");
            final BitSet emptyStreams = new BitSet(files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9185db7e-1d98-477a-aeb1-defd71bff25a");
            for (int i = 0; i < files.size(); i++) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c7c1259c-96bf-442c-9556-c3d3b3cbc66b");
                emptyStreams.set(i, !files.get(i).hasStream());
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3d04f9d8-3b36-4d89-bf62-a43929855d12");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "61a804f0-8ce2-41b7-882d-125725058071");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "bf8e12d6-a9fe-4225-8922-ba9ee21aa0eb");
            writeBits(out, emptyStreams, files.size());
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "537705f9-3eed-4e0a-a097-39cabc417684");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "976e344d-28eb-4fbb-af6b-b94de2b44d54");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a2a373fa-3d59-489e-a67b-ada27d3bec3c");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "47c0dec5-216d-4ef3-9d9b-fb8b278fe7b0");
            header.write(contents);
        }
    }

    private void writeFileEmptyFiles(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "abeccd78-26c0-4775-b926-2bd1ffb8e173");
        boolean hasEmptyFiles = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "131a62e5-a392-4ab2-be15-7b6bee86bf04");
        int emptyStreamCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ea3dd51c-87b2-4256-96c1-19393bf0a313");
        final BitSet emptyFiles = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c9a59348-e6c1-4c21-9903-3c5b78e8a9aa");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0bf0a1db-65a2-43fa-afda-c9cfecb43de8");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9c38dd5d-1db4-471e-b080-94558563bf68");
                final boolean isDir = file1.isDirectory();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "dee71ccf-4c8a-4f26-aac8-083fa6fe733a");
                emptyFiles.set(emptyStreamCounter++, !isDir);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f14574d8-990a-4498-abd9-587fb082cf74");
                hasEmptyFiles |= !isDir;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d38f7e52-8b5b-4be1-902c-dfc64f5c860a");
        if (hasEmptyFiles) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7fa7609e-6745-4175-b095-43bb5cc22f2d");
            header.write(NID.kEmptyFile);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3f8609da-5bb0-459a-9285-0bd723331836");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b76fee78-8fc0-45a1-9547-3768280761af");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a2d940c0-01ea-438d-97a0-32f46149fdcb");
            writeBits(out, emptyFiles, emptyStreamCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0aef497a-a845-4017-8821-905648b253ea");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "78094365-aa19-4a32-97b2-461ddd67157a");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3fa3efce-dd55-42f8-95d9-e8440fa672f8");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5f253e18-1b48-4764-abb0-e65f6bf9da07");
            header.write(contents);
        }
    }

    private void writeFileAntiItems(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "fd03e950-52b5-49ec-8606-92c233ade931");
        boolean hasAntiItems = false;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a8432303-2075-431e-8ee3-c2d5fa4d53a5");
        final BitSet antiItems = new BitSet(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e2ef86d3-574b-4522-a71e-8ecd32d16767");
        int antiItemCounter = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "83036c18-e09c-4588-b5cb-ac1e4d4bd423");
        for (final SevenZArchiveEntry file1 : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c4a7d59b-f888-4310-b98f-c07e9abf38ba");
            if (!file1.hasStream()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d6113fb6-a26c-4aa7-8b36-1a49593f51c0");
                final boolean isAnti = file1.isAntiItem();
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "aa67b411-ce1c-43d4-9963-a6f6fae62f9d");
                antiItems.set(antiItemCounter++, isAnti);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "51cbd30f-26e8-4f23-aab8-585dd2b1de39");
                hasAntiItems |= isAnti;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "99de2fa4-c041-458d-9998-b4c475dfb67e");
        if (hasAntiItems) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "39549911-1296-4aff-859e-b9294928cb30");
            header.write(NID.kAnti);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e64ce4bc-d284-426c-982f-503af6c255e6");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "da4798ba-3254-4140-9588-3bc47c2072b8");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4383eb32-aee0-40e3-afe3-e42f22fb58fd");
            writeBits(out, antiItems, antiItemCounter);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9dc123fc-0a5c-4522-9030-4f198d29b99d");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a5c87958-f315-4cc1-b0a6-d17e39623ed3");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "fda13962-2fad-40de-b189-251c8dcf0d20");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9cc4a664-e476-4e65-91f3-62716e4efd6f");
            header.write(contents);
        }
    }

    private void writeFileNames(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9ba5bc76-39f8-44c6-885c-9716081a404b");
        header.write(NID.kName);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5e01b1da-01cf-4ece-8e1e-e60f179c1f2e");
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d50c2dbc-b52f-422e-8c5b-686f7f792217");
        final DataOutputStream out = new DataOutputStream(baos);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f649e794-8c30-4615-bb59-2dc9e18832ae");
        out.write(0);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3373b168-b231-4734-afd2-68e9f70e28fe");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b4b882e4-8ab7-4884-aa90-f1146ccb75da");
            out.write(entry.getName().getBytes("UTF-16LE"));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "79fe80bf-d33f-4d08-bc2d-05efc065c243");
            out.writeShort(0);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e234db5b-649e-40e1-ac31-97405a99b06b");
        out.flush();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "227f9281-ab3f-4efc-91b0-4b269179cb26");
        final byte[] contents = baos.toByteArray();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "741747fa-f7dd-4e84-bf95-bf41a2855b56");
        writeUint64(header, contents.length);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "bea1a25e-2b73-4ffd-9119-adde4a21c909");
        header.write(contents);
    }

    private void writeFileCTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "eb7ccf99-f254-4b27-b309-a9bd96b16cc7");
        int numCreationDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c16bfe07-df94-4f35-976a-ffcc181f196f");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b20ecf06-4e12-4711-9d0d-7884e5debd3f");
            if (entry.getHasCreationDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "324207b2-4653-48cb-9642-36f274e6b239");
                ++numCreationDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0273f690-0f66-4c3b-88bf-b4fd335bd1b1");
        if (numCreationDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "55e11c98-3dba-417b-913f-435a60e87e4e");
            header.write(NID.kCTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6d9ca79c-d611-4f25-9e4c-455b93bc104f");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9681a859-bf71-4a2b-b9ed-753425a9c134");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e943ba2e-6f00-4ec0-8fe5-5a76efd08c3a");
            if (numCreationDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2641efc4-77ed-49db-9fdf-bb631e6ea880");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "118f5c95-d894-4e2e-8935-60c12b373b4e");
                final BitSet cTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "fab2ad70-09c7-4c31-9218-649522becbc4");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "aa7e7dfa-4017-4078-9684-853f4c4cc709");
                    cTimes.set(i, files.get(i).getHasCreationDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a2795756-2378-41f5-8b29-57754af3ac6a");
                writeBits(out, cTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "69e1b91c-7e7d-490d-8ac9-4664923e5ecc");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f6f48d00-4ac7-44d6-b1a4-875c7871d68f");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6b0b9897-a272-42bb-b814-f27f96756e85");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "edb046a4-04bc-4251-a701-74ec788cb68b");
                if (entry.getHasCreationDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b837aae2-a859-42c1-83c2-440307a5cc44");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getCreationDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "bca50457-6ca8-4d96-a0d1-e6c9f764aba5");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "df36e69d-170f-4cef-9f0e-4f6f970d7149");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "8072128d-3098-4a6c-91a2-841a89c81109");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "bed54a70-ef5d-492a-baa6-48fdd6fe4a9a");
            header.write(contents);
        }
    }

    private void writeFileATimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c74b3f77-d682-4241-ae36-0978f1ba57e0");
        int numAccessDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d328a646-f892-4302-99bc-2c5523d16347");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "11bf6054-051b-460f-a15d-fc7bdcbe9f2c");
            if (entry.getHasAccessDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "849375da-85cf-4540-9066-30c93d37f3a0");
                ++numAccessDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5eaf4cb6-3602-40cf-9b3d-379eacf522a2");
        if (numAccessDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b620d782-2a63-43a9-869d-ff1b11aa640e");
            header.write(NID.kATime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f541acf6-076b-400b-aefd-7afb623ef571");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "82b6dd7a-fd68-4d51-9269-09181d0b3b3e");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f4ed59f8-d460-4105-8734-69d3d4ea3f42");
            if (numAccessDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1489964e-425c-4ecc-9b55-d7584b340bf6");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "038049ce-25cc-4b18-b613-d1455520c4a1");
                final BitSet aTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5e5a4681-993b-4957-9c86-4536e33baafa");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b03a1977-081a-426d-aee9-aead8ebf7b5a");
                    aTimes.set(i, files.get(i).getHasAccessDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "6492ee32-42c3-48c7-a0e3-c4411dbe749b");
                writeBits(out, aTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f4f7680c-c319-4e8c-ad6c-aa799bd75175");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "099f2ce6-d32b-4450-834d-4687b2d4256a");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "3c211b88-fa7c-471b-b226-fd53abc25c25");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4df51919-91f4-4fd5-8dbf-2ca3bf012e85");
                if (entry.getHasAccessDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b7fdd7a8-110d-4392-bde2-b788380f14c2");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getAccessDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "78ec7856-914a-4453-bd98-6e4d45c68398");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "50eaa1e1-4f2b-4147-b664-3251b1268ac2");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "75aadf7c-882c-45e4-9cf9-c5a2e668439e");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ba3d2323-5528-42f3-838b-4285063cd80c");
            header.write(contents);
        }
    }

    private void writeFileMTimes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5b6fa086-511b-4aba-8c96-9ef55b5180c0");
        int numLastModifiedDates = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5c175d07-a4d1-4c9f-8f83-95cf4129fcd3");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "344bc4ca-dac4-4e48-8b91-8ce857c3e490");
            if (entry.getHasLastModifiedDate()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "77680ff4-5e1e-4151-b364-3d3f203c2790");
                ++numLastModifiedDates;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2582bf18-bd3c-4f10-b230-1eb23e97bf2b");
        if (numLastModifiedDates > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7fa2c0cb-149c-4563-903b-7ef091442e8a");
            header.write(NID.kMTime);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "559432b5-59a0-485b-9056-0500532eedb3");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "eba8e228-6f59-46cc-b1ae-117a2b67a727");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "10f74126-d45b-4772-9aed-9e8054c6df9c");
            if (numLastModifiedDates != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "32e5b47f-d5ba-4cbd-ab2b-51a8fc7cee16");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "fd1cec24-6f5a-4360-820c-c2a36ea34e29");
                final BitSet mTimes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cb913f3b-9f7c-455c-8b2e-e341cea36f22");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "50eb4ca8-2a31-4c80-9a65-43226216dac1");
                    mTimes.set(i, files.get(i).getHasLastModifiedDate());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "5a5ec330-3392-4c88-a893-946427ad0455");
                writeBits(out, mTimes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "58314c86-34fb-432e-9ae1-a2f83307c17d");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a53b34c4-7bd6-434e-a7ba-60fdef6d7d5e");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cc02fab9-3b3f-431e-92ea-41f147c3b57c");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d6b95748-97fe-44b7-a20e-ee8a9b9b047d");
                if (entry.getHasLastModifiedDate()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "cc4ef003-0511-4f86-adce-ad1d4843605d");
                    out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getLastModifiedDate())));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1a4859c2-c7a8-4b73-8f02-c498ab2ce114");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ecd63a5d-9889-41d2-ac3b-0c487b48fffa");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "a6d07952-3fcb-4b8d-827c-5ad3a259e412");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "68ca13a0-b78f-44da-b75e-8d9dd86249c5");
            header.write(contents);
        }
    }

    private void writeFileWindowsAttributes(final DataOutput header) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7d9f3e43-14a4-4ae1-804a-30a7a3b35d4e");
        int numWindowsAttributes = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ffeb1487-4a85-4620-aa1f-96dae0c511e2");
        for (final SevenZArchiveEntry entry : files) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "24a4b79d-6584-4d1d-be73-c1e97dda7cd9");
            if (entry.getHasWindowsAttributes()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "15d64376-9f0c-42c0-84c4-09f0c8169e2e");
                ++numWindowsAttributes;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "567df516-5bb1-4072-a343-121d6f417334");
        if (numWindowsAttributes > 0) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f6d0c067-62b0-49aa-9985-00a84d918f84");
            header.write(NID.kWinAttributes);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2517c1ac-ca8d-4cfd-b296-c47ab73d06fe");
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ccc3baea-f2e7-4d69-a268-a728bc33df5b");
            final DataOutputStream out = new DataOutputStream(baos);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "678bdeb3-5d6c-43b7-91ef-bf707bccadee");
            if (numWindowsAttributes != files.size()) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "258a93c8-049d-4c43-a94c-65baf46f3249");
                out.write(0);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "94549878-4977-4fd9-8d0c-46a948443a7c");
                final BitSet attributes = new BitSet(files.size());
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "59ff3688-6758-45ec-a428-dd0b3be9c2ab");
                for (int i = 0; i < files.size(); i++) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e55ba2d9-fc54-4563-badd-ae165b75abbb");
                    attributes.set(i, files.get(i).getHasWindowsAttributes());
                }
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "59c9d3d1-5cf5-430e-8752-510bf61415ce");
                writeBits(out, attributes, files.size());
            } else {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "01c4d501-31b5-41b3-b34e-9e9324c4fdd3");
                out.write(1);
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "15fd56eb-8a56-4cc8-ac97-c9baf7845d76");
            out.write(0);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "50cf7035-d776-4f11-bd1f-14d6954745f8");
            for (final SevenZArchiveEntry entry : files) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "250e053e-e31d-40ca-801b-92c9793f8cac");
                if (entry.getHasWindowsAttributes()) {
                    writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4ac0eeda-b64f-446b-9d33-924b9318ca5c");
                    out.writeInt(Integer.reverseBytes(entry.getWindowsAttributes()));
                }
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "65872d6e-4cd9-4799-8f42-0a59c29bb22c");
            out.flush();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9e442d35-fb8e-4ba9-acdd-58157b15c6bf");
            final byte[] contents = baos.toByteArray();
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e6458b0f-da9d-4052-b92f-6d036a366b69");
            writeUint64(header, contents.length);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d966eda8-744d-4b31-8662-4c779af088e8");
            header.write(contents);
        }
    }

    private void writeUint64(final DataOutput header, long value) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "896a24bc-e012-42ed-8818-9a502274ba32");
        int firstByte = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "28293538-df99-4127-a201-73c387bb6491");
        int mask = 0x80;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "971f2a02-301e-4479-b767-0f0b8d59749e");
        int i;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "f114d955-b80e-4474-a9f2-bb53633f8ff8");
        for (i = 0; i < 8; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "7bf708a5-1a7d-40f6-9c02-f82ece03fec4");
            if (value < ((1L << (7 * (i + 1))))) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b80967fe-52d7-45f0-a2a5-8b02ab456b0a");
                firstByte |= (value >>> (8 * i));
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e8be63ff-8d89-4bc6-820f-62b3a4221bfe");
                break;
            }
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "b738bb34-9993-42d0-93ba-ea5efd304772");
            firstByte |= mask;
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ae3c88db-0186-4d68-ae5e-249772511d5a");
            mask >>>= 1;
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "2d14b4d4-d41e-426a-b188-d4d0fc2c945b");
        header.write(firstByte);
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "ecb0f3a7-ecc8-4d09-8c78-2c9f8db8bb3b");
        for (; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "c7203450-b618-44cd-9bea-70227fca3cce");
            header.write((int) (0xff & value));
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "015c68f0-33b2-45a3-929e-875b02b141c2");
            value >>>= 8;
        }
    }

    private void writeBits(final DataOutput header, final BitSet bits, final int length) throws IOException {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "912091c9-3e8f-4d00-8328-88ad082d7172");
        int cache = 0;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1aae40fe-824d-4015-a77f-dfcc16351114");
        int shift = 7;
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "e79b9e4b-5bc0-43b8-8a1b-c084cd8dfcda");
        for (int i = 0; i < length; i++) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d454bba7-8067-4967-9e0e-4361180b3cbb");
            cache |= ((bits.get(i) ? 1 : 0) << shift);
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "0856282b-0a2c-4c50-a819-a385de8c34aa");
            if (--shift < 0) {
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "d4ed87fb-b8c3-47a9-8fa1-c256f0489424");
                header.write(cache);
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9e73f449-cb12-4554-91ec-bc1a2ed4b039");
                shift = 7;
                writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "64c6301e-6412-40ca-8aca-6cbaf470e044");
                cache = 0;
            }
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "4a34bf59-e55d-4e50-9c16-7d0bf023d5ec");
        if (shift != 7) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "9dcd34a0-346e-4353-a9c4-348245f5248a");
            header.write(cache);
        }
    }

    private static <T> Iterable<T> reverse(final Iterable<T> i) {
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "513ce26c-d28a-4152-83f2-815b9e8e4f43");
        final LinkedList<T> l = new LinkedList<T>();
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "1ef33149-d8d5-469f-a78c-e93a9c522634");
        for (final T t : i) {
            writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "462002c9-4c99-4022-bbed-e68db536c5af");
            l.addFirst(t);
        }
        writeline("/home/ubuntu/results/coverage/SevenZOutputFile/SevenZOutputFile_3_10.coverage", "40838a6b-6a5e-4b5e-8092-711a62c091ca");
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
