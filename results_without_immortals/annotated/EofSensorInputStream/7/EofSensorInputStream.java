/*
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */
package org.apache.hc.core5.http.io;

import java.io.IOException;
import java.io.InputStream;
import org.apache.hc.core5.util.Args;
import java.io.*;

// override markSupported(), mark(), and reset() to disable them
public class EofSensorInputStream extends InputStream {

    /**
     * The wrapped input stream, while accessible.
     * The value changes to {@code null} when the wrapped stream
     * becomes inaccessible.
     */
    private InputStream wrappedStream;

    /**
     * Indicates whether this stream itself is closed.
     * If it isn't, but {@link #wrappedStream wrappedStream}
     * is {@code null}, we're running in EOF mode.
     * All read operations will indicate EOF without accessing
     * the underlying stream. After closing this stream, read
     * operations will trigger an {@link IOException IOException}.
     *
     * @see #isReadAllowed isReadAllowed
     */
    private boolean selfClosed;

    /**
     * The watcher to be notified, if any.
     */
    private final EofSensorWatcher eofWatcher;

    /**
     * Creates a new EOF sensor.
     * If no watcher is passed, the underlying stream will simply be
     * closed when EOF is detected or {@link #close close} is called.
     * Otherwise, the watcher decides whether the underlying stream
     * should be closed before detaching from it.
     *
     * @param in        the wrapped stream
     * @param watcher   the watcher for events, or {@code null} for
     * auto-close behavior without notification
     */
    public EofSensorInputStream(final InputStream in, final EofSensorWatcher watcher) {
        Args.notNull(in, "Wrapped stream");
        wrappedStream = in;
        selfClosed = false;
        eofWatcher = watcher;
    }

    boolean isSelfClosed() {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "7d294641-622f-46bc-88d2-c82b3de5f6ee");
        return selfClosed;
    }

    InputStream getWrappedStream() {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "0ba30d25-3863-45aa-ae8f-80c348153578");
        return wrappedStream;
    }

    /**
     * Checks whether the underlying stream can be read from.
     *
     * @return  {@code true} if the underlying stream is accessible,
     * {@code false} if this stream is in EOF mode and
     * detached from the underlying stream
     *
     * @throws IOException      if this stream is already closed
     */
    private boolean isReadAllowed() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "f07c8b06-0370-4562-8899-5f30fbaa5dbd");
        if (selfClosed) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "1edb24f7-7768-4b0c-972d-48da057b10f2");
            throw new IOException("Attempted read on closed stream.");
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "8a6f4c72-abe7-4d1d-aecb-51f977ac1ab9");
        return (wrappedStream != null);
    }

    @Override
    public int read() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "cd19d98f-8cf0-49c9-b4ab-4d76732f6a70");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "dafc8220-b22c-416d-a0b5-fdf9c378dc69");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "e2ca8ae6-8889-42de-988b-a7f4e402418d");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "b04c8679-0c1c-4cc8-a536-45a961004f0c");
                l = wrappedStream.read();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "2ca864a4-d202-4fcc-a3ec-c120626e2848");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "4a98fc72-c02b-46f5-93c9-6af2612daeda");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "b8f65a5f-bd63-4ac8-b7b9-21651dac902a");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "53f82471-69e7-42c5-ae68-d7ac7c975509");
        return l;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "6e1eb331-a189-4ef3-942c-a9c0bbc96784");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "6a2b8df0-bcb8-4d49-be60-576fbec3424f");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "68d1c079-d71b-4645-9704-5ec7bcef240c");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "65b94123-54ba-46fe-8a9f-032ed3cd252e");
                l = wrappedStream.read(b, off, len);
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "62a5d64b-e890-493d-8da1-96344fc636a2");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "f19db0c7-2cf0-48af-9753-46dcf496d716");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "f911096b-4b84-4524-ae9f-c28bc33494d4");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "006af343-0c3c-4296-a408-98152c9f126d");
        return l;
    }

    @Override
    public int read(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "ff364669-5c21-4403-b7e5-435ce398a0c6");
        return read(b, 0, b.length);
    }

    @Override
    public int available() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "f6ef95e0-7258-4a53-bedf-379b2d92ca86");
        // not -1
        int a = 0;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "7a38f528-2385-4403-b185-2659b1aa27ba");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "6067482b-eccb-4b5f-8a39-8b4e69594b73");
            try {
                // no checkEOF() here, available() can't trigger EOF
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "41c55848-acb4-4ce1-8e34-5b21e0795174");
                a = wrappedStream.available();
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "1305b5b7-7773-439e-84ce-1fd594623b65");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "09c0942a-d2c0-443b-b887-1e53b16f4b96");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "cdcd04c4-3c2d-4951-9c80-351f201dff00");
        return a;
    }

    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "ecc43c1d-d00f-4e52-ab12-35891905aca8");
        // tolerate multiple calls to close()
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "48fdd4ce-0afd-4219-aeed-878c83c16066");
        checkClose();
    }

    /**
     * Detects EOF and notifies the watcher.
     * This method should only be called while the underlying stream is
     * still accessible. Use {@link #isReadAllowed isReadAllowed} to
     * check that condition.
     * <p>
     * If EOF is detected, the watcher will be notified and this stream
     * is detached from the underlying stream. This prevents multiple
     * notifications from this stream.
     * </p>
     *
     * @param eof       the result of the calling read operation.
     * A negative value indicates that EOF is reached.
     *
     * @throws IOException
     * in case of an IO problem on closing the underlying stream
     */
    private void checkEOF(final int eof) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "ec5bcdf0-71ad-4521-b251-abbd175686b7");
        final InputStream toCheckStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "3e841939-e018-4837-80d0-47190d849eb9");
        if ((toCheckStream != null) && (eof < 0)) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "4814dd67-e7f6-4c04-b7d5-1c9f002fb2d7");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "99b14c73-c252-4cae-b98d-9d638ffed154");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "2b3a5c13-6879-445c-ba09-7a81e80be8ca");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "628197cb-aec7-4af2-88b1-f0f7a600e27c");
                    scws = eofWatcher.eofDetected(toCheckStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "6099593a-d661-4899-b3c1-9e1e6739349d");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "56b3799a-22cc-4395-8be9-4a452c3a163b");
                    toCheckStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "f45de7ec-1348-4f32-92f3-57a6d7daab3f");
                wrappedStream = null;
            }
        }
    }

    /**
     * Detects stream close and notifies the watcher.
     * There's not much to detect since this is called by {@link #close close}.
     * The watcher will only be notified if this stream is closed
     * for the first time and before EOF has been detected.
     * This stream will be detached from the underlying stream to prevent
     * multiple notifications to the watcher.
     *
     * @throws IOException
     * in case of an IO problem on closing the underlying stream
     */
    private void checkClose() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "7cf87c67-bb8b-41b6-a06e-a0d6c6932a35");
        final InputStream toCloseStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "aebb2c8c-d41e-41db-a8f7-cf7f20158a45");
        if (toCloseStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "3b6188cc-8e60-4e72-b4d3-1a0df8a8339e");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "d5f3af73-d94e-483b-a11c-ec7222596ccc");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "5ca29401-dfdf-4d63-9f36-fa080d56b4e1");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "d4a7cab1-133d-4925-b92a-e3e1f3613db0");
                    scws = eofWatcher.streamClosed(toCloseStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "af9ab257-c704-46c5-ab1f-b46097838743");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "7d9f68c4-adc0-4352-b288-9084efab1da4");
                    toCloseStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "e644b22e-15c9-456f-90d3-c5a986e301cd");
                wrappedStream = null;
            }
        }
    }

    /**
     * Detects stream abort and notifies the watcher.
     * There's not much to detect since this is called by {@link #abort()}.
     * The watcher will only be notified if this stream is aborted
     * for the first time and before EOF has been detected or the
     * stream has been {@link #close closed} gracefully.
     * This stream will be detached from the underlying stream to prevent
     * multiple notifications to the watcher.
     *
     * @throws IOException
     * in case of an IO problem on closing the underlying stream
     */
    private void checkAbort() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "d8161a7b-a744-486b-88f9-df3706d7cf34");
        final InputStream toAbortStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "6e1239c3-c760-450c-b5f5-f4b19446a695");
        if (toAbortStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "be55ee77-4efc-4929-a3c0-daa5b554fb49");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "bca58f9e-21f6-44d2-a1e5-e61b4ecef522");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "1411b790-0064-4e50-9263-6ea976d56788");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "0dfc841f-a94f-4083-accb-5b60614d02e3");
                    scws = eofWatcher.streamAbort(toAbortStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "3e78f18f-1ba9-431a-a6f5-b375ba1ae5b5");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "dccb07ba-2c00-4fc0-b3a6-55207cdc0ab3");
                    toAbortStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "828cc750-5c15-4114-8821-c1be4cf301c9");
                wrappedStream = null;
            }
        }
    }

    /**
     * Aborts this stream.
     * This is a special version of {@link #close close()} which prevents
     * re-use of the underlying connection, if any. Calling this method
     * indicates that there should be no attempt to read until the end of
     * the stream.
     */
    public void abort() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "a3e80000-b6c4-4758-a82c-c8b163d7021b");
        // tolerate multiple calls
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_7_10.coverage", "dc15c62e-5cbf-41b4-ba1a-b791ad1c10c9");
        checkAbort();
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
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
