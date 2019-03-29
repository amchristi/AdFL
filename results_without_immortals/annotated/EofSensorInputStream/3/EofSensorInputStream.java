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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "65e3b849-9b3b-49d3-803b-4e458cccb015");
        return selfClosed;
    }

    InputStream getWrappedStream() {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "aa759999-0e43-485c-88ca-8f2fa74c16e9");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "c152ecbd-f917-42b1-8567-a760b872bfb7");
        if (selfClosed) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "6faea8d4-885f-4243-bb4d-f3cd7ead19d0");
            throw new IOException("Attempted read on closed stream.");
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "95bb0b4b-a79a-4e76-94a9-62daf91ed7b7");
        return (wrappedStream != null);
    }

    @Override
    public int read() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "09a63e9f-74b2-49ea-a56c-2b9cff62238a");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "037f8ec7-2680-4d69-af65-ff16c995b006");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "a927a0b1-5d84-49f9-8016-5e3956ec3229");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "649003c7-caf1-41b3-8025-2ac0913fd07a");
                l = wrappedStream.read();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "a7c7df29-f2c3-425d-b85a-97b719432bdd");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "5428ccef-fa64-4f32-9496-31d8dcc3ba13");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "cbea45b2-0512-4769-bdf6-8826c3cd978f");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "b723259e-b3a9-4306-a6f6-cbcfe8704153");
        return l;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "93e8fffe-247b-41a3-a4d1-b662f712a0bf");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "59d1e4c5-b986-4264-87e9-8c2e45d4790d");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "50be1271-27fa-4b32-85a0-431389e269de");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "3688ab8b-13c6-4c79-9f83-b2d19df290a3");
                l = wrappedStream.read(b, off, len);
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "27308a59-af43-4f7e-a530-17c28ad9de66");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "aba2a348-1b5a-404f-8272-e7d4eda2fa9a");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "0de806c0-5171-45a0-a2c6-ddd0fa029dc9");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "5da27ead-efe7-4a49-b3a0-dfe5affa1628");
        return l;
    }

    @Override
    public int read(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "d2560710-8fbf-4ec1-858b-25a833c2251b");
        return read(b, 0, b.length);
    }

    @Override
    public int available() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "8751f163-70d9-46e6-9fd3-d6b76dc3ebfa");
        // not -1
        int a = 0;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "fdeeddcf-80ca-498e-98da-b877f3491c12");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "4e3d36b1-6290-4e9a-8315-5381c520e831");
            try {
                // no checkEOF() here, available() can't trigger EOF
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "929839d5-912e-43ba-aade-e9e02f0c1a12");
                a = wrappedStream.available();
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "cb396465-8b75-43c9-ac25-382c15119cbf");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "9fe0a4ee-b863-489e-9505-645f70ddb194");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "ab768d27-e4f4-49b5-88ad-8d1995c41117");
        return a;
    }

    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "51e003b4-00a2-48d7-9833-0e24bd0f0f98");
        // tolerate multiple calls to close()
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "38a197e4-f5c1-4be7-a553-d45a4e781bdb");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "a205876e-503e-45d8-8c99-51d198ef0f69");
        final InputStream toCheckStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "a1b4d580-ac5d-4f9a-b15b-d11d35d1d714");
        if ((toCheckStream != null) && (eof < 0)) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "0043845f-b690-4a15-baac-f5c8850eda66");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "45f6372d-478e-4f3a-960b-6a2563e2a986");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "5fb91755-7e6a-428a-b624-cf5ddd504ebc");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "148a2ff4-c42c-48bc-a80f-c7df6c30f77e");
                    scws = eofWatcher.eofDetected(toCheckStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "e49c2f3d-1cc2-4dcc-a5fb-b6d9459cdc96");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "430686d7-a5ec-4468-9d01-3983c25f911c");
                    toCheckStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "672cfbd2-accb-4a57-8139-18d1cc6f3dc3");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "2f69aa27-4768-468b-b33b-5deebd0b80f7");
        final InputStream toCloseStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "dd590bf1-8860-4912-b50d-9befa169d68b");
        if (toCloseStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "09aa052f-babb-4c71-8511-6a547baabedc");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "5b855e85-aa0a-45af-aede-69001357a2f1");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "691759e6-ecbd-49c3-bf7f-f490a4e35780");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "85e6915c-01a0-4d01-b098-1dd6d483a5a5");
                    scws = eofWatcher.streamClosed(toCloseStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "5b4034e4-f6f4-4683-846a-16d325d83f90");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "2a1140e4-e259-484a-b4fa-e764b81be26d");
                    toCloseStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "4bc16009-3045-4b73-b400-e106a645a2ac");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "6bdb844d-07b4-4a74-8e0f-dc4bc357e5e2");
        final InputStream toAbortStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "858bce4c-4cb9-4562-810e-cebb5e894ebc");
        if (toAbortStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "6df1bd50-e014-4d55-9376-d11ad068bb80");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "2d66b180-2419-4b8e-96f6-ab064071a713");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "a3b25e73-d4c3-4465-9f6f-6c3c8d6f902e");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "2b69013c-205f-4bd5-a138-5b898859db35");
                    scws = eofWatcher.streamAbort(toAbortStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "f37c6aaa-e061-4f36-a147-390750b372c7");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "aa23269d-701a-4bb6-802a-9f135d0eada1");
                    toAbortStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "256e8729-8cb0-4acc-9345-b1359c5d9b4a");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "31254211-652a-4d25-8f0f-9dac7cdedc59");
        // tolerate multiple calls
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_3_10.coverage", "e2f49166-ec8c-4fa8-8f31-0bf180b77b1c");
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
