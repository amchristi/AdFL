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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "2e280df8-3ac3-42a1-a244-37dec76a5d3e");
        return selfClosed;
    }

    InputStream getWrappedStream() {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "7cc9ccce-52e3-4b7f-8d77-2f62be04e8cf");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "87e77fba-2aaf-40b1-8673-abdf481106d5");
        if (selfClosed) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "36269915-872a-48d4-98ba-db292b64c9c4");
            throw new IOException("Attempted read on closed stream.");
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "683ac4f5-6bc4-44de-a3ca-396654a3d453");
        return (wrappedStream != null);
    }

    @Override
    public int read() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "5f3fc166-99c1-43fb-9441-bf822428c021");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "47936e53-9b4f-404d-9d46-78815edacd8b");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "22be5de9-0850-4e25-b613-5e1522087dcb");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "2cadb902-4c26-4fd6-bc0d-b237861d4e88");
                l = wrappedStream.read();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "6f3697b2-f1c5-4b3d-9f0b-5530265008a5");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "8c253bb3-01a2-4968-a579-22045214f9d1");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "92b9aaf2-4897-42ad-8ef0-59b2addc3164");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "4ce66b27-2a82-4651-8b04-6e0c11dde184");
        return l;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "7ca78489-44fe-48a1-8052-6b5501106679");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "e32b0c2d-f9d6-4e44-a8af-9e3f61bcaec0");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "4228e9d3-02ca-45b4-a64b-5761d5f0e720");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "53ecb177-6931-433f-abc3-08381b555ba1");
                l = wrappedStream.read(b, off, len);
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "584d6955-1492-42bb-8b4e-aa6e606b0bba");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "d13b4d6f-9051-45e4-8b4e-59c995d6af28");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "1958d55b-7baa-4311-b54b-ce1f28d44b13");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "2a5aae88-df89-413f-b612-c45f1dc569c3");
        return l;
    }

    @Override
    public int read(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "4c9b4931-eced-4b64-9fac-b15530c35cb8");
        return read(b, 0, b.length);
    }

    @Override
    public int available() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "aa389273-7dbc-47ea-afc0-96f126eebef6");
        // not -1
        int a = 0;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "84a96153-ed48-42e0-a685-47a0bb1242b7");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "713b075f-0836-4840-a42e-f25f79d002af");
            try {
                // no checkEOF() here, available() can't trigger EOF
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "8b43e7a1-f5d2-49bf-9fa9-feb09947d57f");
                a = wrappedStream.available();
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "1765ce31-d49d-4867-afb6-b8e174e65058");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "21c0f971-aaa2-4d36-a673-ff53c1644849");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "6101c2b8-925d-499e-bb20-80729a09be37");
        return a;
    }

    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "83b534dc-16df-47d6-8282-2617ba6b66f0");
        // tolerate multiple calls to close()
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "922c7933-8935-4c76-9661-604dc55c40f9");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "2c951acd-e4ef-494e-a877-4a97f02f57fb");
        final InputStream toCheckStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "709221c4-6050-403b-ba82-02316bf9080f");
        if ((toCheckStream != null) && (eof < 0)) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "33a3c973-c7ab-43c5-8e59-34614e6ada2a");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "9f2f659f-c015-402b-b46a-1d5410bfaf89");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "a77d815d-5d3b-45a6-a8a2-e977e9d1ea35");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "edc511e2-3017-4a79-a68f-1f3bf0f5da2a");
                    scws = eofWatcher.eofDetected(toCheckStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "58ae8a76-addc-4f26-b8fb-c66d7a9bc9ae");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "d8abc0a3-45d4-472e-a45c-de53185f5cb7");
                    toCheckStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "9430cc26-55b8-469c-b96c-88af0567131e");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "cb72a5fe-41aa-4712-ad39-633b7946b2f6");
        final InputStream toCloseStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "60b31d03-24ff-4455-b6b4-59ad8d817f7a");
        if (toCloseStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "7914f068-6d8f-4e6b-9196-72b2fb70e860");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "e5d7fbbf-2597-46d8-8e66-b2cc02bd2fb0");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "f29690b7-3061-4889-a96f-402f2b76fdb7");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "0372d918-2379-43d4-a5ab-7ce69faff58f");
                    scws = eofWatcher.streamClosed(toCloseStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "61e90ca6-2af7-4e56-b9a6-d4479a02a35b");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "3329c948-36fd-4cac-b02b-7a51d9c1087e");
                    toCloseStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "7080b306-2e25-4bc0-913f-afc8534c7a4e");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "bd1787d6-bc1a-435e-960a-78ce5a19b73c");
        final InputStream toAbortStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "660cb8d6-d012-4355-85da-cb92229121ce");
        if (toAbortStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "0a3553fc-534e-4b40-b0a4-033e08137d0d");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "770450ec-866e-4cf3-80f0-2a2099def67a");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "e7e56ee1-2f0f-4174-96a2-4b2064922bdd");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "b75a5c86-ed0c-43c2-8891-1d07e306c2e8");
                    scws = eofWatcher.streamAbort(toAbortStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "b71dfa20-5a2b-472c-a2c2-27855ec33930");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "db497882-a103-4bc1-97e9-6f6d0c30688c");
                    toAbortStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "e8fdb9f6-8bd4-48fa-a2c3-0aa108991841");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "c02f78ec-ccc3-4234-971f-e4348260329d");
        // tolerate multiple calls
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_2_10.coverage", "5547fc3c-dd9c-4d01-8066-522b681c827f");
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
