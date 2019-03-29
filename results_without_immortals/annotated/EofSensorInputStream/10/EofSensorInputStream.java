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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "b1b4a258-be88-4070-bcff-137c90fd2bc0");
        return selfClosed;
    }

    InputStream getWrappedStream() {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "3e4a55b7-ee1c-4639-ac16-0fc1fedb5063");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "6435af0b-c0b6-41c0-a27d-6be447f52525");
        if (selfClosed) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "40658166-565b-4ebe-b192-0cbd2bbbd352");
            throw new IOException("Attempted read on closed stream.");
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "23a667b9-3799-499a-90ad-cd5e504faed6");
        return (wrappedStream != null);
    }

    @Override
    public int read() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "f425cf4b-640f-4791-b04e-81da7ad01024");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "ece73962-9a6a-4bc1-b345-657ada548ce7");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "8e00fbff-0170-4f74-a5c4-8cea2300f4c2");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "5982a5a1-6a0e-4c56-8e84-3b6a47f3d4ed");
                l = wrappedStream.read();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "4f883fce-2e80-4a13-9e59-866c92acdb09");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "594e1fdf-02e0-42a5-930f-cbbbe9b0625f");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "e9e14ac4-cb31-4043-8cd9-edef1d89ed50");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "8262a665-85ec-4dc6-8989-40a904666c9e");
        return l;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "9c7e8f06-3f5a-4abb-9a13-8f4c87d7b3db");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "f48b3e5d-000c-4733-8bf0-8272f187fb7d");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "8eab96dc-1cb2-4d76-b9c7-b3f984798cb3");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "fcd81c95-8751-433f-a266-ecc6433865ea");
                l = wrappedStream.read(b, off, len);
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "8fb7f401-7f04-4ed4-bbe1-eb47c5af540b");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "f9c24137-afe1-4ec8-a186-6102f8e2da7a");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "5401f675-a402-443f-8579-c1850354f97a");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "403b0724-39b9-4751-af9d-2912a1be6fb6");
        return l;
    }

    @Override
    public int read(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "3c1a212e-8473-4898-b67e-218d59a88c47");
        return read(b, 0, b.length);
    }

    @Override
    public int available() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "4e1770eb-04a0-4720-bf9e-2d693f97d72d");
        // not -1
        int a = 0;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "b93c1880-1a67-49f7-b93d-502ef88c0ae7");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "ec914403-3692-4040-9f7e-a57bc22ae272");
            try {
                // no checkEOF() here, available() can't trigger EOF
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "a6cf6b54-f130-4ca7-a1f9-cd655ced6fe4");
                a = wrappedStream.available();
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "49710c89-229b-4501-bb79-355bf895e131");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "b509017e-60a2-41ac-9613-b29a87b9f0a3");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "eb86ff39-1855-4083-9b20-11147a9588bd");
        return a;
    }

    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "e391f822-c6ce-452c-b386-c4babcc1f558");
        // tolerate multiple calls to close()
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "9623f88e-30e5-46e7-b8f5-a299bf815896");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "79b70e70-9a01-440a-b3fc-ece14d0f280f");
        final InputStream toCheckStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "4ef13a40-f0cd-4b79-9374-2aa796a12c6e");
        if ((toCheckStream != null) && (eof < 0)) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "a8766b1f-7c0e-4a24-9d60-19212963be42");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "3a0556ca-f1e1-42af-8122-e278383c9417");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "cd93ba46-3459-49dd-94a0-aefec0bd96a0");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "c8ff1213-a729-468d-90bd-9dcfeb0c369f");
                    scws = eofWatcher.eofDetected(toCheckStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "e9d38bcb-102e-419f-9631-28e5ba293f82");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "dd9c5db4-8251-4f95-ac08-4607a266075c");
                    toCheckStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "46171f4e-3bb2-44a6-9bf9-ee4fe2343e31");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "c5223ce8-55f6-40e6-b75c-dbac23487a3b");
        final InputStream toCloseStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "67ea23e4-f770-46ce-89b8-916da418e5b7");
        if (toCloseStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "3c16a3b9-f656-4ea1-a8c5-54b4561136fc");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "491b3592-86b6-4568-8d4b-e68e96a6f381");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "3f9cdfb1-1663-4663-90f1-72f963f5f052");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "ce3bf776-54be-4edb-84c6-ad04d5014856");
                    scws = eofWatcher.streamClosed(toCloseStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "324c40b9-ef24-486f-86aa-bc1f585d1859");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "efa31dec-cade-4e6c-9a4a-73016a0f844f");
                    toCloseStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "a87fc842-a889-4e37-adf3-392d3ebc406b");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "29577f0f-35dc-4f09-9f0f-8688ee99b4e3");
        final InputStream toAbortStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "f9ecebeb-2654-4203-bcd4-d30be79e8d75");
        if (toAbortStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "84e6b24a-c7f4-4be0-81a3-9617cb10233d");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "59777ab7-7513-4ccd-bf56-dc8be90b8256");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "b4443e17-4bb4-485a-9612-51887c259ae2");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "cfe2f31b-615c-431d-8e81-170c90fd159b");
                    scws = eofWatcher.streamAbort(toAbortStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "835ff748-bbbc-4915-8895-155d3c050d17");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "0ae7dd11-951e-4f51-a537-4749b84178b9");
                    toAbortStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "63c89954-864e-446a-8db7-74082a9eefbb");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "0635ef31-5297-4042-b5a7-c172dbea0d43");
        // tolerate multiple calls
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_10_10.coverage", "60db5e4a-96ad-4f26-a303-3b3e537e1f40");
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
