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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "8252b10c-d5e7-4f5c-952a-57e4142e02db");
        return selfClosed;
    }

    InputStream getWrappedStream() {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "b1492a3d-e7b3-4392-b3b5-13d59d8d152f");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "22499b1a-41e7-4849-bf93-2d55587e68b4");
        if (selfClosed) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "4c1f118e-7dd3-4d92-aa4a-e474f2d672f2");
            throw new IOException("Attempted read on closed stream.");
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "0c787358-b251-4b7e-9427-9ef86831a7bc");
        return (wrappedStream != null);
    }

    @Override
    public int read() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "58c4bbcd-8529-4bca-962c-3d1a4c4f3b98");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "51109d2f-211c-4a0c-9541-20d8cf42624a");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "816427ac-6daa-458a-ad74-84aad23017d0");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "1614111d-9736-4482-97db-d4e8d8a33e03");
                l = wrappedStream.read();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "4439651d-cd47-4d94-baca-83521e9c3b71");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "04cc95bc-2b20-4461-b471-01e4deeec402");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "518f5ecb-2172-4313-8c7f-47f61edf17a6");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "e03eb728-b21c-4122-a013-eb3ef0999daa");
        return l;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "d13a81ca-e84b-498f-8a6d-a135d8a180c5");
        int l = -1;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "c687f4e7-20c7-41c3-bd2f-d886caad0b09");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "fb18dc06-272c-4cbf-a2c6-c81185c7d221");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "dec72027-1121-46a5-9265-1a435324930f");
                l = wrappedStream.read(b, off, len);
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "601a59a6-6571-4a0e-88eb-2d4709ab757b");
                checkEOF(l);
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "4271ce60-016f-4162-8876-96e4aaefba73");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "5f28464c-0ea1-4896-b75f-3c059e1c88f5");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "e4087d95-0b6b-4dab-91ef-be3eb65ed935");
        return l;
    }

    @Override
    public int read(final byte[] b) throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "296e5a4f-c2b7-4bc7-9736-5bb375fa3e3c");
        return read(b, 0, b.length);
    }

    @Override
    public int available() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "32f17be6-f83f-4155-98ff-f12cc023c25b");
        // not -1
        int a = 0;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "62f5b9ec-3c1c-4d32-88e5-b8ac3aea3a34");
        if (isReadAllowed()) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "a1aecb91-f367-4fe9-8b0c-e278c0c19ed1");
            try {
                // no checkEOF() here, available() can't trigger EOF
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "03f4111c-c2c8-4e3e-b927-b5c737fe75d7");
                a = wrappedStream.available();
            } catch (final IOException ex) {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "081c599b-8f87-43a0-a29a-2ef1ae4a05d9");
                checkAbort();
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "8b89bebc-e566-4c55-823a-ae06b1c2d898");
                throw ex;
            }
        }
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "bfdac74b-4a20-4a49-a08d-d1793ed3e377");
        return a;
    }

    @Override
    public void close() throws IOException {
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "b20412a1-365f-4fe5-b8d7-ef84ccef1469");
        // tolerate multiple calls to close()
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "11782bb4-e032-4018-8e78-f54bc44e6945");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "cab38e2a-58ea-4a61-b0c5-a23186507ae8");
        final InputStream toCheckStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "c3750451-2e06-43d4-bd8d-87ec52e9fa1d");
        if ((toCheckStream != null) && (eof < 0)) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "689ac652-d70e-4376-8390-217105366f95");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "9fcc77af-de66-4195-9d4e-90ddbae5f044");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "3463cb0e-7f21-4e61-a38f-f7f4b9574f91");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "73e8e1bd-c509-4f28-b342-f82b81f55dc1");
                    scws = eofWatcher.eofDetected(toCheckStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "55dff9cc-65d1-4473-a626-8150ea11ca09");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "5e522d3b-b657-4f00-bee9-7c8d5af39bd5");
                    toCheckStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "3f639a06-270d-418a-94d0-595b72df70ac");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "38aefc71-a5f6-4246-8c27-b60c4c48f46f");
        final InputStream toCloseStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "5126539c-c68e-4f16-b578-d424d4c60c3c");
        if (toCloseStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "8e174d59-d029-433a-aded-a20a0412a1f6");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "26646727-8588-4fdf-883c-495fe4082391");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "c392ba56-7575-4698-96f9-86cfc4f2fda0");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "3148a47f-0976-4f4a-8d5d-89ba5cd8a006");
                    scws = eofWatcher.streamClosed(toCloseStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "32e9b1e4-ae07-4f57-9ee3-60b5df4a5395");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "18a4705d-7684-4b99-9008-18985eeb5644");
                    toCloseStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "8d061dff-b682-49ce-951a-384562dfb30a");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "14e65f37-d0d5-4873-ac6e-7b0a5a997887");
        final InputStream toAbortStream = wrappedStream;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "904af50f-b98c-4838-acc6-4709d9e67a2c");
        if (toAbortStream != null) {
            writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "0de21e03-a2b1-42fa-8641-7e43f978efa3");
            try {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "77aeb950-b053-4724-bbdc-7078bccacfc7");
                // should close wrapped stream?
                boolean scws = true;
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "7e50b180-7615-42ed-8dc7-5e1c4536a849");
                if (eofWatcher != null) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "e1807b93-423a-47df-ae5b-5948642190ea");
                    scws = eofWatcher.streamAbort(toAbortStream);
                }
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "24ba943d-c8de-44c7-9a5b-d8745c858675");
                if (scws) {
                    writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "3d57a8e7-74b8-4b84-a789-ec29a02ec7bc");
                    toAbortStream.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "47860c49-2b58-42e4-95a3-63fc1dac2955");
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
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "cf91ea8b-2c26-418b-9614-6d35c66e09d8");
        // tolerate multiple calls
        selfClosed = true;
        writeline("/home/ubuntu/results/coverage/EofSensorInputStream/EofSensorInputStream_1_10.coverage", "e3a47afd-8d54-4b35-8b67-89baaa206dd3");
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
