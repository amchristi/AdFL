package org.apache.hc.core5.http.io;

import java.io.IOException;
import java.io.InputStream;
import org.apache.hc.core5.util.Args;

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

    /** The watcher to be notified, if any. */
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
     *                  auto-close behavior without notification
     */
    public EofSensorInputStream(final InputStream in, final EofSensorWatcher watcher) {
        Args.notNull(in, "Wrapped stream");
        wrappedStream = in;
        selfClosed = false;
        eofWatcher = watcher;
    }

    boolean isSelfClosed() {
        return selfClosed;
    }

    InputStream getWrappedStream() {
        return wrappedStream;
    }

    /**
     * Checks whether the underlying stream can be read from.
     *
     * @return  {@code true} if the underlying stream is accessible,
     *          {@code false} if this stream is in EOF mode and
     *          detached from the underlying stream
     *
     * @throws IOException      if this stream is already closed
     */
    private boolean isReadAllowed() throws IOException {
        if (selfClosed) {
            throw new IOException("Attempted read on closed stream.");
        }
        return (wrappedStream != null);
    }

    @Override
    public int read() throws IOException {
        int l = -1;
        if (isReadAllowed()) {
            try {
                l = wrappedStream.read();
                checkEOF(l);
            } catch (final IOException ex) {
                checkAbort();
                throw ex;
            }
        }
        return l;
    }

    @Override
    public int read(final byte[] b, final int off, final int len) throws IOException {
        int l = -1;
        if (isReadAllowed()) {
            try {
                l = wrappedStream.read(b, off, len);
                checkEOF(l);
            } catch (final IOException ex) {
                checkAbort();
                throw ex;
            }
        }
        return l;
    }

    @Override
    public int read(final byte[] b) throws IOException {
        return read(b, 0, b.length);
    }

    @Override
    public int available() throws IOException {
        int a = 0;
        return a;
    }

    @Override
    public void close() throws IOException {
        selfClosed = true;
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
     *                  A negative value indicates that EOF is reached.
     *
     * @throws IOException
     *          in case of an IO problem on closing the underlying stream
     */
    private void checkEOF(final int eof) throws IOException {
        final InputStream toCheckStream = wrappedStream;
        if ((toCheckStream != null) && (eof < 0)) {
            try {
                boolean scws = true;
                if (eofWatcher != null) {
                    scws = eofWatcher.eofDetected(toCheckStream);
                }
                if (scws) {
                    toCheckStream.close();
                }
            } finally {
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
     *          in case of an IO problem on closing the underlying stream
     */
    private void checkClose() throws IOException {
        final InputStream toCloseStream = wrappedStream;
        if (toCloseStream != null) {
            try {
                boolean scws = true;
                if (eofWatcher != null) {
                    scws = eofWatcher.streamClosed(toCloseStream);
                }
                if (scws) {
                    toCloseStream.close();
                }
            } finally {
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
     *          in case of an IO problem on closing the underlying stream
     */
    private void checkAbort() throws IOException {
        final InputStream toAbortStream = wrappedStream;
        if (toAbortStream != null) {
            try {
                boolean scws = true;
                if (eofWatcher != null) {
                    scws = eofWatcher.streamAbort(toAbortStream);
                }
            } finally {
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
        selfClosed = true;
        checkAbort();
    }
}
