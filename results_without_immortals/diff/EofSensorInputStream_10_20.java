EofSensorInputStream
~~~
isSelfClosed
~~~
getWrappedStream
~~~
isReadAllowed
~~~
read
~~~
read
~~~
read
~~~
available
~
// not -1
int a = 0;
~
if (isReadAllowed()) {
    try {
        a = wrappedStream.available();
    } catch (final IOException ex) {
        checkAbort();
        throw ex;
    }
}
~
return a;
~~~
close
~~~
checkEOF
~
scws = eofWatcher.eofDetected(toCheckStream);
~
toCheckStream.close();
~
// should close wrapped stream?
boolean scws = true;
~
if (eofWatcher != null) {
    scws = eofWatcher.eofDetected(toCheckStream);
}
~
if (scws) {
    toCheckStream.close();
}
~
wrappedStream = null;
~
try {
    // should close wrapped stream?
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
~
final InputStream toCheckStream = wrappedStream;
~
if ((toCheckStream != null) && (eof < 0)) {
    try {
        // should close wrapped stream?
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
~~~
checkClose
~~~
checkAbort
~~~
abort
