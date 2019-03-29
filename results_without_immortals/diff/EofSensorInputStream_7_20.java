EofSensorInputStream
~~~
isSelfClosed
~~~
getWrappedStream
~~~
isReadAllowed
~
if (selfClosed) {
    throw new IOException("Attempted read on closed stream.");
}
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
~~~
checkClose
~~~
checkAbort
~
toAbortStream.close();
~
// should close wrapped stream?
boolean scws = true;
~
if (eofWatcher != null) {
    scws = eofWatcher.streamAbort(toAbortStream);
}
~
if (scws) {
    toAbortStream.close();
}
~
wrappedStream = null;
~~~
abort
