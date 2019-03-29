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
~~~
checkClose
~~~
checkAbort
~~~
abort
