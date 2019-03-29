HeaderGroup
~~~
clear
~~~
addHeader
~~~
removeHeader
~
if (header == null) {
    return;
}
~~~
setHeader
~~~
setHeaders
~~~
getCondensedHeader
~
valueBuffer.append(hdrs[0].getValue());
~
for (int i = 1; i < hdrs.length; i++) {
    valueBuffer.append(", ");
    valueBuffer.append(hdrs[i].getValue());
}
~~~
getHeaders
~~~
getFirstHeader
~~~
getSingleHeader
~
int count = 0;
~
for (int i = 0; i < this.headers.size(); i++) {
    final Header header = this.headers.get(i);
    if (header.getName().equalsIgnoreCase(name)) {
        singleHeader = header;
        count++;
    }
}
~
if (count > 1) {
    throw new ProtocolException("Multiple headers '" + name + "' found");
}
~~~
getLastHeader
~~~
getAllHeaders
~~~
containsHeader
~~~
containsHeaders
~
// as that creates an Iterator that needs to be garbage-collected
int count = 0;
~
for (int i = 0; i < this.headers.size(); i++) {
    final Header header = this.headers.get(i);
    if (header.getName().equalsIgnoreCase(name)) {
        count++;
    }
}
~
return count;
~~~
headerIterator
~~~
headerIterator
~~~
removeHeaders
~
i.remove();
~
return;
~
final Header header = i.next();
~
if (header.getName().equalsIgnoreCase(name)) {
    i.remove();
}
~
if (name == null) {
    return;
}
~
for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
    final Header header = i.next();
    if (header.getName().equalsIgnoreCase(name)) {
        i.remove();
    }
}
~~~
toString
