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
~
this.headers.set(i, header);
~
return;
~
return;
~
final Header current = this.headers.get(i);
~
if (current.getName().equalsIgnoreCase(header.getName())) {
    this.headers.set(i, header);
    return;
}
~
if (header == null) {
    return;
}
~
for (int i = 0; i < this.headers.size(); i++) {
    final Header current = this.headers.get(i);
    if (current.getName().equalsIgnoreCase(header.getName())) {
        this.headers.set(i, header);
        return;
    }
}
~
this.headers.add(header);
~~~
setHeaders
~~~
getCondensedHeader
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
