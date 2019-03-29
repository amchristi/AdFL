StrBuilder
~~~
getNewLineText
~~~
setNewLineText
~~~
getNullText
~~~
setNullText
~~~
length
~~~
setLength
~~~
capacity
~~~
ensureCapacity
~~~
minimizeCapacity
~~~
size
~~~
isEmpty
~~~
clear
~~~
charAt
~~~
setCharAt
~~~
deleteCharAt
~
if (index < 0 || index >= size) {
    throw new StringIndexOutOfBoundsException(index);
}
~~~
toCharArray
~
if (size == 0) {
    return new char[0];
}
~~~
toCharArray
~
if (size == 0) {
    return new char[0];
}
~~~
getChars
~~~
getChars
~~~
readFrom
~~~
appendNewLine
~~~
appendNull
~
if (nullText == null) {
    return this;
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
append
~
return appendNull();
~
if (obj == null) {
    return appendNull();
}
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendln
~~~
appendAll
~~~
appendAll
~~~
appendAll
~~~
appendWithSeparators
~~~
appendWithSeparators
~~~
appendWithSeparators
~~~
appendSeparator
~~~
appendSeparator
~~~
appendSeparator
~~~
appendSeparator
~~~
appendSeparator
~~~
appendSeparator
~~~
appendPadding
~~~
appendFixedWidthPadLeft
~~~
appendFixedWidthPadLeft
~~~
appendFixedWidthPadRight
~~~
appendFixedWidthPadRight
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
insert
~~~
deleteImpl
~~~
delete
~~~
deleteAll
~~~
deleteFirst
~~~
deleteAll
~~~
deleteFirst
~~~
deleteAll
~~~
deleteFirst
~~~
replaceImpl
~~~
replace
~~~
replaceAll
~~~
replaceFirst
~~~
replaceAll
~~~
replaceFirst
~~~
replaceAll
~~~
replaceFirst
~~~
replace
~~~
replaceImpl
~~~
reverse
~
return this;
~
if (size == 0) {
    return this;
}
~~~
trim
~
return this;
~
if (size == 0) {
    return this;
}
~
while (pos < len && buf[pos] <= ' ') {
    pos++;
}
~
while (pos < len && buf[len - 1] <= ' ') {
    len--;
}
~
if (len < size) {
    delete(len, size);
}
~
if (pos > 0) {
    delete(0, pos);
}
~
return this;
~~~
startsWith
~
return true;
~
if (len == 0) {
    return true;
}
~~~
endsWith
~
return true;
~
if (len == 0) {
    return true;
}
~~~
subSequence
~~~
substring
~~~
substring
~~~
leftString
~~~
rightString
~~~
midString
~~~
contains
~~~
contains
~~~
contains
~~~
indexOf
~~~
indexOf
~~~
indexOf
~~~
indexOf
~~~
indexOf
~~~
indexOf
~~~
lastIndexOf
~~~
lastIndexOf
~~~
lastIndexOf
~~~
lastIndexOf
~~~
lastIndexOf
~~~
lastIndexOf
~~~
asTokenizer
~~~
asReader
~~~
asWriter
~~~
appendTo
~~~
equalsIgnoreCase
~
return false;
~
if (this.size != other.size) {
    return false;
}
~~~
equals
~
return false;
~
if (this.size != other.size) {
    return false;
}
~~~
equals
~
return false;
~
if (this.size != other.size) {
    return false;
}
~~~
hashCode
~~~
toString
~~~
toStringBuffer
~~~
toStringBuilder
~~~
build
~~~
validateRange
~
throw new StringIndexOutOfBoundsException(startIndex);
~
if (startIndex < 0) {
    throw new StringIndexOutOfBoundsException(startIndex);
}
~~~
validateIndex
~
throw new StringIndexOutOfBoundsException(index);
~
if (index < 0 || index > size) {
    throw new StringIndexOutOfBoundsException(index);
}
