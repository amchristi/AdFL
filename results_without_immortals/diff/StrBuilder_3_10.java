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
~~~
toCharArray
~~~
toCharArray
~~~
getChars
~~~
getChars
~~~
readFrom
~~~
appendNewLine
~
if (newLine == null) {
    append(System.lineSeparator());
    return this;
}
~~~
appendNull
~
if (nullText == null) {
    return this;
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
}
~~~
append
~
if (obj == null) {
    return appendNull();
}
~
if (obj instanceof CharSequence) {
    return append((CharSequence) obj);
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
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
~~~
insert
~
if (obj == null) {
    return insert(index, nullText);
}
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
~~~
endsWith
~~~
subSequence
~
throw new StringIndexOutOfBoundsException(startIndex);
~
if (startIndex < 0) {
    throw new StringIndexOutOfBoundsException(startIndex);
}
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
return true;
~
return false;
~
if (this == other) {
    return true;
}
~
if (this.size != other.size) {
    return false;
}
~~~
equals
~~~
equals
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
~~~
validateIndex
