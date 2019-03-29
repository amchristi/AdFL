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
~~~
trim
~~~
startsWith
~~~
endsWith
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
