AlphabetConverter
~~~
encode
~~~
decode
~
return null;
~
if (encoded == null) {
    return null;
}
~~~
getEncodedCharLength
~~~
getOriginalToEncoded
~~~
addSingleEncoding
~~~
toString
~
for (final Entry<Integer, String> entry : originalToEncoded.entrySet()) {
    sb.append(codePointToString(entry.getKey())).append(ARROW).append(entry.getValue()).append(LINE_SEPARATOR);
}
~~~
equals
~
return true;
~
return false;
~
if (obj == null) {
    return false;
}
~
if (obj == this) {
    return true;
}
~~~
hashCode
~~~
createConverterFromMap
~~~
createConverterFromChars
~~~
convertCharsToIntegers
~
return new Integer[0];
~
if (chars == null || chars.length == 0) {
    return new Integer[0];
}
~~~
createConverter
~~~
codePointToString
