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
~~~
equals
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
~
if (Character.charCount(i) == 1) {
    return String.valueOf((char) i);
}
