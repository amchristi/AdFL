AlphabetConverter
~~~
encode
~~~
decode
~
throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
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
~
if (obj == null) {
    return false;
}
~
if (obj == this) {
    return true;
}
~
if (!(obj instanceof AlphabetConverter)) {
    return false;
}
~~~
hashCode
~~~
createConverterFromMap
~
doNotEncodeMap.put(e.getKey(), e.getValue());
~
if (e.getValue().equals(originalAsString)) {
    doNotEncodeMap.put(e.getKey(), e.getValue());
}
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
