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
~
throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
~
while (lettersLeft / encodingCopy.size() >= 1) {
    lettersLeft = lettersLeft / encodingCopy.size();
    lettersSoFar++;
}
~
encodedLetterLength = lettersSoFar + 1;
~
final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
~
ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
~
return ac;
~~~
codePointToString
