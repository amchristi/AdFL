AlphabetConverter
~~~
encode
~~~
decode
~
throw new UnsupportedEncodingException("Unexpected end " + "of string while decoding " + encoded);
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
~
throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
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
~
if (!originalCopy.contains(i)) {
    throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
}
~
for (final int originalLetter : originalCopy) {
    final String originalLetterAsString = codePointToString(originalLetter);
    if (doNotEncodeMap.containsKey(originalLetter)) {
        originalToEncoded.put(originalLetter, originalLetterAsString);
        encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
    } else {
        Integer next = it.next();
        while (doNotEncodeCopy.contains(next)) {
            next = it.next();
        }
        final String encodedLetter = codePointToString(next);
        originalToEncoded.put(originalLetter, encodedLetter);
        encodedToOriginal.put(encodedLetter, originalLetterAsString);
    }
}
~
return new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
~
if (encodingCopy.size() - doNotEncodeCopy.size() < 2) {
    throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
} else {
    // first division outside the loop
    int lettersSoFar = 1;
    // letters can't be in the leftmost place
    int lettersLeft = (originalCopy.size() - doNotEncodeCopy.size()) / (encodingCopy.size() - doNotEncodeCopy.size());
    while (lettersLeft / encodingCopy.size() >= 1) {
        lettersLeft = lettersLeft / encodingCopy.size();
        lettersSoFar++;
    }
    encodedLetterLength = lettersSoFar + 1;
    final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
    ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
    return ac;
}
~~~
codePointToString
~
if (Character.charCount(i) == 1) {
    return String.valueOf((char) i);
}
