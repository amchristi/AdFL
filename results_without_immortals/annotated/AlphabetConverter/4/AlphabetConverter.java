/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.text;

import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.io.*;

/**
 * <p>
 * Convert from one alphabet to another, with the possibility of leaving certain
 * characters unencoded.
 * </p>
 *
 * <p>
 * The target and do not encode languages must be in the Unicode BMP, but the
 * source language does not.
 * </p>
 *
 * <p>
 * The encoding will all be of a fixed length, except for the 'do not encode'
 * chars, which will be of length 1
 * </p>
 *
 * <h3>Sample usage</h3>
 *
 * <pre>
 * Character[] originals; // a, b, c, d
 * Character[] encoding; // 0, 1, d
 * Character[] doNotEncode; // d
 *
 * AlphabetConverter ac = AlphabetConverter.createConverterFromChars(originals,
 * encoding, doNotEncode);
 *
 * ac.encode("a"); // 00
 * ac.encode("b"); // 01
 * ac.encode("c"); // 0d
 * ac.encode("d"); // d
 * ac.encode("abcd"); // 00010dd
 * </pre>
 *
 * <p>
 * #ThreadSafe# AlphabetConverter class methods are threadsafe as they do not
 * change internal state.
 * </p>
 *
 * @since 1.0
 */
public final class AlphabetConverter {

    /**
     * Original string to be encoded.
     */
    private final Map<Integer, String> originalToEncoded;

    /**
     * Encoding alphabet.
     */
    private final Map<String, String> encodedToOriginal;

    /**
     * Length of the encoded letter.
     */
    private final int encodedLetterLength;

    /**
     * Arrow constant, used for converting the object into a string.
     */
    private static final String ARROW = " -> ";

    /**
     * Line separator, used for converting the object into a string.
     */
    private static final String LINE_SEPARATOR = System.getProperty("line.separator");

    /**
     * Hidden constructor for alphabet converter. Used by static helper methods.
     *
     * @param originalToEncoded original string to be encoded
     * @param encodedToOriginal encoding alphabet
     * @param encodedLetterLength length of the encoded letter
     */
    private AlphabetConverter(final Map<Integer, String> originalToEncoded, final Map<String, String> encodedToOriginal, final int encodedLetterLength) {
        this.originalToEncoded = originalToEncoded;
        this.encodedToOriginal = encodedToOriginal;
        this.encodedLetterLength = encodedLetterLength;
    }

    /**
     * Encode a given string.
     *
     * @param original the string to be encoded
     * @return the encoded string, {@code null} if the given string is null
     * @throws UnsupportedEncodingException if chars that are not supported are
     * encountered
     */
    public String encode(final String original) throws UnsupportedEncodingException {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "26b26b01-5218-406c-a8ba-0f993a783e2e");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "fde96a4d-dcad-4986-8b93-4b4d9ff53d8e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "5f998f0a-4b56-4a7b-b7ab-3e962f0af12f");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "7676ec90-c690-4957-9a8d-69fee072ae2f");
        for (int i = 0; i < original.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "8ba3db94-c9d6-477a-89f8-cc8e32769cc7");
            final int codepoint = original.codePointAt(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "7c56d86f-29c0-4d06-8158-a7abd4e64f4b");
            final String nextLetter = originalToEncoded.get(codepoint);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0cd4e0e6-8004-4e22-8d50-45e481abf5c4");
            if (nextLetter == null) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "953d1db3-d9e2-44d6-ab52-e897487330f1");
                throw new UnsupportedEncodingException("Couldn't find encoding for '" + codePointToString(codepoint) + "' in " + original);
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "964df940-b52d-4e0d-9db5-9bf90b002e05");
            sb.append(nextLetter);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "f7b0b3f7-f1b5-46f8-b499-4869d991c822");
            i += Character.charCount(codepoint);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "f4a598a7-7161-4e72-9d8d-3a4e73fa2866");
        return sb.toString();
    }

    /**
     * Decode a given string.
     *
     * @param encoded a string that has been encoded using this
     * AlphabetConverter
     * @return the decoded string, {@code null} if the given string is null
     * @throws UnsupportedEncodingException if unexpected characters that
     * cannot be handled are encountered
     */
    public String decode(final String encoded) throws UnsupportedEncodingException {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0b7a56ac-6481-4259-b5f7-ac8e15ac6d6b");
        if (encoded == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "ef82d73c-7950-4bff-a484-91905ff2f168");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a4eb3de8-d5ea-431e-b804-2cc66200a5d4");
        final StringBuilder result = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "8bc519cd-c703-40ab-b7ae-2b9d73f2bce8");
        for (int j = 0; j < encoded.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0903deae-37e2-4deb-8399-21cad582e094");
            final Integer i = encoded.codePointAt(j);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "58a5a432-f1ec-497c-831a-0a0a2151de56");
            final String s = codePointToString(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a5a2b90d-8bb3-42d7-aafa-5409b2798e9f");
            if (s.equals(originalToEncoded.get(i))) {
                // length of each encoded char is 1
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "48c8a4c7-6d86-459c-8981-3bc5eeec40cf");
                result.append(s);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "8b9fce78-782b-4cc5-9ceb-a2cfa85d480b");
                // because we do not encode in Unicode extended the
                j++;
            } else {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "6eb3b954-5264-40b6-a2be-6d007b4d886b");
                if (j + encodedLetterLength > encoded.length()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "31632b00-a3a4-4499-a68c-9645469bd86a");
                    throw new UnsupportedEncodingException("Unexpected end " + "of string while decoding " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "f7eff0bf-75ff-4763-9c4a-dd8d028ec4aa");
                final String nextGroup = encoded.substring(j, j + encodedLetterLength);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "76321778-ef90-4544-9f7e-0b3c2e999ee4");
                final String next = encodedToOriginal.get(nextGroup);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "e5885aa9-1717-41c5-951f-b72127c93738");
                if (next == null) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "46cc4077-cf91-47ed-825b-5374bb67c1c6");
                    throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "076498e8-6785-4d2a-89c4-d0967f23def1");
                result.append(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a0126daa-cf71-423d-b42c-5d3f77aa60e8");
                j += encodedLetterLength;
            }
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "54881e6b-8ef7-486f-966d-22a7dbf83667");
        return result.toString();
    }

    /**
     * Get the length of characters in the encoded alphabet that are necessary
     * for each character in the original
     * alphabet.
     *
     * @return the length of the encoded char
     */
    public int getEncodedCharLength() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "3daf1b1d-3a0b-471a-9a5a-7e8420d094c3");
        return encodedLetterLength;
    }

    /**
     * Get the mapping from integer code point of source language to encoded
     * string. Use to reconstruct converter from
     * serialized map.
     *
     * @return the original map
     */
    public Map<Integer, String> getOriginalToEncoded() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "956cc333-e4d1-4547-87ad-776a2069e7fb");
        return Collections.unmodifiableMap(originalToEncoded);
    }

    /**
     * Recursive method used when creating encoder/decoder.
     *
     * @param level at which point it should add a single encoding
     * @param currentEncoding current encoding
     * @param encoding letters encoding
     * @param originals original values
     * @param doNotEncodeMap map of values that should not be encoded
     */
    @SuppressWarnings("PMD")
    private void addSingleEncoding(final int level, final String currentEncoding, final Collection<Integer> encoding, final Iterator<Integer> originals, final Map<Integer, String> doNotEncodeMap) {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "eb113b13-4a88-4ce9-8155-f8e9e7378948");
        if (level > 0) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "675b8f7f-1c1f-472f-8e32-6b2dff299062");
            for (final int encodingLetter : encoding) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "c71f6291-742b-4d33-9659-c124ca4fcbf3");
                if (originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "5a204cff-e77c-497a-8802-9a6d9e79822b");
                    // leftmost place
                    if (level != encodedLetterLength || !doNotEncodeMap.containsKey(encodingLetter)) {
                        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "027e1574-c5dc-4305-9e74-02f6a4836dc3");
                        addSingleEncoding(level - 1, currentEncoding + codePointToString(encodingLetter), encoding, originals, doNotEncodeMap);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2fd8252f-c723-4687-ae98-ae6a49af143a");
                    // done encoding all the original alphabet
                    return;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "74fa1353-8cd9-495b-8402-13400bdc1093");
            Integer next = originals.next();
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "10bd29bc-2166-4b4f-872d-2a75942628ad");
            while (doNotEncodeMap.containsKey(next)) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "e71af236-ff95-4fa0-8c39-5ca8cb347962");
                final String originalLetterAsString = codePointToString(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "ff50e302-c447-480e-b952-fa4db6e1dec0");
                originalToEncoded.put(next, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2e4bdbdf-2d9c-495d-a5ee-672231b09ad8");
                encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "6a0a3514-5c44-4385-a0b3-3d9094aa7aea");
                if (!originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a17557cc-acfd-4703-be0e-302a7d5657d7");
                    return;
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "1448c93d-3780-457d-9164-c626aa9d2917");
                next = originals.next();
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "ca6a71d3-fead-4835-9d47-921042dd9726");
            final String originalLetterAsString = codePointToString(next);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a4d84818-69d2-4d88-bda4-61ea2b2e6528");
            originalToEncoded.put(next, currentEncoding);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "81387870-87f3-4a46-ad44-6716892acf0f");
            encodedToOriginal.put(currentEncoding, originalLetterAsString);
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "3f0cc9d6-47b2-437e-bf4e-b55450f0223d");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "6a00daad-643b-4ff2-b22a-754b018dce2c");
        for (final Entry<Integer, String> entry : originalToEncoded.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "f5c239dc-6b00-41b2-bfd2-c821bf9e3331");
            sb.append(codePointToString(entry.getKey())).append(ARROW).append(entry.getValue()).append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a93b48cd-8bd1-4d3f-8dac-e956d528d128");
        return sb.toString();
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "9eda47f1-3fdd-4657-8599-c680e8f0f3e7");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "d1fa991d-735a-4a3a-8c23-fc2cb77849af");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "4a9ca218-9cf4-4278-949d-4fee28f84881");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "30191df3-1d3f-4731-baf8-9447212542df");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "18a8ae15-e709-47c3-b2bb-d1f2a4692c36");
        if (!(obj instanceof AlphabetConverter)) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "1bec1c71-4dc8-4a43-aa38-f7b9d7a4c906");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "6e11f2d1-363d-4114-aaf2-5092928669ec");
        final AlphabetConverter other = (AlphabetConverter) obj;
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "40bb332c-66ba-40fb-a1bc-472a5b5dbf4e");
        return originalToEncoded.equals(other.originalToEncoded) && encodedToOriginal.equals(other.encodedToOriginal) && encodedLetterLength == other.encodedLetterLength;
    }

    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "54b78f42-8708-4ec5-a6f8-a7ac913fff29");
        return Objects.hash(originalToEncoded, encodedToOriginal, encodedLetterLength);
    }

    // -- static methods
    /**
     * Create a new converter from a map.
     *
     * @param originalToEncoded a map returned from getOriginalToEncoded()
     * @return the reconstructed AlphabetConverter
     * @see AlphabetConverter#getOriginalToEncoded()
     */
    public static AlphabetConverter createConverterFromMap(final Map<Integer, String> originalToEncoded) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "ea46fb1f-c932-4c73-b48e-c0f5801a7db7");
        final Map<Integer, String> unmodifiableOriginalToEncoded = Collections.unmodifiableMap(originalToEncoded);
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0b0792ee-c200-4d1f-9d6c-609014d78793");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a6911665-2090-4139-b578-1aacc8181e3a");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "6f28e8a6-605a-49b5-86d8-1a8e82451f38");
        int encodedLetterLength = 1;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "839e9d71-228f-423b-946c-c212917e13eb");
        for (final Entry<Integer, String> e : unmodifiableOriginalToEncoded.entrySet()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2b61234f-774b-45bc-b70a-524b6f924621");
            final String originalAsString = codePointToString(e.getKey());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "67ffab91-e40b-4146-8c89-9046b22bd3bc");
            encodedToOriginal.put(e.getValue(), originalAsString);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "7474c664-9b67-4f99-b19f-c4f89cc0c47b");
            if (e.getValue().equals(originalAsString)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "20baa7b7-f657-427c-9c18-7c37b48e108d");
                doNotEncodeMap.put(e.getKey(), e.getValue());
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "5dea9b53-bf67-4fb0-b57c-3ecf3ae3c728");
            if (e.getValue().length() > encodedLetterLength) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "3325c3df-7461-4bab-93df-1fe6966b902a");
                encodedLetterLength = e.getValue().length();
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "f9045aeb-06b3-43c3-a28d-70ad4777516c");
        return new AlphabetConverter(unmodifiableOriginalToEncoded, encodedToOriginal, encodedLetterLength);
    }

    /**
     * Create an alphabet converter, for converting from the original alphabet,
     * to the encoded alphabet, while leaving the characters in
     * <em>doNotEncode</em> as they are (if possible).
     *
     * <p>Duplicate letters in either original or encoding will be ignored.</p>
     *
     * @param original an array of chars representing the original alphabet
     * @param encoding an array of chars representing the alphabet to be used
     * for encoding
     * @param doNotEncode an array of chars to be encoded using the original
     * alphabet - every char here must appear in
     * both the previous params
     * @return the AlphabetConverter
     * @throws IllegalArgumentException if an AlphabetConverter cannot be
     * constructed
     */
    public static AlphabetConverter createConverterFromChars(final Character[] original, final Character[] encoding, final Character[] doNotEncode) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "36070d6e-3650-4aff-b7b2-2a95082f0506");
        return AlphabetConverter.createConverter(convertCharsToIntegers(original), convertCharsToIntegers(encoding), convertCharsToIntegers(doNotEncode));
    }

    /**
     * Convert characters to integers.
     *
     * @param chars array of characters
     * @return an equivalent array of integers
     */
    private static Integer[] convertCharsToIntegers(final Character[] chars) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "684a5cc9-8b7d-4946-aff3-81a3dc07fab3");
        if (chars == null || chars.length == 0) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "969e2aa3-c515-457c-a7f5-6a430e559fca");
            return new Integer[0];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0e872db2-facd-4a46-a2cd-92ae3ea6d539");
        final Integer[] integers = new Integer[chars.length];
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "7bb3192c-465b-4f5e-a0ee-f5662800c924");
        for (int i = 0; i < chars.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "ba24c0d1-5bb4-4e23-9095-ac203d293577");
            integers[i] = (int) chars[i];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "d82784c0-2287-4723-848c-a52400b4e410");
        return integers;
    }

    /**
     * Create an alphabet converter, for converting from the original alphabet,
     * to the encoded alphabet, while leaving
     * the characters in <em>doNotEncode</em> as they are (if possible).
     *
     * <p>Duplicate letters in either original or encoding will be ignored.</p>
     *
     * @param original an array of ints representing the original alphabet in
     * codepoints
     * @param encoding an array of ints representing the alphabet to be used for
     * encoding, in codepoints
     * @param doNotEncode an array of ints representing the chars to be encoded
     * using the original alphabet - every char
     * here must appear in both the previous params
     * @return the AlphabetConverter
     * @throws IllegalArgumentException if an AlphabetConverter cannot be
     * constructed
     */
    public static AlphabetConverter createConverter(final Integer[] original, final Integer[] encoding, final Integer[] doNotEncode) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "6b66c9ce-1999-4e3e-ad69-1508499bc877");
        final Set<Integer> originalCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(original));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "d8c60afc-93fa-4d3e-9af6-3b278da5244a");
        final Set<Integer> encodingCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(encoding));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "e243ab0b-3319-4035-b2cc-e229a8d39cb5");
        final Set<Integer> doNotEncodeCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(doNotEncode));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "3a3b915a-ab84-4a4a-8c40-2b519621e40a");
        final Map<Integer, String> originalToEncoded = new LinkedHashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "bad98dcf-c0cd-444c-bd95-1846a3718661");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0d4a97b3-be2c-457c-9ee1-86aeb6298b7c");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "d5a0a206-2b01-4e09-b859-e61698515b26");
        int encodedLetterLength;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "9184d6e6-0c24-408d-9df1-f08ec157049e");
        for (final int i : doNotEncodeCopy) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "4d595da9-eaf6-4d88-bdfd-d0612e3d970e");
            if (!originalCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "8a7c8eef-6e3d-49d3-a73f-576992a5ef11");
                throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "95a8d9ec-22e2-4e88-81fc-864a7c2bb38e");
            if (!encodingCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2db8e504-066a-47b5-8ae1-a9c764b89e33");
                throw new IllegalArgumentException("Can not use 'do not encode' list because encoding alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "9d5eadf0-b7ba-4280-a8d6-8fd49ed8f5a3");
            doNotEncodeMap.put(i, codePointToString(i));
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "b37a8fca-12e6-4520-8e51-e4e465c56f7a");
        if (encodingCopy.size() >= originalCopy.size()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "b90856a4-a248-4b95-864c-e8b8151d7d76");
            encodedLetterLength = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "8e8f233d-cfd8-4676-9e1b-7bb74219a9fc");
            final Iterator<Integer> it = encodingCopy.iterator();
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "613afc07-0684-48fb-b5c1-afcba93afa7d");
            for (final int originalLetter : originalCopy) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "b890bd76-a3c8-4cb3-a019-be693441334c");
                final String originalLetterAsString = codePointToString(originalLetter);
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "192a2648-340c-463c-a090-ed342a7b0f2a");
                if (doNotEncodeMap.containsKey(originalLetter)) {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "55fba5e5-9c19-4c90-89a7-113c14429b15");
                    originalToEncoded.put(originalLetter, originalLetterAsString);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2ec1b3fb-d4a3-4df6-aa1c-10c594541e5b");
                    encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2a3be797-5f00-478b-ac5b-d6f5da5d8305");
                    Integer next = it.next();
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "759aa698-7b87-4c59-8033-1cce42e5c08c");
                    while (doNotEncodeCopy.contains(next)) {
                        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a2db0d4b-f510-4018-927c-aecbfabfe6f4");
                        next = it.next();
                    }
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "9c026832-9d2d-4832-b08c-d5664f73f9d0");
                    final String encodedLetter = codePointToString(next);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "06c802d6-16da-475e-a969-0aaa475e215d");
                    originalToEncoded.put(originalLetter, encodedLetter);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "1406b3a7-cd4f-4588-9147-c6ea90de7d6c");
                    encodedToOriginal.put(encodedLetter, originalLetterAsString);
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "75d76d6a-13a0-43df-a3ae-9fb0a0002f65");
            return new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
        } else if (encodingCopy.size() - doNotEncodeCopy.size() < 2) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "483dd50f-bcc3-42bf-9404-214e830a323e");
            throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2cc64ef1-f069-42e6-be36-a62a0aba6a42");
            // we start with one which is our minimum, and because we do the
            // first division outside the loop
            int lettersSoFar = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "a51f05bd-12e8-4784-9112-4fa400548798");
            // the first division takes into account that the doNotEncode
            // letters can't be in the leftmost place
            int lettersLeft = (originalCopy.size() - doNotEncodeCopy.size()) / (encodingCopy.size() - doNotEncodeCopy.size());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "77b1ecd6-3ad3-4f8c-8fbb-54d1489f43a8");
            while (lettersLeft / encodingCopy.size() >= 1) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "694a5b57-8a69-4be0-b385-c3d83a23165d");
                lettersLeft = lettersLeft / encodingCopy.size();
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "068e697f-a6c2-4e53-b4fd-3438e3a640d9");
                lettersSoFar++;
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "0f7fdb79-1866-40e3-95c9-3dad9f087e5d");
            encodedLetterLength = lettersSoFar + 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "8a45e90f-e829-49e7-9ddc-b72b2bb55ec3");
            final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "e68315d6-0767-47e1-b830-7ea57143e37e");
            ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "7f2e8878-375a-4b90-a7cd-ea7d92c119e3");
            return ac;
        }
    }

    /**
     * Create new String that contains just the given code point.
     *
     * @param i code point
     * @return a new string with the new code point
     * @see "http://www.oracle.com/us/technologies/java/supplementary-142654.html"
     */
    private static String codePointToString(final int i) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "b87a8697-3fca-4b6b-be5f-869cae3a0502");
        if (Character.charCount(i) == 1) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "d543b4b2-6b33-462b-a592-829439b260e8");
            return String.valueOf((char) i);
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_4_10.coverage", "2ece7bbf-0eae-40d3-8896-f456ff329a2b");
        return new String(Character.toChars(i));
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
