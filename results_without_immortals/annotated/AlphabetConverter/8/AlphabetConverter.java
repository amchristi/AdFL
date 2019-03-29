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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "89b7d4a3-9462-44d6-93c7-23859367f525");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "488ddff7-445d-4d8f-867e-3819173711fe");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "fab69d68-db2f-451d-8457-12f6b60f7676");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "b289e8d9-1ad6-4d2b-ad57-3234d49b6e45");
        for (int i = 0; i < original.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "42eea57d-8fb1-4246-bf78-c43b94fefda9");
            final int codepoint = original.codePointAt(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "24e94375-8c5a-4c4d-817e-a79b2dce9110");
            final String nextLetter = originalToEncoded.get(codepoint);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "28dfa86f-82d1-4050-8157-2508e154ad03");
            if (nextLetter == null) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "832257f6-ba3b-484f-ba83-4d7aadcc0969");
                throw new UnsupportedEncodingException("Couldn't find encoding for '" + codePointToString(codepoint) + "' in " + original);
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "036d9a6b-8a5d-4f4a-b699-e91626e7d49a");
            sb.append(nextLetter);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "7443c93c-015f-4bbd-a73e-94cd52a607e7");
            i += Character.charCount(codepoint);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a3188c1e-949b-4d2f-ac97-57373db69f86");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "14087c77-15fa-4ed7-a146-70afb833f5cb");
        if (encoded == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "368f47af-e372-47c1-9eea-b4b01fff5787");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "18db50b8-904b-4d2e-a594-e04e58f5620c");
        final StringBuilder result = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "9de25708-7e51-4bb9-9cd1-f976cb38a3bb");
        for (int j = 0; j < encoded.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "aba74f92-df0d-4740-9167-58dd23a9bca1");
            final Integer i = encoded.codePointAt(j);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "9f603ad2-437e-490b-ab8a-7475fc78eca0");
            final String s = codePointToString(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "04ea0465-06cb-4cec-804c-d169c82b2256");
            if (s.equals(originalToEncoded.get(i))) {
                // length of each encoded char is 1
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "1c01bb40-4034-4af3-a55a-a330fe9352cb");
                result.append(s);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "494632e9-fe00-40c2-ace9-28bc4123f57f");
                // because we do not encode in Unicode extended the
                j++;
            } else {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "d5fef7f7-caa4-4f92-ac31-4557fd31961c");
                if (j + encodedLetterLength > encoded.length()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "eeadf2cf-dd7e-4feb-848f-e400e59d4dff");
                    throw new UnsupportedEncodingException("Unexpected end " + "of string while decoding " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "0184600a-63a4-4170-aea9-d6a066309c09");
                final String nextGroup = encoded.substring(j, j + encodedLetterLength);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "70ba47b4-92ee-4d97-9662-3117dbc2b7ef");
                final String next = encodedToOriginal.get(nextGroup);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "e4f3f072-1a0f-4f75-8560-b5d615a118fd");
                if (next == null) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "06878381-ea1e-4c85-8ea2-4b9c1eb75352");
                    throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "de5194d5-cdc3-4d2c-bdf9-d5e87859e753");
                result.append(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "e5aa279b-593b-4251-8bd9-ce530dc74c13");
                j += encodedLetterLength;
            }
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "9f0acd98-b51e-4319-b02f-ac9f00e0eae5");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "133988f1-4244-4b1f-ac66-25899f18c7b8");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "74d0e84d-2bb6-4d00-8eaa-0f812a622a3a");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "1b4d5729-9e42-440e-a507-b1aadbb858fc");
        if (level > 0) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "8638656f-bb3a-4d02-a384-aca61b987ba2");
            for (final int encodingLetter : encoding) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "23306f53-b1eb-4a3c-8d53-4df1ff33d2e6");
                if (originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "4c1dd3d7-75f7-4db5-9197-557e468d88e1");
                    // leftmost place
                    if (level != encodedLetterLength || !doNotEncodeMap.containsKey(encodingLetter)) {
                        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "50ebc6e2-48eb-4bef-8407-c5f2ecb38775");
                        addSingleEncoding(level - 1, currentEncoding + codePointToString(encodingLetter), encoding, originals, doNotEncodeMap);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "7e9f1bac-7907-492e-8f74-e35e40bd6b06");
                    // done encoding all the original alphabet
                    return;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "ab319712-094e-4f69-b8c0-e7f3e0e6c365");
            Integer next = originals.next();
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "417415e4-18df-40ab-a762-06823de312f8");
            while (doNotEncodeMap.containsKey(next)) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "b4d92d4f-bcaf-4d61-a6ea-4d5ec3e65662");
                final String originalLetterAsString = codePointToString(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "455431ce-a11c-4bd2-b932-9ce513066726");
                originalToEncoded.put(next, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "f1b07f49-0303-461d-b0a7-8ec82ee619fe");
                encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c9ac049e-5568-4ed3-8783-3c9fceac8e61");
                if (!originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "940f57fe-53b5-404d-ad05-3b030468aab3");
                    return;
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "2767cdc3-e938-4c65-9abf-aa1124bcf18f");
                next = originals.next();
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "37905632-ee1c-4d0a-b0a2-82f12e1650fe");
            final String originalLetterAsString = codePointToString(next);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "ee914242-7c20-47d4-87c4-378947fd1154");
            originalToEncoded.put(next, currentEncoding);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "58a05de0-e5cf-4fa1-888f-c570ca45de62");
            encodedToOriginal.put(currentEncoding, originalLetterAsString);
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a44dd333-b5c4-43de-912f-85375f22d23d");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "86485bb4-a3c8-4f50-9f8c-b923059643f2");
        for (final Entry<Integer, String> entry : originalToEncoded.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "6b086baf-18ea-442f-8e60-f832f61f9108");
            sb.append(codePointToString(entry.getKey())).append(ARROW).append(entry.getValue()).append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a58870fd-e010-4bbe-81e7-86c21e351804");
        return sb.toString();
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "f29b73f6-83a7-425a-967b-37ddfec4d3f0");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "daee5245-3d87-46b3-964a-15cd28380005");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "b8369602-cf9e-4798-8837-2fa33e27fff4");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "59b76d2e-34e4-4753-b3f1-69fb62cb741a");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c50cbbe7-eb35-45ac-937b-a044672997ec");
        if (!(obj instanceof AlphabetConverter)) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "3b55a95a-c223-4606-a20a-f58ed4f1bb11");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "eace0a5c-ab25-40b6-bad3-60df5853c828");
        final AlphabetConverter other = (AlphabetConverter) obj;
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "4ca880c7-4d5c-42c2-b669-63be0b639ce4");
        return originalToEncoded.equals(other.originalToEncoded) && encodedToOriginal.equals(other.encodedToOriginal) && encodedLetterLength == other.encodedLetterLength;
    }

    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "988319fc-2a61-45dd-9612-3b299560a817");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "ad0d61cc-9390-4626-a25d-49625091850c");
        final Map<Integer, String> unmodifiableOriginalToEncoded = Collections.unmodifiableMap(originalToEncoded);
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "304f9e12-f460-4d78-91f6-5901faacdf6b");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "e9b8bc4e-fdfa-47e1-8266-6d8c0906e265");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c6b5e692-0c1d-456c-8301-e441ddec8fb6");
        int encodedLetterLength = 1;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "456638be-6f82-47fc-84f0-3d3ea9fe8027");
        for (final Entry<Integer, String> e : unmodifiableOriginalToEncoded.entrySet()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "6b9e131c-cd0e-4165-aca2-76d90233afb4");
            final String originalAsString = codePointToString(e.getKey());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "5171329e-3616-4d7d-8343-2a9f8c829cde");
            encodedToOriginal.put(e.getValue(), originalAsString);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "6ce2f985-041b-4ab5-94c4-e73f9047575e");
            if (e.getValue().equals(originalAsString)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "fd88a320-5573-403e-b9a7-152812413fd3");
                doNotEncodeMap.put(e.getKey(), e.getValue());
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "558da557-4d7d-45d4-9762-a9ee1be202a0");
            if (e.getValue().length() > encodedLetterLength) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "5bd8b3ce-3bb9-41ef-b065-5f2328f47109");
                encodedLetterLength = e.getValue().length();
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a225857c-32e9-45f5-acf2-880a5b82c8d9");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "819462ed-b9b6-4cfd-bf65-69e23c9e41bc");
        return AlphabetConverter.createConverter(convertCharsToIntegers(original), convertCharsToIntegers(encoding), convertCharsToIntegers(doNotEncode));
    }

    /**
     * Convert characters to integers.
     *
     * @param chars array of characters
     * @return an equivalent array of integers
     */
    private static Integer[] convertCharsToIntegers(final Character[] chars) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "121e095d-39c5-4bb5-98e3-cc6916c0ec43");
        if (chars == null || chars.length == 0) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a05e9bd6-6ada-49ff-af8b-325f2ed1b298");
            return new Integer[0];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "620f4ec4-bfc4-4a45-9c3e-2a266d008ef1");
        final Integer[] integers = new Integer[chars.length];
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "eb521237-1807-44e3-be11-9bc552422fd1");
        for (int i = 0; i < chars.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "9216aafe-d396-402c-9fc9-cc5a1fabd308");
            integers[i] = (int) chars[i];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "cd0b4432-d50b-4203-82bd-4230d704e6ab");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "dc1ad8f1-552c-4522-8138-3170643a469f");
        final Set<Integer> originalCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(original));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "133da290-0377-4ea2-a3cb-404b1aa3dbd3");
        final Set<Integer> encodingCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(encoding));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "07ae3aa1-f612-4201-a9fb-28d20e0c336e");
        final Set<Integer> doNotEncodeCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(doNotEncode));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "f60cac20-b80d-4855-922c-6ab85ef38059");
        final Map<Integer, String> originalToEncoded = new LinkedHashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "8a45b6df-8c88-4685-834a-8d8cb193556c");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "cf2f9abf-4ac8-4d2b-af6a-81a9adc32fa0");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c9760b15-21f8-4219-a36c-34804579939c");
        int encodedLetterLength;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "32131d12-d939-4032-aff8-e41c98e96097");
        for (final int i : doNotEncodeCopy) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "ef501afe-ace5-45fe-9c03-6a4f2effb15c");
            if (!originalCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "ba52fd9b-e66e-4bf2-98d7-3c42f09d31fe");
                throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "b85f5e39-c2b2-499c-b8ce-fc80c9171a3d");
            if (!encodingCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "8d75fd91-1773-414a-972b-d0f0aa805088");
                throw new IllegalArgumentException("Can not use 'do not encode' list because encoding alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "90b656be-2129-49cf-a3bc-25948f8e9000");
            doNotEncodeMap.put(i, codePointToString(i));
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "760b2bd9-46ab-4e2d-bfab-0a28cebad74c");
        if (encodingCopy.size() >= originalCopy.size()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "6f076fcb-729b-4cb7-b149-605ca4322105");
            encodedLetterLength = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "4ec892f5-e36b-4459-80e7-58a591b11933");
            final Iterator<Integer> it = encodingCopy.iterator();
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "887a750a-b544-463e-894d-894e353aeff4");
            for (final int originalLetter : originalCopy) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "832396b6-b526-497e-b43c-ce5d564efb63");
                final String originalLetterAsString = codePointToString(originalLetter);
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "4cd97b80-dfe4-4824-9b78-5303d0e8e25a");
                if (doNotEncodeMap.containsKey(originalLetter)) {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "dcae6955-617b-44df-9190-02deeda8ba35");
                    originalToEncoded.put(originalLetter, originalLetterAsString);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "be7fc269-a3c7-49ac-9e82-7a6c09771758");
                    encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "dee97508-557d-4162-aa79-25eef926f24b");
                    Integer next = it.next();
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "237db316-6c67-4445-80b7-97499d9611e5");
                    while (doNotEncodeCopy.contains(next)) {
                        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "11f3a69c-fa4d-42be-934e-11315f94a08b");
                        next = it.next();
                    }
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "86f7cdac-7474-4424-a4a2-1b3397ae309a");
                    final String encodedLetter = codePointToString(next);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c3dd3935-6340-49a7-adc1-8a8e14e4bf2c");
                    originalToEncoded.put(originalLetter, encodedLetter);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "1f1f05c7-b912-4c2b-aa8b-698499b29102");
                    encodedToOriginal.put(encodedLetter, originalLetterAsString);
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "0c012833-a591-4a23-b6d8-74e56fba2d25");
            return new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
        } else if (encodingCopy.size() - doNotEncodeCopy.size() < 2) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a910c382-de38-44ca-ad43-fdb0773bda77");
            throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "5d12ef45-cfae-401c-b13c-195eb3dcb0f0");
            // we start with one which is our minimum, and because we do the
            // first division outside the loop
            int lettersSoFar = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "ca452946-c365-45f3-b278-08c637506b4d");
            // the first division takes into account that the doNotEncode
            // letters can't be in the leftmost place
            int lettersLeft = (originalCopy.size() - doNotEncodeCopy.size()) / (encodingCopy.size() - doNotEncodeCopy.size());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "a991a592-af4f-4397-9547-3c3e7ab70e48");
            while (lettersLeft / encodingCopy.size() >= 1) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "06054057-e1e3-4086-989c-65ff7db75693");
                lettersLeft = lettersLeft / encodingCopy.size();
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "57a9e66d-a15c-439c-a4bf-87c0eb6d4b9c");
                lettersSoFar++;
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c68b1280-d129-45c9-aa98-56e35bd22a44");
            encodedLetterLength = lettersSoFar + 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "38f7a3d2-3aa9-4741-83e3-131197bd7838");
            final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "e9b954c8-1f1b-4376-8769-42052143156e");
            ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "5700ded6-d13a-4b37-9d81-35199af89e99");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "813cfecb-ba4f-4491-aeee-55f6777f674f");
        if (Character.charCount(i) == 1) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "c1039628-e15c-4841-8cf6-e0178999100e");
            return String.valueOf((char) i);
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_8_10.coverage", "352fdc71-a662-4a86-bab8-53bb5ff560ea");
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
