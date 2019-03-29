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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "4c4bac8b-3c89-49af-b13c-586365615046");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "9108ad26-1496-48ef-861f-fb042d96373f");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "84ab08b0-af6b-4f34-9c46-aa65bed0d34f");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "ae0700c0-f486-48ce-a45b-2f0d427ff73b");
        for (int i = 0; i < original.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7dacff18-bf6b-4229-9804-4583a2af3a42");
            final int codepoint = original.codePointAt(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7347e624-bdde-418c-8404-66d45eb8d47f");
            final String nextLetter = originalToEncoded.get(codepoint);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "dec81788-6317-41c1-bc5b-45ac5fb4b8f5");
            if (nextLetter == null) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "a95cad12-e0a1-438a-b3a6-990d238b9edf");
                throw new UnsupportedEncodingException("Couldn't find encoding for '" + codePointToString(codepoint) + "' in " + original);
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "cb473485-a19f-4493-9e65-bed7c5080628");
            sb.append(nextLetter);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "0f3fb065-000e-4a8c-9e9d-faae3e2bd6de");
            i += Character.charCount(codepoint);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "4f7ae94a-6a53-423e-b5d6-40721935d65c");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "135fd4a9-7f04-4fc0-8af3-d040b5b472bd");
        if (encoded == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b861cf88-0dbe-4fd3-b8b7-9f8ffb9b846c");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "2776c243-1e3c-4e4d-bd52-ca559c981916");
        final StringBuilder result = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "37c8ae52-be9f-4dcd-83cf-a5175685b940");
        for (int j = 0; j < encoded.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "2da37d2a-565b-4109-8cb9-b0aad5387793");
            final Integer i = encoded.codePointAt(j);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "a876f8d5-0103-4e0f-bbb7-7e460e71fbd8");
            final String s = codePointToString(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b11fad7f-aa2e-46ea-a26d-08f507708288");
            if (s.equals(originalToEncoded.get(i))) {
                // length of each encoded char is 1
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "dcd145a9-8c1a-4daa-b821-40f1a4489d46");
                result.append(s);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c4b6f403-d4e1-4b44-aa76-0e65ffc1dc9c");
                // because we do not encode in Unicode extended the
                j++;
            } else {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "6df75788-a83a-4302-969a-60f5f24d52ea");
                if (j + encodedLetterLength > encoded.length()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7d2eeac3-608f-4634-a556-bc180ff25ccb");
                    throw new UnsupportedEncodingException("Unexpected end " + "of string while decoding " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c23df2ba-d5cb-4be5-b87b-af278f8ac47b");
                final String nextGroup = encoded.substring(j, j + encodedLetterLength);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "323259ba-eb03-4166-974d-74e252026933");
                final String next = encodedToOriginal.get(nextGroup);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "3b454a5a-ff7e-4e6a-a740-78cff7bd052f");
                if (next == null) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "149bf41b-d4c7-4062-b0a5-b67becc42eb8");
                    throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "ec6e7889-e122-4283-b447-770584962bd4");
                result.append(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b1e1fb8c-947f-49d1-9592-8d03edb13c48");
                j += encodedLetterLength;
            }
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "99d73657-b5ce-4fb6-afc5-78f927424cfa");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "562f4fed-646c-4ece-9549-be2cabfff603");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c73d64bb-eaf2-43f6-898b-209e971cdc0f");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "f82b68cc-4c8e-4878-958f-5e1486a5de67");
        if (level > 0) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "fd836267-69e5-468b-b53e-7ee28246a628");
            for (final int encodingLetter : encoding) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "76c6f8cd-82ce-4eae-9ee3-112f7e63b2d0");
                if (originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "9186b9b5-d12b-4e44-9eac-9e06b3cb4258");
                    // leftmost place
                    if (level != encodedLetterLength || !doNotEncodeMap.containsKey(encodingLetter)) {
                        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7aad28c2-0104-4eb2-8501-2a16cdfba701");
                        addSingleEncoding(level - 1, currentEncoding + codePointToString(encodingLetter), encoding, originals, doNotEncodeMap);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "5c463e64-cd3b-41d0-bbb3-45c90b4fb413");
                    // done encoding all the original alphabet
                    return;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c4ac2703-83ee-46dc-ab91-f2ecbb2c3342");
            Integer next = originals.next();
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "13cb40f4-37c4-4be6-a1a4-f91c3bd9dc3d");
            while (doNotEncodeMap.containsKey(next)) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "a8beae87-7eb7-4651-8307-04effcf8521a");
                final String originalLetterAsString = codePointToString(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "414d5b63-2635-4e22-bf36-cc76d5a66297");
                originalToEncoded.put(next, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "26bccc84-936a-4c8a-acf2-6ca4e320b9dc");
                encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "af414800-2670-475d-a579-464a0e05668e");
                if (!originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "98b3c3a8-367b-45cb-af9c-dee1dc0cc686");
                    return;
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "ae906a80-bfdb-4a72-bbe9-0bff4db4a4f9");
                next = originals.next();
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "d3c2ab31-634d-49a9-b83c-3aa520c40c73");
            final String originalLetterAsString = codePointToString(next);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b0dcf870-718f-4bfe-b7e8-6dd7f5a320cf");
            originalToEncoded.put(next, currentEncoding);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "07f28411-1733-4abd-974a-e7ec85e0e132");
            encodedToOriginal.put(currentEncoding, originalLetterAsString);
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "4af6c52b-eeb8-4c15-b85c-99f333506f9e");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "032b5c7d-91d9-45ab-93f8-fc44dc9389d8");
        for (final Entry<Integer, String> entry : originalToEncoded.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b4c6259e-7c8f-4ea6-a5cf-b57ed17f9b2b");
            sb.append(codePointToString(entry.getKey())).append(ARROW).append(entry.getValue()).append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "8945c079-5b01-43ca-b645-96464773512e");
        return sb.toString();
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "4f7de92f-3af4-4b56-a562-60e5aef063f7");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "6c6a14eb-9084-4ecc-81be-56b6fae6e00a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c8c275f6-ab08-464e-81c4-1b6bee38908f");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "15fbb5ab-3787-433d-a9c6-cdf829ffb53f");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "0fd4ffc2-63f7-40fd-aea4-89ce4fe5a59b");
        if (!(obj instanceof AlphabetConverter)) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b56645cf-0f30-4a63-a63b-1688ad06203d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "b7bf365e-fe8d-410a-9474-428853b688f3");
        final AlphabetConverter other = (AlphabetConverter) obj;
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "06a61c00-44ee-4f59-a8d8-fb9a63a3ae65");
        return originalToEncoded.equals(other.originalToEncoded) && encodedToOriginal.equals(other.encodedToOriginal) && encodedLetterLength == other.encodedLetterLength;
    }

    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "042385a8-a860-4070-b4c1-b7ed5f7c8015");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "dd8b2519-321d-4dfa-b223-699c9b7a4c06");
        final Map<Integer, String> unmodifiableOriginalToEncoded = Collections.unmodifiableMap(originalToEncoded);
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "324f504a-6ff0-4894-8e3f-d4fbfae2bce3");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "a30749a8-d12e-4a5b-bf76-c5f3c9cf9bcc");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "dc91c860-843e-4fef-8844-22e3ddb2870a");
        int encodedLetterLength = 1;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "81b38d39-5820-4c4b-b6ef-d35059f318c0");
        for (final Entry<Integer, String> e : unmodifiableOriginalToEncoded.entrySet()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "dbfb4e17-fd4e-4808-aadb-e3e1da48551e");
            final String originalAsString = codePointToString(e.getKey());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "e53d0871-2619-48a8-89bc-03b758b23e25");
            encodedToOriginal.put(e.getValue(), originalAsString);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "66cfef93-d929-4394-a1ec-88ff3a3ccd48");
            if (e.getValue().equals(originalAsString)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "46500035-8f22-4fa8-8369-380c28c8d19b");
                doNotEncodeMap.put(e.getKey(), e.getValue());
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "f57d6c07-8870-47d3-bdf4-996712142228");
            if (e.getValue().length() > encodedLetterLength) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "2a35058e-6a56-4538-b510-50b258c153ed");
                encodedLetterLength = e.getValue().length();
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "6a8ee989-e807-4de3-b95f-e4665f0a7db7");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7efbc7a3-4e98-4d7c-ac16-490d4dc52e20");
        return AlphabetConverter.createConverter(convertCharsToIntegers(original), convertCharsToIntegers(encoding), convertCharsToIntegers(doNotEncode));
    }

    /**
     * Convert characters to integers.
     *
     * @param chars array of characters
     * @return an equivalent array of integers
     */
    private static Integer[] convertCharsToIntegers(final Character[] chars) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "079704ae-1f85-4bed-a439-1e0b8bf0ebfb");
        if (chars == null || chars.length == 0) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "8399b793-44d7-4397-8447-6e901ca64511");
            return new Integer[0];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "ac063ed1-cb4a-41e3-8e65-80774d322308");
        final Integer[] integers = new Integer[chars.length];
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "a98ca6ed-8dfb-4656-9ae0-254f3a5ff43c");
        for (int i = 0; i < chars.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c07f0978-a75a-4607-b5c9-6ac6ecd194bc");
            integers[i] = (int) chars[i];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7fa1f7e0-b3b6-4018-80c3-b1a18079216d");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "3aa8c61f-d8af-4f62-b81c-4983f6b566ec");
        final Set<Integer> originalCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(original));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "50dc9e8c-5af3-44b3-80af-6adbad559276");
        final Set<Integer> encodingCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(encoding));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "1aec4c25-adc8-4aad-9989-30d8d82a5ff3");
        final Set<Integer> doNotEncodeCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(doNotEncode));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "01ee43dd-49b1-4c86-a3d7-99a2904db0cb");
        final Map<Integer, String> originalToEncoded = new LinkedHashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "797724fd-b940-4927-9478-834bc7cd6ee0");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "afe9be4d-6074-47ff-b2d4-f58688a0a9d5");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "db723512-870b-4674-b116-d99889d812f4");
        int encodedLetterLength;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "570802df-5f52-4a5d-bef4-036674f67d65");
        for (final int i : doNotEncodeCopy) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "91a98039-4fde-4707-b10b-2db30a01a17f");
            if (!originalCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "f0950cd9-e648-4e3d-bfcf-e34a8274d999");
                throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "ecc55eb6-2310-47b5-9e35-0f4ac0d01b09");
            if (!encodingCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "2222e157-a2a8-40db-b6df-e92dbfd3790e");
                throw new IllegalArgumentException("Can not use 'do not encode' list because encoding alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "91bad58b-e3ed-4546-bc86-198ee9d78796");
            doNotEncodeMap.put(i, codePointToString(i));
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "81940a47-971f-47e8-8166-de9a71dac6aa");
        if (encodingCopy.size() >= originalCopy.size()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "0fb62ba0-7a80-4fee-9f9c-710202038a04");
            encodedLetterLength = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "0133ce8c-07d9-40a8-88e6-61fe10757f2f");
            final Iterator<Integer> it = encodingCopy.iterator();
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "0e56ba12-9353-4bae-9289-a2c63a7fff87");
            for (final int originalLetter : originalCopy) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "02aeab8f-0cfb-4d61-ae6b-9b6d326d7cf9");
                final String originalLetterAsString = codePointToString(originalLetter);
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "ca632170-090a-43c4-9cb7-cf2586b8ac6b");
                if (doNotEncodeMap.containsKey(originalLetter)) {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "bdab8b9c-f386-40cc-b1c4-49394c04f2ac");
                    originalToEncoded.put(originalLetter, originalLetterAsString);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "6a771b67-d08c-4ad2-aac4-b6a8078a6d0f");
                    encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "6a56c5e1-a914-4390-9afc-d80276aed3ea");
                    Integer next = it.next();
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "5ddb8b2a-c5f9-4d92-9e23-a494dcf409eb");
                    while (doNotEncodeCopy.contains(next)) {
                        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "eba179c0-ee1a-438b-a82e-641ea74dc551");
                        next = it.next();
                    }
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "4d53302c-14d5-41e5-aa51-0834e84b8a39");
                    final String encodedLetter = codePointToString(next);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "8514b1d8-59a9-47f7-b6aa-d5f8cd92dffd");
                    originalToEncoded.put(originalLetter, encodedLetter);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "312f90e5-effe-4044-a9ba-8deebbfa1664");
                    encodedToOriginal.put(encodedLetter, originalLetterAsString);
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "5780ba43-121c-4667-bafc-ef91a8fc1c03");
            return new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
        } else if (encodingCopy.size() - doNotEncodeCopy.size() < 2) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "f2416f9d-6b80-4f6f-9be0-5943293a67aa");
            throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "7a956752-9d9c-4c0d-ad3f-3db1000ebffb");
            // we start with one which is our minimum, and because we do the
            // first division outside the loop
            int lettersSoFar = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "5f691fe0-2017-4799-b04d-a87475732f50");
            // the first division takes into account that the doNotEncode
            // letters can't be in the leftmost place
            int lettersLeft = (originalCopy.size() - doNotEncodeCopy.size()) / (encodingCopy.size() - doNotEncodeCopy.size());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "9226e869-5df9-4867-bdcf-70fc1c46f87e");
            while (lettersLeft / encodingCopy.size() >= 1) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "e657d04a-9200-45d8-80cd-2bd3ed38abfb");
                lettersLeft = lettersLeft / encodingCopy.size();
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "04b42abd-7d83-48c7-a253-d769a1c3fe40");
                lettersSoFar++;
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "92f2c8d3-cecf-4908-a790-31dc93383dbf");
            encodedLetterLength = lettersSoFar + 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "9b571f62-a006-41a8-b704-791cc0d757f7");
            final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "c3bfd883-91c6-4cba-b0ca-305b38b4a618");
            ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "afe70adc-f3a9-4f38-aef6-8ce250056309");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "321e415c-ee8a-4b28-8b97-08eb836378af");
        if (Character.charCount(i) == 1) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "926e821b-f8c8-4f03-be63-33c45c0ec7f8");
            return String.valueOf((char) i);
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_3_10.coverage", "abd7dfca-32dc-4851-a5ba-21aa8861b552");
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
