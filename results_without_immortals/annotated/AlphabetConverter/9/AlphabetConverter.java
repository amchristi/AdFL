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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "d68da27a-3ebb-4643-91e1-597a7f15f46c");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "55ed5dba-7af2-4408-a9c7-d239b3b47779");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "8b120a9e-5129-4825-9c36-61411e83cd53");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "7373d38c-1bcd-4079-960a-0f3919c2c940");
        for (int i = 0; i < original.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "892075fe-2172-4373-b090-a5e6211c5cb0");
            final int codepoint = original.codePointAt(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "6b45da45-9a10-476f-bdf6-a4faf85d596f");
            final String nextLetter = originalToEncoded.get(codepoint);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "16516385-0eea-45f0-b332-fb968a456552");
            if (nextLetter == null) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "401aec8f-370c-41f9-ae53-69158a51f3eb");
                throw new UnsupportedEncodingException("Couldn't find encoding for '" + codePointToString(codepoint) + "' in " + original);
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "537608c9-be74-444a-a68b-aa9a370e5411");
            sb.append(nextLetter);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "6314453a-d394-4823-a706-8e023e4c8401");
            i += Character.charCount(codepoint);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "39dc1d20-f328-42dd-9da7-e219268aa8fd");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "1cd84016-578d-45bc-b4ca-8766d42ed295");
        if (encoded == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "c57164c9-d867-4a4e-a8ff-9d637ce05afc");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "1e69cde4-f366-4993-88b9-60281d8dda4c");
        final StringBuilder result = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "5bfac6ad-a6d0-4b74-a2ce-59e6309ae8fa");
        for (int j = 0; j < encoded.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "d19f1d89-d345-4494-af8a-054589239d33");
            final Integer i = encoded.codePointAt(j);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "5fb492a7-b008-4680-bba1-20029334f23e");
            final String s = codePointToString(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "813bd4c2-cdee-47e7-bc93-b899c7117932");
            if (s.equals(originalToEncoded.get(i))) {
                // length of each encoded char is 1
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "8921479b-8bd3-42c5-aa54-a9626f0718d6");
                result.append(s);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "044473c1-5b0a-469a-8632-451d85c09ce8");
                // because we do not encode in Unicode extended the
                j++;
            } else {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "6a47e2d8-318b-4bba-ad91-680c29eb789e");
                if (j + encodedLetterLength > encoded.length()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "a972eb4d-3f65-4a56-9863-a362b12b41d0");
                    throw new UnsupportedEncodingException("Unexpected end " + "of string while decoding " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "38500ae2-b58e-4833-a620-3abb3464990a");
                final String nextGroup = encoded.substring(j, j + encodedLetterLength);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "40935f39-d190-4d13-9946-d0aa2516cdd2");
                final String next = encodedToOriginal.get(nextGroup);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "dccb812b-5c4b-4cc9-95bb-9e7ddb8416e2");
                if (next == null) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "86af34ff-6527-4436-814c-b8785bac643d");
                    throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "4f6c0a00-83e0-44f4-af5e-7557e65866e4");
                result.append(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "87def795-2f0f-4ff9-8768-b2b72177cf13");
                j += encodedLetterLength;
            }
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "10ea666d-12cb-4828-8728-c73196da7a60");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "7a8aec49-46eb-4e66-bcb5-b34690f81e44");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b2641513-bfe1-4ffb-9032-bec987d9bd19");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "de05898b-e74a-4865-8655-96b8129394e0");
        if (level > 0) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "941e8a84-755d-4c30-a41d-5087acdcd9c0");
            for (final int encodingLetter : encoding) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "0d07ee10-aa98-4d59-b0d0-7609281b712d");
                if (originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "4f4da80a-f05b-4ac6-be33-78d61617c648");
                    // leftmost place
                    if (level != encodedLetterLength || !doNotEncodeMap.containsKey(encodingLetter)) {
                        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "a6cd93df-ef53-4592-a855-59b2283b052e");
                        addSingleEncoding(level - 1, currentEncoding + codePointToString(encodingLetter), encoding, originals, doNotEncodeMap);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "7e769d2b-37cc-43dc-b227-87fd5430213b");
                    // done encoding all the original alphabet
                    return;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "1ff663d1-ef1b-493b-aeb4-668b10736519");
            Integer next = originals.next();
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "d3d80c41-8e8b-4778-a494-f85916ab75d1");
            while (doNotEncodeMap.containsKey(next)) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "67af37a4-9035-493b-81fb-f18569ac2667");
                final String originalLetterAsString = codePointToString(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ede93fa9-b54e-4ac6-b703-923f7aa62bf0");
                originalToEncoded.put(next, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "7db0d617-8643-417e-be14-8a34fda3d059");
                encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "d1103012-c132-444d-809e-f8a43c722571");
                if (!originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "34e5bfe2-1936-42fb-95ca-57f7bbc7b3c0");
                    return;
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ee3e522a-7951-4367-baa8-ed8c5233a24e");
                next = originals.next();
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ad8f8cab-cd28-4ef7-a9ba-2527c0394681");
            final String originalLetterAsString = codePointToString(next);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "fd717d39-2a4e-4ef1-b413-ffe81260e105");
            originalToEncoded.put(next, currentEncoding);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "56451f99-0c43-4798-823d-3f91ac546846");
            encodedToOriginal.put(currentEncoding, originalLetterAsString);
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "e90e7064-70c3-4314-b9f3-6840cd0163dd");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "c4cee295-be42-4974-afe9-77b9f8eb717f");
        for (final Entry<Integer, String> entry : originalToEncoded.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "dc6a27f1-1c1d-4777-ae06-6787a7f5e07c");
            sb.append(codePointToString(entry.getKey())).append(ARROW).append(entry.getValue()).append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "292531cf-8155-4588-9846-4bd516c8c7b4");
        return sb.toString();
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "9178bc95-4780-470b-ba13-c88064cf4a69");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "41d2c202-8581-482f-aef5-034156c6c475");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "38057f0d-02ec-4c12-a6da-8674be1b501a");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "89715049-b501-49aa-a37f-1198e793013c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "533a70d5-2cb6-478c-b06f-d9c110cb69d9");
        if (!(obj instanceof AlphabetConverter)) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "f6554c54-3813-4394-87c0-55b5daaa613d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b35ad8b8-b942-4a96-badd-cfdb9d53433d");
        final AlphabetConverter other = (AlphabetConverter) obj;
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "24cee9be-2152-468e-8d01-1dadfdf2d422");
        return originalToEncoded.equals(other.originalToEncoded) && encodedToOriginal.equals(other.encodedToOriginal) && encodedLetterLength == other.encodedLetterLength;
    }

    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "21a77419-5d94-4c06-a2d0-c1937369e93f");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "692acee7-e756-4983-bcf4-b5bf07233d98");
        final Map<Integer, String> unmodifiableOriginalToEncoded = Collections.unmodifiableMap(originalToEncoded);
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "3397e13d-8141-4e2d-a5f3-35adc379b6f8");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "0831409b-8178-4b4f-a951-d981617587af");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "43ff9730-7673-46c2-81f4-293c74200e2c");
        int encodedLetterLength = 1;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "0d732087-77e1-485b-9865-4d5404dbeffb");
        for (final Entry<Integer, String> e : unmodifiableOriginalToEncoded.entrySet()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "c2d9f8df-001b-4662-a4a0-a52469f80ee3");
            final String originalAsString = codePointToString(e.getKey());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "f45d7989-0ef5-48b7-b295-64cf4f9fba25");
            encodedToOriginal.put(e.getValue(), originalAsString);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ebf2374f-eefa-4173-a974-92aabcb0f3ec");
            if (e.getValue().equals(originalAsString)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "22cb7ca7-8847-4a95-af4c-a317748b22fd");
                doNotEncodeMap.put(e.getKey(), e.getValue());
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "c79d499d-79d6-46ea-a04e-2b3a6e1c39ce");
            if (e.getValue().length() > encodedLetterLength) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "c9a32e97-a0a6-498e-9d31-60680fad71b6");
                encodedLetterLength = e.getValue().length();
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "33399e3e-4b26-4b3a-a0c1-def94f5b1c7e");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "be0e5965-2212-48e1-ac15-1d6f4492a736");
        return AlphabetConverter.createConverter(convertCharsToIntegers(original), convertCharsToIntegers(encoding), convertCharsToIntegers(doNotEncode));
    }

    /**
     * Convert characters to integers.
     *
     * @param chars array of characters
     * @return an equivalent array of integers
     */
    private static Integer[] convertCharsToIntegers(final Character[] chars) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "d7eee39c-b273-4a5b-8a5a-4ddb5e085cd7");
        if (chars == null || chars.length == 0) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "96cf32af-0395-4dea-a070-c2162b1e0a84");
            return new Integer[0];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "27f6b2b5-9eeb-4294-84af-9e999d8db27f");
        final Integer[] integers = new Integer[chars.length];
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "3be9e550-98c3-4552-8f5b-74aa28d25358");
        for (int i = 0; i < chars.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "438a0b06-ae84-4e0d-bf1f-cc2e30835dda");
            integers[i] = (int) chars[i];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "089cb096-f757-49a6-ae2a-9095275ccc9a");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "0ea62d30-affe-4782-9bd2-5556fa200fc0");
        final Set<Integer> originalCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(original));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "86ca2a09-b065-44fa-8200-f12b01122033");
        final Set<Integer> encodingCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(encoding));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "7559e386-b0b6-4f36-8803-cc22217b5b95");
        final Set<Integer> doNotEncodeCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(doNotEncode));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "e244663f-e120-475e-9b8b-f2f875c2d075");
        final Map<Integer, String> originalToEncoded = new LinkedHashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "535d4d0f-44e8-49fa-9286-0ab157d4294d");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b9c3ec3e-8d0b-4a30-a170-24bbeb4765fa");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "9336e95e-0993-420b-9027-c64d37443049");
        int encodedLetterLength;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "090b4007-d70a-414f-b46e-19ca3aa648ec");
        for (final int i : doNotEncodeCopy) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "69120f35-0f79-48c8-a805-4cea25fca03a");
            if (!originalCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "fa76de2f-d252-44db-a719-01caafad3296");
                throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "99665a98-0ba9-4fe3-bcdd-0a1f249b4d03");
            if (!encodingCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "d0eef305-1d80-40e3-874f-dd2bd21ad253");
                throw new IllegalArgumentException("Can not use 'do not encode' list because encoding alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "8e2d7e49-514a-4526-be1d-141d0496778b");
            doNotEncodeMap.put(i, codePointToString(i));
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "f97f163e-5771-411f-adb4-c703eab7a6d3");
        if (encodingCopy.size() >= originalCopy.size()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ce201ee7-117b-478d-9372-a23c0e30d695");
            encodedLetterLength = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "bc7f302f-f3b2-497d-8ae1-582b30cf6ac5");
            final Iterator<Integer> it = encodingCopy.iterator();
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "926b7802-82ef-453a-b3b2-2dcd512ef7e4");
            for (final int originalLetter : originalCopy) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "261a0911-2ff5-499d-9e34-e9da55c52614");
                final String originalLetterAsString = codePointToString(originalLetter);
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ffbe4590-da18-44bc-a2e6-da47906b91e0");
                if (doNotEncodeMap.containsKey(originalLetter)) {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "2b741e14-a0e5-4158-81dc-d3c7a7e60383");
                    originalToEncoded.put(originalLetter, originalLetterAsString);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "0d962c58-84a7-4d0d-88cc-b03cf0caf218");
                    encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b4114851-2f39-45c7-a179-60790dd6fe02");
                    Integer next = it.next();
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "0f51d5b1-14b6-4ef2-9a22-92beb492ded0");
                    while (doNotEncodeCopy.contains(next)) {
                        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b86c2703-6e4a-4a4d-905f-419b0351682b");
                        next = it.next();
                    }
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "990fbd0a-0622-44f6-96b8-743b3589d960");
                    final String encodedLetter = codePointToString(next);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b7b9f4e2-b438-4c3c-a3ad-2b545b4704d2");
                    originalToEncoded.put(originalLetter, encodedLetter);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "b4998ad9-70af-4002-ba84-ff0f5d62ca06");
                    encodedToOriginal.put(encodedLetter, originalLetterAsString);
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "ef4ee017-de94-49fe-8060-256187c5cdea");
            return new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
        } else if (encodingCopy.size() - doNotEncodeCopy.size() < 2) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "9b1f6a93-71bc-4a61-b571-fd5504bcb9e5");
            throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "99273eb1-9caa-4dd4-ba41-e380d00291c4");
            // we start with one which is our minimum, and because we do the
            // first division outside the loop
            int lettersSoFar = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "9b15c70b-047f-4bb6-bb4d-0345fa2eae5f");
            // the first division takes into account that the doNotEncode
            // letters can't be in the leftmost place
            int lettersLeft = (originalCopy.size() - doNotEncodeCopy.size()) / (encodingCopy.size() - doNotEncodeCopy.size());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "3885a136-66eb-45a6-8069-1ece9a4740b2");
            while (lettersLeft / encodingCopy.size() >= 1) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "09406ed7-371b-48d3-b0af-1b1669ec7f88");
                lettersLeft = lettersLeft / encodingCopy.size();
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "129e50d5-518e-485b-a1ae-92e0c7629869");
                lettersSoFar++;
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "dd336a3b-09bf-4576-9b5e-8a3fde36c492");
            encodedLetterLength = lettersSoFar + 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "2bf7ab6d-aa45-4113-8ac3-f075452e6781");
            final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "83a158d9-2921-47a8-a361-1e9b08678950");
            ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "53a9fcc4-b0b2-443d-ab3d-f2b1317b9dac");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "600467df-7d9e-4856-b5eb-55b674c219a2");
        if (Character.charCount(i) == 1) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "de5aaab6-679f-4a02-86c1-286aedba1db8");
            return String.valueOf((char) i);
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_9_10.coverage", "c56793ef-fb02-46a0-a77f-91fd1f77699d");
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
