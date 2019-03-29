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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "6fb95f1f-15a5-40a8-aed5-138ebd072265");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "f7775a45-5e80-4c6a-9b8c-b7ea35fde07e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "93821a71-d20b-4eeb-b3f4-82a8ce97866c");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "cbbd5656-b273-4e2a-9c24-0c93d9b9b3a1");
        for (int i = 0; i < original.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "553a047b-09ea-41c9-bd17-ec13d614ef70");
            final int codepoint = original.codePointAt(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "c24a9104-cb9d-42d1-8b0c-c8e9e200de96");
            final String nextLetter = originalToEncoded.get(codepoint);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "364bfb61-4dd8-461f-b99c-a31c647229e8");
            if (nextLetter == null) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "7ef70993-c27c-433e-8fb5-95d491f55cc7");
                throw new UnsupportedEncodingException("Couldn't find encoding for '" + codePointToString(codepoint) + "' in " + original);
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "30402784-9c26-448f-9e95-a6151c33713b");
            sb.append(nextLetter);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "0a8a8e4d-b7b5-4726-a815-5e2d49f7f294");
            i += Character.charCount(codepoint);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "fe5d2fe9-3644-4ff8-8462-9f3d80dbb9e1");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "8b446d1b-6094-424c-80af-bd4192a7fc53");
        if (encoded == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ff43fc54-f9c0-4f1e-a810-aff8c952da9f");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "58e9e005-63d1-4674-be14-5fb69f11e809");
        final StringBuilder result = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "bf243ebd-8323-4a6b-8995-8108f1cbb87e");
        for (int j = 0; j < encoded.length(); ) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "a1410390-2177-4860-bc35-e2c5d404ada3");
            final Integer i = encoded.codePointAt(j);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "f350f08f-a901-4bf7-a306-e02622d5920c");
            final String s = codePointToString(i);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "48917421-f56b-4b4e-b98f-99ee74d8d95f");
            if (s.equals(originalToEncoded.get(i))) {
                // length of each encoded char is 1
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "d384c169-a16c-41a0-a423-14dceebf630e");
                result.append(s);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "91cbd157-0db4-4886-9de8-f7fce219a0b2");
                // because we do not encode in Unicode extended the
                j++;
            } else {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5bd1ca34-8811-4aa6-9718-919cc3380d65");
                if (j + encodedLetterLength > encoded.length()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "9606b2ba-2f25-4e4b-b33f-32686ec1388c");
                    throw new UnsupportedEncodingException("Unexpected end " + "of string while decoding " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "dc530b01-aaef-483e-9d3a-c99583c8ad1d");
                final String nextGroup = encoded.substring(j, j + encodedLetterLength);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "9bcdfd17-8424-4b5e-995b-96a4bc82f966");
                final String next = encodedToOriginal.get(nextGroup);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ded6c366-215e-4a30-8a8b-f8ad8c9c96f4");
                if (next == null) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "9d3430c7-eea4-4bee-940c-06074a958bc4");
                    throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "a42ad889-1819-4b79-ad83-df11e9c0026a");
                result.append(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "8a0492f9-42b7-4ff9-af1a-28a58ddaa1da");
                j += encodedLetterLength;
            }
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "1d864a64-3f06-4e78-b4d2-c5eba1d2ad02");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "7db3d36a-b76f-4c58-934b-031039f39457");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "601d840b-091d-412d-b097-01320550b2f1");
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
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "94ec44fa-5807-4e27-9b5e-ec98661ce048");
        if (level > 0) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "6000ba6c-698b-4f19-a2da-0d3a2e471cb3");
            for (final int encodingLetter : encoding) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "bbe38f2f-388e-423b-a82c-81bff20b6ba7");
                if (originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "891d1279-2233-4b7e-8821-22664e07d60d");
                    // leftmost place
                    if (level != encodedLetterLength || !doNotEncodeMap.containsKey(encodingLetter)) {
                        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "770bb229-5c7b-4605-b015-ba9a0316150e");
                        addSingleEncoding(level - 1, currentEncoding + codePointToString(encodingLetter), encoding, originals, doNotEncodeMap);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ad3f2646-d94a-4b0d-8d9b-fec08edbf2e6");
                    // done encoding all the original alphabet
                    return;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "e9a6781e-3f4d-4597-bd22-06e6ef5094f4");
            Integer next = originals.next();
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ba97a047-8c78-4ae7-98f9-aefed11bf2e7");
            while (doNotEncodeMap.containsKey(next)) {
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "bfa4ca59-de42-4ac4-a056-824d11e6fb84");
                final String originalLetterAsString = codePointToString(next);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "b37fa5d2-8a8a-40cc-a851-f54f543b1319");
                originalToEncoded.put(next, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "90c9ac2d-c4f5-4475-95d0-28ea62083248");
                encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "2ac4c1ad-a8c1-4719-9446-296d238865dd");
                if (!originals.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "289a8038-f223-44de-8fbe-68e5b6ba75fc");
                    return;
                }
                writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "2bdd0a65-8c07-44ec-b6b7-a2e1ea85d3d5");
                next = originals.next();
            }
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5509c7ed-dc14-4edb-89d8-f4cc1e0fe747");
            final String originalLetterAsString = codePointToString(next);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ac5f7716-3182-4949-bbcd-99e02f7fceb3");
            originalToEncoded.put(next, currentEncoding);
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5ced1895-e09c-46ec-9d7d-72a970e051a3");
            encodedToOriginal.put(currentEncoding, originalLetterAsString);
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "3c1664c0-eb89-4468-ad45-f58fa9bda31c");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "133d8de4-9b97-4135-8866-dbc4c00e65b8");
        for (final Entry<Integer, String> entry : originalToEncoded.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "34622a85-56dc-46a4-8d02-9b7ef3632913");
            sb.append(codePointToString(entry.getKey())).append(ARROW).append(entry.getValue()).append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "e205efc6-f962-4ada-b587-f4b665a3a620");
        return sb.toString();
    }

    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "1c799386-f5c5-4f3e-82d0-9f7d26963cc8");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "a00eb322-866e-45ec-9c19-091131f87194");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "e8490299-fada-40cf-aba4-e423448ffed5");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "3a68ae9e-3a2e-419e-823d-513782c5490d");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "93336887-9c72-4920-bc6e-72e3f926b882");
        if (!(obj instanceof AlphabetConverter)) {
            writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "a58d7d5c-73d3-4270-ae18-6ba924e123e3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "2c3caa98-0be5-473f-a634-9745e81a2a4a");
        final AlphabetConverter other = (AlphabetConverter) obj;
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "f76008b2-461b-4184-9e71-bb773bd149fb");
        return originalToEncoded.equals(other.originalToEncoded) && encodedToOriginal.equals(other.encodedToOriginal) && encodedLetterLength == other.encodedLetterLength;
    }

    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5be47878-878d-4a9d-8976-d175c4f4f376");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "3f587ea3-6763-45e6-9221-a9debe7941ea");
        final Map<Integer, String> unmodifiableOriginalToEncoded = Collections.unmodifiableMap(originalToEncoded);
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "42682a89-b9ab-4967-a6a6-2c22147f1383");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "b17d2e6d-2bf4-46d7-8c82-6ddd7b97d09e");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "6f092dcf-86e0-492d-95fb-15b3da87741d");
        int encodedLetterLength = 1;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ab735fa3-8e86-492f-a603-1b7c6a59ab7b");
        for (final Entry<Integer, String> e : unmodifiableOriginalToEncoded.entrySet()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "120b81bb-a16a-49d2-875f-fb79203dbeba");
            final String originalAsString = codePointToString(e.getKey());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "198c86e1-96a6-41f3-a64f-37e875fa10ad");
            encodedToOriginal.put(e.getValue(), originalAsString);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "7abfda71-ae87-4bc9-baa2-ec5cf04f3628");
            if (e.getValue().equals(originalAsString)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "9dd7a7d4-7cfe-47fb-808c-a7f48ee840ce");
                doNotEncodeMap.put(e.getKey(), e.getValue());
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "b91b89f1-1278-40b7-ab06-14eefb2703af");
            if (e.getValue().length() > encodedLetterLength) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "2f3241ed-3104-4c5c-ba40-0203e3a82f36");
                encodedLetterLength = e.getValue().length();
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "49c7b115-763d-432f-9877-ed552d6e66ea");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "3073c647-35f2-413d-925b-5c7f89351f85");
        return AlphabetConverter.createConverter(convertCharsToIntegers(original), convertCharsToIntegers(encoding), convertCharsToIntegers(doNotEncode));
    }

    /**
     * Convert characters to integers.
     *
     * @param chars array of characters
     * @return an equivalent array of integers
     */
    private static Integer[] convertCharsToIntegers(final Character[] chars) {
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "2ab4914c-af38-435d-84a4-44b5b3424ff5");
        if (chars == null || chars.length == 0) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "4e185be4-9add-49bb-96f2-0e9a22d52856");
            return new Integer[0];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "49dfae96-8732-408e-8750-2932df3095e7");
        final Integer[] integers = new Integer[chars.length];
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "d1ef4a25-4f1a-496d-9bc7-e65eda71f31c");
        for (int i = 0; i < chars.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "e8bfc9c3-174b-4bd0-8325-9c1b0b92a9cf");
            integers[i] = (int) chars[i];
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ecd46cda-6dba-46ef-92ac-bd8d3711f9b1");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "0998d860-530a-4a1f-94db-6b6371c9529e");
        final Set<Integer> originalCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(original));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "67754a78-5931-4add-bd2b-4a0a4ce82364");
        final Set<Integer> encodingCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(encoding));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "3c81eef5-5747-4acb-98a8-2cc9c4059811");
        final Set<Integer> doNotEncodeCopy = new LinkedHashSet<Integer>(Arrays.<Integer>asList(doNotEncode));
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "b47fdd33-3d67-4a1e-be52-8f72994623d4");
        final Map<Integer, String> originalToEncoded = new LinkedHashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "890195d5-ab2f-477d-9c3f-0695776bfe69");
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "9ffb04ce-4911-4e47-bc0b-d650e2ad820b");
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "21344e04-5e82-4ea0-b8a6-e2d993af431c");
        int encodedLetterLength;
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "c345f40f-c18c-4db5-9118-03fa65c46a62");
        for (final int i : doNotEncodeCopy) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ef6aab28-f08d-4e82-88ef-97718cc74c2a");
            if (!originalCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5fd769d6-6604-4fdd-9661-3315c8221fbb");
                throw new IllegalArgumentException("Can not use 'do not encode' list because original " + "alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5b6f1c16-dc23-4686-a0f3-600418dad399");
            if (!encodingCopy.contains(i)) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "67ebbede-0819-438a-a9b1-1325e6b1c0bd");
                throw new IllegalArgumentException("Can not use 'do not encode' list because encoding alphabet does not contain '" + codePointToString(i) + "'");
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "c8d3cd5e-959b-4496-bb3b-aed72c2e268a");
            doNotEncodeMap.put(i, codePointToString(i));
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "6d694f71-416f-4de4-9290-47734cc5433f");
        if (encodingCopy.size() >= originalCopy.size()) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "8eed9650-40b7-45b2-8d55-c2717cf0512b");
            encodedLetterLength = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "30e0407a-715f-4e59-8f2e-5afe0790352e");
            final Iterator<Integer> it = encodingCopy.iterator();
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "95e35daf-daf8-46cd-b4c2-46869176b66e");
            for (final int originalLetter : originalCopy) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "f1d33662-05bc-43b9-8584-f304ed063269");
                final String originalLetterAsString = codePointToString(originalLetter);
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "94feb0a2-411a-4bf7-aee5-134babc9f91b");
                if (doNotEncodeMap.containsKey(originalLetter)) {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "08e4982e-7131-47ac-b05b-6073bee7bde2");
                    originalToEncoded.put(originalLetter, originalLetterAsString);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "75d1e09e-958f-4512-9de1-d05f50993d6e");
                    encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "ea2636e1-0e31-4d50-8799-658ad0563b91");
                    Integer next = it.next();
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "87b62b16-e3e1-498b-98b6-40024358fa72");
                    while (doNotEncodeCopy.contains(next)) {
                        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5e1d4640-bda2-46d3-b102-2e31955b4400");
                        next = it.next();
                    }
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "d069443b-dc8f-4233-90e1-ce294c07c3bc");
                    final String encodedLetter = codePointToString(next);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "8fac6ae7-69af-40a3-8a10-a7748ec0a8e6");
                    originalToEncoded.put(originalLetter, encodedLetter);
                    writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "42c2f371-f94f-45d0-9418-a6c3e9eb22ad");
                    encodedToOriginal.put(encodedLetter, originalLetterAsString);
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "c0c3d8be-5b68-47ad-84b7-99eb65a21122");
            return new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
        } else if (encodingCopy.size() - doNotEncodeCopy.size() < 2) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "5e2b69cc-bd09-4a30-8b12-7c13c49d8984");
            throw new IllegalArgumentException("Must have at least two encoding characters (excluding " + "those in the 'do not encode' list), but has " + (encodingCopy.size() - doNotEncodeCopy.size()));
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "7a4ddc49-0a90-4afc-91c1-591084b8428b");
            // we start with one which is our minimum, and because we do the
            // first division outside the loop
            int lettersSoFar = 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "95ae47e2-2579-459a-8c61-bdc9da1d0913");
            // the first division takes into account that the doNotEncode
            // letters can't be in the leftmost place
            int lettersLeft = (originalCopy.size() - doNotEncodeCopy.size()) / (encodingCopy.size() - doNotEncodeCopy.size());
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "87c5ee42-201c-440c-8aad-244d6a01b1bb");
            while (lettersLeft / encodingCopy.size() >= 1) {
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "9434d87b-df4c-43f0-b4c8-76673d8f4bb1");
                lettersLeft = lettersLeft / encodingCopy.size();
                writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "04438aaa-b3ee-4158-b904-2d2aea1a0027");
                lettersSoFar++;
            }
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "4113e3fe-3e65-4efa-9f1c-667de4fad58b");
            encodedLetterLength = lettersSoFar + 1;
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "c0e1e3f3-3ccf-4e3f-b24a-900e96e9cfb6");
            final AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, encodedLetterLength);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "606e4912-8b84-4714-8b60-5f9069254ce2");
            ac.addSingleEncoding(encodedLetterLength, "", encodingCopy, originalCopy.iterator(), doNotEncodeMap);
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "399ef38b-3c67-45b5-a939-bb5ec3006995");
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
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "19ddf34b-bc2c-4e41-9a0a-049ac9a5b977");
        if (Character.charCount(i) == 1) {
            writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "85b45662-27b9-45f1-bc15-04abb7315be7");
            return String.valueOf((char) i);
        }
        writelineStatic("/home/ubuntu/results/coverage/AlphabetConverter/AlphabetConverter_2_10.coverage", "6b23e7ac-37d2-4092-92af-a176f3b65abe");
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
