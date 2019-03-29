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

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import java.nio.CharBuffer;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.io.*;

/**
 * Builds a string from constituent parts providing a more flexible and powerful API
 * than StringBuffer.
 * <p>
 * The main differences from StringBuffer/StringBuilder are:
 * </p>
 * <ul>
 * <li>Not synchronized</li>
 * <li>Not final</li>
 * <li>Subclasses have direct access to character array</li>
 * <li>Additional methods
 * <ul>
 * <li>appendWithSeparators - adds an array of values, with a separator</li>
 * <li>appendPadding - adds a length padding characters</li>
 * <li>appendFixedLength - adds a fixed width field to the builder</li>
 * <li>toCharArray/getChars - simpler ways to get a range of the character array</li>
 * <li>delete - delete char or string</li>
 * <li>replace - search and replace for a char or string</li>
 * <li>leftString/rightString/midString - substring without exceptions</li>
 * <li>contains - whether the builder contains a char or string</li>
 * <li>size/clear/isEmpty - collections style API methods</li>
 * </ul>
 * </li>
 * <li>Views
 * <ul>
 * <li>asTokenizer - uses the internal buffer as the source of a StrTokenizer</li>
 * <li>asReader - uses the internal buffer as the source of a Reader</li>
 * <li>asWriter - allows a Writer to write directly to the internal buffer</li>
 * </ul>
 * </li>
 * </ul>
 * <p>
 * The aim has been to provide an API that mimics very closely what StringBuffer
 * provides, but with additional methods. It should be noted that some edge cases,
 * with invalid indices or null input, have been altered - see individual methods.
 * The biggest of these changes is that by default, null will not output the text
 * 'null'. This can be controlled by a property, {@link #setNullText(String)}.
 * </p>
 *
 * @since 1.0
 */
public class StrBuilder implements CharSequence, Appendable, Serializable, Builder<String> {

    /**
     * The extra capacity for new builders.
     */
    static final int CAPACITY = 32;

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 7628716375283629643L;

    /**
     * Internal data storage.
     */
    // package-protected for test code use only
    char[] buffer;

    /**
     * Current size of the buffer.
     */
    private int size;

    /**
     * The new line.
     */
    private String newLine;

    /**
     * The null text.
     */
    private String nullText;

    /**
     * Constructor that creates an empty builder initial capacity 32 characters.
     */
    public StrBuilder() {
        this(CAPACITY);
    }

    /**
     * Constructor that creates an empty builder the specified initial capacity.
     *
     * @param initialCapacity  the initial capacity, zero or less will be converted to 32
     */
    public StrBuilder(int initialCapacity) {
        super();
        if (initialCapacity <= 0) {
            initialCapacity = CAPACITY;
        }
        buffer = new char[initialCapacity];
    }

    /**
     * Constructor that creates a builder from the string, allocating
     * 32 extra characters for growth.
     *
     * @param str  the string to copy, null treated as blank string
     */
    public StrBuilder(final String str) {
        super();
        if (str == null) {
            buffer = new char[CAPACITY];
        } else {
            buffer = new char[str.length() + CAPACITY];
            append(str);
        }
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the text to be appended when a new line is added.
     *
     * @return the new line text, null means use system default
     */
    public String getNewLineText() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f1fe36ad-b13b-4574-a5a4-48eb7e028e25");
        return newLine;
    }

    /**
     * Sets the text to be appended when a new line is added.
     *
     * @param newLine  the new line text, null means use system default
     * @return this, to enable chaining
     */
    public StrBuilder setNewLineText(final String newLine) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b6e41480-00ee-4129-9f09-5eb5ee49a595");
        this.newLine = newLine;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8d64960d-a45e-45c0-9b16-038a74d514da");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the text to be appended when null is added.
     *
     * @return the null text, null means no append
     */
    public String getNullText() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6fb60e44-5998-4544-bf42-ca1fb39c52a4");
        return nullText;
    }

    /**
     * Sets the text to be appended when null is added.
     *
     * @param nullText  the null text, null means no append
     * @return this, to enable chaining
     */
    public StrBuilder setNullText(String nullText) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "98ccf2f7-54e5-4e9a-a21a-bb4289fe817e");
        if (nullText != null && nullText.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "825c88e0-31eb-4838-b8df-beb0bae22941");
            nullText = null;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2495a368-34f1-4d87-8c1b-a4cb5ab46ea3");
        this.nullText = nullText;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9ce24c2a-53e4-4257-9366-54f075e12cd7");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the length of the string builder.
     *
     * @return the length
     */
    @Override
    public int length() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a7f65fc0-b577-4608-b10b-b88f4574c8e0");
        return size;
    }

    /**
     * Updates the length of the builder by either dropping the last characters
     * or adding filler of Unicode zero.
     *
     * @param length  the length to set to, must be zero or positive
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the length is negative
     */
    public StrBuilder setLength(final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c80f582d-4665-4c12-9b4c-5b19585a62eb");
        if (length < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "39759678-7424-499b-b32f-95244ffdbc96");
            throw new StringIndexOutOfBoundsException(length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f0ed7f90-2318-482e-ab28-7695d264225b");
        if (length < size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "20faeb52-21c3-4f1d-adc8-c2ce5fc4a22f");
            size = length;
        } else if (length > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5348692d-1e53-423e-9483-c312fdf79112");
            ensureCapacity(length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e92c1b5a-d8d3-438b-a35c-f9871d49bb4a");
            final int oldEnd = size;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "62c55cab-9297-4c85-a4f9-36ee2df5e354");
            final int newEnd = length;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "de8ac9d9-b679-42e3-9d27-b8b53223f63d");
            size = length;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4fedef8d-346c-45de-a8a9-3f5a96705ea5");
            for (int i = oldEnd; i < newEnd; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d9d2e226-88fc-4b94-95f1-9c34211083aa");
                buffer[i] = '\0';
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a2f38d3f-ae74-4235-8eeb-058abfec764a");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the current size of the internal character array buffer.
     *
     * @return the capacity
     */
    public int capacity() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "91801e6e-dfd1-4c17-8bf7-918d9fd98347");
        return buffer.length;
    }

    /**
     * Checks the capacity and ensures that it is at least the size specified.
     *
     * @param capacity  the capacity to ensure
     * @return this, to enable chaining
     */
    public StrBuilder ensureCapacity(final int capacity) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8b846b26-6988-48bc-9ce7-aeb8fa6bbd4e");
        if (capacity > buffer.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e7ea03ad-402d-4318-b612-d36b57bf750e");
            final char[] old = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fc2332ac-8715-410b-bc4e-b9e48589e300");
            buffer = new char[capacity * 2];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8e3afdbf-b664-44e1-a4a7-2c8bcdfa0f0c");
            System.arraycopy(old, 0, buffer, 0, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "335d0571-70b0-44fe-bccb-5ee6c1be781d");
        return this;
    }

    /**
     * Minimizes the capacity to the actual length of the string.
     *
     * @return this, to enable chaining
     */
    public StrBuilder minimizeCapacity() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "60f06fb8-5f15-42b6-b1b0-005dd1f35569");
        if (buffer.length > length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "63981bff-c565-4e69-9be7-c05d830ecc8a");
            final char[] old = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d3f2d0f5-0dad-404d-8d0a-40fa080d88c6");
            buffer = new char[length()];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "265b8fcd-f74c-4349-9627-fd14ac0aa116");
            System.arraycopy(old, 0, buffer, 0, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5fde17d7-9d01-4a62-bcae-6535ba68e2f1");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the length of the string builder.
     * <p>
     * This method is the same as {@link #length()} and is provided to match the
     * API of Collections.
     *
     * @return the length
     */
    public int size() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f9defc6e-2719-4bfd-bf6f-99c80347c667");
        return size;
    }

    /**
     * Checks is the string builder is empty (convenience Collections API style method).
     * <p>
     * This method is the same as checking {@link #length()} and is provided to match the
     * API of Collections.
     *
     * @return <code>true</code> if the size is <code>0</code>.
     */
    public boolean isEmpty() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bb4de655-d49d-43b3-9f5e-cd2afc31b542");
        return size == 0;
    }

    /**
     * Clears the string builder (convenience Collections API style method).
     * <p>
     * This method does not reduce the size of the internal character buffer.
     * To do that, call <code>clear()</code> followed by {@link #minimizeCapacity()}.
     * <p>
     * This method is the same as {@link #setLength(int)} called with zero
     * and is provided to match the API of Collections.
     *
     * @return this, to enable chaining
     */
    public StrBuilder clear() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "332387ce-44b1-43f4-a4f3-2a4867c3e762");
        size = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9870f6a9-fe4d-400f-ab00-2bc6fcfbd827");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the character at the specified index.
     *
     * @see #setCharAt(int, char)
     * @see #deleteCharAt(int)
     * @param index  the index to retrieve, must be valid
     * @return the character at the index
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @Override
    public char charAt(final int index) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5b21d73a-5408-481d-b07a-7af50effef4b");
        if (index < 0 || index >= length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f851027c-7893-4f6c-a14a-0e6a6d5fc230");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "feb2ea4a-7166-4305-9242-e4b99ceb5ca1");
        return buffer[index];
    }

    /**
     * Sets the character at the specified index.
     *
     * @see #charAt(int)
     * @see #deleteCharAt(int)
     * @param index  the index to set
     * @param ch  the new character
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder setCharAt(final int index, final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e65cb3b6-0c28-405a-9c1f-0dbdf22daff0");
        if (index < 0 || index >= length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "edfa89c2-5528-4252-bb16-e41c994176a1");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c5728e9e-b18e-487b-96f8-0ffc49ca21b0");
        buffer[index] = ch;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "631dece8-f687-416d-98ca-9c0dca6c2cce");
        return this;
    }

    /**
     * Deletes the character at the specified index.
     *
     * @see #charAt(int)
     * @see #setCharAt(int, char)
     * @param index  the index to delete
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder deleteCharAt(final int index) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "afbbd467-5394-4ce8-b23b-b98043004f6f");
        if (index < 0 || index >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5b862fb2-83b0-45fe-90a6-eef74e64cad7");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0e29a6e9-63a6-47d2-b1ff-922a9f0278c9");
        deleteImpl(index, index + 1, 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "639c5e5f-4a6b-4c1e-98ab-a1abd83231eb");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Copies the builder's character array into a new character array.
     *
     * @return a new array that represents the contents of the builder
     */
    public char[] toCharArray() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "79e236a7-2c76-4b69-a5b0-a2a299b92db5");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ce34168e-7902-4526-a00b-b1ac47bdd647");
            return new char[0];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4c4ae91f-9584-4abb-9222-d2d29a2c6633");
        final char[] chars = new char[size];
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7d2d4d7b-ca2d-4abb-ab89-dc4ba103a220");
        System.arraycopy(buffer, 0, chars, 0, size);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5129b99c-2ebd-4929-a79e-20361f1eeb92");
        return chars;
    }

    /**
     * Copies part of the builder's character array into a new character array.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except that
     * if too large it is treated as end of string
     * @return a new array that holds part of the contents of the builder
     * @throws IndexOutOfBoundsException if startIndex is invalid,
     * or if endIndex is invalid (but endIndex greater than size is valid)
     */
    public char[] toCharArray(final int startIndex, int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c5b706bc-9cd6-4739-9c38-d5f949a45599");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "01d59ba8-ade0-4fe2-912e-3e8942734d41");
        final int len = endIndex - startIndex;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "10ad49eb-a6c0-41f3-9b13-65fc17ce3702");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "75c1e822-c6cb-4143-b395-9bb1ef254522");
            return new char[0];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9d566e4e-ae81-430e-a2a0-ac11b1123e15");
        final char[] chars = new char[len];
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d92402b4-4120-4f63-a36f-8bfeb3ced7ef");
        System.arraycopy(buffer, startIndex, chars, 0, len);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4538bb32-4fea-4cf9-9114-6769a867742e");
        return chars;
    }

    /**
     * Copies the character array into the specified array.
     *
     * @param destination  the destination array, null will cause an array to be created
     * @return the input array, unless that was null or too small
     */
    public char[] getChars(char[] destination) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6edd5a2f-5b9d-4198-9af5-90f64cd14d64");
        final int len = length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2916d337-b2fa-4433-a806-70b9445f2fc1");
        if (destination == null || destination.length < len) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eaaa0f6f-1b94-4c96-be57-e74d4050cc5c");
            destination = new char[len];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0d4b04e3-a5bc-42e2-899f-38b45bdfad81");
        System.arraycopy(buffer, 0, destination, 0, len);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7df96b91-0322-4d5d-a776-3685d9b629ad");
        return destination;
    }

    /**
     * Copies the character array into the specified array.
     *
     * @param startIndex  first index to copy, inclusive, must be valid
     * @param endIndex  last index, exclusive, must be valid
     * @param destination  the destination array, must not be null or too small
     * @param destinationIndex  the index to start copying in destination
     * @throws NullPointerException if the array is null
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    public void getChars(final int startIndex, final int endIndex, final char[] destination, final int destinationIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f055982c-d223-411f-a1e7-fc898ff5c7d4");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3947813f-dc37-480e-ad4d-e0580cde81e4");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "94ea5716-ee7b-4eb6-820e-3ee8b9826069");
        if (endIndex < 0 || endIndex > length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "466bc76b-5450-4465-bad3-b29cd71d5006");
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a3237852-543c-44d9-9239-4dc3d4aca0e0");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0602f3f5-d76c-4f01-9629-59dcdb435153");
            throw new StringIndexOutOfBoundsException("end < start");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "19d29e24-0606-458e-b5c1-acb639c04a05");
        System.arraycopy(buffer, startIndex, destination, destinationIndex, endIndex - startIndex);
    }

    // -----------------------------------------------------------------------
    /**
     * If possible, reads chars from the provided {@link Readable} directly into underlying
     * character buffer without making extra copies.
     *
     * @param readable  object to read from
     * @return the number of characters read
     * @throws IOException if an I/O error occurs
     *
     * @see #appendTo(Appendable)
     */
    public int readFrom(final Readable readable) throws IOException {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6fa42606-4102-476f-956b-baf9fccc540f");
        final int oldSize = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "68a34bca-8a04-4ef9-a874-00527a69842c");
        if (readable instanceof Reader) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "96ccd97c-3813-4ce9-8118-b063c3d9bee2");
            final Reader r = (Reader) readable;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "934c5e9a-b5e1-4f40-8229-464bcdc211b3");
            ensureCapacity(size + 1);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "32b7d06e-1df5-46ca-a94c-8ab1517a8a85");
            int read;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "af73b642-757f-4b44-8953-87baab8419b6");
            while ((read = r.read(buffer, size, buffer.length - size)) != -1) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a92875ac-d36d-4d81-a077-22036726f0fb");
                size += read;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "94f6c51b-5e8c-4740-a597-a7ead45e7ae6");
                ensureCapacity(size + 1);
            }
        } else if (readable instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9c13f15f-c931-4f0b-8994-9bde9feacafc");
            final CharBuffer cb = (CharBuffer) readable;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f48f563c-2df9-4bfe-b5a1-cc188bd5bb11");
            final int remaining = cb.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4437fa95-7a2e-4b8a-91a6-3d335cb9732e");
            ensureCapacity(size + remaining);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0732ac2c-f038-4473-b239-fbf78eb0d55b");
            cb.get(buffer, size, remaining);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "56c8a8e0-4ccf-4f46-ba1f-d0efb443bd73");
            size += remaining;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b1239da9-41b4-4a4b-a3cd-75659a68d6df");
            while (true) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b3583292-b00c-4cb5-b845-7f2c2e03cbbc");
                ensureCapacity(size + 1);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "50e212e2-03f1-4d4c-9a53-5b4b36581276");
                final CharBuffer buf = CharBuffer.wrap(buffer, size, buffer.length - size);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "31f5b4ad-ee89-4398-91d9-f42ef792e9bc");
                final int read = readable.read(buf);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fe4ea3f8-804d-4a8f-9362-3de105c7f199");
                if (read == -1) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e31320e8-c1fd-4b75-9e7d-5c712965a4db");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cdca1f89-3369-4fd5-bb90-5c39389683dd");
                size += read;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "75cbce9e-814e-4c41-aef2-50ece342e1b6");
        return size - oldSize;
    }

    // -----------------------------------------------------------------------
    /**
     * Appends the new line string to this string builder.
     * <p>
     * The new line string can be altered using {@link #setNewLineText(String)}.
     * This might be used to force the output to always use Unix line endings
     * even when on Windows.
     *
     * @return this, to enable chaining
     */
    public StrBuilder appendNewLine() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "12da182e-bfaf-4fd7-869f-618aadbe8c1e");
        if (newLine == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f8633a7b-46df-4186-82e9-13d908027344");
            append(System.lineSeparator());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8a8594ce-b270-40b5-8653-d2d54a0f5332");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "73643874-3592-40f7-9ef1-a6847cb385ba");
        return append(newLine);
    }

    /**
     * Appends the text representing <code>null</code> to this string builder.
     *
     * @return this, to enable chaining
     */
    public StrBuilder appendNull() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "90a9404d-de72-4468-96f1-65f805a022a2");
        if (nullText == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0c6b8873-98f8-4ac4-9153-8f0228ca98d8");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a9ab525d-c60b-4944-9a2a-2a96a72d0287");
        return append(nullText);
    }

    /**
     * Appends an object to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param obj  the object to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final Object obj) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "679c8095-df6c-42ea-900f-f4da356d59d1");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "98a7cf9a-2e7c-47eb-a3f3-2eea52808023");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9733f06a-e1b2-4d28-81fb-0e571d52017b");
        if (obj instanceof CharSequence) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ef196b8c-d911-43e6-b531-03b4947d8a6a");
            return append((CharSequence) obj);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b6b3678e-2099-4084-ba10-eee2f363e6d8");
        return append(obj.toString());
    }

    /**
     * Appends a CharSequence to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param seq  the CharSequence to append
     * @return this, to enable chaining
     */
    @Override
    public StrBuilder append(final CharSequence seq) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f5d02e59-c07d-4e3e-9965-3218fc3fb60c");
        if (seq == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3d97f046-acc1-443f-afd8-9f1ae11f1e34");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ece051af-140e-4da3-a2c6-11350ec1e4db");
        if (seq instanceof StrBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cfe907c2-71a1-4a79-892c-201c9feffca0");
            return append((StrBuilder) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f1344e17-5dd7-45fb-9e39-b6fef3fd514e");
        if (seq instanceof StringBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "393a765b-c930-41a7-bbf6-0286e2ec41a5");
            return append((StringBuilder) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e747736f-bbca-4487-9a15-c89c3aacf76a");
        if (seq instanceof StringBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "520b2ee8-5819-426c-a1e7-29c82cf97e92");
            return append((StringBuffer) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7adfb59f-cdd1-4da2-9986-818018360669");
        if (seq instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3cb135c8-5b84-47e8-9f71-f93ddae8a059");
            return append((CharBuffer) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d8f11186-81b4-472e-ae60-6495af0bad44");
        return append(seq.toString());
    }

    /**
     * Appends part of a CharSequence to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param seq  the CharSequence to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    @Override
    public StrBuilder append(final CharSequence seq, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fbb40c26-73a8-4744-a40c-07223db38000");
        if (seq == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "45090458-e372-48e3-bfe0-615629eb43ea");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f52ff4f2-3f31-43b8-a4cf-90c84871e335");
        return append(seq.toString(), startIndex, length);
    }

    /**
     * Appends a string to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d7be8809-72b5-4bf9-9692-fac4a4f76566");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3bfb28c9-b0fd-4dad-9f2d-a218ddd03fa3");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d3d4cbc3-944e-409d-b8b8-b3cff9c63b49");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "81de3d7a-4bbf-4c30-ac64-cab0cf5ced1f");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "58810081-7a20-45c2-80a9-cd1fe8405078");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "595e98dd-9dbe-4575-b2a7-8862b2328e14");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "94755726-c1e6-49a8-bb40-31cfb4633df9");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d7abf21a-91eb-4f01-ab07-57614ec9618e");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0ba4d42b-594c-414c-9dac-1e1b6eab4386");
        return this;
    }

    /**
     * Appends part of a string to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(final String str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9b4f0946-358c-40fb-9606-db8f544e64b3");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6d620b2a-88fb-4241-a94f-15965c6ea11d");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e2289580-9a93-4f6b-9462-706bcdf37741");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ece36f8b-2beb-4d05-853e-85cee39de963");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5a14a436-addf-49d9-833f-0f635ff0b437");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "64b207fc-cebe-457d-a721-eac1ddef35b4");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bbc11eff-7542-49a0-80bc-01953d9ab095");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d6dade34-a574-413b-a1af-407fb1d1e78f");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2e26d277-c91d-4164-afb0-4bdebcb8b51e");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c29c3955-643f-4ee8-9e7e-c91678a2cf42");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ab22ea40-ef59-4db4-b1bb-efe39f5bc67b");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "18aae97a-888f-4fa8-bfa7-585cb4703190");
        return this;
    }

    /**
     * Calls {@link String#format(String, Object...)} and appends the result.
     *
     * @param format the format string
     * @param objs the objects to use in the format string
     * @return {@code this} to enable chaining
     * @see String#format(String, Object...)
     */
    public StrBuilder append(final String format, final Object... objs) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "56cb9375-9d0c-4469-930a-a0aa34aa1b0b");
        return append(String.format(format, objs));
    }

    /**
     * Appends the contents of a char buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param buf  the char buffer to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final CharBuffer buf) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c6e0ba72-01d0-4cd2-a403-e1938fe58620");
        if (buf == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a5c54b7b-34d9-40c5-ae69-c933d9612c29");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7622d3de-d934-4961-bb54-c262945eda78");
        if (buf.hasArray()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d9c46823-8a75-4070-ac1a-30dcaa371a91");
            final int length = buf.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f8b1efc7-7ba1-40af-aef9-ae521f211c2c");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "227d44a8-c256-4796-8567-b8f44691a51d");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1590b0a9-0378-47c8-9c48-56e5759f02ca");
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position(), buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "427e80ed-512e-458e-859f-76e698403372");
            size += length;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0704846c-d1ee-4bf2-be9f-5459b0903d9f");
            append(buf.toString());
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c4cbf101-7600-4678-82ea-cc8f37a97b15");
        return this;
    }

    /**
     * Appends the contents of a char buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param buf  the char buffer to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(final CharBuffer buf, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b4b77f43-daf5-4c16-8f51-a18d7cd44666");
        if (buf == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "370864f2-820d-450c-9e79-8071d59710ff");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d9c734f8-876b-4a34-9bd6-afa935158532");
        if (buf.hasArray()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3e4a0f64-982b-4a7f-8295-43dd585ac977");
            final int totalLength = buf.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "02a97e53-2c8c-4b5d-a754-50657a0155df");
            if (startIndex < 0 || startIndex > totalLength) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1ff76539-0404-4323-8a44-f54980897aa5");
                throw new StringIndexOutOfBoundsException("startIndex must be valid");
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "251fc48b-24f1-453a-9467-da0c5fa058ef");
            if (length < 0 || (startIndex + length) > totalLength) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "03c7b2e1-804f-4c22-aabf-325850c5fc70");
                throw new StringIndexOutOfBoundsException("length must be valid");
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8b4d3914-2f43-4e0e-8ede-0980354ef22e");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "64b3ea39-e93c-4e28-8cfa-e5366a02c428");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d3989ba3-1363-4ee9-a676-ce9815ae2a5e");
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position() + startIndex, buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4bda9e02-32ae-462e-9e85-35092f3bb4ca");
            size += length;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "024755d9-67d7-487e-b729-f7980ce9dda1");
            append(buf.toString(), startIndex, length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "10cad377-ba7e-4b4c-93d4-8eeb21ea9573");
        return this;
    }

    /**
     * Appends a string buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string buffer to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final StringBuffer str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b4b92a5c-f098-4770-a996-ccd012b1e350");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6371a214-40d4-4804-86e9-7616f758b018");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "aa2a51b7-7447-4e3d-bcde-b78df1c65302");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "77457a6f-2c7d-4acf-8af4-6a55b04a6565");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "803064bd-1da4-400c-8153-6fa6f1dbb154");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ca36e9f7-9eb9-4270-b188-3cd10ce4f3f7");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0d066637-ed26-479b-a5a4-1abafca0b7a4");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e0a20938-5d83-409a-ae98-0c2d4d294f9f");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "aecc39ac-85e5-423f-aaa2-e66ea5700274");
        return this;
    }

    /**
     * Appends part of a string buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(final StringBuffer str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eec1deff-ba8a-4f25-9481-e573fc8c975c");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4586c70c-d076-47a2-bfd0-e2f6acba499b");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ce0cd5a5-806c-4729-bd28-0249f67d09ad");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a581139c-74cb-488e-9fa9-ab6e91e46d48");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ffdf826b-df7d-4664-8685-2c2c0a417213");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0259abd2-ce2a-4ed8-8ecc-d9d9c3559b8f");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5469bb6a-cf60-4ee6-8853-507f1384c25f");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6d058c27-81d9-42a3-b1c7-03af8aab2496");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a076d0cc-31e2-4cb6-a30b-22e48b9f39de");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d6f95087-3638-43a4-92ba-cbc6fcf29b17");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e84ba5c7-8b0d-4c7a-a7f2-40b59ae62e4a");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e0e969f9-b847-4f57-9b16-ade7030df28e");
        return this;
    }

    /**
     * Appends a StringBuilder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str the StringBuilder to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final StringBuilder str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "70bd6373-30d3-4721-8ed7-3bbcbd5963f9");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c814d833-5e2b-4a15-970a-fab4aac4670b");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1c192e29-1fcd-439e-ba69-c76b41b8fa57");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a9f7b216-4899-41ea-bfb0-f5fb19f59e14");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b4717d18-baa9-4fb7-b900-aeb2a20d4000");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "45a2bf4f-88b6-4225-ac22-ee3a1f16e6c5");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9fabbb3a-1359-4773-a964-05a0024f0fd7");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fbb0ed0e-521a-4fec-9c33-bef9b03014ee");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "de5b0d4b-5684-40cf-8093-90fd8ea4a48b");
        return this;
    }

    /**
     * Appends part of a StringBuilder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str the StringBuilder to append
     * @param startIndex the start index, inclusive, must be valid
     * @param length the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(final StringBuilder str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e52cf579-067c-4242-bb99-f250e67ba247");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2cad975b-56ca-4d7b-ba89-2add8fca2e56");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "919c676c-9609-4ef8-b838-d56bb1c9d10b");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ca173ea6-2efe-4f97-82c3-4aa81d7df4e3");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2856dcf2-7189-4450-8763-a90dfe6cccce");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eb6ff09b-3da5-49a0-9694-cb8ae106f4b4");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a834f099-2bbe-46c7-a3ed-2864065ce835");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "10519cef-bc78-446c-a6b7-5efbc1cc17fb");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c0d65229-7818-4423-b37e-89ba81059c92");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6ce0aaed-9100-46da-91ba-5477b05184bd");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0d0b6c02-99b4-492d-b48c-efbb83b4abf5");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e03044f4-4302-462b-ac09-90f92cc974e8");
        return this;
    }

    /**
     * Appends another string builder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final StrBuilder str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "19ec69ad-2005-40f4-9b1b-dc6764b1775e");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2b382ccc-062a-4985-9006-1da85cf01db7");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4eb8b068-4581-44a0-b3f7-2d27856a677f");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "40764c29-c82c-4c41-8dea-4a3638982275");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6c059e0e-cab6-4cd6-8c14-24e5dda34141");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d77b1106-b1a3-48ab-af72-e41dcd3aada2");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2bd6fe0c-566d-4726-a8f8-6625256c8ad7");
            System.arraycopy(str.buffer, 0, buffer, len, strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d24c7cac-c857-478a-a64d-064c4d494232");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0b5dae62-8d49-4cb3-b2b7-b1e080fb70ac");
        return this;
    }

    /**
     * Appends part of a string builder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(final StrBuilder str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a0f922ca-5e97-4703-b711-8682fe663778");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d773aa56-b551-4e63-a411-56ab80b45073");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "850e2172-4b88-4528-b66c-df98ebd0b990");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "df6e852f-512a-4e4f-b4ae-bd88a7a1bb48");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "01e7410c-0660-4fa5-8cac-c8167b94211b");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "75138c06-81b5-44fc-af9e-d6be5d2e9b83");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b88f951e-cc80-48c8-8be4-a7712be3dcb5");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a06bba1b-4f2a-4f04-a1e3-27f7ae5b98ab");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "98b0d7ce-a920-44f8-a1d4-c486f68b13f7");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ed387e5d-48eb-47fa-a23e-7bed9bf2de3e");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "68c4b145-06d1-4376-80b9-32063aec0cc8");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f2ae6bf0-e51b-488c-bcae-785fc2ddb2b0");
        return this;
    }

    /**
     * Appends a char array to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final char[] chars) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4d6dd5fb-9150-4c17-a4f1-147002ccb749");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8a59d763-ce6c-4de6-a0d3-dd56e2d7a6dd");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b79baa6a-2226-4d94-b2d3-d209f226ea67");
        final int strLen = chars.length;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2c96a952-3753-405d-b31c-ac2acf9515ed");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0b9d8f17-92c7-455a-b33d-dae3c111e8a6");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2d9315e3-964d-4101-82b7-4708f5967f87");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "14c452ff-e63a-4850-8cf0-584d802db5dc");
            System.arraycopy(chars, 0, buffer, len, strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ffe3f1d4-e4e9-47c9-a1ba-b71b71c6b8e2");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "54aa28a4-f19a-4bb0-a1bd-65c4a90dea0b");
        return this;
    }

    /**
     * Appends a char array to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(final char[] chars, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "10aff195-8189-4e97-89ce-c33461bb4b5d");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "55cb4ef9-a891-4705-a856-c67dad707b1d");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f15b12c2-4f1e-4f7e-b41f-e4b344f7bc86");
        if (startIndex < 0 || startIndex > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6c00bbf2-3742-4fe7-8645-cd3185821d0c");
            throw new StringIndexOutOfBoundsException("Invalid startIndex: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a42da610-7c1b-408f-8749-8995918f8107");
        if (length < 0 || (startIndex + length) > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b3d700bc-92bf-4b77-b856-0335da9e67aa");
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "81c011b9-53c8-4998-b56e-873c5be7fbe7");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d492f112-65f0-4967-a380-4b04c6049749");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6f0fc28e-71a1-4068-8bb1-3d79a3cc2a14");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9138112e-9a79-4439-908b-8cce26779e63");
            System.arraycopy(chars, startIndex, buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eebd5040-ebca-44b2-ad9d-24d22d20dccb");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "68f63026-367a-4a1c-a3dd-b20d91a3e30e");
        return this;
    }

    /**
     * Appends a boolean value to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "33a7c969-a861-4a24-9b96-7b35d27bbc9a");
        if (value) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3eb82952-6999-43c8-874b-be30cb070599");
            ensureCapacity(size + 4);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "68ad8bce-f7c9-4494-801b-a245ad239454");
            buffer[size++] = 't';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2354cb52-5258-4ca0-99c3-caf15cb265d9");
            buffer[size++] = 'r';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "81fa85ec-8578-4420-9afd-b3883701119d");
            buffer[size++] = 'u';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "380b3ab8-0d53-4945-ac90-af23273cd647");
            buffer[size++] = 'e';
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0e882e75-744a-4c7d-adb9-d11f0faf75d3");
            ensureCapacity(size + 5);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "45af17e1-f02e-4dde-96e1-731638364e44");
            buffer[size++] = 'f';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a7c869a7-6b61-41a7-bba0-23eea5b2b79c");
            buffer[size++] = 'a';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8182d193-3992-4729-aa6c-b633255f6b57");
            buffer[size++] = 'l';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "29114c70-89d3-47f0-856b-ac8cb44e89a5");
            buffer[size++] = 's';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "05d7fcab-1334-47cc-b50a-2b748f0ef43b");
            buffer[size++] = 'e';
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "34b4b9e5-9c48-41aa-b2e8-dae8d54c839b");
        return this;
    }

    /**
     * Appends a char value to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     */
    @Override
    public StrBuilder append(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eaee1418-f739-4f67-8b10-4795a424a145");
        final int len = length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a6dbd119-448a-4e85-aa65-8ddcda9ef859");
        ensureCapacity(len + 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1d3c379f-2fbd-46d6-93ef-f1aa10cf7f31");
        buffer[size++] = ch;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ef98a133-75f0-45d3-b306-1a8bfbb955b0");
        return this;
    }

    /**
     * Appends an int value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e6ce4071-0e1f-43e2-958d-24db0062c713");
        return append(String.valueOf(value));
    }

    /**
     * Appends a long value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0055202b-478f-4373-ab7a-aa684ee2fc07");
        return append(String.valueOf(value));
    }

    /**
     * Appends a float value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8d635d5b-e885-4551-af00-9e3a9e8aa6f3");
        return append(String.valueOf(value));
    }

    /**
     * Appends a double value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "adbd370a-1cd6-4f30-a67b-b6a804458b3f");
        return append(String.valueOf(value));
    }

    // -----------------------------------------------------------------------
    /**
     * Appends an object followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param obj  the object to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final Object obj) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a6c276c6-9860-4a1b-8029-d82d343d07aa");
        return append(obj).appendNewLine();
    }

    /**
     * Appends a string followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "55438aec-e384-42d3-a858-4b512550d946");
        return append(str).appendNewLine();
    }

    /**
     * Appends part of a string followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final String str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4f0701e1-52b2-491c-b312-72e34b9ab9b4");
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Calls {@link String#format(String, Object...)} and appends the result.
     *
     * @param format the format string
     * @param objs the objects to use in the format string
     * @return {@code this} to enable chaining
     * @see String#format(String, Object...)
     */
    public StrBuilder appendln(final String format, final Object... objs) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cd2369ce-46e1-4a30-9300-8057cd73f6dc");
        return append(format, objs).appendNewLine();
    }

    /**
     * Appends a string buffer followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string buffer to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final StringBuffer str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "293d33d9-be8a-438a-874a-7ac796410398");
        return append(str).appendNewLine();
    }

    /**
     * Appends a string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final StringBuilder str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c8d2d62f-8f6f-45f8-b6cc-d5f3abd9936d");
        return append(str).appendNewLine();
    }

    /**
     * Appends part of a string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final StringBuilder str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ab8a04d0-3eab-41e5-922d-7852c74d58b9");
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Appends part of a string buffer followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final StringBuffer str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "497073b9-b73c-4356-b841-5c64959d0802");
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Appends another string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final StrBuilder str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6bf0b24c-14ac-4c7a-8cfb-8b7dd2ea4a60");
        return append(str).appendNewLine();
    }

    /**
     * Appends part of a string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final StrBuilder str, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c9d4adda-a7ee-4248-8c9c-88ea046637e4");
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Appends a char array followed by a new line to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final char[] chars) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9d93aac6-12b0-47f4-bb6b-4bfe15fbbed0");
        return append(chars).appendNewLine();
    }

    /**
     * Appends a char array followed by a new line to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final char[] chars, final int startIndex, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "544834dd-c763-4cba-bda9-5e6b9e730c86");
        return append(chars, startIndex, length).appendNewLine();
    }

    /**
     * Appends a boolean value followed by a new line to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a9e8759b-f178-4069-9bb9-eb8ba6396596");
        return append(value).appendNewLine();
    }

    /**
     * Appends a char value followed by a new line to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "41f8ba16-0ae9-473f-b84c-5e6b4bc71f34");
        return append(ch).appendNewLine();
    }

    /**
     * Appends an int value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a5465045-4061-4838-a982-d1655ea65dbe");
        return append(value).appendNewLine();
    }

    /**
     * Appends a long value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6cae2aef-3904-41ca-933a-770ec0c0679b");
        return append(value).appendNewLine();
    }

    /**
     * Appends a float value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4819572a-9f99-495e-8a3b-64b373861856");
        return append(value).appendNewLine();
    }

    /**
     * Appends a double value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dad6a0ee-c40c-42dc-a5b2-4382db0a1e20");
        return append(value).appendNewLine();
    }

    // -----------------------------------------------------------------------
    /**
     * Appends each item in an array to the builder without any separators.
     * Appending a null array will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param <T>  the element type
     * @param array  the array to append
     * @return this, to enable chaining
     */
    public <T> StrBuilder appendAll(@SuppressWarnings("unchecked") final T... array) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d7966da6-507c-4c81-a5b2-db3a96bfa533");
        /*
         * @SuppressWarnings used to hide warning about vararg usage. We cannot
         * use @SafeVarargs, since this method is not final. Using @SupressWarnings
         * is fine, because it isn't inherited by subclasses, so each subclass must
         * vouch for itself whether its use of 'array' is safe.
         */
        if (array != null && array.length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7bd36051-bae7-41c8-b6b3-9b2014222218");
            for (final Object element : array) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0e5ad37e-49e7-4c30-b9ed-610b4a78ac5f");
                append(element);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2714f9aa-560e-430b-b981-7dc1acaccedb");
        return this;
    }

    /**
     * Appends each item in an iterable to the builder without any separators.
     * Appending a null iterable will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param iterable  the iterable to append
     * @return this, to enable chaining
     */
    public StrBuilder appendAll(final Iterable<?> iterable) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c9895ae3-1023-4909-967f-7c6c3becc767");
        if (iterable != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8aed6f99-7be8-4f37-b2cf-bf15e44c5211");
            for (final Object o : iterable) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2c8ecda3-8ca0-4b4d-abbb-608bb33604e3");
                append(o);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5a15877c-538d-4859-b83f-18523618644f");
        return this;
    }

    /**
     * Appends each item in an iterator to the builder without any separators.
     * Appending a null iterator will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param it  the iterator to append
     * @return this, to enable chaining
     */
    public StrBuilder appendAll(final Iterator<?> it) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6bdc0ee9-eb78-402f-9d78-5952556967e2");
        if (it != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8295ca95-5dac-4e38-8664-934b789acaa2");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "67efddc0-8df5-42ea-b7e6-02f49f21f43d");
                append(it.next());
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "52ff51f7-03cc-4539-b254-12106a2cb834");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Appends an array placing separators between each value, but
     * not before the first or after the last.
     * Appending a null array will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param array  the array to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(final Object[] array, final String separator) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bddeba11-62eb-4a3c-980f-ecf6882ef7a6");
        if (array != null && array.length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "88fc1f92-1c3c-4574-9634-5c04da26c0bd");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4c5d1d42-25ae-428b-b962-def38d1b7e66");
            append(array[0]);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b595cb1e-c37f-45c6-ab9a-eb1da65295cf");
            for (int i = 1; i < array.length; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8a8ee336-39bf-452a-ab54-91f85d553fa4");
                append(sep);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a315e638-2088-4e8c-9bb1-55575ea24807");
                append(array[i]);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7b8f5bd8-01b1-4a60-a322-8e66216e69d8");
        return this;
    }

    /**
     * Appends an iterable placing separators between each value, but
     * not before the first or after the last.
     * Appending a null iterable will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param iterable  the iterable to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(final Iterable<?> iterable, final String separator) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c97d0b20-b7ff-4f57-97af-47cea86d8687");
        if (iterable != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "aed88678-8ec6-4ace-9f41-0bda45a14f18");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1a5d2efd-3f63-4db6-8337-c1fc6bf88185");
            final Iterator<?> it = iterable.iterator();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9372e973-465b-4b44-808f-48071f843c9f");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a37ba1b8-6e5f-46b9-9f3c-c9198cc98933");
                append(it.next());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5ef438b5-7041-4c87-b0ce-a5de8ee459bb");
                if (it.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4df05211-97f0-4a72-bc09-6533de77d34f");
                    append(sep);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f8b3da6c-2df0-4774-81be-1f4c4e7d4b21");
        return this;
    }

    /**
     * Appends an iterator placing separators between each value, but
     * not before the first or after the last.
     * Appending a null iterator will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param it  the iterator to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(final Iterator<?> it, final String separator) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3d7eaef7-590e-452b-bfef-a878064e4503");
        if (it != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a1b5f9ac-52b6-4c35-a553-3b1a99f7b1f1");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7fe26208-3088-46e7-909b-0094eac62a73");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f234e512-e52d-4e00-8cb8-6de8a998386a");
                append(it.next());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "aefa11ec-07a6-40d8-a771-d4ace27aaf97");
                if (it.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9a72ea15-10d3-484a-99a1-23189c511ee3");
                    append(sep);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dceec4ce-8cc6-4045-b914-f3c56fb84d54");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Appends a separator if the builder is currently non-empty.
     * Appending a null separator will have no effect.
     * The separator is appended using {@link #append(String)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * <pre>
     * for (Iterator it = list.iterator(); it.hasNext(); ) {
     * appendSeparator(",");
     * append(it.next());
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendSeparator(final String separator) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "59794203-4b93-4321-aba7-2bec4638436c");
        return appendSeparator(separator, null);
    }

    /**
     * Appends one of both separators to the StrBuilder.
     * If the builder is currently empty it will append the defaultIfEmpty-separator
     * Otherwise it will append the standard-separator
     *
     * Appending a null separator will have no effect.
     * The separator is appended using {@link #append(String)}.
     * <p>
     * This method is for example useful for constructing queries
     * <pre>
     * StrBuilder whereClause = new StrBuilder();
     * if(searchCommand.getPriority() != null) {
     * whereClause.appendSeparator(" and", " where");
     * whereClause.append(" priority = ?")
     * }
     * if(searchCommand.getComponent() != null) {
     * whereClause.appendSeparator(" and", " where");
     * whereClause.append(" component = ?")
     * }
     * selectClause.append(whereClause)
     * </pre>
     *
     * @param standard the separator if builder is not empty, null means no separator
     * @param defaultIfEmpty the separator if builder is empty, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendSeparator(final String standard, final String defaultIfEmpty) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "002a6cdd-f7c5-4ea1-802a-39427e7cb029");
        final String str = isEmpty() ? defaultIfEmpty : standard;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3342a5da-ad83-4080-9ed5-bb1711a03093");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fc08f77e-bea8-4921-93bd-a61e2be1763b");
            append(str);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b320cec2-6bc0-465a-8f0b-0ffe9dd57933");
        return this;
    }

    /**
     * Appends a separator if the builder is currently non-empty.
     * The separator is appended using {@link #append(char)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * <pre>
     * for (Iterator it = list.iterator(); it.hasNext(); ) {
     * appendSeparator(',');
     * append(it.next());
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use
     * @return this, to enable chaining
     */
    public StrBuilder appendSeparator(final char separator) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "14e88938-54e2-4d20-8252-8f4562e11648");
        if (size() > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eb1418a2-4363-438c-b95e-ea4aa1571a85");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fa60b40c-3840-4474-8381-ebe323bd1c19");
        return this;
    }

    /**
     * Append one of both separators to the builder
     * If the builder is currently empty it will append the defaultIfEmpty-separator
     * Otherwise it will append the standard-separator
     *
     * The separator is appended using {@link #append(char)}.
     * @param standard the separator if builder is not empty
     * @param defaultIfEmpty the separator if builder is empty
     * @return this, to enable chaining
     */
    public StrBuilder appendSeparator(final char standard, final char defaultIfEmpty) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e783c045-3f8d-4148-a310-ce598cf734f3");
        if (size() > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2d6437e0-218c-4cbe-b0cb-adf8893afd96");
            append(standard);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9122aff4-08db-4504-9d81-d2ec394e82a4");
            append(defaultIfEmpty);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "332e55d8-c2c1-4a61-89f8-345b045f96a0");
        return this;
    }

    /**
     * Appends a separator to the builder if the loop index is greater than zero.
     * Appending a null separator will have no effect.
     * The separator is appended using {@link #append(String)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * </p>
     * <pre>
     * for (int i = 0; i &lt; list.size(); i++) {
     * appendSeparator(",", i);
     * append(list.get(i));
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use, null means no separator
     * @param loopIndex  the loop index
     * @return this, to enable chaining
     */
    public StrBuilder appendSeparator(final String separator, final int loopIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "62a22327-c57a-448a-a7bd-8ccd26ca39af");
        if (separator != null && loopIndex > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dba8448c-1cd7-4a54-a54e-d2516aa4c4b9");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2490fd93-810f-4ea5-b191-0cd39c4ffbca");
        return this;
    }

    /**
     * Appends a separator to the builder if the loop index is greater than zero.
     * The separator is appended using {@link #append(char)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * </p>
     * <pre>
     * for (int i = 0; i &lt; list.size(); i++) {
     * appendSeparator(",", i);
     * append(list.get(i));
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use
     * @param loopIndex  the loop index
     * @return this, to enable chaining
     */
    public StrBuilder appendSeparator(final char separator, final int loopIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4e30f8ae-35d3-41fa-919c-d46e11888593");
        if (loopIndex > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c2bf9d4d-967d-4ec3-aada-f7963ba9401b");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dd98d26b-eb05-4006-8750-37797bbb85a3");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Appends the pad character to the builder the specified number of times.
     *
     * @param length  the length to append, negative means no append
     * @param padChar  the character to append
     * @return this, to enable chaining
     */
    public StrBuilder appendPadding(final int length, final char padChar) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bca5735d-a84a-419c-bc49-5f3c712b8c18");
        if (length >= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c44fcb18-0517-461b-a803-e304c526eec7");
            ensureCapacity(size + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "23a74dae-aab2-4f9a-9a39-7ad1aefca8fe");
            for (int i = 0; i < length; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f2e49a5f-76ef-4f24-9005-c208ef80948b");
                buffer[size++] = padChar;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9dec7359-d4d5-48ce-9a51-43ac1c45f7b5");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Appends an object to the builder padding on the left to a fixed width.
     * The <code>toString</code> of the object is used.
     * If the object is larger than the length, the left hand side is lost.
     * If the object is null, the null text value is used.
     *
     * @param obj  the object to append, null uses null text
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    public StrBuilder appendFixedWidthPadLeft(final Object obj, final int width, final char padChar) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3130d2cb-3757-4c88-890f-7febb6d1ed28");
        if (width > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b2fd9dbb-90c2-4c8a-ad70-f6d84e5e0402");
            ensureCapacity(size + width);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5fda6484-51f8-4e44-a105-8174f08666b8");
            String str = (obj == null ? getNullText() : obj.toString());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dd88ff35-0a48-42f2-af7d-44539e987fa1");
            if (str == null) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "86d6318a-8fd0-4fff-bca9-036187a6b832");
                str = "";
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1dce00b8-cf01-4d87-97c4-2475f9501407");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8727d7dc-a144-42b4-940f-15e9cef85874");
            if (strLen >= width) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ab10e70c-e176-4ab9-9e2d-99e6c08e8c81");
                str.getChars(strLen - width, strLen, buffer, size);
            } else {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "93eb7c01-f2e1-4380-b03d-7410f96a52c0");
                final int padLen = width - strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3d662ba8-ed1e-4dc7-984c-5c705813b0fc");
                for (int i = 0; i < padLen; i++) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d76e3787-2130-4a09-a3e0-caef8e16bfa1");
                    buffer[size + i] = padChar;
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a85a7ab1-2ec4-4ea1-9270-6b894c06bf70");
                str.getChars(0, strLen, buffer, size + padLen);
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "194f22a8-ba5e-4773-ab28-7973e9eda58e");
            size += width;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e2228214-21bf-4fff-a630-3d84a742b34a");
        return this;
    }

    /**
     * Appends an object to the builder padding on the left to a fixed width.
     * The <code>String.valueOf</code> of the <code>int</code> value is used.
     * If the formatted value is larger than the length, the left hand side is lost.
     *
     * @param value  the value to append
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    public StrBuilder appendFixedWidthPadLeft(final int value, final int width, final char padChar) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5c6a462d-4a43-433c-990b-b27542e283c6");
        return appendFixedWidthPadLeft(String.valueOf(value), width, padChar);
    }

    /**
     * Appends an object to the builder padding on the right to a fixed length.
     * The <code>toString</code> of the object is used.
     * If the object is larger than the length, the right hand side is lost.
     * If the object is null, null text value is used.
     *
     * @param obj  the object to append, null uses null text
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    public StrBuilder appendFixedWidthPadRight(final Object obj, final int width, final char padChar) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "603bc60a-58bd-46a9-8723-afa1551bbf80");
        if (width > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f882de38-ee0c-4de3-87d9-790429c809c8");
            ensureCapacity(size + width);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0d238bf1-1ab7-42a0-bff9-e290cc49a875");
            String str = (obj == null ? getNullText() : obj.toString());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "68eec0ed-72ba-4eb8-83d5-703eb40dd9c7");
            if (str == null) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e15a5224-5d5a-4b21-b622-d2222dad70c2");
                str = "";
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "55ed6462-fc46-47f8-95d9-fb607ab1d8fb");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "50eff9cb-237b-4e1f-a3f6-06628a0f459c");
            if (strLen >= width) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "79aa7979-1a3a-4396-9cd0-5ff43bdba265");
                str.getChars(0, width, buffer, size);
            } else {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4158eeff-21a6-4a91-83d9-af01538c3f6a");
                final int padLen = width - strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9bce9d94-2b90-4d20-be5f-a42a42612cdd");
                str.getChars(0, strLen, buffer, size);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5e246edf-6f6e-478f-a290-e142b29d4c9d");
                for (int i = 0; i < padLen; i++) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "88c965a0-f596-4f1c-806e-cc5be6b1df5e");
                    buffer[size + strLen + i] = padChar;
                }
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d7527366-d569-4981-91f5-9b9340b82073");
            size += width;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2602c2ee-884b-48fc-9c5f-e47485212eb6");
        return this;
    }

    /**
     * Appends an object to the builder padding on the right to a fixed length.
     * The <code>String.valueOf</code> of the <code>int</code> value is used.
     * If the object is larger than the length, the right hand side is lost.
     *
     * @param value  the value to append
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    public StrBuilder appendFixedWidthPadRight(final int value, final int width, final char padChar) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "48e2bb6a-727d-453a-9b5e-ebb2fbac9839");
        return appendFixedWidthPadRight(String.valueOf(value), width, padChar);
    }

    // -----------------------------------------------------------------------
    /**
     * Inserts the string representation of an object into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param obj  the object to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final Object obj) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "471a0aec-da97-48bc-ad1e-004d7789fb85");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "731cc1aa-0583-4c1d-b8f6-70d27fa7cf71");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a9f3d346-0a39-4641-8861-d5443ac22ed7");
        return insert(index, obj.toString());
    }

    /**
     * Inserts the string into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param str  the string to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "78ec0f47-6896-462d-848c-fae1087e510e");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3fde768c-dafd-42fb-9463-33c13d7a2535");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "57b90a6e-9df6-4651-9d31-29ee1a3350c8");
            str = nullText;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e076311f-c4ec-4d3b-8a29-852f86e2e0cd");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "166217a8-4fcc-4a6e-b4cd-1b013d89a854");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f8ec370a-66b7-4fc8-8b43-359a7bbb54dd");
            if (strLen > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0b64e64d-8313-45c4-9f50-d7316f7d7113");
                final int newSize = size + strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1b0911ee-dc89-4178-a7ab-dce05bd18c11");
                ensureCapacity(newSize);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "574d2e83-f2af-49a0-962c-59cbac3f8cf1");
                System.arraycopy(buffer, index, buffer, index + strLen, size - index);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "849218c1-a985-4235-9549-14c816c9e73d");
                size = newSize;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3b645eab-40ba-4115-8610-ad9691f3a422");
                str.getChars(0, strLen, buffer, index);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e07effa9-2dcc-4a5b-8abb-97bcf28256ba");
        return this;
    }

    /**
     * Inserts the character array into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param chars  the char array to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final char[] chars) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7f1a222d-3f74-46b3-b00b-a64b978ea8d6");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2dd3b7c9-657d-44d6-8fa4-7a0832fd4227");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c53d8d34-9eb8-4c83-9b19-0b64410d0191");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6bfffac4-fa9d-44ac-8d34-ed34529636dd");
        final int len = chars.length;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "60490930-c0e3-4958-94aa-39a2831d0dae");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7551586b-4a79-4b30-8eee-75a587c9cfd1");
            ensureCapacity(size + len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5d972f75-cc46-4662-9bc4-a89fae2e32dc");
            System.arraycopy(buffer, index, buffer, index + len, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9f30db95-743a-408d-ad78-6fac4bbbaa39");
            System.arraycopy(chars, 0, buffer, index, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1d73617b-133a-4dc4-9b2b-84a766506828");
            size += len;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "326f2997-9fa9-4855-99e0-8b1bad333979");
        return this;
    }

    /**
     * Inserts part of the character array into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param chars  the char array to insert
     * @param offset  the offset into the character array to start at, must be valid
     * @param length  the length of the character array part to copy, must be positive
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    public StrBuilder insert(final int index, final char[] chars, final int offset, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "62649af7-7317-463d-8f68-2a1cdc697a48");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fe671cf9-7faf-437d-8e45-adddd3cf430f");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "84f67876-bc75-4327-9000-7e71ba498257");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1d2a1f83-102d-4b35-bbf2-edd60545e672");
        if (offset < 0 || offset > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a91f627a-af4a-4b23-af03-fc40a25f620e");
            throw new StringIndexOutOfBoundsException("Invalid offset: " + offset);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "01121184-ada0-4db5-8a35-17063f780e25");
        if (length < 0 || offset + length > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4b4ce172-138f-4c3f-908c-f1ec6bebcf95");
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4e2cee9e-97fe-4fe9-a03f-5dd8eb585922");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "deb9b68e-83ee-4e26-a9bb-8396265ec955");
            ensureCapacity(size + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a856588f-43cc-4ef2-bb69-fa9a3f0f4439");
            System.arraycopy(buffer, index, buffer, index + length, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b1133aa5-71da-42b6-a282-dfd3b68a7a9d");
            System.arraycopy(chars, offset, buffer, index, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "be38356b-d7a8-4057-a93f-a57ea52dc265");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "afe4fd5c-c1f4-4eee-b0d4-b748ca21d5e9");
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fe11c4d3-9c80-4a37-886b-638051ac10c2");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f6139c63-4dd4-4f3e-9edd-6d6752154af6");
        if (value) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a5477022-f019-4421-b8de-0e537c0ec67e");
            ensureCapacity(size + 4);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "810706b3-57f2-46c9-9128-eae53537f0c1");
            System.arraycopy(buffer, index, buffer, index + 4, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0811cdb1-5490-4378-ae57-9c6254c67a55");
            buffer[index++] = 't';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "224341c7-510d-473a-a847-fc7130f64c94");
            buffer[index++] = 'r';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eb57c302-012e-4e3b-bb8d-df11b6a09ee2");
            buffer[index++] = 'u';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4fcbe47e-c430-40ea-beb5-256afa2fe0d0");
            buffer[index] = 'e';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5d7fa19f-9285-45a7-8c5f-3a7a3d9a317c");
            size += 4;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4f0816c1-9908-4651-83d6-8d82cf34c523");
            ensureCapacity(size + 5);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0496fba2-3ea5-4a58-8e5d-2ed02fad85f5");
            System.arraycopy(buffer, index, buffer, index + 5, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cacf0c64-9f16-48e1-aaf0-c0fa00791a59");
            buffer[index++] = 'f';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6676c899-5e36-4f80-9035-39ce845cba2e");
            buffer[index++] = 'a';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2897351c-d9d7-4d1a-b4d8-39b5b4c18686");
            buffer[index++] = 'l';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "41c51607-e256-495f-bd7e-f54332f4f09d");
            buffer[index++] = 's';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "59d60849-d47b-4dc8-bcf9-d5e5b29ee5db");
            buffer[index] = 'e';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "15bf328f-82e0-4d8b-a618-71541609ace5");
            size += 5;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0b92883f-09d3-405b-83be-a74921d3c7da");
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final char value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fa790f21-b79c-4c3e-adbc-0898d3c2c016");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9b942ba5-b571-4b06-8180-8c24be07de48");
        ensureCapacity(size + 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0ea8f186-0db5-468d-958f-117bf00cfd12");
        System.arraycopy(buffer, index, buffer, index + 1, size - index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "160050a9-a708-4129-b52c-8432f1da22f1");
        buffer[index] = value;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "00ce263e-9960-4b03-876b-7417e1d7d6e7");
        size++;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "531733dd-157b-4690-832c-8228b043ea98");
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b4e23b26-6638-40c4-9b56-2fad9083c2e4");
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "589df35d-6f9a-4fe2-905c-18901c50f211");
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5c4f0ad5-5d1d-4ede-ae60-9bdabf3dec59");
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "71480731-d73b-40cf-a9cd-3befb310f11e");
        return insert(index, String.valueOf(value));
    }

    // -----------------------------------------------------------------------
    /**
     * Internal method to delete a range without validation.
     *
     * @param startIndex  the start index, must be valid
     * @param endIndex  the end index (exclusive), must be valid
     * @param len  the length, must be valid
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    private void deleteImpl(final int startIndex, final int endIndex, final int len) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0a0a1efb-cbdf-4a13-b3bf-09aa0f3a8b0b");
        System.arraycopy(buffer, endIndex, buffer, startIndex, size - endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c6efc72b-979f-4dbf-b53a-9ddfef19da76");
        size -= len;
    }

    /**
     * Deletes the characters between the two specified indices.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     * that if too large it is treated as end of string
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder delete(final int startIndex, int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a0614663-e8e7-43ef-a5e5-fed461115c12");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a23ee10b-f4f4-4323-af0f-d1e330b102fc");
        final int len = endIndex - startIndex;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "81a64602-6bb9-4729-82bc-d9f3528791b6");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4ec1fd46-7991-43a7-bddc-cb095eb62452");
            deleteImpl(startIndex, endIndex, len);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2a22a755-fa46-45a9-a250-2f3ee5a88404");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Deletes the character wherever it occurs in the builder.
     *
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    public StrBuilder deleteAll(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0095008d-d14a-458a-9c48-55cc7bcbd7ac");
        for (int i = 0; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fb1b3e06-faac-4060-9e69-bb8a124b4cca");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4b5832c7-bc9a-440e-b2a1-30be95e29847");
                final int start = i;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "336d1767-9806-48ce-ae9e-23237e88e86c");
                while (++i < size) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c2dde78d-22a5-40ad-bcce-20bb9d54e470");
                    if (buffer[i] != ch) {
                        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b0aaac9d-85f4-46da-ac2c-5fbe61284750");
                        break;
                    }
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5fc04e72-c957-4325-8b48-a95e5d11b436");
                final int len = i - start;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ef7c7f2a-85c1-4b2d-b049-15abd1d326d5");
                deleteImpl(start, i, len);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ebe9a377-cdcf-47d6-b8c0-a9cafd2ec876");
                i -= len;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7e5b2d6a-e909-4513-b2e8-90bd621f6616");
        return this;
    }

    /**
     * Deletes the character wherever it occurs in the builder.
     *
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "520ad6af-406b-4f83-b083-8ddf929ea4f9");
        for (int i = 0; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "32bab3be-99ab-48ea-b5f9-a440c9529064");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0f6bc7fc-3026-43c7-92e8-de81802af2cd");
                deleteImpl(i, i + 1, 1);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b1584d7a-9de7-4e5e-a068-0c32690a6fb3");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fd3e92f9-4d00-46e9-a582-a1f7788978d0");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Deletes the string wherever it occurs in the builder.
     *
     * @param str  the string to delete, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteAll(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7b068f22-0f8c-4e61-9c29-aa8d0eafe16a");
        final int len = (str == null ? 0 : str.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "159f7ea2-203d-421d-9e0f-d9a2ff18fdb8");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "88a0f70a-f5ae-4a42-9ca1-be85c22f2c98");
            int index = indexOf(str, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fbf71e57-7f83-4c08-8d13-e5f36d8d6429");
            while (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6385c966-8b5a-4336-8212-ca999263fc2c");
                deleteImpl(index, index + len, len);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6029069d-6406-41d1-890f-ca5b02ae07d8");
                index = indexOf(str, index);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4de5b8ce-74e4-4d22-b56c-ac359b6d1adc");
        return this;
    }

    /**
     * Deletes the string wherever it occurs in the builder.
     *
     * @param str  the string to delete, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7ce9ef0d-dc32-4037-9490-0cd8913e177e");
        final int len = (str == null ? 0 : str.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f3c6ed20-500f-46ba-9711-e7d87c787176");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "49594990-ac9d-493d-9ffc-81acb7350d8b");
            final int index = indexOf(str, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "644870f8-a319-492f-bac4-5c357c9d3126");
            if (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1e53f38b-6377-4ca8-8c4b-b8e744b396e7");
                deleteImpl(index, index + len, len);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b3f5ed6b-c471-4ce1-878a-04a06e021f94");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Deletes all parts of the builder that the matcher matches.
     * <p>
     * Matchers can be used to perform advanced deletion behaviour.
     * For example you could write a matcher to delete all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteAll(final StrMatcher matcher) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ccce86bb-61cc-4558-aeeb-73799dccf911");
        return replace(matcher, null, 0, size, -1);
    }

    /**
     * Deletes the first match within the builder using the specified matcher.
     * <p>
     * Matchers can be used to perform advanced deletion behaviour.
     * For example you could write a matcher to delete
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final StrMatcher matcher) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "98113024-6d6c-4be4-bfe1-bf072087b210");
        return replace(matcher, null, 0, size, 1);
    }

    // -----------------------------------------------------------------------
    /**
     * Internal method to delete a range without validation.
     *
     * @param startIndex  the start index, must be valid
     * @param endIndex  the end index (exclusive), must be valid
     * @param removeLen  the length to remove (endIndex - startIndex), must be valid
     * @param insertStr  the string to replace with, null means delete range
     * @param insertLen  the length of the insert string, must be valid
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    private void replaceImpl(final int startIndex, final int endIndex, final int removeLen, final String insertStr, final int insertLen) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "91c95f77-cb78-4ae6-86ca-729eb13c155f");
        final int newSize = size - removeLen + insertLen;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "11a80908-17d0-4de5-b2b9-fc104fe05108");
        if (insertLen != removeLen) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0dac0d74-587a-445a-a447-821e08b4b299");
            ensureCapacity(newSize);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3f882bd5-fbe7-4f79-92a6-c16430084385");
            System.arraycopy(buffer, endIndex, buffer, startIndex + insertLen, size - endIndex);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fd0a2d5a-19a9-42f6-a3ba-075e1a5501fc");
            size = newSize;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f857164e-22b6-4212-b0e7-753b0e65fbd6");
        if (insertLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5f99ee85-2e19-4eae-abd6-7279c9e5c389");
            insertStr.getChars(0, insertLen, buffer, startIndex);
        }
    }

    /**
     * Replaces a portion of the string builder with another string.
     * The length of the inserted string does not have to match the removed length.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     * that if too large it is treated as end of string
     * @param replaceStr  the string to replace with, null means delete range
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder replace(final int startIndex, int endIndex, final String replaceStr) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c61d6d58-6ce1-4e94-9f24-8cd563129b7d");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bc04c6f5-ff33-4af0-88bd-b36fca7c2468");
        final int insertLen = (replaceStr == null ? 0 : replaceStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "35b7316a-904a-4547-a1bd-8e2a9481613b");
        replaceImpl(startIndex, endIndex, endIndex - startIndex, replaceStr, insertLen);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "03ed2df9-f3ec-4570-ad44-bff659035742");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Replaces the search character with the replace character
     * throughout the builder.
     *
     * @param search  the search character
     * @param replace  the replace character
     * @return this, to enable chaining
     */
    public StrBuilder replaceAll(final char search, final char replace) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "59c79439-28cb-450e-9f3b-fcb0a17d5d91");
        if (search != replace) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b64b54c3-d648-4328-aa43-5867c2c0f2d3");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "926c1075-0e81-4910-8d1b-766a3687d990");
                if (buffer[i] == search) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "80f09716-6eae-48b8-97ba-77804eafd991");
                    buffer[i] = replace;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5985f889-948c-4efb-b758-24c0f8f59612");
        return this;
    }

    /**
     * Replaces the first instance of the search character with the
     * replace character in the builder.
     *
     * @param search  the search character
     * @param replace  the replace character
     * @return this, to enable chaining
     */
    public StrBuilder replaceFirst(final char search, final char replace) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6fc97a50-9fc0-43d0-aff5-7a036fb12759");
        if (search != replace) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "51cfc4fe-1f12-44b0-ba04-95f5b19515e3");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e551f97e-9923-4fcf-870c-974d7a25d43c");
                if (buffer[i] == search) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b3a6b110-cfa1-402a-a305-1fc26c61d244");
                    buffer[i] = replace;
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "81f41f0f-87ce-469c-903c-eeeff09d9794");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6475484f-487d-47d5-8b99-d33d804b0397");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Replaces the search string with the replace string throughout the builder.
     *
     * @param searchStr  the search string, null causes no action to occur
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replaceAll(final String searchStr, final String replaceStr) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "21d32bf3-d564-4e6a-a53d-43ae81a524d6");
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e2eb380d-5b33-4d22-983b-80e18da7e5ea");
        if (searchLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9b04d8a4-76cf-47ec-90ad-c6ba694693a7");
            final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "23064556-3f1e-4f7f-8a7f-174b8906c06b");
            int index = indexOf(searchStr, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b412b44b-2ce0-4104-ac2f-2440c8995e63");
            while (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ab88caaf-0b33-4222-a0c2-07ff4357e716");
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c1890cca-08f3-473c-abcc-160df19fcd90");
                index = indexOf(searchStr, index + replaceLen);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "db4ca8f1-915d-4e7a-90b8-730dc8bdd903");
        return this;
    }

    /**
     * Replaces the first instance of the search string with the replace string.
     *
     * @param searchStr  the search string, null causes no action to occur
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replaceFirst(final String searchStr, final String replaceStr) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b0d98ac1-2690-4bea-a521-16049b7e3f0d");
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2a9c5e10-678a-4a62-b26c-a7a81690e1b5");
        if (searchLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6352b3f5-b819-4431-859c-13ba8344ef07");
            final int index = indexOf(searchStr, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "68ec05fe-148b-43c7-bd08-1490614725f5");
            if (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9fcf5b8e-28c2-4628-9c7b-d12ffce66ab4");
                final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "09d02cfb-944b-4c41-9e92-c02ea8233938");
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b7584d7e-7fc0-435b-8bbc-4cd05c23d466");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Replaces all matches within the builder with the replace string.
     * <p>
     * Matchers can be used to perform advanced replace behaviour.
     * For example you could write a matcher to replace all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replaceAll(final StrMatcher matcher, final String replaceStr) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9cbe7c88-f1af-4bde-b128-d15b3c6b2778");
        return replace(matcher, replaceStr, 0, size, -1);
    }

    /**
     * Replaces the first match within the builder with the replace string.
     * <p>
     * Matchers can be used to perform advanced replace behaviour.
     * For example you could write a matcher to replace
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replaceFirst(final StrMatcher matcher, final String replaceStr) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dfa5008d-b505-43be-88ad-e7fa12bef124");
        return replace(matcher, replaceStr, 0, size, 1);
    }

    // -----------------------------------------------------------------------
    /**
     * Advanced search and replaces within the builder using a matcher.
     * <p>
     * Matchers can be used to perform advanced behaviour.
     * For example you could write a matcher to delete all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the string to replace the match with, null is a delete
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     * that if too large it is treated as end of string
     * @param replaceCount  the number of times to replace, -1 for replace all
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if start index is invalid
     */
    public StrBuilder replace(final StrMatcher matcher, final String replaceStr, final int startIndex, int endIndex, final int replaceCount) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "db2c9efb-aa54-4c2f-9841-9b9cce0b3db3");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c2953458-eddd-42a2-be0e-c9168414dc9a");
        return replaceImpl(matcher, replaceStr, startIndex, endIndex, replaceCount);
    }

    /**
     * Replaces within the builder using a matcher.
     * <p>
     * Matchers can be used to perform advanced behaviour.
     * For example you could write a matcher to delete all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the string to replace the match with, null is a delete
     * @param from  the start index, must be valid
     * @param to  the end index (exclusive), must be valid
     * @param replaceCount  the number of times to replace, -1 for replace all
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    private StrBuilder replaceImpl(final StrMatcher matcher, final String replaceStr, final int from, int to, int replaceCount) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cfa4082c-33a8-4f91-8b32-2a98ecbfad73");
        if (matcher == null || size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "441a1ac1-6858-4680-a7cb-e244d6863719");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "312ceb85-ced9-4107-9647-7a808cb51e02");
        final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cc995300-eb07-4cd8-8454-cbda52a0bc55");
        for (int i = from; i < to && replaceCount != 0; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "60142991-258f-4c54-bcfc-e9763fa17c85");
            final char[] buf = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cde4a9dc-3140-459e-aca2-9a5ee3729f97");
            final int removeLen = matcher.isMatch(buf, i, from, to);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0618ca92-5399-4682-a6af-323d8405258e");
            if (removeLen > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "730714e8-5f70-4309-98ed-dabb2481ae48");
                replaceImpl(i, i + removeLen, removeLen, replaceStr, replaceLen);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e9737049-5f2d-4836-9151-d3b1f5939677");
                to = to - removeLen + replaceLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8ff1dd5c-3099-4cfb-b89a-651c022648a2");
                i = i + replaceLen - 1;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "93095824-bc5d-4e94-9d02-b7adb091c3fc");
                if (replaceCount > 0) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0faa3866-0843-432f-be17-64973a1f6b54");
                    replaceCount--;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f6220a9b-cd76-4a56-96f4-ee4b700143e7");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Reverses the string builder placing each character in the opposite index.
     *
     * @return this, to enable chaining
     */
    public StrBuilder reverse() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2be3cb2f-cc71-46ac-930d-ceb30ec581bd");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "721f7c94-2228-463b-9125-0de5c1b72967");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "023de52c-05d8-4069-b927-cbfe0d66e546");
        final int half = size / 2;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "58a881a7-5de8-4785-bce7-12d0954c9cee");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "24266839-04ea-40ee-a608-c6059f402c9c");
        for (int leftIdx = 0, rightIdx = size - 1; leftIdx < half; leftIdx++, rightIdx--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c6ff86ca-b1f9-41e3-aee9-eb0c20a0bbd8");
            final char swap = buf[leftIdx];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "140c3669-b2c1-4e5d-aaad-93aa7b5a11e3");
            buf[leftIdx] = buf[rightIdx];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2f18b458-7d4b-4f8e-bdd1-5d2fcb0b21bd");
            buf[rightIdx] = swap;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c621123f-a90d-448a-aa50-982a73635b89");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Trims the builder by removing characters less than or equal to a space
     * from the beginning and end.
     *
     * @return this, to enable chaining
     */
    public StrBuilder trim() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "880642b2-147c-4009-92b9-03c9f7e2ce01");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "920f28ef-9954-474e-b230-63cb69ae927f");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a88d6638-ffa1-4c81-8742-2cbb0f67baf5");
        int len = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f40ea78b-8a4b-4ef8-8427-7095ee7c691e");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9ad321d7-e4f8-449a-9da2-72daba2fde33");
        int pos = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e9a1597c-7335-42d6-9962-d2a4265522f8");
        while (pos < len && buf[pos] <= ' ') {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b8ff8b9b-7c22-4349-a63e-b2cbf446a221");
            pos++;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "08dbeeec-767f-49ba-b226-8905e524f208");
        while (pos < len && buf[len - 1] <= ' ') {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7233b36d-2dff-4d59-8110-2ed782598cdc");
            len--;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b349842b-4abe-4181-b7d9-32e999bc8843");
        if (len < size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "afd437c3-b3f7-446c-b33e-f7583e08b267");
            delete(len, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e8c83c28-800f-4d2b-a6e7-1fb760ec7e45");
        if (pos > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "339d3576-a11d-4d36-97bd-c4315b365917");
            delete(0, pos);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "34a17039-7cc2-4319-b166-0d9235d988e6");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Checks whether this builder starts with the specified string.
     * <p>
     * Note that this method handles null input quietly, unlike String.
     *
     * @param str  the string to search for, null returns false
     * @return true if the builder starts with the string
     */
    public boolean startsWith(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "30a3f3ff-530b-49e2-92c5-3bc53edaf3c0");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "88bcd6ec-6885-4167-84c2-77c5032c533e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "90938d1c-285a-4301-ba96-369ec4d82a5b");
        final int len = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cb502269-d216-42cc-bab9-1a2ae8bb65d2");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "55117375-d061-4c91-b9d0-85c8dfd62bfc");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a428811d-8c00-4d13-a496-cb0e1b068d15");
        if (len > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "71a4dc2c-f2ac-47d6-a699-b0b115ae6c0a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6469e18b-a294-46b6-a034-420734f505c6");
        for (int i = 0; i < len; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0ab2df76-1252-4a22-a668-5ffa7d2f8a7b");
            if (buffer[i] != str.charAt(i)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "21f69363-d784-4a9c-b2a4-abf30f32f7eb");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4c9b1b7f-3287-402f-bdb8-91785cf97842");
        return true;
    }

    /**
     * Checks whether this builder ends with the specified string.
     * <p>
     * Note that this method handles null input quietly, unlike String.
     *
     * @param str  the string to search for, null returns false
     * @return true if the builder ends with the string
     */
    public boolean endsWith(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "11dc5407-b354-4f4b-831e-98c578b514da");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6101bb5e-3229-4de1-b242-e7b7ffa063d1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "eb1acdbf-e96c-40e9-8217-97a864d6f56d");
        final int len = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7f90bc08-f658-4a58-9666-64e8dc70f633");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "79a5cee2-ff61-4739-8cc2-a9deb3d83896");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1b48b34c-36bd-4aaf-b1ef-e5180d563c69");
        if (len > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7b9d0ee2-7ba9-4e5f-98a2-7d7973433481");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2c97c619-9edc-41ae-85d4-c74edbf6b6b0");
        int pos = size - len;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ca9872ed-579f-4fa0-b47d-073fbd5a49b3");
        for (int i = 0; i < len; i++, pos++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b7bff1ad-2904-4e2f-8bf8-52f319fab284");
            if (buffer[pos] != str.charAt(i)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d56a7f66-07fd-46a7-a38d-2c5763f5c898");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0793b35e-54c0-41c3-a39f-ce3abf0c7f84");
        return true;
    }

    // -----------------------------------------------------------------------
    /**
     * {@inheritDoc}
     */
    @Override
    public CharSequence subSequence(final int startIndex, final int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "be796189-d9da-4ec7-b0ec-b1629427320a");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6f69bb4e-e1a4-4373-9fbc-75883df96196");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c3bc9749-47a2-4701-9ee9-7a4d66edb3e2");
        if (endIndex > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7f39f8c9-e34c-46b1-82a6-d9074901cd78");
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ca919f6f-b928-40b4-bd8b-c8b236c83f1a");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6a157015-e2a3-44e7-98ad-7f94b25949a8");
            throw new StringIndexOutOfBoundsException(endIndex - startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1be08443-e6cf-44f7-bf9f-643100616152");
        return substring(startIndex, endIndex);
    }

    /**
     * Extracts a portion of this string builder as a string.
     *
     * @param start  the start index, inclusive, must be valid
     * @return the new string
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public String substring(final int start) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "886e2109-ef4c-4b41-94c6-d62b5600fd90");
        return substring(start, size);
    }

    /**
     * Extracts a portion of this string builder as a string.
     * <p>
     * Note: This method treats an endIndex greater than the length of the
     * builder as equal to the length of the builder, and continues
     * without error, unlike StringBuffer or String.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     * that if too large it is treated as end of string
     * @return the new string
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public String substring(final int startIndex, int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cad2f058-2feb-4f3a-8e1b-6451b973b452");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f27ca060-8196-4bf5-83de-977bb81dcef6");
        return new String(buffer, startIndex, endIndex - startIndex);
    }

    /**
     * Extracts the leftmost characters from the string builder without
     * throwing an exception.
     * <p>
     * This method extracts the left <code>length</code> characters from
     * the builder. If this many characters are not available, the whole
     * builder is returned. Thus the returned string may be shorter than the
     * length requested.
     *
     * @param length  the number of characters to extract, negative returns empty string
     * @return the new string
     */
    public String leftString(final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8d4474a4-57bc-4ee8-923f-cabfbcd966cb");
        if (length <= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bc15a99e-0664-478e-a05b-b432e2a02385");
            return "";
        } else if (length >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fe7c21bd-5657-40f1-a569-2dae156554d0");
            return new String(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "191a78ec-e633-4088-ae14-c6b779bf3996");
            return new String(buffer, 0, length);
        }
    }

    /**
     * Extracts the rightmost characters from the string builder without
     * throwing an exception.
     * <p>
     * This method extracts the right <code>length</code> characters from
     * the builder. If this many characters are not available, the whole
     * builder is returned. Thus the returned string may be shorter than the
     * length requested.
     *
     * @param length  the number of characters to extract, negative returns empty string
     * @return the new string
     */
    public String rightString(final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "606f797d-3f78-46e8-8270-10bc8f8c6a84");
        if (length <= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "44a22968-2f69-4b0c-b6e1-e6ae3b010cc7");
            return "";
        } else if (length >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7b277d70-e24a-42a8-b1af-f527becc73f8");
            return new String(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "86437d68-a1dc-418d-9e38-db9b7709a59a");
            return new String(buffer, size - length, length);
        }
    }

    /**
     * Extracts some characters from the middle of the string builder without
     * throwing an exception.
     * <p>
     * This method extracts <code>length</code> characters from the builder
     * at the specified index.
     * If the index is negative it is treated as zero.
     * If the index is greater than the builder size, it is treated as the builder size.
     * If the length is negative, the empty string is returned.
     * If insufficient characters are available in the builder, as much as possible is returned.
     * Thus the returned string may be shorter than the length requested.
     *
     * @param index  the index to start at, negative means zero
     * @param length  the number of characters to extract, negative returns empty string
     * @return the new string
     */
    public String midString(int index, final int length) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f5a4355d-67d1-4e33-942b-ae7f0a6486bb");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2558af9a-4826-4e76-8ffa-37a3887c934c");
            index = 0;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a2250221-6f3a-4bd0-8276-236361d044f1");
        if (length <= 0 || index >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6757e1fc-fef8-4996-8ac5-545ddf582a09");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1aa0b1f8-4787-486d-812f-83f06664db19");
        if (size <= index + length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9e171b05-73cd-4dc7-86d7-a031e7faaefa");
            return new String(buffer, index, size - index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ab89506a-b586-479d-bbaa-a125ed890c5c");
        return new String(buffer, index, length);
    }

    // -----------------------------------------------------------------------
    /**
     * Checks if the string builder contains the specified char.
     *
     * @param ch  the character to find
     * @return true if the builder contains the character
     */
    public boolean contains(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c4330e9e-363a-4472-9797-215128e06ff1");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "12b9deb1-e504-47b0-a06c-0f39cd702566");
        for (int i = 0; i < this.size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b36c975a-357c-46db-a779-f886cceea3cd");
            if (thisBuf[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d1ac8146-57c7-42f6-9cc9-f03c38e42eb7");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c7c3e8ed-749d-4e34-bf26-de02625ebdd0");
        return false;
    }

    /**
     * Checks if the string builder contains the specified string.
     *
     * @param str  the string to find
     * @return true if the builder contains the string
     */
    public boolean contains(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d522ff40-25af-4995-b005-8d89e38ef871");
        return indexOf(str, 0) >= 0;
    }

    /**
     * Checks if the string builder contains a string matched using the
     * specified matcher.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to search for the character
     * 'a' followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @return true if the matcher finds a match in the builder
     */
    public boolean contains(final StrMatcher matcher) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "dbd0fcc5-c528-43ba-9016-293c912c2384");
        return indexOf(matcher, 0) >= 0;
    }

    // -----------------------------------------------------------------------
    /**
     * Searches the string builder to find the first reference to the specified char.
     *
     * @param ch  the character to find
     * @return the first index of the character, or -1 if not found
     */
    public int indexOf(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c08a4f75-b0ec-480a-ae5a-64d15a1073e5");
        return indexOf(ch, 0);
    }

    /**
     * Searches the string builder to find the first reference to the specified char.
     *
     * @param ch  the character to find
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the first index of the character, or -1 if not found
     */
    public int indexOf(final char ch, int startIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "8c1f6ebe-bb07-459e-85cb-ae5e30ec6ade");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5b69efc0-1d71-4028-b6ba-45ddf88b3b35");
        if (startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a9cc55e6-c149-4367-953d-96e6af2a9527");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f2345ba8-b8f0-4459-8bd5-25214d617038");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "76a09c6d-2589-48b2-a6ac-76c5875a66e8");
        for (int i = startIndex; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "57032baf-7bb6-4c9e-830e-927a309a0710");
            if (thisBuf[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "11dac531-076e-4fe5-a217-8f8fdb6244ca");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b532d85f-b43b-4789-b157-dbae718ae7d5");
        return -1;
    }

    /**
     * Searches the string builder to find the first reference to the specified string.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @return the first index of the string, or -1 if not found
     */
    public int indexOf(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bc39aace-3e4f-4f9e-b423-b46b171723ce");
        return indexOf(str, 0);
    }

    /**
     * Searches the string builder to find the first reference to the specified
     * string starting searching from the given index.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the first index of the string, or -1 if not found
     */
    public int indexOf(final String str, int startIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "95001299-ec7d-4645-a552-dbdfd6fd9923");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "033e8d95-ef3a-44ce-a800-a4a121c3a170");
        if (str == null || startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d19ec41c-1543-48ba-95c9-a7cdcdfcb6bf");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7965d1fe-6c80-4d82-bf61-3ddfc4c27329");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6b0b8012-e6ff-452e-99a1-19851d0287f2");
        if (strLen == 1) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b548fb26-4499-4940-8b2c-4e1ec5f720cf");
            return indexOf(str.charAt(0), startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1689d428-05c2-4838-8dc3-e8442544231e");
        if (strLen == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b0c45f33-9b79-4465-b84a-d02102c60407");
            return startIndex;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "857fadfb-9dd0-4554-b53b-7d782925f2b4");
        if (strLen > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "caf322a1-a68a-4c61-9a3a-6a7cf1b7332f");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cfae0887-c79b-4457-993f-08dad0b4db72");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0641ea4d-c736-4c0f-9f16-238908da7211");
        final int len = size - strLen + 1;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "261ead64-faf2-42d7-b2f8-f26d2211806e");
        return -1;
    }

    /**
     * Searches the string builder using the matcher to find the first match.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @return the first index matched, or -1 if not found
     */
    public int indexOf(final StrMatcher matcher) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "993ee166-7a57-40dd-a7a2-d103c248c542");
        return indexOf(matcher, 0);
    }

    /**
     * Searches the string builder using the matcher to find the first
     * match searching from the given index.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the first index matched, or -1 if not found
     */
    public int indexOf(final StrMatcher matcher, int startIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "b54e473e-6baf-4f7b-9023-ccdd9e50fb2d");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7c42054d-301b-4fca-bba7-feebcb314e1c");
        if (matcher == null || startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d6fe27c8-78ba-4749-912c-76ede5c1e3e1");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e6366349-5987-48cf-9be4-882cf10631ed");
        final int len = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "49ba690a-75fe-4132-8d0e-495521255550");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "36838a55-a083-4b2f-9087-06e7aeb88e6f");
        for (int i = startIndex; i < len; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "02462793-1cae-40d3-9617-7441cd74818c");
            if (matcher.isMatch(buf, i, startIndex, len) > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3e3a70e2-d76a-4164-b3f2-efa0caae76ce");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "71cc6fc1-d7db-4083-a46f-4814ba61747c");
        return -1;
    }

    // -----------------------------------------------------------------------
    /**
     * Searches the string builder to find the last reference to the specified char.
     *
     * @param ch  the character to find
     * @return the last index of the character, or -1 if not found
     */
    public int lastIndexOf(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7da343fe-2ab7-4777-af6c-1a7c18ae46b3");
        return lastIndexOf(ch, size - 1);
    }

    /**
     * Searches the string builder to find the last reference to the specified char.
     *
     * @param ch  the character to find
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index of the character, or -1 if not found
     */
    public int lastIndexOf(final char ch, int startIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cd7f2b03-c547-4c80-a40b-bbb8f8152de7");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4c6be77a-3901-40a0-bdc9-30f741ca7ae4");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6d4b1f6e-d535-4928-a6b1-8d48cf57e7be");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ddac2ab5-3e1d-4a01-9ac3-f8797ff2af61");
        for (int i = startIndex; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "894b3e64-8d8d-4943-9787-fdc02d143a35");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e0504349-3925-4e56-892b-2aa73198de10");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ab0c649f-2d64-4fb9-aacf-a0dbaab46338");
        return -1;
    }

    /**
     * Searches the string builder to find the last reference to the specified string.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @return the last index of the string, or -1 if not found
     */
    public int lastIndexOf(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ef38d608-1b72-4e5c-82b2-1db272974d9e");
        return lastIndexOf(str, size - 1);
    }

    /**
     * Searches the string builder to find the last reference to the specified
     * string starting searching from the given index.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index of the string, or -1 if not found
     */
    public int lastIndexOf(final String str, int startIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0f7daf7b-2488-4408-aa12-e57895e96b73");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7d1cccff-ca33-4209-8d48-4a40dce0e888");
        if (str == null || startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d495e764-b4e4-492d-b9b7-3ccd1ab9f85f");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1b74d2f4-1f1d-4d88-b84b-1d10d6389dc2");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "722e86f4-6f8f-40f3-929a-eebe89693636");
        if (strLen > 0 && strLen <= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "54d22385-20e2-4426-8b2d-fdbea6a38ada");
            if (strLen == 1) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e2059f20-4412-4b50-9cca-69bed73c1255");
                return lastIndexOf(str.charAt(0), startIndex);
            }
        } else if (strLen == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3b692e09-43f6-4583-bf10-a368de8136e8");
            return startIndex;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bec21699-a47c-4bd0-8f1f-3e2d907ce3c1");
        return -1;
    }

    /**
     * Searches the string builder using the matcher to find the last match.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @return the last index matched, or -1 if not found
     */
    public int lastIndexOf(final StrMatcher matcher) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f0aa3357-1aa3-43fe-90f4-20376691b6ea");
        return lastIndexOf(matcher, size);
    }

    /**
     * Searches the string builder using the matcher to find the last
     * match searching from the given index.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index matched, or -1 if not found
     */
    public int lastIndexOf(final StrMatcher matcher, int startIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "89c359ed-751f-4929-bb00-6c44e28f8382");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "55a5b1bd-0030-4e5c-a25e-6f00ae692672");
        if (matcher == null || startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0cf580c6-1414-4f8f-909d-a6461e77723d");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "707d1ba9-a3db-46c7-ab69-b07dd0846238");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3d516d98-e475-46f8-896c-98271aeeabe7");
        final int endIndex = startIndex + 1;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "4c5bb75c-e002-4a1c-839a-01106bef31b0");
        for (int i = startIndex; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9fdb2090-648e-4a28-ba55-cfae34f5bf0c");
            if (matcher.isMatch(buf, i, 0, endIndex) > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a967ccda-11f5-43c4-9f3f-349c6c5c3c93");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3392de93-2b61-435e-843e-c1259d52c0d6");
        return -1;
    }

    // -----------------------------------------------------------------------
    /**
     * Creates a tokenizer that can tokenize the contents of this builder.
     * <p>
     * This method allows the contents of this builder to be tokenized.
     * The tokenizer will be setup by default to tokenize on space, tab,
     * newline and formfeed (as per StringTokenizer). These values can be
     * changed on the tokenizer class, before retrieving the tokens.
     * <p>
     * The returned tokenizer is linked to this builder. You may intermix
     * calls to the builder and tokenizer within certain limits, however
     * there is no synchronization. Once the tokenizer has been used once,
     * it must be {@link StrTokenizer#reset() reset} to pickup the latest
     * changes in the builder. For example:
     * <pre>
     * StrBuilder b = new StrBuilder();
     * b.append("a b ");
     * StrTokenizer t = b.asTokenizer();
     * String[] tokens1 = t.getTokenArray();  // returns a,b
     * b.append("c d ");
     * String[] tokens2 = t.getTokenArray();  // returns a,b (c and d ignored)
     * t.reset();              // reset causes builder changes to be picked up
     * String[] tokens3 = t.getTokenArray();  // returns a,b,c,d
     * </pre>
     * In addition to simply intermixing appends and tokenization, you can also
     * call the set methods on the tokenizer to alter how it tokenizes. Just
     * remember to call reset when you want to pickup builder changes.
     * <p>
     * Calling {@link StrTokenizer#reset(String)} or {@link StrTokenizer#reset(char[])}
     * with a non-null value will break the link with the builder.
     *
     * @return a tokenizer that is linked to this builder
     */
    public StrTokenizer asTokenizer() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "bfe2ccf1-b4bc-40e6-bb74-7e4c10b9c6c0");
        return new StrBuilderTokenizer();
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the contents of this builder as a Reader.
     * <p>
     * This method allows the contents of the builder to be read
     * using any standard method that expects a Reader.
     * <p>
     * To use, simply create a <code>StrBuilder</code>, populate it with
     * data, call <code>asReader</code>, and then read away.
     * <p>
     * The internal character array is shared between the builder and the reader.
     * This allows you to append to the builder after creating the reader,
     * and the changes will be picked up.
     * Note however, that no synchronization occurs, so you must perform
     * all operations with the builder and the reader in one thread.
     * <p>
     * The returned reader supports marking, and ignores the flush method.
     *
     * @return a reader that reads from this builder
     */
    public Reader asReader() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "39318051-2b63-4d2f-954a-23e4559fc712");
        return new StrBuilderReader();
    }

    // -----------------------------------------------------------------------
    /**
     * Gets this builder as a Writer that can be written to.
     * <p>
     * This method allows you to populate the contents of the builder
     * using any standard method that takes a Writer.
     * <p>
     * To use, simply create a <code>StrBuilder</code>,
     * call <code>asWriter</code>, and populate away. The data is available
     * at any time using the methods of the <code>StrBuilder</code>.
     * <p>
     * The internal character array is shared between the builder and the writer.
     * This allows you to intermix calls that append to the builder and
     * write using the writer and the changes will be occur correctly.
     * Note however, that no synchronization occurs, so you must perform
     * all operations with the builder and the writer in one thread.
     * <p>
     * The returned writer ignores the close and flush methods.
     *
     * @return a writer that populates this builder
     */
    public Writer asWriter() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d5174386-76e5-4afb-a706-0b8ac0cea68e");
        return new StrBuilderWriter();
    }

    /**
     * Appends current contents of this <code>StrBuilder</code> to the
     * provided {@link Appendable}.
     * <p>
     * This method tries to avoid doing any extra copies of contents.
     *
     * @param appendable  the appendable to append data to
     * @throws IOException  if an I/O error occurs
     *
     * @see #readFrom(Readable)
     */
    public void appendTo(final Appendable appendable) throws IOException {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "95e4a568-4e92-4d17-9777-5d217599d0ad");
        if (appendable instanceof Writer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a14ccbed-d247-455e-8df9-b5e61369a101");
            ((Writer) appendable).write(buffer, 0, size);
        } else if (appendable instanceof StringBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "5dfe7b1a-56f5-477b-b955-5834b837381d");
            ((StringBuilder) appendable).append(buffer, 0, size);
        } else if (appendable instanceof StringBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "379d23c5-75bd-488f-8e32-9231b71ee94e");
            ((StringBuffer) appendable).append(buffer, 0, size);
        } else if (appendable instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "efb6eb58-86fa-42b7-bce3-c512bbe9436b");
            ((CharBuffer) appendable).put(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d48b7ea2-b1b3-407a-9e13-ed74106e8c75");
            appendable.append(this);
        }
    }

    /**
     * Checks the contents of this builder against another to see if they
     * contain the same character content ignoring case.
     *
     * @param other  the object to check, null returns false
     * @return true if the builders contain the same characters in the same order
     */
    public boolean equalsIgnoreCase(final StrBuilder other) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a7cc7c78-66db-4010-b3f4-211ea753de81");
        if (this == other) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c5215545-15cc-48f5-9c7a-90097b5c5947");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "62714b05-30db-4cff-a951-008caf2bff1f");
        if (this.size != other.size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "88181b55-c9ec-49fb-970b-c7e430424b36");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ade124bc-d38f-4d68-81cb-800457551e4d");
        final char[] thisBuf = this.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "7f913844-b8c2-4862-bad2-6c996d372add");
        final char[] otherBuf = other.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fe4931a8-525b-449b-a73a-28982a6c3f95");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "27722e95-d1f0-4004-8acc-62ff416e9d84");
            final char c1 = thisBuf[i];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "f487f19d-45b3-4598-933c-1553a7d050b4");
            final char c2 = otherBuf[i];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ffc9ab74-d984-400b-8d16-09f315aa2cb3");
            if (c1 != c2 && Character.toUpperCase(c1) != Character.toUpperCase(c2)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "cf2380bb-326d-4080-aa37-a632f7fe7b7d");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "91e0fecb-fca6-4bb6-a551-81ea32828a5b");
        return true;
    }

    /**
     * Checks the contents of this builder against another to see if they
     * contain the same character content.
     *
     * @param other  the object to check, null returns false
     * @return true if the builders contain the same characters in the same order
     */
    public boolean equals(final StrBuilder other) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "23173387-f9e4-4d61-bae7-30c32c95cdf8");
        if (this == other) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "14e01fc7-f856-40ec-ae93-75ece17f9f84");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "388edb40-f9ba-43b9-9ac4-887ef092e1c1");
        if (other == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9392492a-a549-43d1-aa98-1cfa2d75d7a2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "200536ca-d8e4-4eaf-8986-cb2ebe9b8553");
        if (this.size != other.size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "c1bdb988-7514-4313-8079-373920a7abc5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "e6cda347-caf3-41aa-a2a5-9a6f73b233f2");
        final char[] thisBuf = this.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "2decab26-b539-4eba-8445-f66b3e083be2");
        final char[] otherBuf = other.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0946e423-1b17-4084-9e99-8c36a543d848");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ec0d7b1f-ce1a-4714-b9ff-56bbc0392909");
            if (thisBuf[i] != otherBuf[i]) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "327950a8-b04e-4bc8-9d50-f928fbed3568");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "49229677-28a3-4f31-bc0e-8c7faecb93d6");
        return true;
    }

    /**
     * Checks the contents of this builder against another to see if they
     * contain the same character content.
     *
     * @param obj  the object to check, null returns false
     * @return true if the builders contain the same characters in the same order
     */
    @Override
    public boolean equals(final Object obj) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "88298b3a-6ea3-4824-b935-decd69a915c9");
        return obj instanceof StrBuilder && equals((StrBuilder) obj);
    }

    /**
     * Gets a suitable hash code for this builder.
     *
     * @return a hash code
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "d7e2da82-41da-486d-8d1a-5602adfc7fac");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "1ecb590e-e7b7-4ff3-a782-902ee15d73e2");
        int hash = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "97b977b1-b92b-40b7-b080-94e3cef1a839");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "67c53ba2-497e-4609-b628-32fbb2b866b2");
            hash = 31 * hash + buf[i];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "6a5136c9-6c60-4299-81dc-6d3d35728976");
        return hash;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets a String version of the string builder, creating a new instance
     * each time the method is called.
     * <p>
     * Note that unlike StringBuffer, the string version returned is
     * independent of the string builder.
     *
     * @return the builder as a String
     */
    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "fb628e70-d7ec-47ec-9690-1880d8f0025c");
        return new String(buffer, 0, size);
    }

    /**
     * Gets a StringBuffer version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuffer
     */
    public StringBuffer toStringBuffer() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a0e2496b-b444-49e8-8ca3-fc92268ede12");
        return new StringBuffer(size).append(buffer, 0, size);
    }

    /**
     * Gets a StringBuilder version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuilder
     */
    public StringBuilder toStringBuilder() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "9f1c670b-52a6-4e4a-a153-264892a6b98d");
        return new StringBuilder(size).append(buffer, 0, size);
    }

    /**
     * Implement the {@link Builder} interface.
     * @return the builder as a String
     * @see #toString()
     */
    @Override
    public String build() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0f9011d0-e2cf-431a-ab22-97cb333da3ae");
        return toString();
    }

    // -----------------------------------------------------------------------
    /**
     * Validates parameters defining a range of the builder.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     * that if too large it is treated as end of string
     * @return the new string
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    protected int validateRange(final int startIndex, int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ec8b5963-39e1-44cf-9ec9-d197e32f4dee");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "391836fa-7dfa-41c2-ab89-51bbda7bbcf5");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "3085c9a3-28f5-49d8-a131-7868217dad81");
        if (endIndex > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "658cd8b2-0633-4c3a-9be8-11d0e44c36ea");
            endIndex = size;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "11d890d8-53e9-40b4-8220-c9eafd28e67b");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "ceb9bf95-103a-45b7-9c53-4674e0e9a06b");
            throw new StringIndexOutOfBoundsException("end < start");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "a0c0fab5-7458-4ac0-95ae-0e150c91cb50");
        return endIndex;
    }

    /**
     * Validates parameters defining a single index in the builder.
     *
     * @param index  the index, must be valid
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    protected void validateIndex(final int index) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "81a44cfa-ed7f-40ef-a20b-f9079a5e996f");
        if (index < 0 || index > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_3_10.coverage", "0e433e00-8255-4e3c-bb64-33ad4c3ec97f");
            throw new StringIndexOutOfBoundsException(index);
        }
    }

    /**
     * Inner class to allow StrBuilder to operate as a tokenizer.
     */
    class StrBuilderTokenizer extends StrTokenizer {

        /**
         * Default constructor.
         */
        StrBuilderTokenizer() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected List<String> tokenize(final char[] chars, final int offset, final int count) {
            if (chars == null) {
                return super.tokenize(StrBuilder.this.buffer, 0, StrBuilder.this.size());
            }
            return super.tokenize(chars, offset, count);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getContent() {
            final String str = super.getContent();
            if (str == null) {
                return StrBuilder.this.toString();
            }
            return str;
        }
    }

    /**
     * Inner class to allow StrBuilder to operate as a reader.
     */
    class StrBuilderReader extends Reader {

        /**
         * The current stream position.
         */
        private int pos;

        /**
         * The last mark position.
         */
        private int mark;

        /**
         * Default constructor.
         */
        StrBuilderReader() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void close() {
        // do nothing
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int read() {
            if (!ready()) {
                return -1;
            }
            return StrBuilder.this.charAt(pos++);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int read(final char[] b, final int off, int len) {
            if (off < 0 || len < 0 || off > b.length || (off + len) > b.length || (off + len) < 0) {
                throw new IndexOutOfBoundsException();
            }
            if (len == 0) {
                return 0;
            }
            if (pos >= StrBuilder.this.size()) {
                return -1;
            }
            if (pos + len > size()) {
                len = StrBuilder.this.size() - pos;
            }
            StrBuilder.this.getChars(pos, pos + len, b, off);
            pos += len;
            return len;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public long skip(long n) {
            if (pos + n > StrBuilder.this.size()) {
                n = StrBuilder.this.size() - pos;
            }
            if (n < 0) {
                return 0;
            }
            pos += n;
            return n;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean ready() {
            return pos < StrBuilder.this.size();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean markSupported() {
            return true;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void mark(final int readAheadLimit) {
            mark = pos;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void reset() {
            pos = mark;
        }
    }

    /**
     * Inner class to allow StrBuilder to operate as a writer.
     */
    class StrBuilderWriter extends Writer {

        /**
         * Default constructor.
         */
        StrBuilderWriter() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void close() {
        // do nothing
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void flush() {
        // do nothing
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void write(final int c) {
            StrBuilder.this.append((char) c);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void write(final char[] cbuf) {
            StrBuilder.this.append(cbuf);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void write(final char[] cbuf, final int off, final int len) {
            StrBuilder.this.append(cbuf, off, len);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void write(final String str) {
            StrBuilder.this.append(str);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void write(final String str, final int off, final int len) {
            StrBuilder.this.append(str, off, len);
        }
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
