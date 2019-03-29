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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "de1bb82b-9cd1-42ee-8c28-9ce5d90f937e");
        return newLine;
    }

    /**
     * Sets the text to be appended when a new line is added.
     *
     * @param newLine  the new line text, null means use system default
     * @return this, to enable chaining
     */
    public StrBuilder setNewLineText(final String newLine) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ac8501df-d020-4d81-a675-e59a13cb911b");
        this.newLine = newLine;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b3ef966b-37a6-4b87-a5bb-38bca88a70d6");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the text to be appended when null is added.
     *
     * @return the null text, null means no append
     */
    public String getNullText() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ff40c4d8-9d3c-4fa0-bfc4-ed6fa7a7c13f");
        return nullText;
    }

    /**
     * Sets the text to be appended when null is added.
     *
     * @param nullText  the null text, null means no append
     * @return this, to enable chaining
     */
    public StrBuilder setNullText(String nullText) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aaed5f5d-214c-4bc5-8ea2-0f0693ef4803");
        if (nullText != null && nullText.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "079cca98-a792-41e7-b5d0-4140e19c0e62");
            nullText = null;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "57c1299f-ba4b-4479-85e2-82b0fae822a6");
        this.nullText = nullText;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f229ac41-1a73-498f-aa6d-e8f94d009042");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "54e75975-69b6-47bf-9290-98f5b6888c62");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "493effbd-962b-4aa0-9f29-0ec3071dca6d");
        if (length < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2c43fd61-8e86-4b1d-b0a0-9335967cfc13");
            throw new StringIndexOutOfBoundsException(length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e21e52ee-c15a-4101-8aa1-6bc0bc3de1c2");
        if (length < size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "20648aa0-b9b3-4ad6-a331-e572d6817218");
            size = length;
        } else if (length > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "415879d7-948d-47ea-aa28-a3f823415ba9");
            ensureCapacity(length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "41782ea2-5c28-4e98-b448-c824394a8172");
            final int oldEnd = size;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fe32a16e-a2f1-409a-b48a-544837b2d35d");
            final int newEnd = length;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "59890bf9-bc32-4d62-86cb-3e9ecfbe75d9");
            size = length;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "52cf5c3b-c530-4b0c-92f2-c328427af618");
            for (int i = oldEnd; i < newEnd; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f1489e87-185c-4de9-ba6a-f56c0e09d049");
                buffer[i] = '\0';
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "33042cbc-4db7-4482-bb1a-90ed8ec8822e");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the current size of the internal character array buffer.
     *
     * @return the capacity
     */
    public int capacity() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ff2c7602-a2e7-4e43-b248-a10df24a4646");
        return buffer.length;
    }

    /**
     * Checks the capacity and ensures that it is at least the size specified.
     *
     * @param capacity  the capacity to ensure
     * @return this, to enable chaining
     */
    public StrBuilder ensureCapacity(final int capacity) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "69a64b4f-0f59-4d88-b7d1-7e31609f102c");
        if (capacity > buffer.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7b22945d-98b3-4651-9612-7f15e476e871");
            final char[] old = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fef867b1-4fbf-43b6-89d1-e53a8de52922");
            buffer = new char[capacity * 2];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "792db685-b7c3-40c2-ba33-e155fea6195c");
            System.arraycopy(old, 0, buffer, 0, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f33c06a8-1ba7-4659-ac16-dff0a41b3d0b");
        return this;
    }

    /**
     * Minimizes the capacity to the actual length of the string.
     *
     * @return this, to enable chaining
     */
    public StrBuilder minimizeCapacity() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1695fb60-2983-4f13-b47f-a83ea7c8e420");
        if (buffer.length > length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "419234a6-6675-49c8-9471-d9f13782425f");
            final char[] old = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9b720f85-1f13-4ed7-a583-4ef0a6e47324");
            buffer = new char[length()];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ef434e77-a7d2-4428-8b97-eaf4ae71863c");
            System.arraycopy(old, 0, buffer, 0, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a2c705b8-8d23-4360-be45-363202907fdb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "837fb542-89e7-45a8-bca9-992baf45ecc5");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "da92fb80-d85c-40f6-afa8-a03158eecbb5");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1efb8e24-c50e-4cdd-b3a0-c2464c0fc06f");
        size = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "148f2f90-2e29-4179-88fb-18358c506cda");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "790ef7cb-1737-4548-b1f1-906219bcdd1b");
        if (index < 0 || index >= length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ab3ffc6f-1d0c-4cfd-8593-b0d76d9cea98");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "53cb7d43-47c2-44ed-9229-b70e74bfc28b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c73a43cb-273c-4b41-a5ef-f525ae808fbe");
        if (index < 0 || index >= length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4b4667ab-9321-452d-b711-d815ec598344");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a0b24766-d81a-4cd8-8256-0564c7a9c86f");
        buffer[index] = ch;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1f251ff5-b1a9-4526-828e-f1309b5527f4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f904f5e2-067c-4b7f-8c13-27da2a1e480f");
        if (index < 0 || index >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "88e5e56a-57b1-4a00-9926-7c498df355bc");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "62ed617b-79ce-4979-8b2f-c7100c445d50");
        deleteImpl(index, index + 1, 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3d4fe58a-b82e-4656-ab85-254091f2e48a");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Copies the builder's character array into a new character array.
     *
     * @return a new array that represents the contents of the builder
     */
    public char[] toCharArray() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bb95dcaf-94cc-46ee-9363-8fa9795d5dbc");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7f563e14-5523-4976-bc90-030f82af77b6");
            return new char[0];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "27b6b529-ab0b-4c50-9816-47e23c9ab458");
        final char[] chars = new char[size];
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1c1fef07-96d2-4c2e-bc0f-49cd4fe54797");
        System.arraycopy(buffer, 0, chars, 0, size);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "76f56e1d-40e6-4e46-82c7-dd173bc913d2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dd5e5560-78e2-473c-a22e-724844463302");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dad80ff6-bda7-4f2a-b2b1-57335dc639f4");
        final int len = endIndex - startIndex;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b97e7bc7-b952-4e82-8fd3-03acc52cf1ce");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2c4981e4-6c9b-4915-ac6f-0cc0c1e96221");
            return new char[0];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ca341c6d-58eb-4083-9cce-b00d5a8ca7f0");
        final char[] chars = new char[len];
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a266fba3-857a-4eef-8af8-7ba71d657616");
        System.arraycopy(buffer, startIndex, chars, 0, len);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5fe40111-1428-49d0-a4c4-c56040673f5e");
        return chars;
    }

    /**
     * Copies the character array into the specified array.
     *
     * @param destination  the destination array, null will cause an array to be created
     * @return the input array, unless that was null or too small
     */
    public char[] getChars(char[] destination) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6a9f87d5-6e8f-4749-be6a-46bfe55e8070");
        final int len = length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4b4e9520-bbb9-4fa5-a860-88c5a03ff457");
        if (destination == null || destination.length < len) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "546044fb-8eee-4b7c-b52c-404393f1ac8a");
            destination = new char[len];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "77320466-ed7e-4019-b6f7-0cecd66097d8");
        System.arraycopy(buffer, 0, destination, 0, len);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "01b63d83-941e-4a16-b50e-71d506311dfa");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "38005c74-35ba-4d1d-ac6d-de0cd6fa3e5c");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "529af18c-cb32-4798-985e-d3b016ecd4dd");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a846ea91-f016-47bb-adca-8da7dd8caec4");
        if (endIndex < 0 || endIndex > length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1ea50008-5cb5-4999-9689-4a6b3ddfcf3c");
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cdbd4823-259c-43b9-a1b0-6a3bc2571c39");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e3c8171b-71fe-41bb-9d97-6cb77a4307bc");
            throw new StringIndexOutOfBoundsException("end < start");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "af466659-af6c-4b93-93d5-77c5146fc16f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b4ad3d36-0361-4caa-8aea-9d54b9219019");
        final int oldSize = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a384ce2f-3f75-4616-be4b-5202bad4dbb2");
        if (readable instanceof Reader) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4f688393-59b5-4ca4-a439-14c9a241fa82");
            final Reader r = (Reader) readable;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "014b6d60-6f90-41a1-9aa2-9f31dbfb206c");
            ensureCapacity(size + 1);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a6b11fb2-d9ca-4466-8b12-aa7676a28b0c");
            int read;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "457c9ff8-cc3b-462e-b426-4e61839f0951");
            while ((read = r.read(buffer, size, buffer.length - size)) != -1) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "70f38c71-9a46-46c6-9f9b-efafd79dfa28");
                size += read;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "139c49d9-c6a4-42b4-8ddf-06d5df09b0ec");
                ensureCapacity(size + 1);
            }
        } else if (readable instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d9a3926a-3809-4796-a3e7-1c0bd9938c91");
            final CharBuffer cb = (CharBuffer) readable;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "16db8816-bb7c-41e3-9ad4-65cfb594732c");
            final int remaining = cb.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "22c67337-7194-4a3c-abff-588c42e83f91");
            ensureCapacity(size + remaining);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4c00bf79-3a04-42c2-9509-4752224f2545");
            cb.get(buffer, size, remaining);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d2a43fe5-2579-414e-840c-03709e5fb57a");
            size += remaining;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d5339828-eb5a-4f19-b199-86a0d0a8c0c5");
            while (true) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4885eb64-e5a4-49f6-a204-4a6fd7bdbae5");
                ensureCapacity(size + 1);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "67b4d37d-1646-4ebf-ad48-e73a582e9337");
                final CharBuffer buf = CharBuffer.wrap(buffer, size, buffer.length - size);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "13ca002c-8a1e-474e-aad1-e2cff0dd0d71");
                final int read = readable.read(buf);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d8604dc1-3e05-46b7-94f0-fb23f1b50032");
                if (read == -1) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "01d068f1-716c-4575-b14d-f679e558bc4c");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "51a7ec36-3db9-4410-b4b4-2595765c0636");
                size += read;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d0f03ca2-9f21-4161-8471-47184bbc3c69");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ae9146c7-abd6-4b98-9baf-2919434794cb");
        if (newLine == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8b568325-343d-4442-8511-7c25d7342e46");
            append(System.lineSeparator());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9be896c6-0c48-43df-8e2c-2c3881fac1ad");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4418d662-7392-42cb-94c7-25386e5e52b0");
        return append(newLine);
    }

    /**
     * Appends the text representing <code>null</code> to this string builder.
     *
     * @return this, to enable chaining
     */
    public StrBuilder appendNull() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "43041c4c-22b2-4bfc-9d73-1c3aafd8bdb4");
        if (nullText == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6d5cd367-4320-4046-ae0a-1d49628e3984");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ee320b8d-c04e-410c-8265-e5fe07dc773e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2d2650e1-d700-465c-a95a-26fc0112c46f");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c332aabf-5d2c-41bf-a430-994ba0b16964");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a5120eee-35a0-4676-a6e1-d1a436ac077f");
        if (obj instanceof CharSequence) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8e532e03-d761-473d-b8de-ea0bd889ebbb");
            return append((CharSequence) obj);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1ef0e865-0654-4827-a441-4157d6a8c61c");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "56fd4716-4fca-43ba-b184-bd6b8b59c930");
        if (seq == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "524a791a-b080-4ca6-ae95-43d4979e6747");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9b0c1aeb-236b-4101-907c-0643fc3b6e06");
        if (seq instanceof StrBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f2647084-d7cf-43d2-a26d-27486f1d4afb");
            return append((StrBuilder) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e4f4874f-3e02-4e49-97f3-16112409bc50");
        if (seq instanceof StringBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "37fb58f1-f700-4b29-82a0-962591603263");
            return append((StringBuilder) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "196bfa8c-3585-4562-abf3-2739cae2b2bb");
        if (seq instanceof StringBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "27c679ab-4b96-4b2c-a85e-a054170dcdf4");
            return append((StringBuffer) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9bbfb7b3-04fa-43d7-93d8-e17dfa79cef2");
        if (seq instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "643739e9-dd6c-45b7-82a6-624814618633");
            return append((CharBuffer) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d3b6c44d-dd91-4a40-aad6-d4eff835a74c");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9a183f81-f6e7-4a56-98fa-c68fd6e016af");
        if (seq == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "66086263-d725-4218-806c-be2486a09354");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cf90e993-89fc-4ade-a77d-9261f08863bb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c8eac7df-581d-4ffa-84f3-147c01cc733d");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5b638ea2-0f93-4739-805f-fb6b2c901af6");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1b80453d-d350-4068-b85e-a17a3d00540a");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "93206f3a-8d4f-4b60-9517-788fec2c8f6d");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e778ddae-0553-41e8-bcd0-0df98dd7baf2");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "68d9528a-2980-400f-ae1c-279b707fa7e2");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "68ad2d92-1d61-4039-a32b-876b6ae14cd8");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e649d743-c831-48fa-a74b-6da2e0c0534e");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "73c88f73-62b2-46a6-944f-8b146b715b8a");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7bdce656-f435-4604-b85c-34e6dcbebaaa");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f1ef87c1-c741-4e58-b32a-e310ee4af75a");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "94429ecd-499a-4dba-9785-a39467ca7a4f");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d7b20cd0-2934-4e1e-8440-5b2f30fd8258");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "27da1ce5-e674-43c7-a39b-114b6dd3e220");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "60ea1103-0416-48f8-b4a3-89fc7cb02ce4");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1f571cf3-9a90-46ba-9252-9db2197b36ff");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7e64c670-da3a-4814-bc83-36f3645e647c");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b9dce572-cd2b-4e66-a31d-aaec5f71d66b");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "440f90c0-32a3-4778-925b-720cda1a3a48");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "161a7d23-d830-4776-9e32-a8ae64b00c2b");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "76c7a93c-6e6a-4cb6-b54d-719b5d52ee40");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fa90106a-636f-4a13-a51a-2ba1cae3ba80");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e773c389-3fa0-49d5-be27-a15dd6ea917f");
        if (buf == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d7816d7f-50e4-4a29-b086-eb321a3dc148");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "03c70211-6c36-411f-b103-c9ea4dbab599");
        if (buf.hasArray()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ca721fac-465a-4126-a205-864c99b0d49a");
            final int length = buf.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f420dd7f-cd1b-40ba-a7b0-890b66c3ec12");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5154974d-5faf-48b6-800c-e94457e1e9be");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c89b933a-0fed-44c5-ba58-4b5ddcc6668e");
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position(), buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fc92a8c6-6918-4bfd-a3c1-6500a4bb2f29");
            size += length;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0b6368f0-c433-4a2d-ae7b-8855922e8c61");
            append(buf.toString());
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "852fa3d1-260f-46aa-80a4-3e3fe0bf5225");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4fdcb733-5040-497b-8c2b-da37ecc65f36");
        if (buf == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "694defc1-697e-4a58-9b9c-009ad7a95fe8");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "edfa5e98-d970-4da0-9253-89500e91a75c");
        if (buf.hasArray()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7ddb77d7-7d37-470e-907e-7c323f01c23f");
            final int totalLength = buf.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "55806251-7685-43d1-af75-e5529d5441b1");
            if (startIndex < 0 || startIndex > totalLength) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0bdc324f-495e-476a-9bfb-7a880b9ded3d");
                throw new StringIndexOutOfBoundsException("startIndex must be valid");
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "94b65c8e-454c-4b74-99af-4eafc47eabe6");
            if (length < 0 || (startIndex + length) > totalLength) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e2d59cc0-4011-4507-86aa-53cf6cd49066");
                throw new StringIndexOutOfBoundsException("length must be valid");
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "085cb1ef-a0ac-4437-8a90-bbf70f2906bc");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cb1ee801-6a66-42fd-b922-4e8fc3a13cf7");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f6711a86-6dcd-46cf-adeb-aff0ec98880b");
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position() + startIndex, buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7b397315-2417-4f7b-b780-6b44e5b28a6e");
            size += length;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "eea5c1b6-05e5-4ebf-a621-26ab13c412c9");
            append(buf.toString(), startIndex, length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "55806d5b-6cd5-4cb4-ac2f-c5f4dec719d8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c31c23f5-ece7-4e3f-9a8f-86ceec65ecbc");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6b2dc4cb-b6b6-4809-927f-8122b6545c51");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aaaa2736-dd9d-401d-82df-afc7942f0f70");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "32e85c19-f249-465e-b1b8-06bf296e3042");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d77f65d2-1d47-4663-8fb7-12abbf665ccd");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d66f280b-2e1c-466e-9f4b-d9de7b9552e3");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fa88265b-7467-47f7-8dd7-bad8bb37f323");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "384f0cad-3caa-471d-bf8f-7c598fa830ee");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d66c0ea0-e9e2-4b2d-8559-4aec4360dcd4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e9d18cef-ca11-443b-9e17-4845e23f77c1");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "46f9e623-422b-41e3-922a-0aa330c96673");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9d6a5b42-3b3e-4f69-bf7d-c44e44b4659a");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b31b5e9f-52de-4ce1-8224-2ad31a708043");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f06acbd9-4c82-41d6-a979-4dada85a0578");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "22d374e6-47ab-4548-bdd5-eefe650e6a69");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cd4ae647-4642-4815-9432-dcacbefa4ae7");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6a977ece-69bc-4dad-afce-aaba8c60aab3");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4aca3fb5-dacb-4d19-85da-75db1313e1e1");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "86442b7d-64f2-4c4d-b551-689d47a41d10");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "28d0fa54-0e35-4420-ae4f-1aeada489c58");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7e03cf22-d531-41fc-a6e4-7ddf33edba3b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7346539b-ca0a-4327-b321-3a0cfbd1d93c");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c965f143-efd9-4e2e-a12e-719af112259f");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a7cb3e3a-4db8-4eeb-b498-62bc286eff71");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "29b4620d-f580-4db9-8279-48ac7f67a576");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "89711b3d-aaa6-4e14-8efc-565af8fb68e4");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "88f01d13-772e-472c-84c0-999b35b36a0f");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "956690bb-73f9-44f1-9ad2-829a61d1dbc4");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6e1ad103-a358-4aaf-b2b0-be36324e3dee");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6439fb94-db60-4a4a-bff7-8d5468996a01");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a882b6cc-b784-46e7-bebf-8479715bb80c");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f399bc18-8380-42f0-b63f-748a37db9465");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d5a937c4-f68c-4c7a-b4cb-fd4b126b8c9d");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4fba9ff2-5d9f-48b7-9923-a140cddeb329");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "325daade-1a77-44c7-a6e7-20854cbaf486");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "688d9d29-3407-43d1-bf4b-d76aa5f85bba");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e3ca8513-8873-404b-87b1-fedee0315901");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "305a48c7-b4a5-40da-a058-9adec72556e3");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3e3557d9-950c-48f1-8365-c75b6ef5dbe1");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d432dcd9-8a72-4fc6-8cd3-386eeb21c462");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e4dbafe7-b887-4c94-8042-22db7470f2d5");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "585021aa-2b2e-4adb-ab38-a309ce8aa923");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7d27ef7e-4cc5-48c3-994e-fc06f515283d");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c4f868d2-94d6-4054-a3a3-d7cae786caa0");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e9f71d6b-bd6a-4839-a711-83e6d0f0841a");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bbef3e55-236f-4999-8fb3-d50d8da33fdd");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "066468d5-74a2-4892-aebc-a1ccd9041bf4");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8bd81030-4e88-4708-b927-5a818673e4d1");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "57b8fb2a-a1df-4970-bf25-ffad1737e784");
            System.arraycopy(str.buffer, 0, buffer, len, strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4f3e7b3c-75b7-4734-b67d-e379b10dc07c");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "85da219e-f297-426b-a0d0-d8ccd8433542");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "def9c6f9-8346-40af-b3f5-5d319460e823");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bf1d1baa-00e4-43c9-b02d-00198115648e");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b003052a-71ce-44a3-b3eb-6ddfb45144ca");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7001f4e7-0771-4259-a4b9-e8ea024ca787");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "170652e4-ba07-45c8-9497-2c5f9f2f21de");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1ebb3c3a-5d25-4ce8-838d-3dcf7a197f44");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2ac461c2-d68c-46aa-a9d4-fa53cd93f186");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "17e7f7c0-dc82-4f8c-aa8f-39d59f4c7bc2");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "87ac136b-18b5-469e-8e15-198752fb906c");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f6e3d505-1230-4a4a-b933-1dfa8954bff8");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "28cd5f33-a7ac-4587-a6c4-658bf02d88a1");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "70065510-35f0-4162-88cf-2b914666d3fa");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6f9b51af-0ee4-42bc-930e-749b66353b55");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e51864ce-a7c0-4d6b-8c62-3958ae44a4b2");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "72fb884d-fdd9-4691-be09-b98b0859d923");
        final int strLen = chars.length;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0f1b077f-e3a2-4148-8e4f-19a95b976266");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "85927cfd-2ad8-4e90-892d-7f83f497f5f9");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2e8ea34a-2745-4c5f-88b1-b89d8ec79db6");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7e55001f-0eb3-48d6-9d58-26c2ea5d3300");
            System.arraycopy(chars, 0, buffer, len, strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0cc96dc2-3ea1-4c81-99d6-cf9518fd166d");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cfec11b1-d358-438f-9d58-7f6d2534963d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3f8c5859-2552-4f4a-830b-a2e7300ebd7e");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f66c02e5-7161-47e0-bd59-c96e90419128");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "67caf11b-1832-4445-b02d-03e6229028fb");
        if (startIndex < 0 || startIndex > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b9df4d54-7a15-433d-bc3e-6b511213ae84");
            throw new StringIndexOutOfBoundsException("Invalid startIndex: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c526ba83-36b6-4378-9951-c83ffd86ba74");
        if (length < 0 || (startIndex + length) > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1d926e47-ae07-4bb2-8a0a-1c6565a03018");
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bf912426-a605-4555-ad0d-faf9bd906c28");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fb1c81cf-9a25-4cc4-af75-f7217ffea531");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ff1b307e-9827-429a-891a-f3b23f2d1219");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "768fec92-a3ab-4436-9b6e-96b773d98e3f");
            System.arraycopy(chars, startIndex, buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7e8ce0a0-5de1-4967-8695-a8a2d00ca441");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ea23a83d-13cb-43f1-a3e4-a087a7824267");
        return this;
    }

    /**
     * Appends a boolean value to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "91573ed0-a5aa-41f7-b74f-534a2c67e382");
        if (value) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d6b7541b-ee4b-4b7b-a597-0ee95a8f996f");
            ensureCapacity(size + 4);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "03e93a38-9fa8-41b3-84a4-603c62cb5437");
            buffer[size++] = 't';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "41968f44-1c12-4cad-9850-83d1b40c1a90");
            buffer[size++] = 'r';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "62d7d109-ce4c-4df2-bc37-ec81dad722f9");
            buffer[size++] = 'u';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "19d037cc-82a9-4bcc-8abf-9e5279c124ef");
            buffer[size++] = 'e';
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cf7a9a72-d85b-4c65-837e-869b23611c2c");
            ensureCapacity(size + 5);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f7ea4674-e55e-45ed-9abc-2a71032a5355");
            buffer[size++] = 'f';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d669b0f4-2482-4978-94a7-44abe7c783c0");
            buffer[size++] = 'a';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e135f3f2-426c-4870-a99f-8df87547f089");
            buffer[size++] = 'l';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9cd2dfa5-c15e-44eb-82d6-442debf3098c");
            buffer[size++] = 's';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "310d0575-4d30-4976-bd77-771d116d765d");
            buffer[size++] = 'e';
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a6a5856c-0abb-41c9-a3b4-810ee3678fc2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9a8bd06b-8439-4fa6-8d45-a813dcc22b66");
        final int len = length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f9285689-b0f3-4440-b035-15d4a9684a95");
        ensureCapacity(len + 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0858eebe-5745-4d45-b625-cbfaa931064a");
        buffer[size++] = ch;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aeb7c8b5-3528-411f-af40-57d76afd7b71");
        return this;
    }

    /**
     * Appends an int value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9da2a597-ea92-43f2-af0f-ed77957550cb");
        return append(String.valueOf(value));
    }

    /**
     * Appends a long value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6e98b0ff-11af-40d4-bff6-ab902e30aa1c");
        return append(String.valueOf(value));
    }

    /**
     * Appends a float value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "307f57f2-6e3d-452d-8cf5-81437ef17997");
        return append(String.valueOf(value));
    }

    /**
     * Appends a double value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9f9100e9-05ee-4e61-b6e8-1f0f73ab9224");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5425014a-b46f-42bf-a9ee-cd4803a5144b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0534b099-9b1d-4a5e-9316-46664acdbe98");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "681a3124-ba1d-4dfb-9969-ae5aa5aed729");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ac47a954-7054-4c98-87fa-02c791919f82");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b795075a-7037-4823-8ff0-1b521e309c02");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a752cad3-e632-4b4e-b5f6-b4870bcab0b2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3cb22763-666c-406e-b573-1bde4e933818");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e1845da2-d7a5-4c87-98f3-3e0e94e24bcb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "652417f3-a6c8-4263-be0b-b6ba6b621bbe");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0b8aa1a8-31d2-446b-b477-15b5a92866a5");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b9a2d2f1-9a9d-4674-a410-af38878932c9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f7f08693-8cfd-48b3-ba83-5c706962484c");
        return append(chars, startIndex, length).appendNewLine();
    }

    /**
     * Appends a boolean value followed by a new line to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fabb5ac0-43c8-4b02-8371-9328c8771482");
        return append(value).appendNewLine();
    }

    /**
     * Appends a char value followed by a new line to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "14a3f034-1e70-4ba8-be07-ed467cce0371");
        return append(ch).appendNewLine();
    }

    /**
     * Appends an int value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "80235673-27ae-4210-bf0f-6c53449ef1ec");
        return append(value).appendNewLine();
    }

    /**
     * Appends a long value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c83cd8fc-7826-4d1e-b13d-ceb6fae3d965");
        return append(value).appendNewLine();
    }

    /**
     * Appends a float value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d13d57e2-add3-41fc-b8ad-c01b36ba387f");
        return append(value).appendNewLine();
    }

    /**
     * Appends a double value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4fa10f1d-d0e2-4b38-8f26-0791f808d773");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "346de873-8000-45f3-81d5-3d75ad636b2d");
        /*
         * @SuppressWarnings used to hide warning about vararg usage. We cannot
         * use @SafeVarargs, since this method is not final. Using @SupressWarnings
         * is fine, because it isn't inherited by subclasses, so each subclass must
         * vouch for itself whether its use of 'array' is safe.
         */
        if (array != null && array.length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8beac0b5-fb49-4a96-b99e-bf3734acf553");
            for (final Object element : array) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "34341b10-f077-4368-ab13-61ec9233ce8a");
                append(element);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f09bf66d-7f52-4e12-9d03-4eee5696e314");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4ca56787-fada-4e44-8944-3a9d3fb4a7b7");
        if (iterable != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "252e4f1d-5b6d-4f98-9ae8-d08be474626a");
            for (final Object o : iterable) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fb8f8c50-8953-4ad8-8bd0-ca02aaffdf8a");
                append(o);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "588fbef5-a663-4dd9-9b41-82886d4d75c0");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9b4eecc9-68b0-419c-b10a-44c872762c9b");
        if (it != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4ad3ead5-cd3c-4be3-956f-c6c2ce307a23");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5ce05279-5b30-49e7-a3ba-8293e5ba20bc");
                append(it.next());
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "daf5bc4f-ef75-429c-8851-5dd236c8ff0b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0f17d596-b6a4-4b6d-8ecd-932cc07464e6");
        if (array != null && array.length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "31d79d1d-3f56-49f2-b005-59e131f53498");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5777528c-0432-44df-bd03-73eddd1d718d");
            append(array[0]);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e52f2ca9-2fa6-4d3b-85ce-6dae7b938e80");
            for (int i = 1; i < array.length; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ef1cedb7-80ed-43b8-9515-743bbdb06c4c");
                append(sep);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5df83712-10a8-45a2-9e83-5fa6cf6daeb7");
                append(array[i]);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ab15635c-4794-486b-afde-234ab258b37b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "36a531e6-c8ae-4624-acc6-2fe76a20751f");
        if (iterable != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9ac5e8b5-660f-491d-8830-04534a5a1591");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6009114a-81ae-4cda-8743-3049682a5f9e");
            final Iterator<?> it = iterable.iterator();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3b040b2b-87c9-4e06-9a3c-d07250315a07");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "25e2c850-5079-4232-9a68-62fb7c065416");
                append(it.next());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8128da1f-94cc-4a15-a984-f05d7e0ad7b4");
                if (it.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1f14d88a-7e9d-4b97-88db-d9fefb62bbbf");
                    append(sep);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a7ba7eb1-ac42-407a-be93-40a7d914c85b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e0aed0a0-db50-4b0d-a295-d26c6b547d79");
        if (it != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9dcf3cb6-b59f-42c8-b22a-056729137a23");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "07271bbf-3b1b-4422-a266-a48ac85f6334");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "205cd062-fe3e-4369-bc03-7018f898dd81");
                append(it.next());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "82d84572-8e6b-4af2-ab36-c37aeb634c2f");
                if (it.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c4767bb8-9e95-491d-846b-1d9011df8f88");
                    append(sep);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5447f4a9-a161-464c-a646-217b57654b8f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fc102fdd-2b57-4485-b8ea-cd0dfcff214f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "efaa5b7c-0ff9-463f-b18b-f9b2d6a99ebf");
        final String str = isEmpty() ? defaultIfEmpty : standard;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "18c49c88-b837-4dc4-b913-80ac6fd69149");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "43c5d0d9-8971-4ddc-b1e4-7fade46eace6");
            append(str);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7e3d9db4-5890-4146-844a-88c407a30fe8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "62614814-f7e1-40cc-ad34-f750e46ca73b");
        if (size() > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "500421a0-4f6a-4229-a436-0ceb23f525ce");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ad96e6f7-cd63-447d-b0b8-274bdcf3cdf6");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fda63b21-9923-460b-ab4e-90661228611a");
        if (size() > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8134f0a2-31a9-4ac6-b465-245f2d8f7e46");
            append(standard);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5577d1bf-87d1-4388-9696-e7e3f43ba094");
            append(defaultIfEmpty);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dc5ec5b9-3f56-4bc5-b7f2-3ecb11214390");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a00004cb-5411-49e0-8f23-f3e2cc9704a2");
        if (separator != null && loopIndex > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c3fdf27f-553a-461e-85a7-448165c75d62");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a0b3d227-0675-4cd4-8c89-f0f3356e3a58");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "03d4203d-efd5-4d45-80c1-5eb33416ddd7");
        if (loopIndex > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ea351198-35f5-49f7-a210-8d40e0456e83");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "67d4fe94-af9e-47cc-940c-6011bf285218");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "30475736-3b07-4f8f-af73-ef249c9e684f");
        if (length >= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1561f786-f590-4c21-acf6-3df3aee529f9");
            ensureCapacity(size + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3c79ccc0-a0ff-4814-a3e6-0699bee03497");
            for (int i = 0; i < length; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1d1b74c1-8718-4de1-8ce4-2e5d8f4c81b8");
                buffer[size++] = padChar;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ad71299f-caf9-44c5-aec2-02a846b22053");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "05783377-983b-4b3e-83a1-7b5b8908d1ad");
        if (width > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4b0dc25f-3765-422f-a3bd-6a5710ad8ec8");
            ensureCapacity(size + width);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e85e175e-9782-4073-98db-cfb239b273ba");
            String str = (obj == null ? getNullText() : obj.toString());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fbedae1b-01fd-4a4d-97a8-638369411111");
            if (str == null) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "061fd6c8-b2fa-4602-a5ee-d258d8981cee");
                str = "";
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "46e62b46-d242-4f53-981c-572ef30645f8");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a0e9b63d-1208-4077-a5a2-928afbf05596");
            if (strLen >= width) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "293edc19-85c8-41b8-9fb7-68ab9878483c");
                str.getChars(strLen - width, strLen, buffer, size);
            } else {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "527cf134-dc8c-47e6-88ac-206eb0346a39");
                final int padLen = width - strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "61fb8209-0edd-4880-9573-be048d784ab9");
                for (int i = 0; i < padLen; i++) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d9c5e0b2-2afd-462d-96e4-644f97b6f7dd");
                    buffer[size + i] = padChar;
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4206adcc-a9db-45e2-b057-b3f362cd06d3");
                str.getChars(0, strLen, buffer, size + padLen);
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7d8a97a1-1622-469c-9389-3e73eadfc59d");
            size += width;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2121ee3f-0e94-48b8-8a21-a9296d14b4fd");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "12f61172-cdad-4d34-8128-1d1e618c1b7a");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c2d339be-0875-4dae-a6b3-e7674c0271a7");
        if (width > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "91795d1e-1487-4135-a376-bb6d800d2df4");
            ensureCapacity(size + width);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0047f6db-d438-4396-bc74-76307c2accaf");
            String str = (obj == null ? getNullText() : obj.toString());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "49034ec0-a126-4658-8b33-3b0d54185fd5");
            if (str == null) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6d855208-c15c-4d2c-a017-1d6cbcfe402b");
                str = "";
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7e2b0e79-2017-4a11-95a3-8fc34b72e31b");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6ab0352d-ed2d-464f-a41c-ca1325167ab4");
            if (strLen >= width) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b81e04fa-1439-47dc-8f51-ed9cee47b608");
                str.getChars(0, width, buffer, size);
            } else {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a2a53dd1-e8d4-489e-8ccb-916db1847195");
                final int padLen = width - strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3f97101c-710d-4d2f-80e5-fb20d0a58c49");
                str.getChars(0, strLen, buffer, size);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5be2b339-d7e6-4b49-9145-3f603cbab7ea");
                for (int i = 0; i < padLen; i++) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0df2cca5-7347-4a91-aa78-6051a5546d42");
                    buffer[size + strLen + i] = padChar;
                }
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bbfbb9a4-60b6-48a3-bd8f-093352534107");
            size += width;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8726c1f0-939f-459a-b739-a4bf433cd766");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "47cb9b82-1a3d-4f11-99e5-6bedf6bf053f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d1320ca1-0c1a-4863-95ea-498b91d2a667");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "77da5317-2c62-4acc-99cb-24e363a0bb7c");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "74d7e25e-f901-4d68-a988-2e1c4e36db11");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "668727d8-58f1-480e-a507-c692e25f785f");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "58cecd23-d55d-4158-9cff-df7fff4d88a5");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "26e9e7a2-c1b7-4c66-9979-5aa6a91833fa");
            str = nullText;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "db34b474-7dc0-4440-8ae1-6c2f2e7cba4b");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "84b18dd2-b54c-4d61-bbb7-340418ec2c34");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5e419512-2756-4e1a-a583-eb3c1c6e86f5");
            if (strLen > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d5965db0-eaab-4b90-ac20-2a6d8f1c96e3");
                final int newSize = size + strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6406c4ee-d9e6-496d-96fa-bc52a6083ebd");
                ensureCapacity(newSize);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8976d0ae-5e27-4194-86a5-f613d444046f");
                System.arraycopy(buffer, index, buffer, index + strLen, size - index);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7927d2e3-1184-46fb-b2ad-4c930d47a205");
                size = newSize;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d4643687-d330-4bb0-bc73-a767834f0eab");
                str.getChars(0, strLen, buffer, index);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4e7a4762-d32f-4823-a28b-1d530f0f63ca");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3a45937b-c685-4473-a981-e6c3f4f00c57");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d561e98f-4328-461b-8759-5626b5a9f55d");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7af7647e-cb94-4ae9-8125-b0f28fa94262");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a90a4f16-ec67-4455-ab09-d50d90f85c80");
        final int len = chars.length;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "28c7e311-6be8-4bc4-b978-a039bf85906c");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c99a4757-bbc4-4b86-a1e6-9432887a1408");
            ensureCapacity(size + len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0bea55f5-8381-448b-8a6e-3bf29c1c09c3");
            System.arraycopy(buffer, index, buffer, index + len, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "515d04b8-0e33-4aa9-a215-3723a4e11498");
            System.arraycopy(chars, 0, buffer, index, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1a360bf1-96e9-431c-a4d1-b860339c50e4");
            size += len;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fb32b4d5-a804-4c52-9f49-4c1bfabe1ac7");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d8afa735-153d-4015-863d-b843771d39b0");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e517a034-4f08-4943-9d56-4c5e443c41bd");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b0aa638b-9123-46fc-9a5c-a372e0d5c462");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9d88c957-97cc-46f4-a95e-e1567cd8f387");
        if (offset < 0 || offset > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0d9abe86-40cd-4ab3-9c6e-b87ee5174942");
            throw new StringIndexOutOfBoundsException("Invalid offset: " + offset);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c51d9ace-8402-4820-afcd-994cd14a2ae1");
        if (length < 0 || offset + length > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fd75faee-0cd9-4e1d-812c-2b7dee6f51ce");
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "975e41a4-b622-4169-86fc-187a66fef16a");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "11cbcbf1-4e4c-40b1-a75a-d14277277953");
            ensureCapacity(size + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "eb78463e-a1f1-44c0-b133-4dc0cbae5507");
            System.arraycopy(buffer, index, buffer, index + length, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a52d2bab-dac3-4325-8c46-fe28e7c0f89f");
            System.arraycopy(chars, offset, buffer, index, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e86f5a52-1db3-4e9e-ac23-b91fbb372297");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d040e725-d06c-46c0-81fd-856b7c3b5155");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d11b663f-a95d-4d1c-959c-a54b20771f31");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7782cad9-8bec-49e8-8ee6-759c879f2da4");
        if (value) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "49d82801-fa98-45ca-9448-4cd6ae1d71c0");
            ensureCapacity(size + 4);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a2babc7f-c58c-4ab9-9fae-c339090171c1");
            System.arraycopy(buffer, index, buffer, index + 4, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "119248ab-e6c1-4378-88dd-104528f10915");
            buffer[index++] = 't';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9586a272-3114-4335-a4c7-2c8542df0612");
            buffer[index++] = 'r';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bc9d2363-38a8-40ce-b7bb-fc0c18df25f2");
            buffer[index++] = 'u';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ac238b3e-d970-4bc7-bc1a-c09393ae2157");
            buffer[index] = 'e';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "df3201d1-0293-47d6-b19a-ca2714ecc479");
            size += 4;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f29fd6d7-26fa-4901-a9a1-8da380d1a4af");
            ensureCapacity(size + 5);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dd8bc0af-3907-49ee-ba54-2c5c700fc773");
            System.arraycopy(buffer, index, buffer, index + 5, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a37aa21a-8831-4197-b038-3112321c222e");
            buffer[index++] = 'f';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "27da7d1c-cff9-4d9f-92a9-0f03d8a09dfb");
            buffer[index++] = 'a';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9abcef04-fcb1-46d6-9764-420573dec695");
            buffer[index++] = 'l';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c693ff3d-2c9f-4f6d-881f-be81c1229697");
            buffer[index++] = 's';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f0b051ae-bd01-4354-81c5-e754c3634a78");
            buffer[index] = 'e';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1946f45e-18ee-4934-9494-30409759dfa0");
            size += 5;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ef9dc9d7-642a-4e6a-9dc3-b72214bbc291");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5ca6b794-e420-470d-ba25-74cd54a983bd");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c10eddc9-43e5-4a3b-ae0e-43abc78b001a");
        ensureCapacity(size + 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e1f1a185-01e7-4bf1-a14a-65b527abd3f7");
        System.arraycopy(buffer, index, buffer, index + 1, size - index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "226cd8c7-4a0c-4ef2-b697-92ce3347b2dc");
        buffer[index] = value;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a1ed0226-b02b-41c1-9294-5571038fb732");
        size++;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "76cef4b1-2f9e-4bde-9c67-f223c408629b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b89a09fd-b53e-4a13-8915-6f5e45b3db8d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a48eae77-ea74-4e07-8dbe-d882e575477a");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f4b34577-8694-4572-9c1f-b102062fb184");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "acb14dce-1191-4b50-b482-f756a847f422");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6f7b859f-2d77-4d2a-af6d-8836c5b467be");
        System.arraycopy(buffer, endIndex, buffer, startIndex, size - endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "729ea23c-f595-4311-9023-1d99f803302e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7aff450b-987d-4f2d-a63d-555054116dd4");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "534d248f-d0aa-4b93-adce-8abf8c0c6d06");
        final int len = endIndex - startIndex;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7f2d8038-5d8c-48a9-a9e3-861eb3fca5fc");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7345527a-e797-4d66-af21-c5887ff8fe4a");
            deleteImpl(startIndex, endIndex, len);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9b2fe6f1-912c-485d-866f-ce41a7897cdb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c90abf73-0afc-4d9d-949d-804cd2e1531d");
        for (int i = 0; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "179823c8-83f6-4d62-9348-6d9d05e84e09");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "14525759-db47-49b1-837b-aa57f75b082a");
                final int start = i;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "edc7b9d5-412b-490b-8eee-88a884dae0c0");
                while (++i < size) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b97654f5-9b69-4029-9ff6-d3389d4e9673");
                    if (buffer[i] != ch) {
                        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0dabccdd-6a68-467a-882c-d37c1fbc32f3");
                        break;
                    }
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2d41f811-7473-4956-83eb-012b93873333");
                final int len = i - start;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "986877b0-f08c-4861-bfb7-28e356c5b78f");
                deleteImpl(start, i, len);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "27725e00-db32-4893-9468-fb13dca220e8");
                i -= len;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9c4ea526-b4b2-4aea-89af-83386cc44634");
        return this;
    }

    /**
     * Deletes the character wherever it occurs in the builder.
     *
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "25d8564d-697a-4acc-84e6-b78c5e83784b");
        for (int i = 0; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3336d508-520e-4337-a3f5-dcdaf162b9a6");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a2812e9a-f4ea-4e26-9e8e-1ed8f994571e");
                deleteImpl(i, i + 1, 1);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2d6487e2-bf4a-45c4-9a87-ab703cb4e591");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4b1e2ba6-2118-499a-ab2b-011fbfae4fba");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dbcaf559-bcaf-40df-bffb-c352962ff93a");
        final int len = (str == null ? 0 : str.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fcb8d7a6-f6eb-40dd-b283-1c472de6f9e1");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d97c802f-7fc6-4005-8f0d-d3cac0b53a8f");
            int index = indexOf(str, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "18d9b13c-b4ed-47f0-873a-2724742d1bda");
            while (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6b15be64-e5c8-41ef-8020-4d02de6a30b3");
                deleteImpl(index, index + len, len);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d53b3564-cc7a-4784-908c-d88ca403dca5");
                index = indexOf(str, index);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ae83b591-2e4e-495d-b8f2-f45d608c8c5a");
        return this;
    }

    /**
     * Deletes the string wherever it occurs in the builder.
     *
     * @param str  the string to delete, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d91df18d-4999-47e2-93d8-622140f626d1");
        final int len = (str == null ? 0 : str.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3256bcea-f536-4a60-9515-9bfa0c5b6b95");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c84bb8bb-d4e8-4c47-8383-2073461a05fd");
            final int index = indexOf(str, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "183413d5-f371-47c8-8608-3f90cd7782c2");
            if (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "74640fc9-2be6-4fdf-9ac6-577ca7b3d1a9");
                deleteImpl(index, index + len, len);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "69292a8e-ea71-43c6-b477-39d8f3973982");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a5ae562d-25ef-493a-b6b8-ef827d736a76");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fc750294-a9ff-4053-a2e7-ff6e34280ddb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8c01456e-29de-4fad-bcd6-ba17d4f988b4");
        final int newSize = size - removeLen + insertLen;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fa4f5034-d9b4-4803-a8eb-ce19e3c5a9da");
        if (insertLen != removeLen) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c3bb6985-cfdb-45f3-98c7-06f7a890bf84");
            ensureCapacity(newSize);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "abc920a1-ce5d-4cd5-b56a-51100666873f");
            System.arraycopy(buffer, endIndex, buffer, startIndex + insertLen, size - endIndex);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "52e2bc18-853e-4227-b761-313044e7b457");
            size = newSize;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fb4cb73c-d76e-43ed-86a4-acb188e68da5");
        if (insertLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6af90aec-7784-42d2-8adc-ef829240d62d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2b7e4e96-5dab-429c-b75d-456df5bf364c");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2bab6142-5d2f-4878-8609-c341d92b12c3");
        final int insertLen = (replaceStr == null ? 0 : replaceStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f5310ede-c401-4092-a05a-7fe8437db6f8");
        replaceImpl(startIndex, endIndex, endIndex - startIndex, replaceStr, insertLen);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8ccb3502-20fb-4c4b-974f-934641e79c28");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "038878fd-ba54-40aa-9524-5a2e1e1c1240");
        if (search != replace) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c19ea454-e72f-45ea-8fc1-b2d04c0143a1");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b4b6a09d-b747-45b8-918a-eb483b9f970c");
                if (buffer[i] == search) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "43cc8e69-662d-44d3-8a01-85a070b18918");
                    buffer[i] = replace;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "078569c2-d3c4-43c6-904c-77de8cece322");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1397d64f-3442-48f7-9e50-6ee67430345d");
        if (search != replace) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "334cbd13-ac19-4dcb-8d89-0b4e039552c4");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "81ccb9e4-4871-4ce4-b919-b209b18e4ca6");
                if (buffer[i] == search) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fc786a57-52cd-4b03-b057-e0fd35bdf7f8");
                    buffer[i] = replace;
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "68002469-228a-4552-b94f-af0130031775");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "501891af-f6f9-47e7-8d39-b50becf9a4c3");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "11e7d735-55d0-49fd-95c7-040df6c155bd");
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2982b879-1e33-4cf8-9af1-5701895593cb");
        if (searchLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d154877c-8229-4ba7-9c8b-d095c5a7d0d2");
            final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ffa35c05-5e32-4d14-ac79-519a1fa70f21");
            int index = indexOf(searchStr, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "08590e73-0e41-443d-aba9-341fd294b03a");
            while (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e2b1f6b4-62c1-4876-8289-94f912f06d48");
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8141c171-2c67-4585-9a14-4f7b724bb6d8");
                index = indexOf(searchStr, index + replaceLen);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0fe3810c-e090-43c5-a92f-ad84605a53c0");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d9b32e4c-e161-461c-9a72-c2d3cc6ac501");
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d3d157aa-4554-48d1-b9a2-61347ea6b946");
        if (searchLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "84fd56e1-cdfd-4a4d-a9f5-46d22aa134cc");
            final int index = indexOf(searchStr, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "328565f8-d6d8-4248-9353-8ac21ee595c4");
            if (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "366bb5ea-ae11-45f4-aa08-6a4a58ff0302");
                final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5f8918b4-be8d-4560-a9fb-d903c3df812b");
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "71b28c40-0464-42aa-af66-73a3afbf95f4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "878337b7-ed18-4cb9-b650-2bb308345be0");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "37025588-d3b3-4f16-9c85-2df00cb4a33f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "efb7c3ae-e850-4e47-b52f-b34328102760");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cf237838-452d-469a-bcc8-4d24bb950db1");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2c476e01-d85a-45f1-a2d5-225f9bda582b");
        if (matcher == null || size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "01cd2ef3-e117-48e9-b41e-f959ce916e3d");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "31442665-a552-4418-9e3d-31568f92b8fc");
        final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7f80618e-30b2-43b0-8fb0-637c7d7e057b");
        for (int i = from; i < to && replaceCount != 0; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2f3167c8-248e-4f56-b5c8-0ec0dde1e191");
            final char[] buf = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "121487bf-971e-4fc7-a5cb-1ac68c46c6f0");
            final int removeLen = matcher.isMatch(buf, i, from, to);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "72f19036-5369-4802-998f-8bddec976613");
            if (removeLen > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4b6dbf0f-0e69-42c9-935e-b57690aa68b2");
                replaceImpl(i, i + removeLen, removeLen, replaceStr, replaceLen);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ce2905ca-0d3d-4e3d-b789-5ac97e858e44");
                to = to - removeLen + replaceLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cadde1c7-4b2d-4fce-9dea-92327f4015ff");
                i = i + replaceLen - 1;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0a8309ac-0ec5-452e-b081-8dc06936c461");
                if (replaceCount > 0) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e8368980-df49-4816-853e-9b269e461ec1");
                    replaceCount--;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d0a5c397-33c6-4d2b-9396-424aadb2346a");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Reverses the string builder placing each character in the opposite index.
     *
     * @return this, to enable chaining
     */
    public StrBuilder reverse() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "865351d3-5166-4ca1-8e6d-b7a43923e6c4");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8f79d17b-e090-47f8-bfc3-9b336f87db7f");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c35b47cf-b5c9-4572-991f-ba19501e4078");
        final int half = size / 2;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8b1e8523-41a1-4878-a3ef-a5d6126c4d49");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e66366cf-ae50-4d24-a071-c969dc14c2ff");
        for (int leftIdx = 0, rightIdx = size - 1; leftIdx < half; leftIdx++, rightIdx--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "edcaaab7-fb80-4d86-a900-425a973381e4");
            final char swap = buf[leftIdx];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b0e5fec7-779f-44d5-9e85-c0ab8e09bb13");
            buf[leftIdx] = buf[rightIdx];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "05ce2418-0395-4e8d-aa99-4f40008781e2");
            buf[rightIdx] = swap;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "839d9b2c-c989-48e4-89d5-06aeddb3c300");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d29e2281-10b3-4ff6-a858-0806a487c9b5");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "960e20bc-a56a-40dc-b91a-7e43695684b8");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9748f001-af97-4d89-a1b5-fd04f7b72078");
        int len = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "63ad86fc-c72a-482b-9a33-46dfc141b824");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "89813ab3-8ebe-4d7a-8f14-a8e83f93b3b0");
        int pos = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "025f403a-a1b4-4177-b75f-d7acdcc10622");
        while (pos < len && buf[pos] <= ' ') {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8ef5d6c3-3d89-4c51-8ed5-4d8b7e5064f6");
            pos++;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "65e4d9e7-6e94-47f7-b1ce-ed5f4d2381f7");
        while (pos < len && buf[len - 1] <= ' ') {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "be04524b-bc75-47eb-8ed9-3de03d97c6dc");
            len--;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "60cd65a9-2da4-4e87-aecc-d4c78d5ba665");
        if (len < size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d14df653-5792-4a59-b19b-e6ca8ca7f0c1");
            delete(len, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0ec83888-34d0-4f78-9480-e475cbf5c387");
        if (pos > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8fac7daa-92ec-481b-9a5e-16794ede4485");
            delete(0, pos);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "273e23c7-5ff1-4a81-b0e6-edc5d60fde7e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cb86d3e9-8d21-4523-b923-403422e8e56a");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "efdd4707-a5a8-4e45-aa9d-e0315967ddc5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "497c01a9-024e-49a4-acd4-7839ba127524");
        final int len = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "76417372-5af4-40c9-b19e-474736ece667");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "18e597af-74f2-48d8-a2fd-07886f45b993");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6d6a6649-9ba5-44f2-b1b6-cdb858d27312");
        if (len > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a60dedf2-d613-4879-97f8-113dd169a07c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "55ecb493-6cbe-4019-abd6-adcda2d7a9d4");
        for (int i = 0; i < len; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e86e0c53-d03c-4251-8c86-08221a33fc77");
            if (buffer[i] != str.charAt(i)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6221f64c-38e5-485e-b2c4-4eafcd865a0b");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e2c62769-a1f6-446a-86e2-8d1cf956c7d0");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "297f10f4-cd3c-49b1-9bb6-c049c6c54358");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4aa5d6a2-17a7-4bf2-a678-7ccfb28f2e2e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "340f2cac-c95f-4fd9-9c1e-13b64cd2703d");
        final int len = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f1ba5dde-2523-4b3a-b2aa-67c95168e0fc");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8d3ecd2b-412e-4ba4-9b3e-eabb3c036587");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2cd63c42-0868-411f-b190-57bf57022eb0");
        if (len > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a9812790-c819-435b-9f27-212c85e17aef");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aedbe9c7-aba3-4335-aa92-d5388f502424");
        int pos = size - len;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b27cf75e-0f5f-4e27-aa81-d52b80e3bb43");
        for (int i = 0; i < len; i++, pos++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bbef6717-4d96-44bd-9618-e535a17b3181");
            if (buffer[pos] != str.charAt(i)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d95687ab-7054-41f6-8723-fcbe152f3c75");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b3b070f9-67a4-4ba8-bd02-93f763f9bad7");
        return true;
    }

    // -----------------------------------------------------------------------
    /**
     * {@inheritDoc}
     */
    @Override
    public CharSequence subSequence(final int startIndex, final int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "be14a350-4797-4cd5-9745-15e469853c81");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0d02112f-b067-493c-9357-062e0031f0f9");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "643761de-7520-49aa-b863-7dc0e1fd0aed");
        if (endIndex > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b87deefb-c4ca-4ee4-ad37-95c74b8d30c9");
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "484b1761-6c74-4d43-88f6-d83f4544c949");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b3be1cd4-c7b2-4d66-9c1c-e554c31e9b2f");
            throw new StringIndexOutOfBoundsException(endIndex - startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fb2025de-7f5b-44be-82f8-fd3836683caa");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "83244102-d4a3-411c-bf8d-015199b314ee");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4596e7cb-1833-40b8-89b1-098f816386c5");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "beea9bf1-ac93-4b43-8072-86a0aaca6be6");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "17a26b5d-97fe-458f-bda3-6bf377248b46");
        if (length <= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4971e6f0-948a-4afc-ba8e-3ca7faf95006");
            return "";
        } else if (length >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2835e93d-a393-473d-9f70-b312a13d2a9b");
            return new String(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "37b6272d-a615-4ceb-88d3-26742af40779");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c3adee32-6139-47b4-bb91-32c39b70762e");
        if (length <= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cf388c5d-fe83-4bb5-96f5-92b8d08a9afb");
            return "";
        } else if (length >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2b5f6ac6-0336-4dc6-b563-12f5b8470b45");
            return new String(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bb36a22a-bd75-4cad-b641-c3ab15482b28");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dd4e7047-0741-4894-828c-1883cc7515a7");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a5ba4ffa-824a-4007-a305-10c7a7f8d595");
            index = 0;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ac7b9ca7-a7e3-4510-b5a8-a1891caff493");
        if (length <= 0 || index >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "249ef305-caa8-4bb7-9219-83801e471ba3");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "271fe33c-0ec3-4c0a-9b82-6dc8a3184ce3");
        if (size <= index + length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "75d33f6e-2d41-41af-94b8-5e9f54d1aaa7");
            return new String(buffer, index, size - index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "969bcae0-3a8a-429c-b7e7-7230b253b5a9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "42a71c0e-5a37-4480-a0fe-1c8bfae47720");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aa06e83e-2a3d-4fd8-a6cf-bd12f250aee3");
        for (int i = 0; i < this.size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "de138972-bfd7-4a4b-93db-c7c70509f10e");
            if (thisBuf[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c684574b-4d62-4138-8151-d2bcdca2d629");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a1de8f71-318e-496a-ac7b-e94e1ba7145e");
        return false;
    }

    /**
     * Checks if the string builder contains the specified string.
     *
     * @param str  the string to find
     * @return true if the builder contains the string
     */
    public boolean contains(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "343100a7-8c4a-40fd-97be-cc8854905e46");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e4d55dd8-967e-48e3-9539-4485910fa6ae");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "61dbec78-be62-4735-96a2-1803692f854c");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b0c07e5e-ba36-4355-a007-6fdc6caaa0ff");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "77b975e3-e631-4aed-afe5-e80d4e068db1");
        if (startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "87a9b899-8d96-4334-be24-b6588b085293");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "70956f6d-6a3a-4353-9609-5f110585913e");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e5a0310e-2399-4c8e-95ce-bdabdef67b26");
        for (int i = startIndex; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1e6f9fc6-bac1-4827-a3ca-aac5b5c8a704");
            if (thisBuf[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "591c41f8-d719-46ab-b7de-ec7d34b49300");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c4bd2162-b11f-416a-ad6d-b480a38213a4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "09f36016-d3ab-40c9-a2c1-667060a1ad4e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "33f6e37b-19f0-42d2-b395-826ddd7f548a");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "81117e1f-9131-49ad-9dd5-ac29b6e9dfc7");
        if (str == null || startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b163d39c-7c03-44d5-b120-f32413cca020");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "685ce6b3-1d21-4c17-9ae9-1719292ddfac");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "280eee46-5176-4935-86c0-27ae495ed0d5");
        if (strLen == 1) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0a62cc4f-d98a-415f-8ea4-d28a551f0248");
            return indexOf(str.charAt(0), startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0dc43587-39e3-4661-8c95-b38930a11d3d");
        if (strLen == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8c49ff41-928f-4cee-ab23-9385fce57f1b");
            return startIndex;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "74689ab2-cc3e-4c40-b5d6-bad594c6c618");
        if (strLen > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "442fcb5a-b462-4f98-80b2-9d8c4d7d76c9");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "713000c9-df50-4d12-937b-3a5bb11c3051");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8c0b278c-697e-4a73-b320-545424c4ee69");
        final int len = size - strLen + 1;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5b62f932-dae4-48db-ab4e-3b40707bd77e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "260ee285-c205-40ff-aa72-6966b0d8e715");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3a544d80-1df3-407c-8fcb-fde3d4f69dc3");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4d77f842-a13a-4c7d-9c53-b67583fa456e");
        if (matcher == null || startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4cd5bfbb-1bce-4623-92fa-809492160c49");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "229dc609-0f91-477b-b65e-8f10f950bcff");
        final int len = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dbf500d4-3129-4b69-ab01-5eeef69244c0");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d778281a-7847-4632-b570-9cd591654a7d");
        for (int i = startIndex; i < len; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "99a60f44-7a5b-40b1-a689-f13243e5e225");
            if (matcher.isMatch(buf, i, startIndex, len) > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2e0ec1f2-dbfe-4191-b440-a89d1c6c4ed7");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8ed9e24f-d174-4c94-af7b-324da5b94b92");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b126c1a9-cb34-4da6-853d-3827ef60e17d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c1668108-52e6-4313-9240-58bde3ca5901");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cb92a05b-f323-466a-a244-521a5008c17d");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d14490d7-d1b1-42ee-bb7a-86f77594e473");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aa1d95c5-1dc9-4bae-aa8f-0baca7abb490");
        for (int i = startIndex; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0099794c-741e-432a-9a8b-058e5d504a02");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d73d3f3e-ec0b-4992-9a2c-0fc1c514b764");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1f00416b-a96a-47f9-8a99-ec41de92d4d2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a852b0af-5643-4aae-8bb9-b5b6762852be");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "244a5946-2e17-43b0-9b28-0374978462c3");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "0d1c2cd0-d76b-4463-84f4-52b98df441d7");
        if (str == null || startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "15466276-cce2-44ea-b22a-2d8baa3d90c2");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "add803fe-d996-42eb-8b1e-369e597af56a");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a4f3a151-d065-4eef-b7c6-4e683b45b9cf");
        if (strLen > 0 && strLen <= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "516ed561-5191-48ed-95a8-77230e068184");
            if (strLen == 1) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "42065c2f-2adc-4dfe-a31d-669c65291931");
                return lastIndexOf(str.charAt(0), startIndex);
            }
        } else if (strLen == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5ff71825-564e-4a0c-9a5a-8b6fb959fc3d");
            return startIndex;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "950625bf-6c5e-4d68-bf30-48fbac3ffd5e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "e0515d1c-97cf-44c1-b76b-dcb66d95caac");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d6d3b4e4-9db3-4b7e-a402-499d8bd23a01");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "6972e92e-71ba-4288-a19e-dcefcd798b6d");
        if (matcher == null || startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f8a162f8-5b17-43fd-9293-0ef786aeb9b6");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "a0d52ac8-9325-4552-abd1-27d9cd0ba0e6");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "8062da25-bb31-416e-87fc-8ba7d90a2921");
        final int endIndex = startIndex + 1;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f0cd6703-51e7-49ba-892c-028d85710fd9");
        for (int i = startIndex; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bcf1f19f-1a4e-49d2-879d-fa769e11bdfe");
            if (matcher.isMatch(buf, i, 0, endIndex) > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d0850b75-f134-4bfa-8671-e4d197100812");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fcc809cf-51ae-4072-8eee-fdde7645be79");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3c39ac43-8768-4d26-a1b3-139b0483f1b8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "80833771-844f-45c4-8c14-028d45ff3a1f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "16ddcbbc-c7a5-4f14-8350-6d9bcc593f72");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "68d08b6e-167c-4e0f-9531-f4f6d67453a7");
        if (appendable instanceof Writer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4e6ab179-af54-48ca-9ca6-76837fa819c3");
            ((Writer) appendable).write(buffer, 0, size);
        } else if (appendable instanceof StringBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "32524393-1c71-4dc2-a4ff-3e5837cd5efd");
            ((StringBuilder) appendable).append(buffer, 0, size);
        } else if (appendable instanceof StringBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bf6e6309-a98a-45af-9283-8b12877d5c90");
            ((StringBuffer) appendable).append(buffer, 0, size);
        } else if (appendable instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c34335bc-8d95-47c4-ba58-5a8366351cf7");
            ((CharBuffer) appendable).put(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "94488960-48a4-4dcb-8136-4d3205a72002");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f5f184c9-51fa-4a2f-a7eb-79baa3ff757d");
        if (this == other) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "43372e54-e40c-41e9-958d-9297bf864d0c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1104d897-1302-44e1-9e37-e7d6760ca5ae");
        if (this.size != other.size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "197422de-2db6-4074-9295-48e456e83ec8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "fd71a66f-e391-4b7d-92fe-dba8f88e9a69");
        final char[] thisBuf = this.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c5cd1825-3682-423b-8f8d-46b818a582b5");
        final char[] otherBuf = other.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "f6544bc3-50d2-4735-bacb-e2f075de10cb");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4b543c4e-dede-413f-b29b-ae6d0d2e4bd0");
            final char c1 = thisBuf[i];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7b714052-5cbd-4919-bd23-fa37562d82ef");
            final char c2 = otherBuf[i];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7984f79e-2e5f-4632-97aa-4fa14f394689");
            if (c1 != c2 && Character.toUpperCase(c1) != Character.toUpperCase(c2)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "23897c86-5eb8-4edc-88ac-bb391082074f");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2779efed-9d8b-412a-acfd-fae9a9308e62");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "71285f7f-e74a-4d34-aa58-8d01cab8fa8d");
        if (this == other) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7a5a7d12-d46b-426a-889f-ac7a7412ee66");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dfadbebf-a76b-4714-aaa6-a13ab66158ae");
        if (other == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "73263394-2a41-499a-9ea5-29c4d291b6cf");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3128fb86-a01d-4f30-b988-904067c71160");
        if (this.size != other.size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "bcb3ab51-3f1e-4337-9b06-648e051021d5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "aaf8c046-2c8c-45a5-b2b0-98f91de6e192");
        final char[] thisBuf = this.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1439e6d8-b53f-4898-8933-85a33d0896d2");
        final char[] otherBuf = other.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "4cf380f8-8d52-44d0-b087-9469898e486f");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "cae0eade-42cb-40fd-82d3-2014808dd955");
            if (thisBuf[i] != otherBuf[i]) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3a39d1ea-6e2e-4046-b4bb-1c8ef9b9ba21");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "20e427a0-5a8a-4a7f-8bc6-9cf5d28d599d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "3176dc60-aa68-4b83-9d73-9133e682b706");
        return obj instanceof StrBuilder && equals((StrBuilder) obj);
    }

    /**
     * Gets a suitable hash code for this builder.
     *
     * @return a hash code
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1d8c1a4f-1439-4163-bb5f-78f087d1eeac");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c69862c6-aad8-411f-bbf5-ef2e92f98c66");
        int hash = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9753c5f3-f93f-4fd7-90ad-3c4c9d26a18c");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d4390200-d81f-44e6-b289-809e8824c159");
            hash = 31 * hash + buf[i];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "9ec5b64c-ee42-4d74-bbdf-228a29442c2d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5faeb8f9-7f09-4474-b29c-cca8d2549de0");
        return new String(buffer, 0, size);
    }

    /**
     * Gets a StringBuffer version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuffer
     */
    public StringBuffer toStringBuffer() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "dca10b04-3a5b-4d4d-9299-fac56addc0d0");
        return new StringBuffer(size).append(buffer, 0, size);
    }

    /**
     * Gets a StringBuilder version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuilder
     */
    public StringBuilder toStringBuilder() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "b38319c3-6b22-4927-ab63-cbaa3c5323c2");
        return new StringBuilder(size).append(buffer, 0, size);
    }

    /**
     * Implement the {@link Builder} interface.
     * @return the builder as a String
     * @see #toString()
     */
    @Override
    public String build() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "c6ec8346-aa3a-4f7c-a1de-d629eaa880e4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "d2b90eb0-0e63-4399-9c03-73f58553dd0c");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "2dd22e30-524e-4ac0-9e93-b79f609416e9");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "5c9d1c83-12f2-4547-b915-cc099054e889");
        if (endIndex > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "1be4407e-7a1e-4143-a3b3-74384caa1f13");
            endIndex = size;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "23890955-3776-4dca-a27d-f38e5b5f0b07");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "71c5648f-05cb-4ae4-bfd7-dc0283b6850f");
            throw new StringIndexOutOfBoundsException("end < start");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "7013f061-8f36-4ea9-9ed1-388a95984898");
        return endIndex;
    }

    /**
     * Validates parameters defining a single index in the builder.
     *
     * @param index  the index, must be valid
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    protected void validateIndex(final int index) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "17a735c0-c9ef-4fdd-a963-f8466d1047ad");
        if (index < 0 || index > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_2_10.coverage", "ca5443a0-bb00-4559-811e-fac78238cb00");
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
