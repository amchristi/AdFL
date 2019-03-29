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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e1016963-a596-4d06-8c46-e9858f00409d");
        return newLine;
    }

    /**
     * Sets the text to be appended when a new line is added.
     *
     * @param newLine  the new line text, null means use system default
     * @return this, to enable chaining
     */
    public StrBuilder setNewLineText(final String newLine) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5e5d5e01-8441-4e0f-b557-2dd649bf56af");
        this.newLine = newLine;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "db4a2022-db14-4f7c-aad2-a7bac928662f");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the text to be appended when null is added.
     *
     * @return the null text, null means no append
     */
    public String getNullText() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d69bf4c7-6c9b-4b16-b688-d190cb35e617");
        return nullText;
    }

    /**
     * Sets the text to be appended when null is added.
     *
     * @param nullText  the null text, null means no append
     * @return this, to enable chaining
     */
    public StrBuilder setNullText(String nullText) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "27fb1c34-53d4-445d-98b2-5730c50c14e5");
        if (nullText != null && nullText.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "64db106e-8180-43aa-8666-7c5be37f5205");
            nullText = null;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fae53eb6-54a6-4870-bd91-dd4b6e2ada14");
        this.nullText = nullText;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b79275c4-23fe-484f-aae1-8d3de32e7a09");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "34492d0b-925c-40b3-ae20-cc593f42246e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c3a5d224-a4e6-4fd3-9276-debff7d90e48");
        if (length < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b6394704-5fba-4eaf-8f20-7235de20fe45");
            throw new StringIndexOutOfBoundsException(length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "79472715-190a-41b6-83d1-7a63912fe10c");
        if (length < size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8a446d5d-adb0-4f8e-87ea-533d61abb531");
            size = length;
        } else if (length > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "77034c4b-54e1-44ca-96bc-fd43932d854a");
            ensureCapacity(length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0438822d-9565-4604-84ee-44f61569abd9");
            final int oldEnd = size;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "88cce4b7-9435-4350-b9ee-d8875d8f7ca5");
            final int newEnd = length;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "85a74630-96dd-4dc5-8a6a-86d8a1012e48");
            size = length;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "67293353-091a-4177-ba30-0131bc8d0310");
            for (int i = oldEnd; i < newEnd; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e7342a74-6e16-4a6c-9c6b-6db9016e3038");
                buffer[i] = '\0';
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c5dc7e53-9d78-4d8c-918c-5c1be729abc3");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Gets the current size of the internal character array buffer.
     *
     * @return the capacity
     */
    public int capacity() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f3c421ae-af9c-48a2-a5d7-8b41f0a81e38");
        return buffer.length;
    }

    /**
     * Checks the capacity and ensures that it is at least the size specified.
     *
     * @param capacity  the capacity to ensure
     * @return this, to enable chaining
     */
    public StrBuilder ensureCapacity(final int capacity) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c1fd942c-0f69-4a59-9688-8e57cf05234a");
        if (capacity > buffer.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "69d727e2-1ebe-43ad-b15d-783ea819d3f5");
            final char[] old = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c571a881-b0e8-4f74-9860-f08567bfb872");
            buffer = new char[capacity * 2];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ff65a1f8-2f5b-4ba7-a441-800c3ed98121");
            System.arraycopy(old, 0, buffer, 0, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e8252a8b-dc1d-406c-b6b1-c5135b2797b7");
        return this;
    }

    /**
     * Minimizes the capacity to the actual length of the string.
     *
     * @return this, to enable chaining
     */
    public StrBuilder minimizeCapacity() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "272121d1-7e4b-4799-9be9-1bffdc4a5aff");
        if (buffer.length > length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2dea6cc4-588b-4468-bf6c-5bfc810b9884");
            final char[] old = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9d13ac56-7b1c-47b4-bf10-fa9eac486d5b");
            buffer = new char[length()];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3c316b65-1752-4a2c-b613-eae04a15f93f");
            System.arraycopy(old, 0, buffer, 0, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0ad286c9-b072-4c7d-af41-07fdc805db1c");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "63281879-7760-4d17-acf0-c8dcd70b234c");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7fab1827-bbb3-478a-b822-e7db710b010f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cb1bde2c-bf63-42ce-9fb0-92ea5c280bd1");
        size = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2039d5f8-71f4-42fc-b9a0-0a0319ab229d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "859e3435-0f31-49f9-926a-00a63e6fdc26");
        if (index < 0 || index >= length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "086f6183-7de2-4b34-a79e-3f9286ac1fe6");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a6aece52-8814-4229-a19d-4cd6b2d83759");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1ce55c04-b2b1-4280-af2e-f8c9ad6d191c");
        if (index < 0 || index >= length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a2b91f90-4122-4cf6-9b8a-8bd490da6761");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "af514a1b-d342-4042-90d2-6df8ee8218cd");
        buffer[index] = ch;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8017a980-27a4-4709-8f83-ee34af3166c2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7d7c0756-35a2-4d7f-a47c-1e26c2b4ed26");
        if (index < 0 || index >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1d87d08e-95a9-40b0-b613-9f614a8fc7b8");
            throw new StringIndexOutOfBoundsException(index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "aa9e1d4b-8925-4027-a0e5-70439d841952");
        deleteImpl(index, index + 1, 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "170b0d75-bf85-4006-ae49-d133b20a29d6");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Copies the builder's character array into a new character array.
     *
     * @return a new array that represents the contents of the builder
     */
    public char[] toCharArray() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b843676e-b484-4abe-b101-f1563038a9bb");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ce4e4e25-2e12-43e3-9a2c-dd445e90792c");
            return new char[0];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d670acd4-5966-4186-9e1f-8b75fcb7efa9");
        final char[] chars = new char[size];
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3b664aef-34fa-42a7-a479-3ca62513591f");
        System.arraycopy(buffer, 0, chars, 0, size);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e878bd02-25bb-4c89-ab88-c02724dfb1e1");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "785529b8-2975-4e8c-b3e0-0f36571479cd");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "76979d8a-8814-4092-880f-23ee88faad51");
        final int len = endIndex - startIndex;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f9c3c561-18ff-4a42-82e2-cb5c84ac1878");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "88043586-39e1-41a3-ae06-0acd2c8b10d7");
            return new char[0];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "36baeabf-f745-4aee-ba3c-3bd52871cd48");
        final char[] chars = new char[len];
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "741226e6-bf23-4cc6-a29f-5828b36d6b29");
        System.arraycopy(buffer, startIndex, chars, 0, len);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "46de74b9-e215-4ccd-b584-2104b3d42fd7");
        return chars;
    }

    /**
     * Copies the character array into the specified array.
     *
     * @param destination  the destination array, null will cause an array to be created
     * @return the input array, unless that was null or too small
     */
    public char[] getChars(char[] destination) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1d41ba14-824d-4ddc-9f6e-5beca89435e8");
        final int len = length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e7974cf6-45e7-4f7d-901d-85f5d6c68a3f");
        if (destination == null || destination.length < len) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a295549f-1466-4c7a-ae0a-3d42a126abdf");
            destination = new char[len];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f194c394-a8f3-4cf3-b43f-002991951cc7");
        System.arraycopy(buffer, 0, destination, 0, len);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fab33291-0fff-48d1-b64a-1a1af6883ce8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4af2466e-b03e-4279-abc2-3ec638d9315f");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4761dbb2-d4a8-4e3e-ad88-1d7aa3426dca");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "99b39e16-7da1-4967-a2a6-0cf29967b013");
        if (endIndex < 0 || endIndex > length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7b79b246-7667-4abe-9b1b-a9bc1fa3f90f");
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3b4b16ac-e999-44cd-9791-0065e6a6e284");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cf926c67-ac11-4e63-9ea2-e505503928fa");
            throw new StringIndexOutOfBoundsException("end < start");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ce3ef082-c18c-4722-8dc3-3db6afc5ea66");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c6d654a6-11cb-40e7-851a-358b25cea544");
        final int oldSize = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3ac72c78-5036-4e38-952f-4a4095e0dbc5");
        if (readable instanceof Reader) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "beac3c37-63ce-4ad3-8e5c-335738227e8a");
            final Reader r = (Reader) readable;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a1102af9-25ff-4cd3-816c-91a9cd46bf2b");
            ensureCapacity(size + 1);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "605d4d9c-7dbc-4dbf-a811-67e53224065a");
            int read;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b7553171-2698-49a5-857a-42239a2ac90a");
            while ((read = r.read(buffer, size, buffer.length - size)) != -1) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cb2f2607-5bca-4a03-9427-2a5150586c54");
                size += read;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ef01bad2-8bdc-4341-8b02-d709924519f4");
                ensureCapacity(size + 1);
            }
        } else if (readable instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f6aace42-5c4f-4610-b574-bb4f960f81bd");
            final CharBuffer cb = (CharBuffer) readable;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8ef05c50-1f58-4104-8f97-791f345f8d41");
            final int remaining = cb.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f53c8708-e4d7-49b4-9207-30d70a47b601");
            ensureCapacity(size + remaining);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "851d9ebe-280b-45c7-a64c-3680b4fc9b2a");
            cb.get(buffer, size, remaining);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cca847d7-6449-4253-a826-e51e5ed80022");
            size += remaining;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f7a8b28e-adb9-4407-89a8-fe4a79312e70");
            while (true) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "caf289a3-12f8-4e51-8094-932460026a79");
                ensureCapacity(size + 1);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b87ceab4-8d15-4497-a390-81082e2cfdea");
                final CharBuffer buf = CharBuffer.wrap(buffer, size, buffer.length - size);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5793f0d0-73bc-4e6c-a410-1f31a5f54668");
                final int read = readable.read(buf);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5dc87d05-16b3-4204-85ce-1ce3ef2fb2e3");
                if (read == -1) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "db96baa8-55cc-4f78-9fa6-8d02a56db40d");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c40a9f99-70f1-4291-a120-f839d7fe9b95");
                size += read;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "93a34251-cc40-4491-8375-9266048ad1f6");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2c5c5e9a-1c22-4465-be06-1426437406b8");
        if (newLine == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "903de838-399d-447a-98c2-4a3c19934d7e");
            append(System.lineSeparator());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "25cbefce-0c9b-4e79-b411-8504acf67b3b");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0d4d197c-76b6-4be9-b197-f72ed5ba07d4");
        return append(newLine);
    }

    /**
     * Appends the text representing <code>null</code> to this string builder.
     *
     * @return this, to enable chaining
     */
    public StrBuilder appendNull() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dec77818-4b5f-41d1-857c-3ba1d647d2fc");
        if (nullText == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "43a98fa7-3427-420c-9742-fd12891f0c60");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "23c8db87-6ab3-420f-a022-742ea55ee136");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "45a7710c-7692-40da-a50f-fc1e41fbe583");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "77d98e72-50bc-40e9-9b36-4a152fadbe87");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cd600833-5987-49c6-8892-35a2dc71277a");
        if (obj instanceof CharSequence) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "96f56979-3ac7-447f-8950-1b629402706c");
            return append((CharSequence) obj);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e36ea1a7-a085-4f26-8241-f73b09e44741");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7888e428-8f45-49d9-ab7b-898ea18aaafb");
        if (seq == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4c7719d8-00b7-4ab4-b9f4-45f22ccdfdbf");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e795bef6-f504-4fc6-800d-a233dfcec246");
        if (seq instanceof StrBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "819f96b0-0405-4a30-9579-1d833536b95d");
            return append((StrBuilder) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5c3895cb-0fea-4af9-9a11-55e013a5aec7");
        if (seq instanceof StringBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f23822ca-8238-443d-8dcb-efd61a93010e");
            return append((StringBuilder) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "bb709345-5e14-458d-af6d-f8ebd5e37e75");
        if (seq instanceof StringBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b558e7a7-b90a-4e93-8ae8-614dce064da9");
            return append((StringBuffer) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6c2020fc-1b4f-46ce-937a-b7188aec35a9");
        if (seq instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "48ee1825-a428-4ff7-b6ca-e6759866ecc9");
            return append((CharBuffer) seq);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4fd3dc4e-b173-4724-98a9-46f8e26b7c79");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6e51b272-9c4b-48c3-bb98-129e05bf9ea0");
        if (seq == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "45add06d-d480-49a0-927c-1f33a2c41275");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d231bc4e-82d4-4e91-8c3f-927786213a99");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9e912381-d6c5-4317-9195-92ec4d769001");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f8289750-6c8e-4181-8eae-51501e8ed792");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "93a137d1-318b-4e84-a477-5558864e4d36");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a8fce430-9e3f-46d4-bc99-3e1719e97dc3");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4dcb4d9d-218c-4ba0-afe1-9570f19d8b6f");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a61d15d3-26ba-439f-9b65-9b4c2cb6b877");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "bc1e52ec-a2c3-4216-9ca3-3a9c748a9082");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a1c51147-986b-474d-9bec-71073b9cbcab");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4447fc35-0676-4502-9431-68d4387b4bb9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "195111f8-4717-4f4a-a5fa-e6a957f5b10e");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b3dd64cb-a61c-4295-89d3-92a4d35e5d76");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ec627245-c0ca-480b-81dc-dd2c11b31138");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0a4cfe19-a2a3-43ca-9869-2533b8712adc");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1395046b-e168-4404-ac41-44a872c87f2b");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a8013b14-30a7-42d6-8981-1ef19ff3e18b");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ab2f5ef9-1a89-40bc-8ef0-7477f34c2ded");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a4fcf7a4-01f5-4251-aa14-a038922ae1a2");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f10ba58f-0bdd-4ae3-a626-5b35fa131e3b");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "226c9665-584d-419b-985c-d37dbd95d48e");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c0af6d9c-450c-4f07-a1b8-6a2ab19c7302");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5afbe50a-8b58-4785-bfa1-eefe85b71276");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d459832f-f60d-475d-be51-733c5dbef910");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "66402d8a-c010-4312-90e4-fe7a20c7ae16");
        if (buf == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e313dbc9-4247-4faa-8d07-ba38f57baffb");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "67ccd01d-dc0b-4820-89a1-45907b8ba091");
        if (buf.hasArray()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "023a7fef-d2b6-42af-a78e-da0190ef783d");
            final int length = buf.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1bb74ad3-e6dc-4f71-9e66-736ce9ae94ac");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fe8e931d-c7d4-46c3-b45c-1a10aa630985");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "11657924-47b4-48c0-9fea-ac98a8f5b267");
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position(), buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "69ab64bd-5403-4d55-946a-1fdfed0322ee");
            size += length;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9e1cb642-ad16-43a5-a6a3-ba475995e15e");
            append(buf.toString());
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2fd86042-b0a5-4847-8135-beb09d2ad2be");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c33d0f45-1ee7-4eef-853e-88c73686b50b");
        if (buf == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "475b5231-41d9-4f01-a033-31c517196667");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "56ef8f33-791a-45bf-97ae-ca17ea25f21e");
        if (buf.hasArray()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "aea153c3-b8fd-4bae-a9ba-efab2d1f6ded");
            final int totalLength = buf.remaining();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fc2b9213-4c67-42f9-8581-f0935ef98e41");
            if (startIndex < 0 || startIndex > totalLength) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c24daada-f852-4097-b3cf-24974c79c54a");
                throw new StringIndexOutOfBoundsException("startIndex must be valid");
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "994ad94a-11ce-4222-99e5-519e6eddc8d9");
            if (length < 0 || (startIndex + length) > totalLength) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "85e5ea00-1cf2-4b9d-b036-5c1d4d336d55");
                throw new StringIndexOutOfBoundsException("length must be valid");
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "57a557f9-f8d1-4857-b506-05efe8ce1dcf");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "575c197d-e869-4a80-9934-d1712a05589b");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "504ed630-02cd-4dc4-9a27-757a6bebe18b");
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position() + startIndex, buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6bd46314-5e9d-4157-8be2-9842bf8c59e8");
            size += length;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d92f9370-ef55-4ec3-b8e1-bd73bda26787");
            append(buf.toString(), startIndex, length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e567de68-1140-435a-9679-970800ec37fc");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c346873b-a9f0-469f-8ad5-565799316386");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5e22d10f-9fe5-471f-ab2a-445acd8f565c");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0d64434d-6ea8-4348-bb29-bf9531742dcf");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a8599d45-afd6-4f08-904e-5520208bb65d");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c429d3db-9664-40d4-97f6-fd2b646ea0e0");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8ccbe1f4-32f9-43f7-a60d-0fe83705d934");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7b8f12c0-ccba-4204-b0d6-8b70dd0d826b");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0aa8111e-7395-4aaf-82a8-5bd13b3c2230");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b98d5288-491d-43fc-9a67-ef67656cafa9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f5f388ea-7ede-426c-a8ae-319bfa4fef46");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8da41b0d-0829-4231-8e3a-8a4c4777595c");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cde7293b-22da-4003-bf7f-0aa3ee8a8971");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7e7f99d5-827c-40cd-b275-c0e86b8e51a1");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7733afe3-51e8-49ae-aacf-a804c520059c");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fc2f67a6-1625-4002-aa1c-53af64cf8385");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "141b143b-d278-4135-bbab-20ae1bc89134");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "eaf9c371-4443-41d3-be00-07968e6e511c");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1fe726ff-f89b-43f8-9278-baa112e58279");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2be2b5f3-583e-44e0-b176-1c0af580ac51");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c7991dc5-e166-441e-91dd-b968c8ac873f");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1fbabd5b-f32c-4145-bee9-5550247af963");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ac7e416b-31d5-4c0d-b819-410b7896d768");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "08d3b516-ca2f-404f-bd0f-4ad52a85ad10");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "57b393c5-d8fd-4d25-8ad6-c69a924e8f63");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e2400647-4a25-43b9-8853-52f186a782f5");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "13df2003-e038-4954-ad6f-d745f286581a");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "69ac894e-33c0-460d-98c3-68c4c761b199");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7eb393bb-79e0-48b0-94d2-c9beb49fc02c");
            str.getChars(0, strLen, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d12cecbd-eb0d-47ea-8dbf-4084c930ece0");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "06f838c5-2040-4d99-8a83-9a00d62a791b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dde86435-b143-4350-bc42-8bddc1a2ee37");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1e5f801f-e3bb-41e6-b3fc-30ef34501b85");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f74d38c4-81bd-43b6-839c-f858291f2260");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f651fbb6-97d1-4ef1-a1fc-6fd120279696");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6348132c-4a77-401b-915b-f4f58787e134");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2df67483-173b-480e-b272-2273594b47f3");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "712f9add-5731-45c9-b65c-3625e9efc3ab");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e41af27b-f4cf-4ce9-b0b4-dcbcfccf1982");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "86741f41-9b48-4cb9-b13c-c62b172aaf46");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "91568652-32a1-49de-a9c8-3d24b78fe12c");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "381fa6fd-9db7-4ab9-807a-f47ed2c7795b");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "92f80d62-3663-403d-8bb2-48b303549643");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7d9a037f-0a0b-4fe2-bb65-eb0d78bca52b");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8d6bae5a-6e3f-434b-9fb5-70cd93866e6a");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3e8640f4-4e54-4d34-b2ae-9cb9a37f8082");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ed937c7b-caea-446b-8d4a-4f5e288580d6");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f5c1c824-7756-4cba-b1c3-b3bc61bfc6fd");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "98f3c1fd-e1da-41ca-8c5e-c51842160a47");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6e10a7a0-1013-4884-98b7-e5f8576c94a0");
            System.arraycopy(str.buffer, 0, buffer, len, strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3be73fc6-d857-4e90-8b7b-67d5805fc8f0");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "57bf665e-3fef-4e21-9cde-7a235444463f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a574fa92-f217-48c3-8732-458de627cf6a");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "bce6f8e2-0efd-406d-b94a-23b1f03fe07c");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b48e09aa-746e-4d9e-a73f-eac9c7ceefa3");
        if (startIndex < 0 || startIndex > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c4f38297-931d-464c-a082-e2b47a20bb96");
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "73eda85d-4bf6-4239-90a3-4f426c675d04");
        if (length < 0 || (startIndex + length) > str.length()) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "bf3f48a3-4308-4107-bb86-176d8db0e6c8");
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cb526b32-8404-4c81-a97a-956fc2ef5469");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ccf65bf1-5d4f-4646-a4f3-175596b24052");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "efd8c43a-91a1-4e96-b897-78950edd4087");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "816cf661-4e8f-4c5f-b0e0-5e90539064ab");
            str.getChars(startIndex, startIndex + length, buffer, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "40090ee6-39fa-4d6d-a407-4ec31bff126d");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "86ce2361-c449-402c-92d4-ca5144c0bc29");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "02f1ccbf-88e6-44df-b932-b052450b06d1");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f2c070ba-4ad2-4e05-a585-fafcb027f0fa");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "115bb7ae-eed6-42bf-b17f-6a4699d3cb6b");
        final int strLen = chars.length;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0f6c60e3-433c-4934-8857-4e0165e24a2f");
        if (strLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6bcd58ec-4d56-401e-b9e8-8f99a5b73267");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9590b3c0-5313-44a8-be96-570f5aedcd02");
            ensureCapacity(len + strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "27dbb160-b154-4af0-b102-fb7f29c002f8");
            System.arraycopy(chars, 0, buffer, len, strLen);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e5748f52-ba44-44d8-a7b4-8dd96799603f");
            size += strLen;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1a8b904d-558d-4708-be1b-44633fbc3877");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d9284116-8476-4560-8dd9-b81b2530d0c5");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "626b3633-4c16-4b79-a246-7fc19ecf8a46");
            return appendNull();
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "80b19a2d-e85f-4a84-9f31-3d71e2cb5dc5");
        if (startIndex < 0 || startIndex > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f14e1814-1d3a-47a8-bbea-c69fe9a26db8");
            throw new StringIndexOutOfBoundsException("Invalid startIndex: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "021bee84-2911-4726-83ae-6f47d5251ae8");
        if (length < 0 || (startIndex + length) > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f87404d2-4ddf-4f8a-b59e-6a6fe8d83aa4");
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ec4bf2d8-45c3-4c6f-bdd4-962f00fbdbee");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "29b78672-fe28-4f70-984b-1fd7f859f1b3");
            final int len = length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "869a61aa-f538-45f4-bd99-8ed767938a04");
            ensureCapacity(len + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c840ac27-dbb6-4762-a606-48b5332285a3");
            System.arraycopy(chars, startIndex, buffer, len, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6fc0d057-e31c-4d3a-8477-32de8292ab1a");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "01908b2e-2630-4804-8ecf-d8d77fcbf2a2");
        return this;
    }

    /**
     * Appends a boolean value to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6782561d-84c3-4c65-9dbc-a6eab3497130");
        if (value) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2440bfdb-2b8e-4696-b4a0-288c0b39dd23");
            ensureCapacity(size + 4);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "98e54654-6f8a-40be-8bb4-2d1b469f874d");
            buffer[size++] = 't';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "75b58cf8-1586-4478-b416-f872927b792c");
            buffer[size++] = 'r';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "bf630e05-a73d-42fd-bf0f-a2392e70ca4f");
            buffer[size++] = 'u';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e0f6358b-bab8-4c93-bbd7-e58adb32a42f");
            buffer[size++] = 'e';
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5ff4dfd9-8eb6-4bf1-883e-20cd1397e98b");
            ensureCapacity(size + 5);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9d586fe1-03d6-4930-9334-99d95ee0f1d6");
            buffer[size++] = 'f';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c7ecbda6-60c9-4490-8253-6bfdc4113316");
            buffer[size++] = 'a';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ec884559-9584-46d3-bf60-760a8546ce44");
            buffer[size++] = 'l';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "46bd4ddd-28a8-424a-831a-b1055e4bc462");
            buffer[size++] = 's';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9b5a680d-935f-4748-9c14-c03f23f80041");
            buffer[size++] = 'e';
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b8ece166-654b-473a-85c4-ecfbe1d5ac5e");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "87b2b986-949a-4f21-a7b1-d464befb20e0");
        final int len = length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5278e44a-13cf-4bfb-b717-85076407178b");
        ensureCapacity(len + 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "de5e186f-1ff2-4861-b163-596f676cb04f");
        buffer[size++] = ch;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "645ff841-0da8-4c43-a522-00d1cdb36d67");
        return this;
    }

    /**
     * Appends an int value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b9080071-4fd8-4c52-9430-0d53c7f69bf5");
        return append(String.valueOf(value));
    }

    /**
     * Appends a long value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d9887287-4af5-4bd7-91c2-1923fd3c8a45");
        return append(String.valueOf(value));
    }

    /**
     * Appends a float value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7e15ad14-f347-47aa-9afd-041688f05745");
        return append(String.valueOf(value));
    }

    /**
     * Appends a double value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dd2e6ea4-b71a-40cf-8bd1-bd8d690dd6bb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "87f78d92-aab5-4f0d-994e-0bf172d957f5");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c337d802-6fd5-46df-99d0-9132c3698ed3");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9648f73b-56b2-40d2-9202-97b9b7e578ac");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "40d51f30-6d18-4ba5-941a-10b7b11451ee");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5b3bcd2e-2457-4327-9201-2b98c6398f53");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "46ecf07e-7a98-4044-94d3-3a3a499d8960");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1be87fb8-1f75-4c1b-8301-e1c388b329d8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "39e2d190-870f-4b21-acf4-b3a1466f74ee");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0be10d36-5854-4961-8a10-41732ecdb716");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8532e909-63bb-41e5-97e9-1b18e1009fab");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9f0de48e-56e8-4869-9bba-d53850bf87ff");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1e70daba-a258-4ed8-94d6-db786880b4f1");
        return append(chars, startIndex, length).appendNewLine();
    }

    /**
     * Appends a boolean value followed by a new line to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final boolean value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0012ec6e-716f-41d3-8c41-7fe780796549");
        return append(value).appendNewLine();
    }

    /**
     * Appends a char value followed by a new line to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2b4bb9b9-7865-46a1-a04f-33381c42dc67");
        return append(ch).appendNewLine();
    }

    /**
     * Appends an int value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final int value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c23c9005-9455-4f20-b7ad-c5338d30270a");
        return append(value).appendNewLine();
    }

    /**
     * Appends a long value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final long value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8ba2ead1-21ea-4e06-af9b-5933e5d78230");
        return append(value).appendNewLine();
    }

    /**
     * Appends a float value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final float value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4f22355e-0086-48b1-a8e3-38f8528fd10b");
        return append(value).appendNewLine();
    }

    /**
     * Appends a double value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder appendln(final double value) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d1516f21-6382-4459-8066-789904168440");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8b5f53d4-6b25-49d3-9b9c-fbc2d92a2088");
        /*
         * @SuppressWarnings used to hide warning about vararg usage. We cannot
         * use @SafeVarargs, since this method is not final. Using @SupressWarnings
         * is fine, because it isn't inherited by subclasses, so each subclass must
         * vouch for itself whether its use of 'array' is safe.
         */
        if (array != null && array.length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1f258f25-e477-402e-8435-09c7c3d41fae");
            for (final Object element : array) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3d2cdaa7-8678-44db-8583-ca3ab4649253");
                append(element);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b69963ae-228c-4459-9a28-ca83d20c67f2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a9191c19-be53-4c55-a31e-80bd09e7e91e");
        if (iterable != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a787d60b-f309-4b69-ab5b-01a8ecb07d0f");
            for (final Object o : iterable) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ed9dfcec-a6bf-42dd-a82a-6e57cf86361e");
                append(o);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5085f2fa-8cf0-4d52-9b80-14d30171eb4a");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5380f5c2-3812-4e70-997e-52da7f08aad5");
        if (it != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ac4e846d-862c-4179-b988-fbf93e27ce4c");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "411c41e5-fa57-4664-aa9d-e792f11e3557");
                append(it.next());
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "64d92b68-8e2d-4218-aed6-d92a024e296c");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ffb568da-080a-47b2-b108-6f1ccd1e8ed7");
        if (array != null && array.length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9ecd12c1-d9de-4824-bfb6-c030b1c912cb");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a095f4f9-c240-41b0-a38f-5b83371a6ebb");
            append(array[0]);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6e11b6ea-56ea-4d0e-806f-3723f6c7dd8b");
            for (int i = 1; i < array.length; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6a1e8e14-7bdc-47b7-9297-597031c5c924");
                append(sep);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0d92e8f8-5138-409d-9f40-0f8009566018");
                append(array[i]);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "733c78ad-3577-46c5-8e5f-b1c24109c826");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e1009e77-3b01-4d3c-a659-dfc4b2da6ebe");
        if (iterable != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fb90b76d-7bac-4331-8549-c47481e3c58f");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7393dfde-1a7a-46ff-92ab-8338b09a8e33");
            final Iterator<?> it = iterable.iterator();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c3e63f9d-83ec-4598-afa1-4b771028d0ff");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "68895434-75ff-4d13-b6c3-1d28fc002d7d");
                append(it.next());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c24083b2-7286-4d47-8861-4dae3293eb1c");
                if (it.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "65ea64b5-01ea-449c-a4b1-551628fa2622");
                    append(sep);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9d26b682-0864-43b6-9fca-3631ad7be8eb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9b9c44f7-76d0-42d7-a82d-7a6b7882dbaa");
        if (it != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e7f048ce-0475-4f1a-b308-0aa4dd6636d6");
            final String sep = Objects.toString(separator, "");
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "66b018df-1267-4eb1-9f71-ea8bc61d084d");
            while (it.hasNext()) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "447a76bd-c681-4a13-88ce-7318fd021648");
                append(it.next());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "99209057-620b-4aea-acef-d4b2e4543de6");
                if (it.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dbee209e-87ca-4f3a-a275-f272cc75c2cf");
                    append(sep);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1130b2c8-fd61-4dc3-9ad9-547217121d0d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2d106bfe-e98a-4676-b0ba-fe26dd227087");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "95124317-56ec-4561-9488-812a382cc5ac");
        final String str = isEmpty() ? defaultIfEmpty : standard;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "91ab65c1-bf88-4ec4-802e-9019537018c1");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6599dbde-96a8-4897-a7ff-fa63e88c77cc");
            append(str);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b1f87f81-0533-4d90-8b49-9ae927ed9b58");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2f336bd5-e1a0-4b9f-a237-6bf745380afb");
        if (size() > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4934270b-a37d-46ca-bac5-8ed34ad48391");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c5a1d00d-c697-4180-a592-8510d9f58afe");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "772d6bbb-5a7d-4437-8bc1-c232edf77276");
        if (size() > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5cdd3990-d2be-4d26-be5b-277c1eacf34d");
            append(standard);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "eb95af51-51d5-4267-b748-146d60608dc9");
            append(defaultIfEmpty);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "580966b1-aa78-4351-8742-3abc90aedabb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "836d32ad-ace0-420e-b5b2-717b5ac21d7c");
        if (separator != null && loopIndex > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e3570419-2baf-45d9-abda-aa9f61bad7ca");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "362d55c0-a83d-43cc-8cc2-644921d6eca0");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "42dff41f-e36f-4af7-a935-23b2ac2142bf");
        if (loopIndex > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9ebf2efa-45da-4fb9-9cdc-0f7e838d4a37");
            append(separator);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "411f3e24-0611-45c2-b929-163e93c1f851");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a4fa468b-2111-454c-8d12-da4564ee1b4d");
        if (length >= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ff2a7940-9929-450a-be98-eaf169dbad21");
            ensureCapacity(size + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "23258f53-8659-4557-af58-e8210c93b21a");
            for (int i = 0; i < length; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e89739a5-adde-4a67-8ea6-c933592c9d49");
                buffer[size++] = padChar;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f3edadba-3a86-43c9-8dcc-73774d496757");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3a2ec8f5-776a-4219-8677-0feaaf14c0bc");
        if (width > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "004c80c1-93d5-4eb0-980e-5b0941378e7b");
            ensureCapacity(size + width);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1e75819a-81b9-46c7-8b60-0d63a7426312");
            String str = (obj == null ? getNullText() : obj.toString());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "186bd006-5fd7-44c4-8533-2b36f76052a7");
            if (str == null) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "20e1e0b3-1619-4117-b1f4-7d035921a4bd");
                str = "";
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "83eef586-e8f4-4b1e-908f-1160bafc4393");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1199c402-90e4-4923-aeb2-87293d0f3955");
            if (strLen >= width) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "15818a49-5bc2-4ec2-bc1a-3df788e59566");
                str.getChars(strLen - width, strLen, buffer, size);
            } else {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "176fac72-e199-4bd2-b36c-f781b48730b3");
                final int padLen = width - strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "640aa028-74d1-4064-8651-3f9aa2575e9f");
                for (int i = 0; i < padLen; i++) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "72339446-0a1f-4d0b-8729-80291a09ce1e");
                    buffer[size + i] = padChar;
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "122bb1eb-75a4-42c4-861d-6aeef845582a");
                str.getChars(0, strLen, buffer, size + padLen);
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d1e5d34d-9c84-457a-a283-c8042b6c264e");
            size += width;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5f315a72-e775-4231-a909-0eb8098f80a2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "79f017e0-7b9b-4089-ab20-aaf2de043b38");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "32145a73-aedd-406f-8152-0168fc42cd96");
        if (width > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3d92d533-a36d-40b2-940d-a314e3e6dac0");
            ensureCapacity(size + width);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "84869c9a-8186-4531-b408-c710ff51da01");
            String str = (obj == null ? getNullText() : obj.toString());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "451845d4-c210-4e08-b83c-90c21c58e3c5");
            if (str == null) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a0c365d8-1173-42c1-8251-9a6030d05806");
                str = "";
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f8ba0391-598f-48f5-a12f-30099b552b59");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "52c9093a-22a3-4a94-b06e-3be2486765f8");
            if (strLen >= width) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4776b238-51e7-4057-8619-53f8e1028b20");
                str.getChars(0, width, buffer, size);
            } else {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1da9d60d-87b7-40f2-9398-0608ba8aa4a3");
                final int padLen = width - strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f4aa595c-06c2-4ef8-aafe-4d01dae798a7");
                str.getChars(0, strLen, buffer, size);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8cb71c5e-6bc0-4c33-9caa-b0a62b077e58");
                for (int i = 0; i < padLen; i++) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e70fcde1-d338-4838-8db6-a9c2ce110425");
                    buffer[size + strLen + i] = padChar;
                }
            }
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1e88c302-f2df-49c8-b4bf-2c699dfdaea6");
            size += width;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "706e2ddb-22b3-49fc-a233-d788b3575383");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e4766d4c-66b8-4775-9863-e2c69fd51bb1");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4eafb735-db8d-472d-b9e2-7812b10cd880");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7c434ddd-5d43-4449-985f-b01165542bd7");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8c976966-0ef5-4bc5-9434-c3ddff7a1daf");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dfca8ffa-eef3-4ba1-94af-cf2fbaf98dbf");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b0a514fd-1a8f-4e58-9b4c-801d45b727bb");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fef783a1-2649-4f4d-af36-adf8d7073b85");
            str = nullText;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "12b8630c-8c73-4384-91fa-1315a0ef7f86");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d19dbb8f-c952-4f04-8f04-8b71100aa75a");
            final int strLen = str.length();
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4907793a-d81c-4ad6-800d-1caf7a13beb2");
            if (strLen > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1e9e17b4-157e-47e1-9b69-5f774acc974b");
                final int newSize = size + strLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "98162b81-ead0-4d43-9585-a6890db934a0");
                ensureCapacity(newSize);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "090ef550-dc60-4216-a7e3-7fd4e4f7c703");
                System.arraycopy(buffer, index, buffer, index + strLen, size - index);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "63cc4b9d-2268-4847-9e10-5704a5817043");
                size = newSize;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7b72540f-6904-4b57-89ee-0869a5b3fdd4");
                str.getChars(0, strLen, buffer, index);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2f2a8a5d-0801-43f3-bff0-6badf15e18d6");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6d2b7ec4-ec41-437e-bc2d-02f024d26d30");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "66e19147-fb2f-4037-8116-866f170c1663");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c6635e1f-857d-40cb-92e3-93d38a1d30c6");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d4a6fc86-1ebe-4a48-9792-face7d87363b");
        final int len = chars.length;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a118d501-4195-49c2-ae5e-29e29e6857c2");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ac4d61a1-75b9-4f83-bb95-8f6b2b02d089");
            ensureCapacity(size + len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "19581574-33a6-4b51-909f-5aeeb7a1bb66");
            System.arraycopy(buffer, index, buffer, index + len, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e0e09c61-0c19-4a1b-97b9-7c9465f791ae");
            System.arraycopy(chars, 0, buffer, index, len);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "efa2e918-bd28-4617-b06e-5a3dc3e9ecc0");
            size += len;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b5901a1f-3c22-4146-b245-b4748de5bea7");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7b1654d6-056a-4222-afc6-f0c21c603850");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1911ebfe-fb8d-4a96-a8ca-4f64899b26e5");
        if (chars == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "efa77f2f-50d0-492d-bf75-6ce8135e53e7");
            return insert(index, nullText);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "72dcf557-693a-445e-9e4c-736b605534de");
        if (offset < 0 || offset > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "88f3808f-6c00-4211-965d-7a94d0c4f0b1");
            throw new StringIndexOutOfBoundsException("Invalid offset: " + offset);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7dcaa4bd-9f4e-4565-be66-d05ac6467495");
        if (length < 0 || offset + length > chars.length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "91320430-318b-412b-a0f9-4ee82d2accf6");
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "40e28ca7-b52b-42ca-8881-d32357bffedd");
        if (length > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b29f2fb4-24a7-425c-beaa-956434121183");
            ensureCapacity(size + length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e7af8e59-3388-4f93-aec2-2a43c2a10b67");
            System.arraycopy(buffer, index, buffer, index + length, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a7d17d07-9c27-49e8-baf2-ebb5d5a0efe6");
            System.arraycopy(chars, offset, buffer, index, length);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e243f088-f4a2-409e-a18f-25689e9dd90c");
            size += length;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3892450e-1de0-4747-be92-7861a922708f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a65a19bb-7df2-4030-8cb0-cbd0665385fb");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e0e6cb3b-c0d3-47f7-a7c8-30ffe1149b1c");
        if (value) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f953927a-7f33-467a-b157-cef9b3e802e4");
            ensureCapacity(size + 4);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ccdb95e6-439a-4a9c-87f1-d8ae265beb57");
            System.arraycopy(buffer, index, buffer, index + 4, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b60551d1-3d30-4e82-8367-58c03f3cb843");
            buffer[index++] = 't';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ecf504a8-0e57-4fd3-b2a3-3c1e18b23945");
            buffer[index++] = 'r';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c7a820b5-adce-4be2-8430-1acb869706e8");
            buffer[index++] = 'u';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6b46d0a8-d32e-42e8-9e8c-20043de144f1");
            buffer[index] = 'e';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f1e363bd-2a5f-4e40-8342-97cab9e04ab3");
            size += 4;
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3d194100-2d2c-4036-9144-340f32c8fa07");
            ensureCapacity(size + 5);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "076e7db4-4be3-4d69-9a7b-e3192ab2ad94");
            System.arraycopy(buffer, index, buffer, index + 5, size - index);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "18d2d383-7e0c-41a7-a0c4-786520ee2f89");
            buffer[index++] = 'f';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c8072594-0e58-4d5b-b5dd-315feaa3f385");
            buffer[index++] = 'a';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0eb35151-6e7a-4966-b5d8-18fefced2a38");
            buffer[index++] = 'l';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c6d8b237-ce54-4306-a742-f72e354b0e0d");
            buffer[index++] = 's';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "13aa9f98-e8dc-4f6e-af0c-2b7c5728a76f");
            buffer[index] = 'e';
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0ca2a2d5-fd80-4f3f-86be-fe160c516387");
            size += 5;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4e397be4-1086-4577-99b4-5c5d4138c977");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c04df523-688a-46fe-b53e-7e65199e43b4");
        validateIndex(index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "16f8d4ac-ce4f-4cf4-aeb2-973adbc6c784");
        ensureCapacity(size + 1);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e391ff94-9938-456b-95cc-bb9da27b8429");
        System.arraycopy(buffer, index, buffer, index + 1, size - index);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a6cae15a-c0b6-46fe-9ea0-43a793ac9fad");
        buffer[index] = value;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0db0eac7-d512-4268-ba1a-f034b742c308");
        size++;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e3459234-c3d9-4433-a19c-c2eab87e451a");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d3381281-180c-467c-978b-c29684583ac4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6ce6f85e-a3d8-4ea1-8d2d-a697013fca2d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f61677ca-8619-41f1-9b89-478744456a54");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8ae0856e-fd91-4eb1-b446-542829be4ae3");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "39be031b-60e2-4ce6-92b5-1455bba89a74");
        System.arraycopy(buffer, endIndex, buffer, startIndex, size - endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f0c49814-f198-457d-8942-55ca0ea7ac6f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "893b84ae-5438-471f-8f55-9f67d9b4b9f9");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "87324951-1449-4423-a9ef-5f9a7ef1e1a4");
        final int len = endIndex - startIndex;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8a39a754-9675-4fb5-b4ce-a182e0a8c527");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "45f600ad-7f53-4849-ad16-3b8446f76c7e");
            deleteImpl(startIndex, endIndex, len);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "37126f16-de26-4155-a7c5-ae3981172d19");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4b8b3bcd-aa39-4fd7-9a7e-d39a5932c335");
        for (int i = 0; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8bfb5d73-55b2-4086-9f9e-a0344866d6fa");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cbc82ac4-6edd-4912-aa5b-830f7c4f699e");
                final int start = i;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8774e927-0a98-4b6d-8af2-f42d5bd1ac4c");
                while (++i < size) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b09203cc-1ea0-43e0-9218-3d3d6342a99a");
                    if (buffer[i] != ch) {
                        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "157b8f41-44e9-48ba-ad5f-43f88f903e07");
                        break;
                    }
                }
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ccad8209-8091-4d36-9a79-881472bd0045");
                final int len = i - start;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "81560cb7-731d-4a14-b261-4c9ca23e00ca");
                deleteImpl(start, i, len);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "365aebd9-9890-478a-bf25-ee5362a1f72e");
                i -= len;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9b30adad-201c-48ab-b29d-f1c9c9837e38");
        return this;
    }

    /**
     * Deletes the character wherever it occurs in the builder.
     *
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final char ch) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d58764c3-318d-4fcb-95c8-13513987f828");
        for (int i = 0; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d5d4f7d8-646e-44da-82c2-fd00c1253ed2");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "636b9f40-0abb-433e-b18e-c700094c4dc6");
                deleteImpl(i, i + 1, 1);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "101a98c4-cca1-477c-a194-3f41796f3c04");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "25de8907-23e1-4549-825e-4c0175aa3204");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "367e75e0-347b-45f9-8906-be9a3b0f2ba9");
        final int len = (str == null ? 0 : str.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "aba3ed1f-69ef-406e-b403-6c652de7707c");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "327588ea-f547-4ec6-a273-7c69f3f742f0");
            int index = indexOf(str, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5c7d5501-1ce6-4614-aa9d-fb402e6e3978");
            while (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1099cf1b-165d-46e7-b9c8-a37e8f5934dc");
                deleteImpl(index, index + len, len);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "31105131-5796-4f19-93a5-befe8a67aae2");
                index = indexOf(str, index);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6280832b-4d82-4f65-b72a-ca00a3f8b708");
        return this;
    }

    /**
     * Deletes the string wherever it occurs in the builder.
     *
     * @param str  the string to delete, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c168ee91-2c14-45df-936c-a47952f229c2");
        final int len = (str == null ? 0 : str.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c02a96a7-dddf-479d-bb57-af807df32eca");
        if (len > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "faa584dc-b985-4f37-8689-a495fd8512bc");
            final int index = indexOf(str, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "22230087-97ea-470d-b135-b99861ccc99d");
            if (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3ef5d644-b809-4b06-a16d-a54421a59fc6");
                deleteImpl(index, index + len, len);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "031ea444-767b-49f6-b352-ff777b7d7dc1");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5c147c68-d4e5-43cf-b096-c3379e5d7f13");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ac4196dd-502a-4d65-a7aa-12a4a6067591");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d5b75698-06d6-4ea0-aae2-38849891aa69");
        final int newSize = size - removeLen + insertLen;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "11ddf79d-fd0b-4333-810d-eb660852f7ca");
        if (insertLen != removeLen) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "49532562-0bad-4bb6-a76d-0882b7950ceb");
            ensureCapacity(newSize);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b104bd03-ef2e-4ee3-833b-2c66f8920c30");
            System.arraycopy(buffer, endIndex, buffer, startIndex + insertLen, size - endIndex);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ad73b28c-fd10-4902-ba08-aa5e24e7efa5");
            size = newSize;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ae9c22ad-f786-4542-9fb4-5d141822676f");
        if (insertLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0f2288db-e882-4ccc-ba03-4f7c3af1da3d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f9447787-7454-4f3f-a990-2dc8f5fecb22");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4663403f-4ba1-45c1-bbc7-a11ca1a0b7ef");
        final int insertLen = (replaceStr == null ? 0 : replaceStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e62e8413-064e-4bf9-a750-4b397c5ea17c");
        replaceImpl(startIndex, endIndex, endIndex - startIndex, replaceStr, insertLen);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5d0225e8-6d7d-4063-b3f6-b9f04dd0feb7");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "38749b82-4111-4822-8cb7-581f27cc8fb8");
        if (search != replace) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "06368460-1597-4eaa-9d96-8971fafde64e");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "13a37118-9c2f-4ae4-9f0f-48e6cc4e5a46");
                if (buffer[i] == search) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "078e8310-56d9-4766-8d5f-3ca04b74979e");
                    buffer[i] = replace;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2651fb9f-fd65-4683-b019-3d7e4f9b3778");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a9fac106-50fc-4cf3-ac4a-a69e5b0cf3e9");
        if (search != replace) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "968f878d-b199-4301-886f-4df414532454");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3b873358-a8a8-418b-a87a-760d52afd44e");
                if (buffer[i] == search) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8380b3f4-3731-4a16-8225-d4305a8b29c9");
                    buffer[i] = replace;
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "19064129-bbb7-48af-8625-af9b35c1b599");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5bb6c72e-1d73-4a15-b046-1d92be071155");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5b5a26e5-7ae7-45a2-a302-ec02e95d6f75");
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "08744571-38e5-4aa0-b56f-988f7418484b");
        if (searchLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c9382da0-6c32-430c-932f-cd7db6abdb15");
            final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "68fa1967-adf6-4159-9059-cc43751cbcfa");
            int index = indexOf(searchStr, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2ae346e6-8bf1-4cac-aeac-f1e6f87be35f");
            while (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2322b986-59d3-46db-9c79-7975a02bfbe0");
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8d60d32b-6c20-4511-9593-f651588c0144");
                index = indexOf(searchStr, index + replaceLen);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8f177ba7-d247-446a-a597-2f45b4399f31");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6204403a-0bcd-42d7-92d8-062994cfe329");
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5aecf6ea-1c4a-4226-8ecc-6fb361f1ca2f");
        if (searchLen > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "231b2655-74b9-4495-ad8b-f313fa44ab4a");
            final int index = indexOf(searchStr, 0);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "54879c18-6459-442c-bf05-1aea1bfacc5b");
            if (index >= 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b25fc5c5-e217-497b-9eea-5cad4c2e489b");
                final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a3b487be-b539-407f-a09e-40573be2de51");
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ef167b3d-6814-4640-88e6-2394931f6dda");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "404d2ec5-c1b9-44ba-b792-c7f64d3eba84");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c69ae575-f1a0-424e-9021-9178ae058bb2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4165f9d4-b735-4b92-96f7-6698d3f899c6");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "147dddca-c2ce-43cf-abfb-023feffc55e9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fd06f575-235c-42cd-a69e-abe90517ed0b");
        if (matcher == null || size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2c5bd5e6-1a88-412c-bda9-dbee737967e5");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f3f9fe4e-beaf-4ba2-9368-35222cdb98ff");
        final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "28219350-c31f-470c-9caf-6de2700a8a3e");
        for (int i = from; i < to && replaceCount != 0; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b488f262-3d6c-4db5-839b-59f6b25c87b7");
            final char[] buf = buffer;
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "db6b34f9-f6f7-428e-b6d1-c70d0cb2b21c");
            final int removeLen = matcher.isMatch(buf, i, from, to);
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a13364aa-9ef1-4abb-9080-8aa0788b626d");
            if (removeLen > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "56763035-6ff1-4a80-b4cf-e285c05eb7a5");
                replaceImpl(i, i + removeLen, removeLen, replaceStr, replaceLen);
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f69cbf28-1805-4215-a2bb-3f4b241d2b87");
                to = to - removeLen + replaceLen;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "63337bb0-936a-4909-96e3-070b7cdf8e5c");
                i = i + replaceLen - 1;
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "32f24e2f-748b-47d6-b385-aece7065245f");
                if (replaceCount > 0) {
                    writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8ecd0bf9-5440-4819-8936-7b18c0456991");
                    replaceCount--;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2e8aabc1-7c5d-4598-a873-4618ef20b526");
        return this;
    }

    // -----------------------------------------------------------------------
    /**
     * Reverses the string builder placing each character in the opposite index.
     *
     * @return this, to enable chaining
     */
    public StrBuilder reverse() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cd58ba2b-caa8-4801-b902-f0066a47005a");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1ba11509-7ba7-4bde-8a10-688ac746d3c3");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c4df4efa-debd-44f8-94e8-83883e197e97");
        final int half = size / 2;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9b9a0500-2812-4feb-b6e5-200492bcd7cf");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a8680a2d-2c51-42e1-a1f4-bd45a9c50809");
        for (int leftIdx = 0, rightIdx = size - 1; leftIdx < half; leftIdx++, rightIdx--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "579acd3e-0e14-48ed-a3fa-10b2176dd2a2");
            final char swap = buf[leftIdx];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2a52249f-e2da-4f24-901e-2817f17584f2");
            buf[leftIdx] = buf[rightIdx];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a819aee1-864c-4c32-a576-d08dc59e511f");
            buf[rightIdx] = swap;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1fb814f9-3a79-4f6b-9c44-f5d7cb3d3ba8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b292daf7-9860-45f1-80cb-58191c41cbf4");
        if (size == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "23855562-bc4f-46d1-924d-9e9f1c586239");
            return this;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e74cf249-a78a-4937-b6cd-bc98413aedf4");
        int len = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0f6f2c3d-13fb-478a-80fb-243054fae884");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ffe798f9-a6d8-4f9c-88df-d88002db374b");
        int pos = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8ba14806-7994-4ebf-bb93-a91466d3e877");
        while (pos < len && buf[pos] <= ' ') {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "716a9e4a-5412-4bc2-b2f4-b5dab8e839fa");
            pos++;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "91ddb67a-320f-4484-ae36-1c06edab83a0");
        while (pos < len && buf[len - 1] <= ' ') {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9e69e043-9879-4fa0-a008-c905b5f65d0d");
            len--;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b03037ef-da35-403b-8872-cd6c8c337a98");
        if (len < size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "17d9751b-b074-430a-8503-74c0362e7445");
            delete(len, size);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "946d33d9-8f2f-4efa-be4b-63afaa41cb4e");
        if (pos > 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8d546e60-c095-4297-bf48-dc77f5a2e3f6");
            delete(0, pos);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "43f8f2e0-7b08-459d-90ac-6fc3435202cc");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e823ca47-8635-40a6-b0dd-1d60c3607fa3");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3c615324-ddbc-4933-845b-2a705790c621");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1cffcee8-2c3d-494f-9667-33b746d1f4f2");
        final int len = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c860f2c3-a5e0-47fb-a37a-41585c88d043");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a1db2e0b-f521-48a0-b3a6-e9dbfa5e916b");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "43f0658d-ff49-48a4-8493-f8c74f94434c");
        if (len > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b0151079-8e3a-4c73-af25-b8ec594266a5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "17983176-53e9-4fc5-9ce6-33dc673634e7");
        for (int i = 0; i < len; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f0beca2f-2108-4243-8dde-bd23bdbe13ac");
            if (buffer[i] != str.charAt(i)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7e47e886-3542-4de2-a68f-602a9e007d05");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7ab52ba0-63b1-4c53-928a-58575d9ff18f");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fe614736-c4dc-4a1c-b715-45eeaf029352");
        if (str == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2d7792aa-f47f-4c18-8431-007cce335ca9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "74051769-e401-432e-91cc-969dedbf87fa");
        final int len = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e4ef869e-948f-4ec5-b9d4-a05af482aabf");
        if (len == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "98e88f30-54f5-490c-9554-51dc6b2110a9");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2dfca424-d6d8-4ac2-9ce8-6090daa29805");
        if (len > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "06461ea2-2985-48d1-b0c2-27d1daa7f918");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d8339df7-9b03-4763-9021-d029d32ef7ba");
        int pos = size - len;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d37b5b49-591f-4529-97eb-1c99e479b285");
        for (int i = 0; i < len; i++, pos++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dfa86998-b8a4-4567-bd5f-3f6910b85bab");
            if (buffer[pos] != str.charAt(i)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "be927650-a828-4491-a853-e9894b3348e0");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c1d8672c-5a62-4723-aad9-6fe14b106141");
        return true;
    }

    // -----------------------------------------------------------------------
    /**
     * {@inheritDoc}
     */
    @Override
    public CharSequence subSequence(final int startIndex, final int endIndex) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9fa5b69e-44b5-440a-aa5e-f33ed7683583");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a5607b9a-3d63-4cb7-bd32-fbb527f93d39");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4d689cfe-0fbd-4bdb-8295-9bb39cbc337c");
        if (endIndex > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3eaa536d-8dde-4e5a-8094-f2b5b48a93fe");
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e330889d-4c47-41ed-827f-d958b4829b2f");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cf92209b-5a2d-4f85-a87d-a6721cb78bd1");
            throw new StringIndexOutOfBoundsException(endIndex - startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "363dc245-644c-4f91-9feb-0c7baca44bdf");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "74f67267-9801-4923-adff-9fca03a6dbc9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ae7add17-d971-4fc1-a414-365f771b1ba6");
        endIndex = validateRange(startIndex, endIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4b5c72e6-d6cd-45b1-b040-7a83a96fd8fe");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7485f86c-1867-4d0a-9827-2ddbe872f481");
        if (length <= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "be89e521-3672-4ed3-a9d7-442d17673b4b");
            return "";
        } else if (length >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6e17c25e-ea13-426f-9e8b-b7638c036fea");
            return new String(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "39fc0ae4-308c-4f3d-ba4f-07f834368560");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "91120dfc-bb7a-43b5-8028-929b138721f9");
        if (length <= 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "03301e9c-732e-4822-8c0b-a692459faf8c");
            return "";
        } else if (length >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "479524a5-5853-4db0-8a2b-84324ea2a682");
            return new String(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f20ec3bc-4790-443e-9f65-6d5ea6e3cd5b");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "95fdb9d2-b486-476b-864d-6c8ff23ad4b2");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0004c53e-7a10-4d24-971e-175bdb039ece");
            index = 0;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9b675f35-aa17-44bc-9da5-fe6cd6e8a2cb");
        if (length <= 0 || index >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c3880546-27ba-4a34-8008-1f7bd7bd2d29");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fa20187c-e050-40b1-879b-2ddd253b34f8");
        if (size <= index + length) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f73f36e3-70cd-4dbe-a899-43f86b040948");
            return new String(buffer, index, size - index);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0bd0880b-c1c2-497e-b6c5-729fc70f85a4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3f452d38-2a94-4944-b159-a2b4d4433fe2");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4adf33b0-cbf5-4325-8f09-e055d62f2af1");
        for (int i = 0; i < this.size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e96a859c-221a-441d-9186-06d9a097f0c8");
            if (thisBuf[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "99b3bd2e-6f23-428c-a1f0-dc40ff0c2d51");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "33ae3790-9846-4fea-b8f6-276c40147fde");
        return false;
    }

    /**
     * Checks if the string builder contains the specified string.
     *
     * @param str  the string to find
     * @return true if the builder contains the string
     */
    public boolean contains(final String str) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "65875a81-8ad2-4c97-a30b-764b6ab48348");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "86567793-a4e4-4543-b631-53c576490e37");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ca97a935-9144-4aaf-8728-cf1a29bd3df7");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "662b01f2-d5fa-420f-bfec-18c21354aab8");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "90454d6e-a18a-4a9a-a292-4ac1fc88841d");
        if (startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9e0ca82b-2266-4163-a39d-cf0bbf33044c");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7f45b645-c4de-4b87-9b51-51c88573a252");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "37b92a29-151a-4e2c-9d7b-1838d8776b9a");
        for (int i = startIndex; i < size; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6d3467f2-852a-45aa-837f-c8a00edd70ff");
            if (thisBuf[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ea533f11-258a-43b8-99e6-e19ee1e7a151");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "75f70398-b81a-4704-8656-56c0af7aac2d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1d002d85-6b6e-4483-8092-051d557ba0b6");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "544fbbe4-26bb-40cc-b0ae-ba5bff1a1b9a");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "60a71ce8-f0a2-4815-94bd-376e7e8bb950");
        if (str == null || startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "41d8a03e-07eb-4208-823b-928759efd6a6");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "96099697-c877-4f55-8c40-48a9799362d8");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "13034ef1-b4a2-4f37-b7d8-2daccede6d32");
        if (strLen == 1) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8f6cb4c3-e561-437f-9ba2-3896e0d3a68f");
            return indexOf(str.charAt(0), startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7cf1fc70-6e39-4bc1-b2fc-23c1031331ec");
        if (strLen == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2c2005b7-165b-4520-8073-e167413305a5");
            return startIndex;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "30df5cda-175a-4712-a200-08a1163fdd53");
        if (strLen > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b2b5b5ea-9c05-4e2d-b5ca-780eec3b2ed0");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "570df380-eaef-48e0-bd3d-49951af215e7");
        final char[] thisBuf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ba806a1d-1490-4cc9-893a-de9a64200b92");
        final int len = size - strLen + 1;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "92b1c32e-4792-45c8-94fc-5e330bececd2");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "885e2dc5-c75d-452d-8199-3331a9f97ccb");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "80f56bb8-35ed-45c4-ad78-39ebbc093f92");
        startIndex = (startIndex < 0 ? 0 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "00e0d30a-2f0d-4e73-ba07-2810bd0cb184");
        if (matcher == null || startIndex >= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d2d58bb5-bcc0-4627-bfb6-ce4f0b6560ae");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cc5d75cb-8c90-46aa-a58b-48999a7d62c5");
        final int len = size;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d9b0cce6-deaf-407c-a308-524d47a6b958");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "78e56007-beea-4e35-bfac-bb1d5705b969");
        for (int i = startIndex; i < len; i++) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "23a7ee0e-c077-46ad-b167-8b682e619d9d");
            if (matcher.isMatch(buf, i, startIndex, len) > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "45e26e40-fc0a-438f-ba77-33bb70cef38d");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "dbe1b5d1-28a0-4742-aa21-bdd31168fa35");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "10f3332e-e2dc-454a-8062-09ae8cecde4d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "bb786249-fcc9-456d-a2e4-7f175f79a274");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9545314d-ae93-4a68-9e90-8fd7d22d9edc");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2e3c899f-2e10-4c22-9100-85b127898348");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d25e9916-fb94-4ef6-9a75-194d554fa5b0");
        for (int i = startIndex; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6f928fac-ec62-428d-b678-ec787511ddaa");
            if (buffer[i] == ch) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "86a43644-8340-4c33-a83b-c32a4153e26b");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "88cc97d8-d1a7-45af-aebf-a5189f0b76e7");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "854768d1-d623-4fae-8ab8-9a7a4ce6b12d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d012a567-169e-4681-84f6-ad06a1870e46");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "49dec7e7-f272-410c-9a35-bf3b69a04645");
        if (str == null || startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f31805e5-7b59-4684-84a1-d356a247b39a");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "31ac353e-34f7-4166-9ea5-0b98044c5926");
        final int strLen = str.length();
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a95511dd-dafe-4fbc-9855-333372a313f1");
        if (strLen > 0 && strLen <= size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b766b285-bb6b-4cec-9b2c-dca253a5ef84");
            if (strLen == 1) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "16e9e3ea-1586-44fb-8971-a47d92683825");
                return lastIndexOf(str.charAt(0), startIndex);
            }
        } else if (strLen == 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d5dea2b8-e898-49f6-bbc8-137c8e6decdd");
            return startIndex;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7cf47ae6-c21b-4a2c-bc77-810d7aa82d18");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "105502ec-cbf2-4329-a2d6-7f2523ac37f9");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "31689d9b-fce4-446f-ad78-b04c10acaacb");
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7c4dfef2-70a3-40c5-8e8d-3ce413529911");
        if (matcher == null || startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "cb13eb22-b1bf-40b6-985b-b38e6ea2f7e0");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b29f5f52-553f-4aea-8b44-56024e28b9ef");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "347418d1-c583-48ac-9925-76f1e2079854");
        final int endIndex = startIndex + 1;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7938c3bd-857c-422c-ac8e-aa3699e18a6d");
        for (int i = startIndex; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "94db00d9-2022-4151-a355-9f47b19df590");
            if (matcher.isMatch(buf, i, 0, endIndex) > 0) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f2fb1d6f-8644-4fcb-82c4-c45a5e823caf");
                return i;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a55044fd-e736-45d9-91ba-796f61dc5a96");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4c4baece-bcda-4bb0-bfa3-61a8e07b18a8");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1efdafa1-c396-44d8-bf2f-c173a0858af4");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d3aa906e-3508-417a-b0d3-d843d8808e28");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "c9d73f35-dba2-4dbc-b2a2-325e305e8f18");
        if (appendable instanceof Writer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d650a277-526e-4b90-9635-66e2bc76e82c");
            ((Writer) appendable).write(buffer, 0, size);
        } else if (appendable instanceof StringBuilder) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "56c83600-58bd-4a6e-8528-49038852b9cd");
            ((StringBuilder) appendable).append(buffer, 0, size);
        } else if (appendable instanceof StringBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a61ba653-02f9-4449-a8b5-0c014e4b8ce2");
            ((StringBuffer) appendable).append(buffer, 0, size);
        } else if (appendable instanceof CharBuffer) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "5df1c385-208e-4b4f-8327-b4288ee53d8d");
            ((CharBuffer) appendable).put(buffer, 0, size);
        } else {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6153eaf1-8f0d-401b-9c9e-ff9f4a3f9e3a");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "8b15d5f4-ac91-4523-b8c7-a852c754d245");
        if (this == other) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0ddd90b2-b3b4-46de-b918-60573f3e39a5");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e33d3b92-d3dd-4eab-bc1e-fcb30b74ff27");
        if (this.size != other.size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "611a6e28-ffc3-4719-963c-f9e581e4efd5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "03ede746-c392-41db-a7e0-312cc58db06e");
        final char[] thisBuf = this.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f432e583-2d41-45d8-b84e-83912a892358");
        final char[] otherBuf = other.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1fcfac7f-d587-4c10-83bc-11345c5081c3");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4ae15184-8e95-46e4-a437-1b1a4e7b8b1c");
            final char c1 = thisBuf[i];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b9cda7ec-1c9d-45b2-bc4c-ad9618d6f1be");
            final char c2 = otherBuf[i];
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "13f7c97e-6e2f-4e5f-b5f4-0201a8f26354");
            if (c1 != c2 && Character.toUpperCase(c1) != Character.toUpperCase(c2)) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "d0255374-8536-413a-85af-e56b09399141");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "30e63d8b-cd66-4de1-bd7a-1d420e93798d");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "0434b06b-c29c-48b3-9d12-b154408fd586");
        if (this == other) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2aa84899-7a34-411d-b00e-c4846922a4f3");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6b0bfdb9-f7d4-41a7-89c7-da2c80fbb4cf");
        if (other == null) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7b1df642-db40-4a7d-b410-15bd2776b63b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a4564110-1bd9-46d7-aaa1-5b99025cee22");
        if (this.size != other.size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "2be2a968-3cb2-4739-9256-32abe4572a23");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "b3b386cb-b88c-4061-97fd-e96260d090ea");
        final char[] thisBuf = this.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "49461c3b-57c1-4690-9ee1-d3aa696a1ea2");
        final char[] otherBuf = other.buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "19236b70-5cbf-442c-8bd2-0e02c3fb6eb3");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "16d4c1b0-47da-4657-b7e9-6b65903a7df4");
            if (thisBuf[i] != otherBuf[i]) {
                writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e3f4966f-dfc6-438d-89b8-72ca9c82168f");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "ad1d1314-9edd-466c-bee4-7dc3a499b4c3");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6009995e-918e-4b63-a9e4-8b7b4bd5e810");
        return obj instanceof StrBuilder && equals((StrBuilder) obj);
    }

    /**
     * Gets a suitable hash code for this builder.
     *
     * @return a hash code
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "401d2070-1a54-41d9-9e18-b1d88df76af4");
        final char[] buf = buffer;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1b1622fe-553f-4d29-a607-f09a556bbe08");
        int hash = 0;
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "063c6785-830c-42d5-9533-f13974bb680a");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "f5d53664-5747-406b-a407-8d4278a9612e");
            hash = 31 * hash + buf[i];
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "24c64777-bab2-444f-bd6e-2ee45e8f3154");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "7afd206b-ebcb-489f-9045-5aee75a86cbc");
        return new String(buffer, 0, size);
    }

    /**
     * Gets a StringBuffer version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuffer
     */
    public StringBuffer toStringBuffer() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "a54a7265-3c57-4445-82af-4125f99a5812");
        return new StringBuffer(size).append(buffer, 0, size);
    }

    /**
     * Gets a StringBuilder version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuilder
     */
    public StringBuilder toStringBuilder() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "6726a43a-5f9a-4f94-ac46-21fba53a7ebc");
        return new StringBuilder(size).append(buffer, 0, size);
    }

    /**
     * Implement the {@link Builder} interface.
     * @return the builder as a String
     * @see #toString()
     */
    @Override
    public String build() {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9daaf9d9-31f5-4010-a9cc-cceeee679132");
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
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "e09c3775-7d39-4711-b697-9337d3a9d4b6");
        if (startIndex < 0) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "3c0fb43a-840f-4140-9479-3f7b6685bf1f");
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "4e50af45-23f8-4c58-8807-c853d06bd98f");
        if (endIndex > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "9f1813ff-69e8-4af7-9103-1d24e34549a5");
            endIndex = size;
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "040eb9b1-4d01-40ef-a6a6-306f6d68224b");
        if (startIndex > endIndex) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "45706665-8876-488d-bb87-4f32fdb99aa1");
            throw new StringIndexOutOfBoundsException("end < start");
        }
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "be21a418-db33-4747-aa57-a210e1dfb374");
        return endIndex;
    }

    /**
     * Validates parameters defining a single index in the builder.
     *
     * @param index  the index, must be valid
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    protected void validateIndex(final int index) {
        writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "1312fcc8-6847-4c72-b7f1-7954fee04607");
        if (index < 0 || index > size) {
            writeline("/home/ubuntu/results/coverage/StrBuilder/StrBuilder_1_10.coverage", "fe3fe32d-5aa0-433d-a122-f5d348aefa0d");
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
