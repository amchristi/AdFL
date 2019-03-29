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
package org.apache.commons.text.similarity;

import java.util.Arrays;
import java.io.*;

/**
 * An algorithm for measuring the difference between two character sequences.
 *
 * <p>
 * This is the number of changes needed to change one sequence into another,
 * where each change is a single character modification (deletion, insertion
 * or substitution).
 * </p>
 *
 * @since 1.0
 */
public class LevenshteinDetailedDistance implements EditDistance<LevenshteinResults> {

    /**
     * Default instance.
     */
    private static final LevenshteinDetailedDistance DEFAULT_INSTANCE = new LevenshteinDetailedDistance();

    /**
     * Threshold.
     */
    private final Integer threshold;

    /**
     * <p>
     * This returns the default instance that uses a version
     * of the algorithm that does not use a threshold parameter.
     * </p>
     *
     * @see LevenshteinDetailedDistance#getDefaultInstance()
     */
    public LevenshteinDetailedDistance() {
        this(null);
    }

    /**
     * If the threshold is not null, distance calculations will be limited to a maximum length.
     *
     * <p>If the threshold is null, the unlimited version of the algorithm will be used.</p>
     *
     * @param threshold If this is null then distances calculations will not be limited. This may not be negative.
     */
    public LevenshteinDetailedDistance(final Integer threshold) {
        if (threshold != null && threshold < 0) {
            throw new IllegalArgumentException("Threshold must not be negative");
        }
        this.threshold = threshold;
    }

    /**
     * <p>Find the Levenshtein distance between two Strings.</p>
     *
     * <p>A higher score indicates a greater distance.</p>
     *
     * <p>The previous implementation of the Levenshtein distance algorithm
     * was from <a href="http://www.merriampark.com/ld.htm">http://www.merriampark.com/ld.htm</a></p>
     *
     * <p>Chas Emerick has written an implementation in Java, which avoids an OutOfMemoryError
     * which can occur when my Java implementation is used with very large strings.<br>
     * This implementation of the Levenshtein distance algorithm
     * is from <a href="http://www.merriampark.com/ldjava.htm">http://www.merriampark.com/ldjava.htm</a></p>
     *
     * <pre>
     * distance.apply(null, *)             = IllegalArgumentException
     * distance.apply(*, null)             = IllegalArgumentException
     * distance.apply("","")               = 0
     * distance.apply("","a")              = 1
     * distance.apply("aaapppp", "")       = 7
     * distance.apply("frog", "fog")       = 1
     * distance.apply("fly", "ant")        = 3
     * distance.apply("elephant", "hippo") = 7
     * distance.apply("hippo", "elephant") = 7
     * distance.apply("hippo", "zzzzzzzz") = 8
     * distance.apply("hello", "hallo")    = 1
     * </pre>
     *
     * @param left the first string, must not be null
     * @param right the second string, must not be null
     * @return result distance, or -1
     * @throws IllegalArgumentException if either String input {@code null}
     */
    @Override
    public LevenshteinResults apply(final CharSequence left, final CharSequence right) {
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "dc92921d-c644-4b06-ab5a-34c4f9dd5318");
        if (threshold != null) {
            writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e831f272-4d3c-4a25-ac8a-cf74df7ad815");
            return limitedCompare(left, right, threshold);
        }
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "b67b2a0d-7195-42db-b1a6-4fc8029de439");
        return unlimitedCompare(left, right);
    }

    /**
     * Gets the default instance.
     *
     * @return the default instace
     */
    public static LevenshteinDetailedDistance getDefaultInstance() {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ff80dd34-0cb5-4f05-86ce-9f74fa67a9c6");
        return DEFAULT_INSTANCE;
    }

    /**
     * Gets the distance threshold.
     *
     * @return the distance threshold
     */
    public Integer getThreshold() {
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d8ce187a-7586-4ab1-b524-a912d5d1ceb5");
        return threshold;
    }

    /**
     * Find the Levenshtein distance between two CharSequences if it's less than or
     * equal to a given threshold.
     *
     * <p>
     * This implementation follows from Algorithms on Strings, Trees and
     * Sequences by Dan Gusfield and Chas Emerick's implementation of the
     * Levenshtein distance algorithm from <a
     * href="http://www.merriampark.com/ld.htm"
     * >http://www.merriampark.com/ld.htm</a>
     * </p>
     *
     * <pre>
     * limitedCompare(null, *, *)             = IllegalArgumentException
     * limitedCompare(*, null, *)             = IllegalArgumentException
     * limitedCompare(*, *, -1)               = IllegalArgumentException
     * limitedCompare("","", 0)               = 0
     * limitedCompare("aaapppp", "", 8)       = 7
     * limitedCompare("aaapppp", "", 7)       = 7
     * limitedCompare("aaapppp", "", 6))      = -1
     * limitedCompare("elephant", "hippo", 7) = 7
     * limitedCompare("elephant", "hippo", 6) = -1
     * limitedCompare("hippo", "elephant", 7) = 7
     * limitedCompare("hippo", "elephant", 6) = -1
     * </pre>
     *
     * @param left the first string, must not be null
     * @param right the second string, must not be null
     * @param threshold the target threshold, must not be negative
     * @return result distance, or -1
     */
    private static LevenshteinResults limitedCompare(CharSequence left, CharSequence right, final int threshold) {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "7ba80073-dc1e-49a4-b94d-d0ffac791577");
        // NOPMD
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "b3695505-df95-4072-a136-6bc1e12bb564");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "f692a44f-61a0-418d-8e66-a018cc80cd47");
        if (threshold < 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "c12181d5-9b10-4f45-bef8-c30135e3974f");
            throw new IllegalArgumentException("Threshold must not be negative");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6f59b32e-b67d-4297-bc93-bf547224d588");
        /*
         * This implementation only computes the distance if it's less than or
         * equal to the threshold value, returning -1 if it's greater. The
         * advantage is performance: unbounded distance is O(nm), but a bound of
         * k allows us to reduce it to O(km) time by only computing a diagonal
         * stripe of width 2k + 1 of the cost table. It is also possible to use
         * this to compute the unbounded Levenshtein distance by starting the
         * threshold at 1 and doubling each time until the distance is found;
         * this is O(dm), where d is the distance.
         *
         * One subtlety comes from needing to ignore entries on the border of
         * our stripe eg. p[] = |#|#|#|* d[] = *|#|#|#| We must ignore the entry
         * to the left of the leftmost member We must ignore the entry above the
         * rightmost member
         *
         * Another subtlety comes from our stripe running off the matrix if the
         * strings aren't of the same size. Since string s is always swapped to
         * be the shorter of the two, the stripe will always run off to the
         * upper right instead of the lower left of the matrix.
         *
         * As a concrete example, suppose s is of length 5, t is of length 7,
         * and our threshold is 1. In this case we're going to walk a stripe of
         * length 3. The matrix would look like so:
         *
         * <pre>
         *    1 2 3 4 5
         * 1 |#|#| | | |
         * 2 |#|#|#| | |
         * 3 | |#|#|#| |
         * 4 | | |#|#|#|
         * 5 | | | |#|#|
         * 6 | | | | |#|
         * 7 | | | | | |
         * </pre>
         *
         * Note how the stripe leads off the table as there is no possible way
         * to turn a string of length 5 into one of length 7 in edit distance of
         * 1.
         *
         * Additionally, this implementation decreases memory usage by using two
         * single-dimensional arrays and swapping them back and forth instead of
         * allocating an entire n by m matrix. This requires a few minor
         * changes, such as immediately returning when it's detected that the
         * stripe has run off the matrix and initially filling the arrays with
         * large values so that entries we don't compute are ignored.
         *
         * See Algorithms on Strings, Trees and Sequences by Dan Gusfield for
         * some discussion.
         */
        // length of left
        int n = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "f5b869b7-f6c3-48d5-9897-17e448162fc6");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "80fea00a-0182-4328-b609-3d1b81b72de6");
        // if one string is empty, the edit distance is necessarily the length of the other
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "18e6186f-a6c0-476c-8e37-2d678520f69b");
            return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ca8faa0c-fde9-48db-995d-e45731ce2a42");
            return n <= threshold ? new LevenshteinResults(n, 0, n, 0) : new LevenshteinResults(-1, 0, 0, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "edfd26a1-5b68-4faa-94fe-75185dea41e8");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "7e9dff50-1a4c-4425-b257-b4670a11bf96");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "f37b1fca-4f3f-4f4a-99bf-cdaca764bb2d");
            // swap the two strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "bab56ace-754f-4067-b0b1-0c731b5acda2");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e6196ad4-7f3c-45a6-b835-4a7a0e570ad4");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ff85825d-dff4-4b05-aa4a-8b3b4f6625bf");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ca548146-9ef5-4675-9a5c-174c94275d2b");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e5bd0ab3-8321-4d94-a03a-9fd1c2f747a2");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "da152fc1-b907-4d00-83b7-dca166fdcb0c");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "24213063-cf5e-426e-a029-e9298216bdc6");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "0e6bdadb-66e0-4902-9e86-c08114962848");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "40199db3-59d9-4912-b029-0bcc02691024");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6417832b-5ed8-4080-9fdb-70fedf674ce4");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "5aaa4841-ca47-4696-a1b2-d77c1528029a");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "586dece7-2ce6-499a-90fc-d76c33d5b2ae");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "fd271193-d98b-4438-9e07-79a21d5e7e33");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d1aaf735-99da-41bd-9bc2-8c28eb7f0856");
        // fill in starting table values
        final int boundary = Math.min(n, threshold) + 1;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "806fa231-ede9-4d5f-b0c4-010b8cb938f8");
        for (int i = 0; i < boundary; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "1b62cb1c-cbd1-4e45-be50-68d31defaa31");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "473f0a06-736a-4be3-b6d1-49bfec019f5c");
        // these fills ensure that the value above the rightmost entry of our
        // stripe will be ignored in following loop iterations
        Arrays.fill(p, boundary, p.length, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "2c94da1d-b631-4ba3-ae4b-2c2a7a6d1b7e");
        Arrays.fill(d, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a711448d-352e-41f0-8110-91e10d23b366");
        // iterates through t
        for (int j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e1a425a9-26c8-4821-baa8-76e60f5b88b9");
            // jth character of right
            final char rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a06374c4-be11-4a25-905f-31bf42cb376e");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "7262c16a-5219-4cf0-8581-496f54863b30");
            // compute stripe indices, constrain to array size
            final int min = Math.max(1, j - threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "3207e74d-d96c-4f64-8bb0-43ab76045ce3");
            final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "70cd74d7-6a1c-4a4d-bb1e-be6e7a065a46");
            // the stripe may lead off of the table if s and t are of different sizes
            if (min > max) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a47625f4-d739-4782-892f-45b1227fa233");
                return new LevenshteinResults(-1, 0, 0, 0);
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d450eac3-ab4e-4ff3-aabd-6588076ba597");
            // ignore entry left of leftmost
            if (min > 1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ede7c8a1-1aa0-4906-9404-db85e0cb408d");
                d[min - 1] = Integer.MAX_VALUE;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ff9203c6-9659-41d0-9881-292c41e45e98");
            // iterates through [min, max] in s
            for (int i = min; i <= max; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "fb3e8878-27f0-4bcd-99d5-872a7c5a0fcc");
                if (left.charAt(i - 1) == rightJ) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "77c8b6b3-1bee-42ee-bbb6-11961812de42");
                    // diagonally left and up
                    d[i] = p[i - 1];
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "98f4165f-1121-4234-b0b1-df5285b7a78f");
                    // 1 + minimum of cell to the left, to the top, diagonally left and up
                    d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
                }
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "51b92e7e-e224-486e-bbed-8249a498a93a");
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "9f351976-5880-4500-9c5f-522567697b8a");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "1f3ce434-4a3d-4ab9-86db-344cbe07cfe0");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "fedc3e36-fc51-4bd8-8930-aa21ac9f5acf");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "bd9461e2-8298-4238-8b02-17c7f884b1fd");
        // if p[n] is greater than the threshold, there's no guarantee on it being the correct distance
        if (p[n] <= threshold) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "30563862-f532-45bd-8cb2-8af2232c8d08");
            return findDetailedResults(left, right, matrix, swapped);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "dbb1f3af-7c89-484f-852c-a11c98425e04");
        return new LevenshteinResults(-1, 0, 0, 0);
    }

    /**
     * <p>Find the Levenshtein distance between two Strings.</p>
     *
     * <p>A higher score indicates a greater distance.</p>
     *
     * <p>The previous implementation of the Levenshtein distance algorithm
     * was from <a href="http://www.merriampark.com/ld.htm">http://www.merriampark.com/ld.htm</a></p>
     *
     * <p>Chas Emerick has written an implementation in Java, which avoids an OutOfMemoryError
     * which can occur when my Java implementation is used with very large strings.<br>
     * This implementation of the Levenshtein distance algorithm
     * is from <a href="http://www.merriampark.com/ldjava.htm">http://www.merriampark.com/ldjava.htm</a></p>
     *
     * <pre>
     * unlimitedCompare(null, *)             = IllegalArgumentException
     * unlimitedCompare(*, null)             = IllegalArgumentException
     * unlimitedCompare("","")               = 0
     * unlimitedCompare("","a")              = 1
     * unlimitedCompare("aaapppp", "")       = 7
     * unlimitedCompare("frog", "fog")       = 1
     * unlimitedCompare("fly", "ant")        = 3
     * unlimitedCompare("elephant", "hippo") = 7
     * unlimitedCompare("hippo", "elephant") = 7
     * unlimitedCompare("hippo", "zzzzzzzz") = 8
     * unlimitedCompare("hello", "hallo")    = 1
     * </pre>
     *
     * @param left the first String, must not be null
     * @param right the second String, must not be null
     * @return result distance, or -1
     * @throws IllegalArgumentException if either String input {@code null}
     */
    private static LevenshteinResults unlimitedCompare(CharSequence left, CharSequence right) {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "1f66c5d7-507c-4bf6-a69e-4084ded2d86a");
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "334146c2-97c4-457f-a4d7-552fe980f804");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "13627e88-7940-41b2-b0e1-a1470a09d0c5");
        /*
           The difference between this impl. and the previous is that, rather
           than creating and retaining a matrix of size s.length() + 1 by t.length() + 1,
           we maintain two single-dimensional arrays of length s.length() + 1.  The first, d,
           is the 'current working' distance array that maintains the newest distance cost
           counts as we iterate through the characters of String s.  Each time we increment
           the index of String t we are comparing, d is copied to p, the second int[].  Doing so
           allows us to retain the previous cost counts as required by the algorithm (taking
           the minimum of the cost count to the left, up one, and diagonally up and to the left
           of the current cost count being calculated).  (Note that the arrays aren't really
           copied anymore, just switched...this is clearly much better than cloning an array
           or doing a System.arraycopy() each time  through the outer loop.)

           Effectively, the difference between the two implementations is this one does not
           cause an out of memory condition when calculating the LD over two very large strings.
         */
        // length of left
        int n = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "c429d41a-2e8d-4f26-9c6a-e2cf24c5b4e1");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ff45fa03-2231-45f9-b188-d1c5b3d5b0cc");
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e80124d2-020e-41d8-aee1-52da3d219259");
            return new LevenshteinResults(m, m, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "9fa7c9c6-258b-4aa8-92be-34a43edee284");
            return new LevenshteinResults(n, 0, n, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "524077e4-55e3-4166-ad4f-98252ea4061b");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "3bde35f7-dd1f-4b35-9e4b-5b8de08b8f12");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ced4036d-d226-4aff-a44a-fccbc9014211");
            // swap the input strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a1f3046a-0dfc-4203-be1e-6a3b491ec059");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ace005f3-78c2-40fb-8a86-279870a53e0d");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d3d0719c-bab6-4c61-aba7-1edff6dc2ee9");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "99b5cc13-11cc-49a1-9322-9669db4f1dd6");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "69f634e5-25ca-46ae-b46b-a6b48673e1c2");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ace772a7-774f-4937-b869-c357aea305fd");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ff5512fa-895a-4d28-9dba-300a85e5356f");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "efe9f184-f98f-4af8-83d6-960c8af2f0d9");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e5206b07-21cb-4202-8be1-4a4ad3925674");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "40d3a71c-b687-4df3-8c48-47f3297a8eb2");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "1108e025-421b-412b-82e3-fd63dd7aae82");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "7ee5c697-50c4-41e3-b23f-d0adad89255f");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "80c11157-7d12-4e12-8c7a-43f579bed902");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a3c4b1b9-d669-46e0-a59f-8d80197db79c");
        // indexes into strings left and right
        // iterates through left
        int i;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "b1af27fa-bfcc-4772-8be2-0f352811532d");
        // iterates through right
        int j;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "de9b41c5-f0c3-4228-9b57-b012ef454196");
        // jth character of right
        char rightJ;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6bfbe212-e3e7-4f0a-a337-3bfa296a314d");
        // cost
        int cost;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a9386cfc-8bcc-4b2b-8367-49d439156241");
        for (i = 0; i <= n; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "2f28f5da-2bfb-458a-8e5e-3de8c5852b0f");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "4934d14f-1eb4-447a-a4ad-00483e1297bd");
        for (j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e3281d67-1fea-49de-9565-7309a7be6e88");
            rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6f11a194-27ce-414d-a568-f1b63783d6ff");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "00424500-7a71-4c77-8419-96a4018ebf6e");
            for (i = 1; i <= n; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "31c3a17c-961d-46a1-b603-90be0261fe3c");
                cost = left.charAt(i - 1) == rightJ ? 0 : 1;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "25b74af9-2fe1-47a4-901b-76179694a382");
                // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
                d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "269a74e1-1b1c-414c-99b9-68f09cd50fe6");
                // filling the matrix
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "7b5a75d5-3fd4-4088-b92b-727581640f37");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "1dbe7950-a012-4a1f-932d-08cd601e765c");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "f500310a-f557-4f11-ae2e-eb980f4cb94a");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "5600199e-318a-443f-885e-dd34fa875853");
        return findDetailedResults(left, right, matrix, swapped);
    }

    /**
     * Finds count for each of the three [insert, delete, substitute] operations
     * needed. This is based on the matrix formed based on the two character
     * sequence.
     *
     * @param left character sequence which need to be converted from
     * @param right character sequence which need to be converted to
     * @param matrix two dimensional array containing
     * @param swapped tells whether the value for left character sequence and right
     * character sequence were swapped to save memory
     * @return result object containing the count of insert, delete and substitute and total count needed
     */
    private static LevenshteinResults findDetailedResults(final CharSequence left, final CharSequence right, final int[][] matrix, final boolean swapped) {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "4fbb7cdd-39d4-4a34-9887-c301598cef7c");
        int delCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "92bea77e-5e43-46ef-b102-c9e5bc2735af");
        int addCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "3ec2316e-7349-4208-b311-638dd8e5cac0");
        int subCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "1c156238-6388-48f8-a5e5-2363b8342c7d");
        int rowIndex = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "317701b6-f234-471b-8bdd-61d74b37c8ad");
        int columnIndex = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "46d98b92-45bb-4c4e-8913-7cd32914e589");
        int dataAtLeft = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e26c6cad-840f-4d9f-bf89-7af2ff9d5cb2");
        int dataAtTop = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6a1055eb-bcb2-4a89-9c88-de514d1b3954");
        int dataAtDiagonal = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "342b9946-ce1b-4b1e-9918-7eeff47a4465");
        int data = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "9d743b51-dbf0-4c20-8ff2-a0ce2a0eace9");
        boolean deleted = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "77426db8-f12e-4637-afdf-aa9d85c459cc");
        boolean added = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "9f1bbb51-fc95-4eca-a385-4fe9fa150a9c");
        while (rowIndex >= 0 && columnIndex >= 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "9134ba84-a218-4307-bf6e-7d08ffe31092");
            if (columnIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6e984e6e-d41a-49bd-a4f6-8c40f1666d22");
                dataAtLeft = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "0742972f-03aa-4b2c-b195-3fe0ca572cae");
                dataAtLeft = matrix[rowIndex][columnIndex - 1];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d0b17b82-2528-4fa5-a7b2-0c614f1a16f0");
            if (rowIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "4489ff23-3e4d-41b5-9372-2e0746f09349");
                dataAtTop = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "baefedfa-8993-4b67-b7ae-0bbee0c5c66c");
                dataAtTop = matrix[rowIndex - 1][columnIndex];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "8d386947-61e0-483a-9b67-df0a053aef8a");
            if (rowIndex > 0 && columnIndex > 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "72395b06-79b1-47e0-8bf8-b4cd196908ce");
                dataAtDiagonal = matrix[rowIndex - 1][columnIndex - 1];
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a2381614-eaeb-4ab2-976a-fbe085844ed2");
                dataAtDiagonal = -1;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "03af24e4-7906-498a-b77e-457a6167cd7e");
            if (dataAtLeft == -1 && dataAtTop == -1 && dataAtDiagonal == -1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "a7a3bf79-d404-45ff-9c7c-286977c7400b");
                break;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "bfa4b5b8-59cb-4285-a3ce-6d8d5f5ef063");
            data = matrix[rowIndex][columnIndex];
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "fc519592-acd3-449b-a2a2-cff221bcd037");
            // in this case none of the counters will be incremented.
            if (columnIndex > 0 && rowIndex > 0 && left.charAt(columnIndex - 1) == right.charAt(rowIndex - 1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "eaf11e07-938d-4115-8d7e-8ca5d6a57318");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "12cda28b-b382-41fd-b9fe-51c6c4fe6d27");
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "5d81d301-a0e7-4308-8fbf-81dcb3195190");
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "33d2d652-1c3b-48e6-b9f2-b4a593a1d6ab");
            // handling insert and delete cases.
            deleted = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "0fe373f1-4f10-4619-9fce-f22c2cc47b40");
            added = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d59706e5-9293-4c40-8b6f-fc7d19d87d67");
            if (data - 1 == dataAtLeft && (data <= dataAtDiagonal && data <= dataAtTop) || (dataAtDiagonal == -1 && dataAtTop == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "b5ecf1ae-fbb9-4cdd-9f50-931641b7f81b");
                // NOPMD
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "e57c1c79-26ab-4e2d-be93-8eb9b95945b2");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d60a4d0f-b420-4b55-b588-328f0721ea82");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "bffb7bc4-49c8-4026-9c1e-9f8203bb63c2");
                    added = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "ace80003-4847-4e1b-878a-b86953286bef");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "67582db4-9229-4fc9-aab0-8b466cfc5386");
                    deleted = true;
                }
            } else if (data - 1 == dataAtTop && (data <= dataAtDiagonal && data <= dataAtLeft) || (dataAtDiagonal == -1 && dataAtLeft == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "9ed13b8b-c014-4429-992c-1f9d0e69ff6f");
                // NOPMD
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6ba2b224-a43a-42e2-bb81-ab0b45d626dd");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d17b4168-044d-4a02-8cdf-ebd282dcc565");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "3ff9ddba-e578-4b33-b56f-213e4ab4d6ed");
                    deleted = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "5cbb4cd3-ebd4-4c48-b29e-411b0e60c4af");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "6b915c46-c377-40b2-83d3-7486bde54163");
                    added = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "743dd6cb-5480-4f14-aabe-1eaecc1357c2");
            // substituted case
            if (!added && !deleted) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "17aac7eb-abe5-4ce5-9133-f2549a2bcbf6");
                subCount++;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "2670434f-65dd-4c34-9d0b-9d65e95ab041");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "31e183c1-e8ef-453d-9802-be4b012cb310");
                rowIndex--;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_1_10.coverage", "d8168670-372d-4f4c-b8b8-06089503fb85");
        return new LevenshteinResults(addCount + delCount + subCount, addCount, delCount, subCount);
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
