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
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a84ae16b-6e8b-4d53-afdf-94eecc2e5f5f");
        if (threshold != null) {
            writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "d3c74ae3-4def-42c7-ad57-662d2cbda8d8");
            return limitedCompare(left, right, threshold);
        }
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "babf8e26-230a-4ef0-8157-c2907ad09ff1");
        return unlimitedCompare(left, right);
    }

    /**
     * Gets the default instance.
     *
     * @return the default instace
     */
    public static LevenshteinDetailedDistance getDefaultInstance() {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a3f03931-41ea-43c7-98f5-f95a7264da23");
        return DEFAULT_INSTANCE;
    }

    /**
     * Gets the distance threshold.
     *
     * @return the distance threshold
     */
    public Integer getThreshold() {
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "43edfe23-f1d0-4ac6-b83c-c21c7c70227a");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "20859542-5811-4f07-935e-d4fb8c0ce2a1");
        // NOPMD
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "9114da78-9cdd-4a31-902c-2f05b17f1b3a");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "8053a83b-2a4f-4782-835f-37fdcf61cb28");
        if (threshold < 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "ef8a4455-af87-4bc5-83bb-aa48e35985b1");
            throw new IllegalArgumentException("Threshold must not be negative");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "594884cc-bb2d-423c-9fb3-98ecaf3e28aa");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "d1c730a3-1105-4a47-95b8-fc37e9798b37");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "3b0a7108-e8d9-4e52-9c3f-ce38daea97b1");
        // if one string is empty, the edit distance is necessarily the length of the other
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4d610a31-d59c-4c9b-a50d-453ab59794ed");
            return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a1314d77-3b7a-4a87-8718-07e50350d576");
            return n <= threshold ? new LevenshteinResults(n, 0, n, 0) : new LevenshteinResults(-1, 0, 0, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "ea8329fc-bf4f-4793-8086-ee89801f7f6e");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a00a8ff3-9579-4b16-b620-458f3dd14099");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "5373a41a-9210-45eb-8636-b0fbd3d556a9");
            // swap the two strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4cab0612-ad98-45b4-9bcc-f59eba25474b");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "8daa844e-fed5-4eb3-86c8-0425571fb4d5");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b3d4d3e4-972e-41c0-ad14-d0d842f80448");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "bdae39a3-3a72-464b-873e-fe2e78722734");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "2927fd52-a160-4a27-9c37-3f398397ad41");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "1139a8c4-3f8f-44e7-bd3f-504aac55706f");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "00d0c389-5b8d-4ca6-afe1-8c0af200f5a8");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "417742ad-130e-4c2d-ba7b-8a5044070cf9");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "343051be-3889-4c85-a2cf-7dd5f6a836a0");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "97c53adb-8e0f-42ee-8727-57bfd4520a7a");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "072fdf3c-298e-4917-a9e4-492f7394f6bf");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "6b66e77f-0985-4722-91a7-829ac13bd73c");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "aa498351-bff1-4a45-829b-9a77896706dc");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "943c6f7e-e2ee-4806-8eda-49e1a6936e39");
        // fill in starting table values
        final int boundary = Math.min(n, threshold) + 1;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "3e9d8315-42d5-4ec4-bb81-4a6625421da6");
        for (int i = 0; i < boundary; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "86d26779-e6a8-4b18-b9bf-c5911da1efb2");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a94f6754-9ef2-4b34-b7ef-cfbf524a89b8");
        // these fills ensure that the value above the rightmost entry of our
        // stripe will be ignored in following loop iterations
        Arrays.fill(p, boundary, p.length, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "92808d32-7b26-4a44-a73d-401e199eeb3c");
        Arrays.fill(d, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "1ccd8c2f-b49f-49a9-bbac-ce6e90ebe784");
        // iterates through t
        for (int j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "d30c6503-7ba3-4bc3-b605-a600d1e7dadf");
            // jth character of right
            final char rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "dbd0af3a-2fcb-462c-b132-966f236ef459");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "ea9492fb-591a-4079-bc9d-35043a7fb916");
            // compute stripe indices, constrain to array size
            final int min = Math.max(1, j - threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "eae11365-15b9-4a03-a81a-938aaf211c6e");
            final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "440b21f6-2309-4f93-9df9-13cf0578f02c");
            // the stripe may lead off of the table if s and t are of different sizes
            if (min > max) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "247f24ee-0f04-45b0-9008-4df5de890b60");
                return new LevenshteinResults(-1, 0, 0, 0);
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b7d2575b-e3fc-4322-ab41-619eb9260c0d");
            // ignore entry left of leftmost
            if (min > 1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "728399bf-e56f-4f47-8a5b-64b8c1bbede1");
                d[min - 1] = Integer.MAX_VALUE;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "eb0caf5c-1e39-44c2-9927-a90a2ea551e9");
            // iterates through [min, max] in s
            for (int i = min; i <= max; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "2a40d7be-0f42-4076-b140-ede53e3f0b2c");
                if (left.charAt(i - 1) == rightJ) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "9af935b2-8b99-463c-a497-95e28c3818fb");
                    // diagonally left and up
                    d[i] = p[i - 1];
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "19b12541-4b07-4fbd-a235-a3a5e1da2bf3");
                    // 1 + minimum of cell to the left, to the top, diagonally left and up
                    d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
                }
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "6dbf414d-84c8-46d5-bc15-d278dbb1de4e");
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "e53961c0-5146-48a1-9ba3-d30f4be34712");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "df3a780c-eb30-4cc0-8c87-c5eb1eb907db");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "cfbcc889-ece1-4c46-ad77-b9d4017def69");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "38513f57-f481-4f73-bebf-7e107b509b6d");
        // if p[n] is greater than the threshold, there's no guarantee on it being the correct distance
        if (p[n] <= threshold) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "635b7331-7aa8-4071-80f1-2be2faf99e14");
            return findDetailedResults(left, right, matrix, swapped);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "1a42f650-72bb-412a-b4c5-55bcfa8c0c7c");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "5f2eafea-daf4-4713-be08-80133bb3fa50");
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "ab6ba8b9-98fa-4824-aa10-0818bf1842b0");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "e931fc2a-59bd-43d8-95d2-2e9f1c97bc1a");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7f0ec9b0-21c6-4668-b013-b1480703e65d");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "f8ebe5b3-f5dd-481f-ba13-139bc8a6a649");
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "146dc639-0a01-4aab-ac8a-5ddb13a296f8");
            return new LevenshteinResults(m, m, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4e0155db-c35e-4ef5-9c56-7f884b2fc58d");
            return new LevenshteinResults(n, 0, n, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7d314e32-d564-4d75-90c4-8dfb385291ad");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a918fc46-15fa-4f70-842a-fcb43f7e4b15");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "e93ee5b7-f32c-44c6-a975-34bf62860161");
            // swap the input strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "665f385e-5991-4365-a132-d5312ac9635e");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c559bcdd-4d8f-401a-aff9-3b3fb488ef70");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c269a01d-0421-461b-b5ee-e416659dd8cb");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "f76745e6-81a3-4652-b562-801a585c1b97");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "9b397024-a0be-4d93-9f01-73d733473b32");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c9964263-dcc1-4abf-a31e-b076683756d5");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c8d7aeeb-e236-4413-ace8-ef455847a415");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "2928309b-afaf-4376-8338-e623b60b09af");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "856d83da-3394-48b1-a133-d4a4e089ba8a");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b6dff747-26ec-49f9-858f-2f813afe1623");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "5fffa34e-567b-4158-8e45-6c643205e993");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "5ffe3b7d-dd2f-48ad-a05d-49f494ae6ff0");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b13a0e5b-c07b-42c9-ab13-a321fb42f6c9");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "d5e8373f-9361-4ff2-9796-0a74affc72bc");
        // indexes into strings left and right
        // iterates through left
        int i;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "da150c34-4c9e-4cb3-90ba-a4a6c42e01ce");
        // iterates through right
        int j;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "ecd9f90a-f3c7-4353-b148-85048bbdaa08");
        // jth character of right
        char rightJ;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "3371f63c-a87b-45e5-ac6b-e8e9b958b59f");
        // cost
        int cost;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "908a1e70-5e96-4df4-9dbb-a475faf47835");
        for (i = 0; i <= n; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7269e2fc-ff7c-4470-b252-b834a612d7ad");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "cfeacb33-3a05-47f8-a4df-936c4dd042ab");
        for (j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4a5291d5-bcdf-4de6-ae0d-09c7f13847d3");
            rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "40b7dd96-ec5c-45e7-9d94-df483a25ec6f");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "68a66b5e-755d-40d7-9717-efdceb21d507");
            for (i = 1; i <= n; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "67e851e7-a391-4c3b-834a-7bdea1bcd498");
                cost = left.charAt(i - 1) == rightJ ? 0 : 1;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "e4bcd057-7490-4484-9876-0f770e039fb6");
                // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
                d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4258b552-3b1a-42c3-9ffd-d7ba68c284b4");
                // filling the matrix
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "f777f0df-62c1-4c0d-b90c-5f7cb4082524");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "77818cfc-0849-408d-b3d4-d15f9c7eea55");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c27b6c0e-f980-4a74-a410-3fd8a4bb93c8");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b48619ce-4546-488c-b92c-39e296f2432e");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "8345fb5b-7f2a-4e77-81d4-d2b225d9feb6");
        int delCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "6a3e8c7d-a656-4f96-a1a6-d07c489c5566");
        int addCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "a303b308-c2c8-4e90-825a-bf279432c6cc");
        int subCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "2bb457ae-6081-42ce-97ba-8ca9b79318a1");
        int rowIndex = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "9c46bcb2-11bd-46a7-bf9d-af709f075570");
        int columnIndex = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "96cedb50-846b-43fc-9f5d-922aa6dfba8e");
        int dataAtLeft = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "0e2700b0-dcb2-4e57-aff6-88b00203f3cc");
        int dataAtTop = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b244a50a-883c-4201-9c69-c1af90d682b2");
        int dataAtDiagonal = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4da26360-c4cb-4937-8d6d-19ebe62dbc5c");
        int data = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b335af83-6614-4d55-bd64-09c14823a25e");
        boolean deleted = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "9325e319-e12e-4383-b071-9dfa511067b2");
        boolean added = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "84a3a905-7b8a-4f0e-b45c-a2f158d89cc3");
        while (rowIndex >= 0 && columnIndex >= 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "489468d0-cfef-4a71-bd02-29dda4486f27");
            if (columnIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "544c5278-d663-4273-a177-16121261918c");
                dataAtLeft = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7c878bda-46d7-4493-8acd-43dfbb24c619");
                dataAtLeft = matrix[rowIndex][columnIndex - 1];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "f14643a6-cd4e-4f9e-9ebc-a85b7123e088");
            if (rowIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "cccbf4ac-d0ae-4d84-99db-42f5b06d8902");
                dataAtTop = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "aadaa2a3-c1d1-4596-9d7a-a56d4a1b31eb");
                dataAtTop = matrix[rowIndex - 1][columnIndex];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c315f8fd-a0ad-420e-8e2a-e23ea727b0c0");
            if (rowIndex > 0 && columnIndex > 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "fc969647-5e52-4718-a274-99b85295b5da");
                dataAtDiagonal = matrix[rowIndex - 1][columnIndex - 1];
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "00193d98-95e7-48e1-b16c-7e5f949624ff");
                dataAtDiagonal = -1;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "e7f80ea2-eeb3-4b21-b68f-7a0295aca91c");
            if (dataAtLeft == -1 && dataAtTop == -1 && dataAtDiagonal == -1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7cef1814-a409-4436-86b3-967ba94c01ca");
                break;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4ff2699e-9ff9-491d-923b-37783a3d7417");
            data = matrix[rowIndex][columnIndex];
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "c9cfdebf-d62b-4f89-a223-e24985c557ed");
            // in this case none of the counters will be incremented.
            if (columnIndex > 0 && rowIndex > 0 && left.charAt(columnIndex - 1) == right.charAt(rowIndex - 1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "60cf4031-6133-4380-a917-1d9fc1c527fb");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "d63589ff-6417-48d7-bf2b-d870d38b33cb");
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "2945f7cc-d99d-45b4-8572-af0303dc3a93");
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b8f4cee0-89cc-455f-90c4-e0ba868efb3e");
            // handling insert and delete cases.
            deleted = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "2c8a83ab-fe27-4f41-8862-6b2248ee4778");
            added = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "994b58a6-9259-4a03-a7fc-a8b3fcc9e319");
            if (data - 1 == dataAtLeft && (data <= dataAtDiagonal && data <= dataAtTop) || (dataAtDiagonal == -1 && dataAtTop == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "b271912e-8af8-4820-a98f-66c3d0238d84");
                // NOPMD
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "1a1a0c74-ea7c-48da-96a8-3ba0f3af31f5");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7f6c16d7-a2d4-47c9-bfe3-dc5850ec2c59");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "1315bf8e-0c78-4618-a03c-61f421e6f9d9");
                    added = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "bed34b06-3089-4c58-b541-7cd2599b5ff5");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "7771d7d6-1334-4722-9f3a-561e2151cb80");
                    deleted = true;
                }
            } else if (data - 1 == dataAtTop && (data <= dataAtDiagonal && data <= dataAtLeft) || (dataAtDiagonal == -1 && dataAtLeft == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "5e67402e-b278-4bd1-9f62-fe390a5ed0df");
                // NOPMD
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "4d5885b8-775b-49a1-ad8b-73e6a48df48b");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "89d746bc-c00c-47f6-a509-974210f8b048");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "97af4416-db77-41f3-86ff-8da2b4cda1ae");
                    deleted = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "76b1fe45-8aa9-4846-9339-66298125db4e");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "1199284f-b1e8-4d32-9bc7-ee476580869d");
                    added = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "6f5d4cf5-2a41-4e2d-bd9e-a5b369f11e3b");
            // substituted case
            if (!added && !deleted) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "32b7ff4b-e558-48ed-b611-b393529e4dde");
                subCount++;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "327ba758-0377-476f-bae5-d75f8b5449af");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "9a2384cd-5b60-44b0-b73f-ad645a88bad4");
                rowIndex--;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_4_10.coverage", "008b7280-5eb9-469e-b711-aa5ce3d8160c");
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
