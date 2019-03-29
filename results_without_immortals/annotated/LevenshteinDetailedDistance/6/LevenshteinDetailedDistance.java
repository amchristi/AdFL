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
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3e8ef9a5-fd06-427d-a185-17e7174605ad");
        if (threshold != null) {
            writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "72dd11ee-3287-4eb0-96c9-b8e68d2db9e0");
            return limitedCompare(left, right, threshold);
        }
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d38da0ed-2b03-43ec-acf9-14e761d0d380");
        return unlimitedCompare(left, right);
    }

    /**
     * Gets the default instance.
     *
     * @return the default instace
     */
    public static LevenshteinDetailedDistance getDefaultInstance() {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "dd014ee8-946e-44af-9b73-3d004361f90f");
        return DEFAULT_INSTANCE;
    }

    /**
     * Gets the distance threshold.
     *
     * @return the distance threshold
     */
    public Integer getThreshold() {
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "54570841-47f0-4534-af1c-f2c8368af10c");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "6af4fcc7-f707-4c79-a78a-796a12683ccb");
        // NOPMD
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2ffda78e-2bd7-4400-84b3-ce76a03a7c7e");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "b71e584f-8869-48d2-87e3-61869203bb4e");
        if (threshold < 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "33d1a7c4-1c79-4782-96a8-34cd2546f0e1");
            throw new IllegalArgumentException("Threshold must not be negative");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3be57aa8-2b33-42fb-8dad-23727a59e4af");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "1d5ddc36-2f79-4f53-b192-3250203d400b");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "26418e06-9e80-4bb9-9645-d58ca65fbfda");
        // if one string is empty, the edit distance is necessarily the length of the other
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "cb69ca2c-6c46-4c0d-b6e2-d9093016c108");
            return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "fd7494c5-a381-447b-9f2a-c930bcf197e9");
            return n <= threshold ? new LevenshteinResults(n, 0, n, 0) : new LevenshteinResults(-1, 0, 0, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "03ea5afd-93dd-49ac-a77b-ce9186e13d75");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "41763cfa-5adc-443a-a964-e48cb64d97e3");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "50049095-5528-4f03-a524-90d821fb6ca7");
            // swap the two strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "e75a8df8-2482-4b80-aa12-4231654b1354");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7bd92180-a1db-4f5b-8738-9e4e87401ec6");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d6073a99-1b70-4f15-a880-ed8b7b0e6f31");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "bf758cd0-cbdf-435b-9969-25cd063fa444");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "736dd527-02e0-4385-a456-17d48e771334");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "0bfe0212-c033-4737-9210-dbfd758346f3");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "616530d4-d71e-4807-bd6c-389ba69e89a5");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8d566da9-c31c-4fb4-8ef8-50957d04030d");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "07c8964a-454b-46bb-9c88-7168c6c837c7");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "0f8156b3-90cc-4f64-964c-b3f049f2f2b0");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2594eb41-f9df-41a4-b36d-bd6a662c882e");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "19bc0b36-29d9-4aa8-b4ed-b78fc6df1712");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3be071c1-9434-4169-a8d5-d9533e651902");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "c7344106-d23c-431c-aea8-89e1b4a76ba9");
        // fill in starting table values
        final int boundary = Math.min(n, threshold) + 1;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "4bd8cc81-ba43-4a8f-b95a-2c39f4d06e5d");
        for (int i = 0; i < boundary; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "afc7558d-44fe-43f6-9de0-5e00d4a34760");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "f5bdaf41-39e3-47e9-b95f-55df706923e4");
        // these fills ensure that the value above the rightmost entry of our
        // stripe will be ignored in following loop iterations
        Arrays.fill(p, boundary, p.length, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "4a96542b-a665-4b0e-9e6c-16eab92329ee");
        Arrays.fill(d, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "327b1e8a-6aad-4ec2-9dbd-1c3e040a2067");
        // iterates through t
        for (int j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "36c875c5-9c5e-4701-bbc2-665607460b87");
            // jth character of right
            final char rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7862c048-3d8f-414a-8981-26567b7fecb0");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2072f5b0-3374-4182-9f01-3bcc248ce35f");
            // compute stripe indices, constrain to array size
            final int min = Math.max(1, j - threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "fac5fb71-bff3-4749-9721-c84111910dad");
            final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "bf8a8e8a-f43b-4a81-aa7e-f9bdbd458e53");
            // the stripe may lead off of the table if s and t are of different sizes
            if (min > max) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "160dface-47f1-4e42-ae44-5162cd92d7e1");
                return new LevenshteinResults(-1, 0, 0, 0);
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2d88e03f-d59b-403a-a6bf-7372055bbbae");
            // ignore entry left of leftmost
            if (min > 1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "191f59a9-a795-4ad4-9c68-ae56e2665c30");
                d[min - 1] = Integer.MAX_VALUE;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "5d962815-708b-43d8-97a3-b2914f9a414f");
            // iterates through [min, max] in s
            for (int i = min; i <= max; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "6425025b-bd6e-4d5f-8321-3f7575e842ba");
                if (left.charAt(i - 1) == rightJ) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d43485c1-3381-461b-8a18-0726944db282");
                    // diagonally left and up
                    d[i] = p[i - 1];
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7dad688f-3681-4631-b281-e59d4d5a05d8");
                    // 1 + minimum of cell to the left, to the top, diagonally left and up
                    d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
                }
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "354624c8-d3d4-4d8b-8fe5-92c709433014");
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "a7d95fb1-0e9d-4865-b0ac-5a87ffb74b16");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "b05876ee-e383-40df-b310-f69e0a09f497");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "e58c73d2-184c-4962-88cb-fb4b75e22b96");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "9612bd43-af2c-4962-a779-001aecc25f61");
        // if p[n] is greater than the threshold, there's no guarantee on it being the correct distance
        if (p[n] <= threshold) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "4bcfa3aa-9df8-46f1-87da-420d826fdb53");
            return findDetailedResults(left, right, matrix, swapped);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "08d0dadd-43bb-4ae7-8c7d-1b38438239cc");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "f1865ac1-e821-4b25-9fa8-d69df8f1f55c");
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "43fe40b0-8539-451a-8125-04dd1db67d1f");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8e63eeab-a7ac-4ff0-87e3-f481dbba638f");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "0fae2c7e-4c24-49ab-a4ed-c4f1e5eb0595");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7e69bc6d-0158-4943-90fa-8f51f260248e");
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "ab5994ea-d436-445d-85f3-802c5dd4da9e");
            return new LevenshteinResults(m, m, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8bb3331b-54c2-4dac-8485-aac7bf87e0a5");
            return new LevenshteinResults(n, 0, n, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "9871b31c-0ab4-4ae8-9e0a-74e7c64d3d51");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "6bae86d0-9df9-4fb1-9f57-8a7ee9338e32");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "927b06db-6a0d-4055-ad16-e637774d3513");
            // swap the input strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "372de15c-4eb7-45d9-b107-7e53812f549f");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "634e4c0f-2dd3-46bc-bf88-929bd708319c");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "1bc1e976-d6a2-49b5-a159-2dae2d8a4694");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "aa9885f9-98b1-4965-a77a-3be4e1259dfb");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "4f0e2984-8263-437c-8b78-698e7814c35b");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "ed971664-dc36-4b00-934b-8812b6147311");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "82bcf33f-81bd-4d5f-99ef-9d332fb15e7d");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "e95777fe-10cc-4d9d-85b5-f59a811c1b1e");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "eb73978d-8ecd-4d72-9918-dbef34143149");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "251a5ca0-a271-46dc-a35c-5ea660199be0");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "b6eb65f2-4950-4b23-b598-79d4e012e660");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "390d06ee-c0c9-4185-8fcf-48fdf0dfbfe6");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8285ec24-9d7a-45f8-ab38-daa29c199138");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8db6b791-69fc-4930-a524-d0828661e227");
        // indexes into strings left and right
        // iterates through left
        int i;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3809d27f-21f9-421f-bebe-2d6c3fd85ef8");
        // iterates through right
        int j;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "325561ee-6c28-4952-ac01-ce48cbd8b768");
        // jth character of right
        char rightJ;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "4ed123ce-87a4-474a-b9f3-74195f4cdfb1");
        // cost
        int cost;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "5713a87f-24d2-4907-8782-b89eca583d8d");
        for (i = 0; i <= n; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "4a8d4414-8f5a-41c1-a265-9dd26eaf1fc5");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "a53c2e7e-47b3-48cc-b600-e5ed5628f708");
        for (j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8539260a-9db5-4fba-80e3-6fc378156c03");
            rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3a6a6393-a2ff-4a08-815e-782b750c7ea7");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3349680c-10e0-420e-bdc0-6ac3e3c1a377");
            for (i = 1; i <= n; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2aff72fd-4a0f-46ae-badb-d423aeb8dbc2");
                cost = left.charAt(i - 1) == rightJ ? 0 : 1;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "6f256b01-7c1b-403e-8f61-c6ba11a1493c");
                // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
                d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "60f11c70-c2b9-4c5e-92eb-f4b61bd92acb");
                // filling the matrix
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "04fdc081-c60e-4a13-aea8-07a58159e68d");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7c4e8891-0889-4c17-856c-996e4279cac9");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "e8d1ec04-f87e-461e-a1db-f57b92b51890");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "a5e1de38-c8a9-4bbb-89e3-5e2f07959506");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "78370802-c81a-4a19-9d09-d103699cd4e5");
        int delCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "0971f34d-092c-4ac4-aaa4-04b0a701f099");
        int addCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "99a3c756-5acb-4fa2-bb97-4ef275a42c48");
        int subCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7c25a026-92a7-4f8c-916b-69eb03f22272");
        int rowIndex = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2e7582c5-7002-4564-b7f3-fe87d67187fa");
        int columnIndex = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "13abe04a-74b4-4484-acb2-c204ef5f63b7");
        int dataAtLeft = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "90077785-81dc-46dc-acc4-56d98a911874");
        int dataAtTop = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "aba8722e-de4f-4aa4-bde1-494979eec612");
        int dataAtDiagonal = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "27a53697-986a-4684-8562-2a542681a677");
        int data = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "42f69c44-70bf-4253-a0dd-4b5cafcb33fd");
        boolean deleted = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "ce58b164-3b6f-4be0-b5f4-3e0977117abc");
        boolean added = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d6d6bb34-db12-46df-b307-bf41c0e69e56");
        while (rowIndex >= 0 && columnIndex >= 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "41285f91-17a5-4b70-a96f-36c0a5ffa96e");
            if (columnIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d38b29d2-5d30-4a61-917c-f55ae0c842e3");
                dataAtLeft = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2c5aac22-607c-4ebe-b7ee-d386c49a0c29");
                dataAtLeft = matrix[rowIndex][columnIndex - 1];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7448e603-6057-481b-987f-8b6b5fd6be25");
            if (rowIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d949593a-f8d5-43f2-9192-49bb0e6d0199");
                dataAtTop = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "fe6b09ce-418d-48ce-a12c-384b2dcf8069");
                dataAtTop = matrix[rowIndex - 1][columnIndex];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "65a9e264-6acc-43b8-bbe7-82c662433e8b");
            if (rowIndex > 0 && columnIndex > 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "43149b58-7c4a-463a-add0-c927e7430ba2");
                dataAtDiagonal = matrix[rowIndex - 1][columnIndex - 1];
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "1d64a072-8a45-4d6b-9e01-a14356bb4469");
                dataAtDiagonal = -1;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "cbe71383-e4ba-4ed5-b953-c9436c9d2d0f");
            if (dataAtLeft == -1 && dataAtTop == -1 && dataAtDiagonal == -1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "2d24545f-b2c2-485c-8729-71a51dc36227");
                break;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d1dc2b44-1197-4124-b317-dd7787c1c5a8");
            data = matrix[rowIndex][columnIndex];
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "1c6cf6ea-3bf2-4d02-bc07-1a2df5760b2f");
            // in this case none of the counters will be incremented.
            if (columnIndex > 0 && rowIndex > 0 && left.charAt(columnIndex - 1) == right.charAt(rowIndex - 1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "ed6f463a-eae9-46e1-8bec-cae95b75eb3f");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "ed1c3b7b-3ee1-4884-821f-2febef7ba6b5");
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "006ef5c9-cc6f-493e-b76f-46c2f5411ac5");
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d24a0a94-11c2-4cfa-a414-b02648c60ee5");
            // handling insert and delete cases.
            deleted = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "3fb4ce42-f1de-481c-a664-296c48838b5a");
            added = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "562e1805-2b23-4b97-ae32-b9e4c81004a8");
            if (data - 1 == dataAtLeft && (data <= dataAtDiagonal && data <= dataAtTop) || (dataAtDiagonal == -1 && dataAtTop == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "0f4f47fd-2715-4082-bbc9-45db44c192aa");
                // NOPMD
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "a1f1f8ad-1fb2-4b65-8e11-58fe3c836a71");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "c16a5582-8d63-444f-86e3-458633e271b1");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "75ec427e-24e7-486c-a7f2-1cee0da723d2");
                    added = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "43dcdc04-ec4e-4005-ad04-01b0ccc39108");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "cb714caf-4fbe-43fb-b651-5fada1424c35");
                    deleted = true;
                }
            } else if (data - 1 == dataAtTop && (data <= dataAtDiagonal && data <= dataAtLeft) || (dataAtDiagonal == -1 && dataAtLeft == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "7c4aed55-3d85-4a72-837e-6bee15bbe46c");
                // NOPMD
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "36d7e8b3-906b-4558-bf0b-320687246684");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "34e8283e-b2da-4073-ae5a-1f605ed02544");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "56172922-8273-4924-8515-05100b11171e");
                    deleted = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "568f2da1-164a-46c1-a07f-e04ddb36f111");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "8cdd5b72-f110-4a43-869e-bb18a827e42f");
                    added = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "14a8c039-c516-429b-b759-ffa8c489c65d");
            // substituted case
            if (!added && !deleted) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "d3e2a3f7-2c6e-446f-a49d-32b7cdc9e128");
                subCount++;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "1c8b263c-c13b-49da-9129-d1c3ae95e712");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "a02090c3-5bfa-4987-80a4-8400ce5b2087");
                rowIndex--;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_6_10.coverage", "739fb28a-0ef7-49e2-9d9c-ecc71c649b54");
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
