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
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a8ab0a67-b5bd-4eac-8988-c1f879f038c8");
        if (threshold != null) {
            writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "1156cd50-bb03-4b15-9754-3372455566b1");
            return limitedCompare(left, right, threshold);
        }
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c99a2e72-bb9e-47e5-89fb-5a45b5fb081d");
        return unlimitedCompare(left, right);
    }

    /**
     * Gets the default instance.
     *
     * @return the default instace
     */
    public static LevenshteinDetailedDistance getDefaultInstance() {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "198c9652-aadf-43c1-bb6c-55e646ec1030");
        return DEFAULT_INSTANCE;
    }

    /**
     * Gets the distance threshold.
     *
     * @return the distance threshold
     */
    public Integer getThreshold() {
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f08b0b4b-6c30-45ab-81d8-692090495305");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "dc4863de-f417-4235-b1ce-28b62594e1e5");
        // NOPMD
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a7306f7f-d8ab-4eed-a16d-0548e112505f");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9c87cfb5-da70-491a-a414-e2fbb33d4121");
        if (threshold < 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "cb4d917b-3570-42b6-a64a-2da29d0cd840");
            throw new IllegalArgumentException("Threshold must not be negative");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c4ffa310-5d16-45a2-8ebb-72e4d36f713b");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "adb8d7f0-63c8-4db7-bedc-b5ce5f5b9b3e");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "29b62203-74a8-4c67-ad75-529178c6df5c");
        // if one string is empty, the edit distance is necessarily the length of the other
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b021739c-3363-4bd6-a3eb-9553ae3949f8");
            return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "862e2162-611d-4bda-a7e8-c2fa157d5df5");
            return n <= threshold ? new LevenshteinResults(n, 0, n, 0) : new LevenshteinResults(-1, 0, 0, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "44edad65-6de9-4819-bbed-38053c4e4d1c");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8891d83a-81da-440b-8cdc-f30aa809aec2");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "aba0be56-b74c-4055-94b1-8425f37d4244");
            // swap the two strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8c5a261b-e063-4b11-b767-932694eb1538");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "5d608760-347b-4cfc-bf4c-061fe18eff40");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "65f34daf-a11d-4547-80e8-73c9322fc99e");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "2b8aa595-4eb7-49e1-904a-07c7aee81768");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f46f3ac5-34fe-4b26-8304-25c91d2976cb");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "7aa82a73-c130-4fb0-99d2-7321bb83f721");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "e702e4a0-f24d-4631-b6d6-8625fce186cf");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9fe0c704-0eb9-4b37-a373-91852cce0025");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f9f48e6f-106e-438a-bd7f-369bfc0a1bda");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "5dfba735-c986-4d33-bc9d-3ac897ce1e04");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c04fe3c7-d7cd-4781-9da9-36f79f55f374");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "3c70f438-249c-4fb0-a84b-52ad54d26091");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8f9fa06f-1236-40cc-93da-80af990e96db");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "e3eb8ba8-e0f9-464a-b970-0a0fa2b4d1c1");
        // fill in starting table values
        final int boundary = Math.min(n, threshold) + 1;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f65b67f8-1279-4d96-b1be-1f0884ec932f");
        for (int i = 0; i < boundary; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "4374d59d-6e98-4bfb-a43f-d3ac9521bd6a");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "ee2a5bb8-c1bd-4dfc-82ba-3c2752b25eac");
        // these fills ensure that the value above the rightmost entry of our
        // stripe will be ignored in following loop iterations
        Arrays.fill(p, boundary, p.length, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a2cae871-a284-4cb6-b5e9-b29f4d2d8687");
        Arrays.fill(d, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "69bc74b1-f36c-4fee-b04b-b794e033ade5");
        // iterates through t
        for (int j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "90f3eeaf-b84b-493d-9cfc-1fd0a174564e");
            // jth character of right
            final char rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "51daaf34-4ff0-407d-a258-dc3b4c8261fd");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "2ecf4e19-8e05-4c0f-9270-61cacd5f7e58");
            // compute stripe indices, constrain to array size
            final int min = Math.max(1, j - threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8e7e546d-f8ab-4dd3-9ce3-fcc5fcbbad3c");
            final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "39d030ad-6b11-4f33-8e08-946ef7d0c478");
            // the stripe may lead off of the table if s and t are of different sizes
            if (min > max) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "6f2522c0-db71-4d48-bb46-0126063f1e4f");
                return new LevenshteinResults(-1, 0, 0, 0);
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b6f6e187-35c0-4f25-9e6c-34549d2a7b2f");
            // ignore entry left of leftmost
            if (min > 1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "6d0ec07d-47da-43fe-a55a-6d74c1f1f7e5");
                d[min - 1] = Integer.MAX_VALUE;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "90024013-9b55-4fc5-89d6-98a1dc51220a");
            // iterates through [min, max] in s
            for (int i = min; i <= max; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c54b482d-de6b-41ee-8570-0ecc488ccfe2");
                if (left.charAt(i - 1) == rightJ) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "47eb013b-f879-4021-97fe-4473b1df4964");
                    // diagonally left and up
                    d[i] = p[i - 1];
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c3ce7ce0-e2d2-4619-878e-5a1b2238e602");
                    // 1 + minimum of cell to the left, to the top, diagonally left and up
                    d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
                }
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "0ff5d44e-67d0-4dfc-b023-8012f584fd27");
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b769de00-cf73-4408-ba85-3cb868fc5fbb");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "16247ef3-a219-427f-9b45-6e7fab1ac40b");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "22a05809-ca6d-4a8b-bcf3-8335edf71882");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "3d1d575c-4ef1-4d93-8833-01f8d06e0560");
        // if p[n] is greater than the threshold, there's no guarantee on it being the correct distance
        if (p[n] <= threshold) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "e0bcafb0-dabb-4b5a-a449-44c69fe659cd");
            return findDetailedResults(left, right, matrix, swapped);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9a486d5e-f3ae-4e88-8794-71cb660a1e39");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "94a6f6a4-a725-42ea-b2fa-3965b1e4955e");
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "63aec662-ef3a-450b-8910-3078b2637334");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "20d0f5e7-ff27-45c5-aa5e-c893ee557f8b");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "691cb727-42c7-4e29-8dd5-757170f3150b");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "ba6a1aba-1faa-46fe-83be-052055f92c0d");
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "71d28468-e829-4353-a851-8a47af5ee19a");
            return new LevenshteinResults(m, m, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "65dcc315-244f-4ebb-8186-5bca84ea43ee");
            return new LevenshteinResults(n, 0, n, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "1b3b9746-54f4-4786-9413-078181c482cf");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "1717050f-45ed-4425-a1c3-83d761029efa");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8a861898-4c38-4b8a-9f0b-34f069844fc9");
            // swap the input strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "e61ceb0d-a394-40d8-917c-5e313250bbed");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b87975b3-af42-408f-88bc-f0d7b51394c2");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9d7a5881-90bd-45bc-a6d8-c886d473e275");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "d6271bf9-487e-466c-bd8c-091a002692dd");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9a5d8fcf-c0c7-4fb1-99f7-f22c74919921");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9ff389ed-1e39-425c-b184-e13658c03877");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "3b92643c-578e-4ebe-9de3-3c2a7cfec8e9");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "550c0dfb-8355-46aa-b8e4-a58898736a63");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c86aa840-5f86-4a4f-b9bb-a602f2937bee");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "140a5497-c4f3-47a2-857b-e7b269ffff61");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b8820244-9a8b-41b0-a407-a38040e9f9b5");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "da44f320-9ec9-4b80-8285-7e80678d2bd8");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "bf074116-3b30-43e8-8839-f48b960d7e57");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "83047565-9a5e-4eb6-aa62-4f98192e6bb0");
        // indexes into strings left and right
        // iterates through left
        int i;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "ee8e4ca1-0a38-4574-a639-8eedda500223");
        // iterates through right
        int j;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "1b177453-d372-4e00-9c7e-4d3adad48d54");
        // jth character of right
        char rightJ;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f38bbd36-0131-47a7-b003-8953c8773881");
        // cost
        int cost;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "25c7b7e4-b21a-4d16-8e61-ff12dfbff6ac");
        for (i = 0; i <= n; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "e8eca00e-6ee0-4da3-a5f4-ca6c279f09fc");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "61942a48-5b5a-41a5-9d4d-6e45bb59732b");
        for (j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b423f08b-fb96-4ac1-bddb-fb1d63e4ee8c");
            rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "00e7bede-d237-45bd-8fed-b7a2a2992224");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a5def5f2-2ea8-4d94-bf0f-3b62e1d428c9");
            for (i = 1; i <= n; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "af036d6c-2e3e-4179-bdc1-75534b387219");
                cost = left.charAt(i - 1) == rightJ ? 0 : 1;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "fd08902b-f6ad-4eab-8afe-48d320920a1a");
                // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
                d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9bddad0c-6b50-4f60-b3f4-d4113b54a812");
                // filling the matrix
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9f300867-efa8-4b75-b3d6-e87bb848ec54");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "6d9cae2a-7c0f-4b6e-8e10-f3d9e86d3aef");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c5d06162-bda0-45c6-a676-be0f095c6c3e");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "bcd22a3e-47ba-4b93-99a9-a0592759a758");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9304a121-2ec8-4c58-94bd-9cdf8bea7930");
        int delCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "3271466f-797f-4d46-9dea-a13b446274da");
        int addCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "fe47d3d1-3ff1-4cbc-86b5-1c0fd2dde929");
        int subCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "dcba7132-ad36-4bdb-8870-dd0cdba96eef");
        int rowIndex = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "bc7c82da-dd17-44c7-8f5d-5f612ca1287b");
        int columnIndex = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b7b25cef-1ac6-4394-b9c0-7461ac226d22");
        int dataAtLeft = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "d5d54a2b-bf25-42ae-a648-5578d4c17341");
        int dataAtTop = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a1eff727-785d-490e-8d6d-28680cea90ba");
        int dataAtDiagonal = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "58692cc4-db73-4f80-8b1b-e04deb213d7f");
        int data = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "b6d9523d-3c2a-4719-a8ac-69b0b83da644");
        boolean deleted = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "fcdc9101-da17-4a61-b672-d1cff0323da4");
        boolean added = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "093ff133-e3dd-423c-a16d-365e4693b927");
        while (rowIndex >= 0 && columnIndex >= 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "5ac06663-5a06-4275-a1db-c0c9d75d8c68");
            if (columnIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c6bae43c-3216-4daf-8e8f-a4e40bc2341f");
                dataAtLeft = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "fb67eb2c-7a0e-4a6e-9b49-2a028569c816");
                dataAtLeft = matrix[rowIndex][columnIndex - 1];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "0d996d23-7d3f-4653-8480-9390dde41328");
            if (rowIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "086bd86f-b7c0-4830-a32d-6d4218ac41d7");
                dataAtTop = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "7009c4f8-b233-41d1-b499-f1b725199b4f");
                dataAtTop = matrix[rowIndex - 1][columnIndex];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a310e3cb-a4af-4154-8fa8-6a0806a938f2");
            if (rowIndex > 0 && columnIndex > 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "dd60f7fe-cf20-43f2-8187-e0be74150717");
                dataAtDiagonal = matrix[rowIndex - 1][columnIndex - 1];
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "1e0d159d-5d99-4e28-a74a-4a495d1ab471");
                dataAtDiagonal = -1;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "032eb948-5e4a-4cba-ace8-1046d5ecc305");
            if (dataAtLeft == -1 && dataAtTop == -1 && dataAtDiagonal == -1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f9a06c88-c388-448d-8f39-b618b9164ab8");
                break;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "c434ef61-0580-4d28-a007-deca130b0911");
            data = matrix[rowIndex][columnIndex];
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "589e3485-b6d3-4f62-9498-9cd68823fd5d");
            // in this case none of the counters will be incremented.
            if (columnIndex > 0 && rowIndex > 0 && left.charAt(columnIndex - 1) == right.charAt(rowIndex - 1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "3edb929b-6e2d-420a-8e1a-ea371a226c18");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "edf5f72d-9583-49d8-9555-541bccf9f2ed");
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f03c8404-8cd8-4662-a5e9-f20343cb5689");
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "002545ef-0a3b-40d3-97a3-795ff6d5fbc4");
            // handling insert and delete cases.
            deleted = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "cb18a536-fb10-40b1-b1ad-2d0afc815aaf");
            added = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "17aa5c03-015b-4559-a614-ee5ce99dd211");
            if (data - 1 == dataAtLeft && (data <= dataAtDiagonal && data <= dataAtTop) || (dataAtDiagonal == -1 && dataAtTop == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "f5a65b74-ac5e-4cc9-9212-784c564f853c");
                // NOPMD
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "7d685987-68cd-41c5-80a9-4e6488c28e9f");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8c21c642-0cfe-4708-b236-6909235b8f9d");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "47a80486-91f4-4004-9121-5c93607f43e3");
                    added = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "a331cb17-105c-41e1-afd0-801deef38725");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "af74cd7d-88cd-4d7d-90b3-920ed1e8e5bf");
                    deleted = true;
                }
            } else if (data - 1 == dataAtTop && (data <= dataAtDiagonal && data <= dataAtLeft) || (dataAtDiagonal == -1 && dataAtLeft == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "9cf6f442-9f4a-44f4-9894-d999f692d138");
                // NOPMD
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "15898b9e-41f3-48e2-865c-0b2b73bdfa4a");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "82f28de6-c876-486b-a0bf-696b459234a8");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "143711c8-72c3-4612-85d6-da0a4f36bceb");
                    deleted = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "fc561da1-73ab-468d-9ec0-37c455932a67");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "571615fd-d010-40c9-bc89-b738a4559ee4");
                    added = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "290dd3df-d068-45a7-87ad-541027a99d91");
            // substituted case
            if (!added && !deleted) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "8a8afffd-064a-4587-b07f-c06a58352193");
                subCount++;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "0d943117-b2e1-4972-9529-c07fabd460b9");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "2144197d-8ad8-4cc2-a210-9399cd0f8782");
                rowIndex--;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_3_10.coverage", "41498b47-d365-4d84-9e67-0401131907ea");
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
