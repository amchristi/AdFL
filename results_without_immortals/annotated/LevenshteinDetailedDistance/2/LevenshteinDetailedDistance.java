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
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3ca92acf-83c9-4400-b7cf-32cabbabcdee");
        if (threshold != null) {
            writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "6b93f4a0-0735-4e83-93fd-5471d75cdc64");
            return limitedCompare(left, right, threshold);
        }
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "746b919a-8fbe-47cc-af37-31b01abcde6f");
        return unlimitedCompare(left, right);
    }

    /**
     * Gets the default instance.
     *
     * @return the default instace
     */
    public static LevenshteinDetailedDistance getDefaultInstance() {
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "2ff7ee6a-943e-42e1-b350-65f3c14cb47a");
        return DEFAULT_INSTANCE;
    }

    /**
     * Gets the distance threshold.
     *
     * @return the distance threshold
     */
    public Integer getThreshold() {
        writeline("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "904112ba-b13e-472e-ab4e-796b2e1699c2");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "674c5adc-7585-488d-825d-fef56719d970");
        // NOPMD
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "ef391945-4714-4fde-892e-f5bc6482d6ca");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "180c34e7-e09b-43ab-a620-d8fcaecf54ab");
        if (threshold < 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "f3bb0cf7-0f5e-4fe9-8bda-43f8ba185669");
            throw new IllegalArgumentException("Threshold must not be negative");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "7d2bc899-83fb-4aed-adb5-4f6d4945c08b");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "c4ec60cc-87e6-4603-b692-5240594fa556");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "d1cdbd10-5ff2-43a2-831f-f5ecde44f924");
        // if one string is empty, the edit distance is necessarily the length of the other
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "15d27787-036d-46a0-8ee7-92d872343d48");
            return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "4db3386d-f584-4ec2-af7f-b0e7c0cd04cf");
            return n <= threshold ? new LevenshteinResults(n, 0, n, 0) : new LevenshteinResults(-1, 0, 0, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "27b74a42-0fa6-4952-b378-5300f043d597");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "9a80c777-dee9-4577-bd3e-fc0068cff725");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "7d232e15-07b5-4b03-abea-506d5d7ea4ed");
            // swap the two strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "23dfadb7-6fe6-453c-a878-dc0a53253157");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "d29ae562-15f6-4ec8-b5a7-b228fcb1f1de");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5f2ad35e-fd92-482b-8a25-2239bbd40a43");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "fe07ce1c-af66-419f-aedd-9fde1b3e0472");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "59955fb1-1e1f-4df1-a2ed-a781ab59444b");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "88948bd0-7807-40ab-81ed-ecb389fe187a");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "515cf11e-31d8-4728-b731-99b130f6d9bc");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "75feec5e-1e5a-46d0-9f1a-bc6c9920e98a");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "1db66c24-18ce-4a94-b096-acd8bf7b5454");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "6dc3d4c3-ddf0-431f-8e29-4bb41c253fe2");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "51b13c30-0ff9-4e8e-b27b-ab60bfc770b9");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "1cebd917-573b-4376-8d10-a3e82bdd4662");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "597b83ae-b39e-4215-ae3d-806f96b3c6d9");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "9391f91c-41c2-40c0-bf51-1a0d2bf633a7");
        // fill in starting table values
        final int boundary = Math.min(n, threshold) + 1;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "9a8361a0-d411-4934-93a9-3fd6ba9cb457");
        for (int i = 0; i < boundary; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "8d94543b-def3-415a-87fc-0f2991c00d91");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "1f3af452-4c3d-46ea-9822-0e5df1a3fb61");
        // these fills ensure that the value above the rightmost entry of our
        // stripe will be ignored in following loop iterations
        Arrays.fill(p, boundary, p.length, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "0663b329-4d15-4b4c-af91-5e25e37fc25c");
        Arrays.fill(d, Integer.MAX_VALUE);
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "d0eff816-0607-4849-b167-c2eb991725e3");
        // iterates through t
        for (int j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "1f74ec4f-1e9b-4d06-b4f1-ecbbace8a1ed");
            // jth character of right
            final char rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "d1104a77-d683-45ed-ad55-4b8afd498082");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "8af05f8f-2189-4b86-8990-d5f8e3c47784");
            // compute stripe indices, constrain to array size
            final int min = Math.max(1, j - threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "16ca0028-026b-483e-adf1-8180d171e866");
            final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "08facef5-1cae-4047-a561-e8711e7be783");
            // the stripe may lead off of the table if s and t are of different sizes
            if (min > max) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "563b1696-bce8-4def-9216-c6539fb79ce4");
                return new LevenshteinResults(-1, 0, 0, 0);
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5a041772-8a04-43b0-8a08-8684728e3787");
            // ignore entry left of leftmost
            if (min > 1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "878144a7-aba2-4bf8-9de5-cfcbbafb84ea");
                d[min - 1] = Integer.MAX_VALUE;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "70a97002-8383-4d85-8243-5c44011fe56c");
            // iterates through [min, max] in s
            for (int i = min; i <= max; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "bf310e74-25b7-4929-94ec-2d55ad956ac1");
                if (left.charAt(i - 1) == rightJ) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "279584ee-f77d-413f-8169-49c8a5f85f02");
                    // diagonally left and up
                    d[i] = p[i - 1];
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "259cfd01-8f8a-48b2-994f-65862f57bbd2");
                    // 1 + minimum of cell to the left, to the top, diagonally left and up
                    d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
                }
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "1d169f04-62d0-4cc7-b0e1-e491da00bc40");
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "6140e43d-2c11-4fae-beb2-5595c3c49403");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "9c531df5-7755-4f16-afed-7609c4e100ed");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "c1af4bed-6dd1-4f92-8570-59ecd0e0d451");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "8e46441a-c983-4564-8e38-ca14b3ea7e6c");
        // if p[n] is greater than the threshold, there's no guarantee on it being the correct distance
        if (p[n] <= threshold) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "cfdabca8-0fbe-4bfd-90ff-de3ddcdd5a06");
            return findDetailedResults(left, right, matrix, swapped);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5631d98d-9a6c-4e41-9bff-eec1f8cdb57b");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5fc97d04-04a0-4e9e-bf92-59f92ad44d18");
        if (left == null || right == null) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "e0607a69-f593-43d3-a929-de7a6e1afb00");
            throw new IllegalArgumentException("Strings must not be null");
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "33cae5ed-8979-4352-923c-34f62f3d539c");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "8b7ff54e-829b-4091-b41a-9c8ed5eb32d2");
        // length of right
        int m = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "c6760c3d-cc25-4f92-a700-662ed9ab81bf");
        if (n == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "595ba5ab-e6a5-48e4-b32b-f0275288818b");
            return new LevenshteinResults(m, m, 0, 0);
        } else if (m == 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "73585312-5a54-49e2-9e3b-4d8353b347c8");
            return new LevenshteinResults(n, 0, n, 0);
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5e1fe75d-b17b-479e-9b45-7af5db103bab");
        boolean swapped = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "545f74e3-e765-4b65-a647-ddea85f895cc");
        if (n > m) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "575ecaa7-0c15-4b35-9804-6b680c7b9924");
            // swap the input strings to consume less memory
            final CharSequence tmp = left;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "e6473a80-9ad3-480a-8fef-9e8193900797");
            left = right;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "f3f63e48-56fb-412a-8e81-98c7c6ad9c5d");
            right = tmp;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "c43916cc-b15b-44bf-b11e-982dc8e204d9");
            n = m;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "503c2047-ecea-41c4-9908-e5de88382f2c");
            m = right.length();
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "db77d24a-3208-4362-a4c4-cd2204cc24dc");
            swapped = true;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "2c73e5c2-b2b7-428a-a181-87b9e0985a15");
        // 'previous' cost array, horizontally
        int[] p = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "41dec3c0-6d50-4446-84bc-cf939e602812");
        // cost array, horizontally
        int[] d = new int[n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "89e6076e-80b2-47a4-8bfa-d0b3c60634c8");
        // placeholder to assist in swapping p and d
        int[] tempD;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "a3dd7887-0878-4d08-9a12-3e35eadc0232");
        final int[][] matrix = new int[m + 1][n + 1];
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "d98ec80c-8503-4888-9eae-56bf11d921ae");
        // filling the first row and first column values in the matrix
        for (int index = 0; index <= n; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "61119548-9b82-4b7e-a315-b7736a2eaa93");
            matrix[0][index] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "bb033c07-5603-452b-8945-14ba7f5a5e5e");
        for (int index = 0; index <= m; index++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "bb622e89-57a0-4167-ae67-3130dfbd9dd5");
            matrix[index][0] = index;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "2c67cc98-dc3b-421a-98eb-68910860a6e0");
        // indexes into strings left and right
        // iterates through left
        int i;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "79b123c0-61d3-4a2d-92a5-8f943622454d");
        // iterates through right
        int j;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5d34e0d9-57f2-42da-94ba-b57a15f532b9");
        // jth character of right
        char rightJ;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5d5d7347-ad52-4bf1-9bbc-d90bdefeee84");
        // cost
        int cost;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "f91042e5-a7cb-4076-9c62-3224e3243dbf");
        for (i = 0; i <= n; i++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "4b69f7e2-22fe-42a6-a918-f4c94d7df733");
            p[i] = i;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "0d997712-b6a7-461c-b9ba-b3ca3372849d");
        for (j = 1; j <= m; j++) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "622ada5b-ee0e-4654-b998-c72cfe957edc");
            rightJ = right.charAt(j - 1);
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3302f904-dc12-49a4-9df6-38d9a9820491");
            d[0] = j;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "e01fdef2-7f64-4c82-942f-988226cb783d");
            for (i = 1; i <= n; i++) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "e871e125-7047-4ba2-bfea-d4520e0694d6");
                cost = left.charAt(i - 1) == rightJ ? 0 : 1;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "cc5aac21-4563-4ea1-80d5-94418e86bfae");
                // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
                d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "c3b5deab-6c22-44aa-9552-db9bbbfa2148");
                // filling the matrix
                matrix[j][i] = d[i];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "c60bd2f5-d820-4c93-917e-cf4b38a6f550");
            // copy current distance counts to 'previous row' distance counts
            tempD = p;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "d71644e1-24fe-458c-a297-650b7cee00e2");
            p = d;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "7824a77a-3157-4712-a96f-b4e2744451f8");
            d = tempD;
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "7da47e13-420d-4d2c-97ed-54257c955323");
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
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "abaa135c-c6dc-42fd-b15b-7878ecf383c5");
        int delCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "22282b38-6db6-440c-b4f6-f7e05db35954");
        int addCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3fd7ec95-16af-4456-9ca8-4e90e5df65b5");
        int subCount = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "69b828f2-ab94-41f5-8157-639ef90f183d");
        int rowIndex = right.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "2bd7ba69-5ce1-4bde-9081-3855d5985baf");
        int columnIndex = left.length();
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "093dc73c-9bd4-457c-b605-59b98f45bfc2");
        int dataAtLeft = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "7f3f18be-999c-4643-a178-4d859ac220e3");
        int dataAtTop = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "fd7f5151-2314-41ca-958b-d84562b1fc1c");
        int dataAtDiagonal = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "da973453-b12a-46d5-906c-8025fc5d25fc");
        int data = 0;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "a34ad818-eba7-48ce-9203-c212364e5653");
        boolean deleted = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "ded2a795-6e62-4aee-9e04-6f8365a6ec30");
        boolean added = false;
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "297c85dd-b7e9-430a-bdde-8fb70975607b");
        while (rowIndex >= 0 && columnIndex >= 0) {
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "a8cace97-1b1c-414c-8b62-e71565dac113");
            if (columnIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3da7d6ba-78e4-4bfa-a73a-a573f6b9c5b4");
                dataAtLeft = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "b577270b-6541-47c6-801c-462ee8f3c6e1");
                dataAtLeft = matrix[rowIndex][columnIndex - 1];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "8a891e23-4c7d-444c-a8ea-811a23cb6704");
            if (rowIndex == 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "646b7cf3-38eb-4883-8f32-25d2ea995e06");
                dataAtTop = -1;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "eeeab77b-c9bf-465f-8764-95c0ad9c1ca6");
                dataAtTop = matrix[rowIndex - 1][columnIndex];
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "84842690-518f-4317-aa93-66858bdf741f");
            if (rowIndex > 0 && columnIndex > 0) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "fc84d117-bbf7-468a-920c-d3bc1af65f28");
                dataAtDiagonal = matrix[rowIndex - 1][columnIndex - 1];
            } else {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "435edc94-feb1-478b-aa8a-824b8469fa88");
                dataAtDiagonal = -1;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3afb946f-3f06-4d07-b3f3-fa0e1a7002f1");
            if (dataAtLeft == -1 && dataAtTop == -1 && dataAtDiagonal == -1) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "4291551f-87f8-4657-9bfd-8c27ab6e63ba");
                break;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "2befcac7-ed73-4161-859c-f1a16a2e0402");
            data = matrix[rowIndex][columnIndex];
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "73fc08ee-4543-4267-a8b6-a89bd09b539c");
            // in this case none of the counters will be incremented.
            if (columnIndex > 0 && rowIndex > 0 && left.charAt(columnIndex - 1) == right.charAt(rowIndex - 1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "e5298696-4a63-491a-a0d3-246db6b16e8c");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "12d7ff8d-4b29-4107-a769-efd9cb717300");
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "bcb433f2-c8c0-4745-a489-d92f41893b21");
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "0e2f8984-6579-4121-9255-094133c17003");
            // handling insert and delete cases.
            deleted = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3d2190db-5c79-4704-a072-5d9f3458668c");
            added = false;
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "e6fc3302-b7f6-43cc-8ff9-b4314efbcda8");
            if (data - 1 == dataAtLeft && (data <= dataAtDiagonal && data <= dataAtTop) || (dataAtDiagonal == -1 && dataAtTop == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "fba990b9-323f-485e-a764-6d9f7121e65e");
                // NOPMD
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "178a729f-b2a8-4c4f-a090-be5b616fdc61");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "7469d110-8792-43de-8fe5-1c4dc31a20fa");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "6aa8c159-1687-4fca-a868-65776ee4164d");
                    added = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "03e15b06-2fff-42ce-a431-19995923b17f");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "17e37b7a-dc75-4c78-89be-5b302755e90f");
                    deleted = true;
                }
            } else if (data - 1 == dataAtTop && (data <= dataAtDiagonal && data <= dataAtLeft) || (dataAtDiagonal == -1 && dataAtLeft == -1)) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "90896217-1c66-41a6-a513-653d09870b15");
                // NOPMD
                rowIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "3d23f375-7b01-4f63-a3f6-5bf96f97e7db");
                if (swapped) {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "b8d03554-f721-4b9d-bc47-9d7cb886931c");
                    delCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "87b3914a-d6b6-493d-aeb7-f4bff7b2b9c2");
                    deleted = true;
                } else {
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "9ddddf25-1ac9-4b7e-a26d-ded6c4ed14db");
                    addCount++;
                    writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "a69866a0-f1fe-4625-8709-2a4983de5d82");
                    added = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "a251a8ec-e1dd-43be-b915-a5debdfe02da");
            // substituted case
            if (!added && !deleted) {
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "5f0f744f-6992-4d71-9134-313354104042");
                subCount++;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "bfea589f-3c01-4db7-9fcf-32f468a1c19f");
                columnIndex--;
                writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "1939c53c-b5b6-4a43-a546-49b59a952ca5");
                rowIndex--;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/LevenshteinDetailedDistance/LevenshteinDetailedDistance_2_10.coverage", "cff56a48-6dda-46bf-9395-3e60dbc258fb");
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
