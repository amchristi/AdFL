LevenshteinDetailedDistance
~~~
apply
~~~
getDefaultInstance
~~~
getThreshold
~~~
limitedCompare
~
d[min - 1] = Integer.MAX_VALUE;
~
throw new IllegalArgumentException("Strings must not be null");
~
throw new IllegalArgumentException("Threshold must not be negative");
~
return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
~
// swap the two strings to consume less memory
final CharSequence tmp = left;
~
left = right;
~
right = tmp;
~
n = m;
~
m = right.length();
~
swapped = true;
~
matrix[0][index] = index;
~
matrix[index][0] = index;
~
p[i] = i;
~
// jth character of right
final char rightJ = right.charAt(j - 1);
~
d[0] = j;
~
// compute stripe indices, constrain to array size
final int min = Math.max(1, j - threshold);
~
final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
~
// the stripe may lead off of the table if s and t are of different sizes
if (min > max) {
    return new LevenshteinResults(-1, 0, 0, 0);
}
~
// ignore entry left of leftmost
if (min > 1) {
    d[min - 1] = Integer.MAX_VALUE;
}
~
// iterates through [min, max] in s
for (int i = min; i <= max; i++) {
    if (left.charAt(i - 1) == rightJ) {
        // diagonally left and up
        d[i] = p[i - 1];
    } else {
        // 1 + minimum of cell to the left, to the top, diagonally left and up
        d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
    }
    matrix[j][i] = d[i];
}
~
// copy current distance counts to 'previous row' distance counts
tempD = p;
~
p = d;
~
d = tempD;
~
return findDetailedResults(left, right, matrix, swapped);
~
// NOPMD
if (left == null || right == null) {
    throw new IllegalArgumentException("Strings must not be null");
}
~
if (threshold < 0) {
    throw new IllegalArgumentException("Threshold must not be negative");
}
~
// length of left
int n = left.length();
~
// length of right
int m = right.length();
~
// if one string is empty, the edit distance is necessarily the length of the other
if (n == 0) {
    return m <= threshold ? new LevenshteinResults(m, m, 0, 0) : new LevenshteinResults(-1, 0, 0, 0);
} else if (m == 0) {
    return n <= threshold ? new LevenshteinResults(n, 0, n, 0) : new LevenshteinResults(-1, 0, 0, 0);
}
~
boolean swapped = false;
~
if (n > m) {
    // swap the two strings to consume less memory
    final CharSequence tmp = left;
    left = right;
    right = tmp;
    n = m;
    m = right.length();
    swapped = true;
}
~
// 'previous' cost array, horizontally
int[] p = new int[n + 1];
~
// cost array, horizontally
int[] d = new int[n + 1];
~
// placeholder to assist in swapping p and d
int[] tempD;
~
final int[][] matrix = new int[m + 1][n + 1];
~
// filling the first row and first column values in the matrix
for (int index = 0; index <= n; index++) {
    matrix[0][index] = index;
}
~
for (int index = 0; index <= m; index++) {
    matrix[index][0] = index;
}
~
// fill in starting table values
final int boundary = Math.min(n, threshold) + 1;
~
for (int i = 0; i < boundary; i++) {
    p[i] = i;
}
~
// stripe will be ignored in following loop iterations
Arrays.fill(p, boundary, p.length, Integer.MAX_VALUE);
~
Arrays.fill(d, Integer.MAX_VALUE);
~
// iterates through t
for (int j = 1; j <= m; j++) {
    // jth character of right
    final char rightJ = right.charAt(j - 1);
    d[0] = j;
    // compute stripe indices, constrain to array size
    final int min = Math.max(1, j - threshold);
    final int max = j > Integer.MAX_VALUE - threshold ? n : Math.min(n, j + threshold);
    // the stripe may lead off of the table if s and t are of different sizes
    if (min > max) {
        return new LevenshteinResults(-1, 0, 0, 0);
    }
    // ignore entry left of leftmost
    if (min > 1) {
        d[min - 1] = Integer.MAX_VALUE;
    }
    // iterates through [min, max] in s
    for (int i = min; i <= max; i++) {
        if (left.charAt(i - 1) == rightJ) {
            // diagonally left and up
            d[i] = p[i - 1];
        } else {
            // 1 + minimum of cell to the left, to the top, diagonally left and up
            d[i] = 1 + Math.min(Math.min(d[i - 1], p[i]), p[i - 1]);
        }
        matrix[j][i] = d[i];
    }
    // copy current distance counts to 'previous row' distance counts
    tempD = p;
    p = d;
    d = tempD;
}
~
// if p[n] is greater than the threshold, there's no guarantee on it being the correct distance
if (p[n] <= threshold) {
    return findDetailedResults(left, right, matrix, swapped);
}
~
return new LevenshteinResults(-1, 0, 0, 0);
~~~
unlimitedCompare
~
return new LevenshteinResults(n, 0, n, 0);
~
// minimum of cell to the left+1, to the top+1, diagonally left and up +cost
d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
~
// filling the matrix
matrix[j][i] = d[i];
~
return new LevenshteinResults(m, m, 0, 0);
~
if (m == 0) {
    return new LevenshteinResults(n, 0, n, 0);
}
~
// swap the input strings to consume less memory
final CharSequence tmp = left;
~
left = right;
~
right = tmp;
~
n = m;
~
m = right.length();
~
swapped = true;
~
matrix[0][index] = index;
~
matrix[index][0] = index;
~
p[i] = i;
~
rightJ = right.charAt(j - 1);
~
d[0] = j;
~
for (i = 1; i <= n; i++) {
    cost = left.charAt(i - 1) == rightJ ? 0 : 1;
    // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
    d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
    // filling the matrix
    matrix[j][i] = d[i];
}
~
// copy current distance counts to 'previous row' distance counts
tempD = p;
~
p = d;
~
d = tempD;
~
// length of left
int n = left.length();
~
// length of right
int m = right.length();
~
if (n == 0) {
    return new LevenshteinResults(m, m, 0, 0);
} else if (m == 0) {
    return new LevenshteinResults(n, 0, n, 0);
}
~
boolean swapped = false;
~
if (n > m) {
    // swap the input strings to consume less memory
    final CharSequence tmp = left;
    left = right;
    right = tmp;
    n = m;
    m = right.length();
    swapped = true;
}
~
// 'previous' cost array, horizontally
int[] p = new int[n + 1];
~
// cost array, horizontally
int[] d = new int[n + 1];
~
// placeholder to assist in swapping p and d
int[] tempD;
~
final int[][] matrix = new int[m + 1][n + 1];
~
// filling the first row and first column values in the matrix
for (int index = 0; index <= n; index++) {
    matrix[0][index] = index;
}
~
for (int index = 0; index <= m; index++) {
    matrix[index][0] = index;
}
~
// iterates through left
int i;
~
// iterates through right
int j;
~
// jth character of right
char rightJ;
~
// cost
int cost;
~
for (i = 0; i <= n; i++) {
    p[i] = i;
}
~
for (j = 1; j <= m; j++) {
    rightJ = right.charAt(j - 1);
    d[0] = j;
    for (i = 1; i <= n; i++) {
        cost = left.charAt(i - 1) == rightJ ? 0 : 1;
        // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
        d[i] = Math.min(Math.min(d[i - 1] + 1, p[i] + 1), p[i - 1] + cost);
        // filling the matrix
        matrix[j][i] = d[i];
    }
    // copy current distance counts to 'previous row' distance counts
    tempD = p;
    p = d;
    d = tempD;
}
~
return findDetailedResults(left, right, matrix, swapped);
~~~
findDetailedResults
