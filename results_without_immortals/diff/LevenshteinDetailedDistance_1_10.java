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
~~~
unlimitedCompare
~~~
findDetailedResults
