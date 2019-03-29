Array2DRowRealMatrix
~~~
createMatrix
~~~
copy
~~~
add
~~~
subtract
~~~
multiply
~~~
getData
~~~
getDataRef
~~~
setSubMatrix
~
MathUtils.checkNotNull(subMatrix);
~
final int nRows = subMatrix.length;
~
if (nRows == 0) {
    throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
}
~
if (nCols == 0) {
    throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
}
~
for (int i = 0; i < data.length; ++i) {
    if (subMatrix[i].length != nCols) {
        throw new DimensionMismatchException(subMatrix[i].length, nCols);
    }
    System.arraycopy(subMatrix[i], 0, data[i + row], column, nCols);
}
~~~
getEntry
~~~
setEntry
~~~
addToEntry
~
MatrixUtils.checkMatrixIndex(this, row, column);
~
data[row][column] += increment;
~~~
multiplyEntry
~
MatrixUtils.checkMatrixIndex(this, row, column);
~
data[row][column] *= factor;
~~~
getRowDimension
~~~
getColumnDimension
~~~
operate
~~~
preMultiply
~~~
getSubMatrix
~~~
walkInRowOrder
~
visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
~~~
walkInRowOrder
~
visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
~~~
walkInRowOrder
~
visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
~~~
walkInRowOrder
~
visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
~~~
walkInColumnOrder
~~~
walkInColumnOrder
~~~
walkInColumnOrder
~~~
walkInColumnOrder
~~~
copyOut
~~~
copyIn
~~~
getRow
~~~
setRow
