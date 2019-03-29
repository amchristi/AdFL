package org.apache.commons.math4.linear;

import java.io.Serializable;
import org.apache.commons.math4.exception.DimensionMismatchException;
import org.apache.commons.math4.exception.MathIllegalStateException;
import org.apache.commons.math4.exception.NoDataException;
import org.apache.commons.math4.exception.NotStrictlyPositiveException;
import org.apache.commons.math4.exception.NullArgumentException;
import org.apache.commons.math4.exception.NumberIsTooSmallException;
import org.apache.commons.math4.exception.OutOfRangeException;
import org.apache.commons.math4.exception.util.LocalizedFormats;
import org.apache.commons.math4.util.MathUtils;
import java.io.*;

/**
 * Implementation of {@link RealMatrix} using a {@code double[][]} array to
 * store entries.
 */
public class Array2DRowRealMatrix extends AbstractRealMatrix implements Serializable {

    /** Serializable version identifier. */
    private static final long serialVersionUID = -1067294169172445528L;

    /** Entries of the matrix. */
    private double data[][];

    /**
     * Creates a matrix with no data
     */
    public Array2DRowRealMatrix() {
    }

    /**
     * Create a new RealMatrix with the supplied row and column dimensions.
     *
     * @param rowDimension Number of rows in the new matrix.
     * @param columnDimension Number of columns in the new matrix.
     * @throws NotStrictlyPositiveException if the row or column dimension is
     * not positive.
     */
    public Array2DRowRealMatrix(final int rowDimension, final int columnDimension) throws NotStrictlyPositiveException {
        super(rowDimension, columnDimension);
        data = new double[rowDimension][columnDimension];
    }

    /**
     * Create a new {@code RealMatrix} using the input array as the underlying
     * data array.
     * <p>The input array is copied, not referenced. This constructor has
     * the same effect as calling {@link #Array2DRowRealMatrix(double[][], boolean)}
     * with the second argument set to {@code true}.</p>
     *
     * @param d Data for the new matrix.
     * @throws DimensionMismatchException if {@code d} is not rectangular.
     * @throws NoDataException if {@code d} row or column dimension is zero.
     * @throws NullArgumentException if {@code d} is {@code null}.
     * @see #Array2DRowRealMatrix(double[][], boolean)
     */
    public Array2DRowRealMatrix(final double[][] d) throws DimensionMismatchException, NoDataException, NullArgumentException {
        copyIn(d);
    }

    /**
     * Create a new RealMatrix using the input array as the underlying
     * data array.
     * If an array is built specially in order to be embedded in a
     * RealMatrix and not used directly, the {@code copyArray} may be
     * set to {@code false}. This will prevent the copying and improve
     * performance as no new array will be built and no data will be copied.
     *
     * @param d Data for new matrix.
     * @param copyArray if {@code true}, the input array will be copied,
     * otherwise it will be referenced.
     * @throws DimensionMismatchException if {@code d} is not rectangular.
     * @throws NoDataException if {@code d} row or column dimension is zero.
     * @throws NullArgumentException if {@code d} is {@code null}.
     * @see #Array2DRowRealMatrix(double[][])
     */
    public Array2DRowRealMatrix(final double[][] d, final boolean copyArray) throws DimensionMismatchException, NoDataException, NullArgumentException {
        if (copyArray) {
            copyIn(d);
        } else {
            if (d == null) {
                throw new NullArgumentException();
            }
            final int nRows = d.length;
            if (nRows == 0) {
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
            }
            final int nCols = d[0].length;
            if (nCols == 0) {
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
            }
            for (int r = 1; r < nRows; r++) {
                if (d[r].length != nCols) {
                    throw new DimensionMismatchException(d[r].length, nCols);
                }
            }
            data = d;
        }
    }

    /**
     * Create a new (column) RealMatrix using {@code v} as the
     * data for the unique column of the created matrix.
     * The input array is copied.
     *
     * @param v Column vector holding data for new matrix.
     */
    public Array2DRowRealMatrix(final double[] v) {
        final int nRows = v.length;
        data = new double[nRows][1];
        for (int row = 0; row < nRows; row++) {
            data[row][0] = v[row];
        }
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix createMatrix(final int rowDimension, final int columnDimension) throws NotStrictlyPositiveException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "390b7798-e1d5-41bc-bdbe-0258b4d67a44");
        return new Array2DRowRealMatrix(rowDimension, columnDimension);
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix copy() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2274f5ac-32e4-4b25-a3f2-19375eb29d95");
        return new Array2DRowRealMatrix(copyOut(), false);
    }

    /**
     * Compute the sum of {@code this} and {@code m}.
     *
     * @param m Matrix to be added.
     * @return {@code this + m}.
     * @throws MatrixDimensionMismatchException if {@code m} is not the same
     * size as {@code this}.
     */
    public Array2DRowRealMatrix add(final Array2DRowRealMatrix m) throws MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2f7421ea-ff16-4c16-8c88-ba2c4365f5e6");
        MatrixUtils.checkAdditionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "4a79cc7a-7985-4f7d-99f1-78c135a14739");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f6d0d80e-892d-4233-bbf5-cb03c3118453");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c1483f3c-4930-47ae-8cb5-82a4b35bc75f");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "5c2780a9-4818-4c8b-9393-e6d5ca44f97e");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "cfce1396-ded5-43e7-80f7-ffb559fa12b8");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7ac1c3f2-3fd9-41ca-9763-1b5410425225");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "aa085643-d97b-41fb-aef6-d599789c8252");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "81bc49c6-c708-444d-886c-c408b3577e41");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a8636f5d-dc3b-4be3-baf2-0adff9d1c2f2");
                outDataRow[col] = dataRow[col] + mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c4f7a066-1de7-4f36-a820-c6247ddd82ae");
        return new Array2DRowRealMatrix(outData, false);
    }

    /**
     * Returns {@code this} minus {@code m}.
     *
     * @param m Matrix to be subtracted.
     * @return {@code this - m}
     * @throws MatrixDimensionMismatchException if {@code m} is not the same
     * size as {@code this}.
     */
    public Array2DRowRealMatrix subtract(final Array2DRowRealMatrix m) throws MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "09d9e6bc-8e78-4111-b7cf-7524a5da2741");
        MatrixUtils.checkSubtractionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "38406460-1e5b-4d47-a112-58595848ae74");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d9bfe770-da33-473c-94ec-095c3db490f5");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d4227ed9-9965-4bd3-bfeb-c633cd966c8d");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "9cea0417-bbd0-4af6-bf44-9a82e6d1af6b");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7d1a6cd6-2011-47d2-8836-29c300eed69c");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f73041ff-0604-4471-9383-29191f6df599");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "34fd1bc1-5af0-49eb-81b1-6eb0b0850401");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "fbaf8580-0cb3-4358-957a-a1ec181f6832");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d9fe678b-0e2d-4441-b94c-c58007ae6f27");
                outDataRow[col] = dataRow[col] - mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "47312120-604c-4557-9438-fc69d9dbada1");
        return new Array2DRowRealMatrix(outData, false);
    }

    /**
     * Returns the result of postmultiplying {@code this} by {@code m}.
     *
     * @param m matrix to postmultiply by
     * @return {@code this * m}
     * @throws DimensionMismatchException if
     * {@code columnDimension(this) != rowDimension(m)}
     */
    public Array2DRowRealMatrix multiply(final Array2DRowRealMatrix m) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "dc9f2269-3f80-4b19-84ac-dc11e5dce0cd");
        MatrixUtils.checkMultiplicationCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c850ae24-95cf-4fa0-a11e-e182023933df");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "0fcb3198-68ac-4f0e-8f1e-ca23e33d8a88");
        final int nCols = m.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "9bb6b5b8-74dd-4853-9519-de68362eb248");
        final int nSum = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d01eb615-e396-4e98-ad84-04a46b4edf14");
        final double[][] outData = new double[nRows][nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "54197b25-bb35-4b16-ae53-9842e9e8106f");
        final double[] mCol = new double[nSum];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "e5153553-8183-41bc-bb09-cee9a7ba1319");
        final double[][] mData = m.data;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "e8e1d3c4-6a35-4e2a-bfc0-1de82095d658");
        for (int col = 0; col < nCols; col++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "383c44e2-9001-4628-9128-37040bc1372f");
            for (int mRow = 0; mRow < nSum; mRow++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7f90d6dc-e3b6-4bae-88c1-f344e26599ac");
                mCol[mRow] = mData[mRow][col];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "cd220914-9f2e-4fc6-94df-519d8f696e67");
            for (int row = 0; row < nRows; row++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "0d39eff9-9f64-4354-8bb0-5ddb705fdd2d");
                final double[] dataRow = data[row];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "fca2caea-fbcf-4bcc-bd77-290ba6b77d57");
                double sum = 0;
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "0a172cfc-0dcd-4ecc-8e3c-44d1d82b5121");
                for (int i = 0; i < nSum; i++) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "8d068733-4be8-4f5b-8e19-537d12128510");
                    sum += dataRow[i] * mCol[i];
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "0c85cdad-ff89-44fe-95d1-12e8114a64dc");
                outData[row][col] = sum;
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "326fc7af-c6ef-48f6-861b-120aa347119d");
        return new Array2DRowRealMatrix(outData, false);
    }

    /** {@inheritDoc} */
    @Override
    public double[][] getData() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "51f26250-ec65-4e9a-9fa6-676961bde0ea");
        return copyOut();
    }

    /**
     * Get a reference to the underlying data array.
     *
     * @return 2-dimensional array of entries.
     */
    public double[][] getDataRef() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "08b441d1-d6b4-4f97-aebf-f3a001e4a939");
        return data;
    }

    /** {@inheritDoc} */
    @Override
    public void setSubMatrix(final double[][] subMatrix, final int row, final int column) throws NoDataException, OutOfRangeException, DimensionMismatchException, NullArgumentException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "702756f2-41e8-41cb-91b2-4dbd7ad72812");
        if (data == null) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "cbeee9da-4ab2-4fa5-9dd2-1bafcb1d98aa");
            if (row > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2500d691-9bd1-401a-ac61-5aab0e432c9c");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_ROWS_NOT_INITIALIZED_YET, row);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "ce99cfe2-e199-46c0-9f43-2e13ae34ba46");
            if (column > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "858a43c0-9f98-495f-99cc-327fc8eb56bc");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_COLUMNS_NOT_INITIALIZED_YET, column);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "6917eb48-f698-458f-91b6-e5c19d0e4aaf");
            MathUtils.checkNotNull(subMatrix);
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f032611b-d95d-4d19-8cf6-3de2c250b518");
            final int nRows = subMatrix.length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d5e5fa94-c39d-4f2b-a60e-d4f4725f0a6c");
            if (nRows == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b804ae88-d7dd-46d3-9b87-453dd21d2863");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "6159434e-f207-49be-922e-6a195886def4");
            final int nCols = subMatrix[0].length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "34970a7b-40a7-411f-964d-a0d16fb8f4e4");
            if (nCols == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d74ed55b-a01d-4edb-82b7-d4150adb7557");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a78ecb12-18ee-4a86-b32a-3cd76ba30636");
            data = new double[subMatrix.length][nCols];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "606c4b79-7e54-4b41-8072-b8377a32a5f8");
            for (int i = 0; i < data.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "53e0dfe2-256e-4aff-b69b-e9d559232166");
                if (subMatrix[i].length != nCols) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c00bc72c-e072-45c0-8449-d5fea3047517");
                    throw new DimensionMismatchException(subMatrix[i].length, nCols);
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "535d7231-ec67-47d0-bcdb-ecdcf0dc4811");
                System.arraycopy(subMatrix[i], 0, data[i + row], column, nCols);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "1f59706c-eb3a-4d38-bd15-307bc0a2ece5");
            super.setSubMatrix(subMatrix, row, column);
        }
    }

    /** {@inheritDoc} */
    @Override
    public double getEntry(final int row, final int column) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f43c384c-3807-414e-9e10-f814864ddd39");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "1ef70d40-5dd0-4f9c-af5f-e456707601cc");
        return data[row][column];
    }

    /** {@inheritDoc} */
    @Override
    public void setEntry(final int row, final int column, final double value) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "096f944a-fbc0-49d1-9ee3-b3ef5c5fba98");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b3e058da-1a57-479f-b29d-019729d6d5d3");
        data[row][column] = value;
    }

    /** {@inheritDoc} */
    @Override
    public void addToEntry(final int row, final int column, final double increment) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "48f27f41-2d7a-42df-9594-866a6cdd5b2f");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "bea87981-677f-441d-b9a2-3f20f4e4d16b");
        data[row][column] += increment;
    }

    /** {@inheritDoc} */
    @Override
    public void multiplyEntry(final int row, final int column, final double factor) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d756f67b-0bd6-4843-9f78-382ccfc213a9");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7bfb78a1-c891-4385-8d0d-e84143049db1");
        data[row][column] *= factor;
    }

    /** {@inheritDoc} */
    @Override
    public int getRowDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "276f6028-6368-4c04-ad4e-90e8f7fd365a");
        return (data == null) ? 0 : data.length;
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "ce4f6916-c918-4e8d-89a7-5165e56ea6a6");
        return ((data == null) || (data[0] == null)) ? 0 : data[0].length;
    }

    /** {@inheritDoc} */
    @Override
    public double[] operate(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "dd788822-361f-4fef-98c5-f662f620d4db");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d6bbdb9c-dddd-496e-b72a-b032a5510256");
        final int nCols = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "ada443b6-8cf5-4caf-8ba7-443da030b0dc");
        if (v.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "17730a9c-7ee7-495a-936b-af0a4aba510b");
            throw new DimensionMismatchException(v.length, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "dceeb5bc-5c32-499a-9f7b-818e1ba4fd7a");
        final double[] out = new double[nRows];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "01ff7e4f-b857-4e3d-8d6c-1371972c7cf9");
        for (int row = 0; row < nRows; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a0bf3d3c-959f-4588-be15-e40ca105b516");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2001807e-b96a-483c-beae-69f8cb0f7bee");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d2227599-8b52-4c4f-96eb-2779c528e3af");
            for (int i = 0; i < nCols; i++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "203a75bf-403b-47ea-906c-f593b7940bb2");
                sum += dataRow[i] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "cdd45a61-8c27-442c-97e3-f05f5a3923aa");
            out[row] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "8f2c44fa-db02-40a8-a787-2ea2f9cf6128");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public double[] preMultiply(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "1ca37f74-00c3-4dff-b90e-ec8ea5d037da");
        final int nRows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "5f589cf5-2bbc-4e4c-95f7-aa8bd13b478b");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "bb2830e8-8598-4e7d-aa4b-ffea5c0d81aa");
        if (v.length != nRows) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "984ea13c-330e-4555-8b4f-b4af0e631b7e");
            throw new DimensionMismatchException(v.length, nRows);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a9a09175-4616-4346-9d44-8eed9257311d");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b9e65707-5c9b-4e08-a2c9-2c1bef01bbda");
        for (int col = 0; col < nCols; ++col) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "4e57ed5b-5a25-43b8-857f-ff486a2ad3c4");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "183b5f6e-87bf-4d7a-981b-7c9975dd2220");
            for (int i = 0; i < nRows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "71b58f49-be5c-4936-8088-4d5f05b496d6");
                sum += data[i][col] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a6a0de9f-7924-4309-b452-4940978b62e3");
            out[col] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "80fbdccd-6bf7-4a6a-8aea-9cd1eb5f7ad2");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix getSubMatrix(final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f71cb70d-00d4-4b7f-97ae-8840aebfb3f3");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "56a02ce9-1ac0-4983-8226-4d6bf76cd9e1");
        final int rowCount = endRow - startRow + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "9381857c-50c7-4e3d-a076-6dbdf4753d77");
        final int columnCount = endColumn - startColumn + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "4dacd0c8-3e47-4dad-9e67-71396ec257e8");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b37fa1db-a10c-466c-a627-71af67d44d6a");
        for (int i = 0; i < rowCount; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "722cf352-0878-4c49-840d-08f7534a5293");
            System.arraycopy(data[startRow + i], startColumn, outData[i], 0, columnCount);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "76c87f3c-4064-4223-bbbc-8541e101c993");
        Array2DRowRealMatrix subMatrix = new Array2DRowRealMatrix();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "27bd7af2-175d-4aeb-bec6-65f4b9039887");
        subMatrix.data = outData;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "9edb0e77-0692-41ce-ab98-3572493dbca9");
        return subMatrix;
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "40127de4-5480-492b-b049-37a886877ed5");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "6162109a-c4ac-402c-8004-e315d1626e3b");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f4ab3bc1-9fb9-4f0c-ae19-771ac4ea21e0");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f76c46bb-c59a-4b43-b1f6-2802180a0121");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "188782e7-2019-4d96-9b0a-99950878fb11");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a7cfc4b2-0367-4d00-93e3-2189cbfe0882");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "ffc18cd0-622e-4efd-a79c-a6b7f38d69b6");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "69023a3e-1ace-4548-a3de-200404505bb2");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7dd25cb4-e276-476b-8446-bb621ca69e13");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "917b0922-ae89-4e14-8593-6ab9ce6cc528");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b03b9055-941e-4e7f-b2e7-c9bddde69f18");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f001e8ba-298e-4ee0-9da0-a34e4488b4b3");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "282dbc02-7063-4d63-a540-66a69ae86fc6");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "665b924b-fc19-4f0e-83a2-e18751cd5954");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2ac73e1b-cfb7-4937-a182-3335399e8573");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "effc6fa1-5092-426e-9fe3-7416a9362a04");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2cf886b4-ccdc-41e4-88e2-a5ba3de789a8");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "760a9c0f-c5a2-46d1-9c2e-96ac26904477");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "58dd687a-871e-4acc-b1a3-43dbed9f492a");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "ce7369da-05a4-46bc-8e3b-0a6607afad6c");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c0ea8f0c-b3f3-457a-9dfa-4316d877d95e");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "1fb120e8-48f2-4f2a-a2cb-2581f040c0f0");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "29999d49-11e9-40f8-b25a-3d84cbbdf82c");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "528818c8-cb34-48c8-8bc2-965ef970c8da");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "8733732b-41fe-45a8-a48c-2e1640256f7f");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "e5cdb2c8-663a-4855-bdf0-189d0725905b");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "8c14e713-5a7c-4110-a667-a5e5af5dec93");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "17ace725-3125-4a75-aed1-07ed628d9e4a");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7041ddb7-f6c7-44bd-a015-0ace38c1028c");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f48931c8-e134-48bb-80db-197d5ba88035");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "dcb22294-4cb2-433d-b272-e904a1cb15fe");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "70acf00a-0021-46c2-83b8-e032f4b4179f");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "e11ae3cd-685e-4502-a3d6-5ca7068f628f");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "a29b9c9e-dcab-45c4-8ff2-538ac8381b27");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "7d5837fc-fb5b-44b4-bc81-4e4a99bbd876");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "5192f5be-5a56-4888-8bd6-b5b46fed5d70");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "144508cb-8926-40d3-9daa-097eeaf4321f");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "262a980c-6728-4c8e-bac6-9c2e71cd7865");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d0c1f211-9424-4d02-8970-f9095d0eafa9");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "1ee588ff-3c9d-4f14-a3e5-500f2a9885bf");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "85947d4b-cb59-4ccd-88ed-80101547a924");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b92c5f57-4c9e-4c52-bcb5-65ebbb2cb1e4");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "59dc55ae-513d-45dd-a19c-8c607de48ba2");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b2bbbf06-58db-4b3a-9e8e-e81a4b07cfbe");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "35bdd191-fa1f-4609-9c1c-8d0b61d2193f");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b893e14a-bce9-4bc2-abe7-a93113b09bbd");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "f6359060-a2fe-421f-8560-b2a391cc9589");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "397da2b2-8dc3-473d-ac44-ff8fd60a90e9");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "bdede814-540d-48ab-814e-8146cc6d1a47");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "773ef0eb-1bcc-4f31-94d2-6eb0641521f6");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "0f95f4bd-4fdf-4578-b67a-def01c254444");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "36c1036d-5a63-484d-aaea-6fc2718c5b00");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "924b6066-c51a-4ca0-875f-0c25888bb685");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "3a47d0da-16b1-47bd-941c-5727d5df3c61");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c4a0145f-dd18-4ede-b663-cd137702857a");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d6eb86e5-abbe-41ba-9caa-cd5c7a0e9da6");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "c77088fd-e176-40ec-b4b0-bc5c9e826863");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "58c7905e-c15a-46d9-b3d5-668d0c8537aa");
        return visitor.end();
    }

    /**
     * Get a fresh copy of the underlying data array.
     *
     * @return a copy of the underlying data array.
     */
    private double[][] copyOut() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "99f0827e-ab35-4e96-a209-86474819a30f");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "d2fbbfce-ea87-481c-b592-4612642d86ee");
        final double[][] out = new double[nRows][this.getColumnDimension()];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "8d0ca06c-db2b-4d82-b2f7-0e89c4086618");
        for (int i = 0; i < nRows; i++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b7a8dd7a-91ac-481b-98cc-f9b9e6718e48");
            System.arraycopy(data[i], 0, out[i], 0, data[i].length);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "cce75aa7-ccad-40b6-ae00-8510776dc500");
        return out;
    }

    /**
     * Replace data with a fresh copy of the input array.
     *
     * @param in Data to copy.
     * @throws NoDataException if the input array is empty.
     * @throws DimensionMismatchException if the input array is not rectangular.
     * @throws NullArgumentException if the input array is {@code null}.
     */
    private void copyIn(final double[][] in) throws DimensionMismatchException, NoDataException, NullArgumentException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "9f5cc482-3cb6-4512-a309-ded63132f71c");
        setSubMatrix(in, 0, 0);
    }

    /** {@inheritDoc} */
    @Override
    public double[] getRow(final int row) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "bac5691a-13e3-4b33-b5b7-7ea9df96d74d");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "0cfa4bb4-1ea8-48aa-acf1-78ba410e738a");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "3a50fa1f-fa1c-438d-8e88-f7ca26890ce2");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "13ce7a4c-7bb1-4d5d-aa4c-afc2e6252025");
        System.arraycopy(data[row], 0, out, 0, nCols);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "3f8b3e3e-d80e-47b8-8e82-c30f4682e0c7");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public void setRow(final int row, final double[] array) throws OutOfRangeException, MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "e7733f75-885c-487c-80fd-68ed3a4d5c21");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "94d3f072-00df-4549-ad4c-ab92c7a7d602");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "2caa70e9-d38f-428b-acbc-7ca6c5689bbe");
        if (array.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "09dc7ee4-34ec-43a3-aa8d-fc4f4a1ac07b");
            throw new MatrixDimensionMismatchException(1, array.length, 1, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_3_10.coverage", "b494f8e2-2004-4f57-a1e7-a4a78537f40f");
        System.arraycopy(array, 0, data[row], 0, nCols);
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
