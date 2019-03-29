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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c16400f9-70f3-406c-a843-5d72ff1ab182");
        return new Array2DRowRealMatrix(rowDimension, columnDimension);
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix copy() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "e535caa5-b761-43b7-b555-ba8f600b8bb7");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "f2e679fc-763b-48bf-9919-32f121d0948b");
        MatrixUtils.checkAdditionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c851015c-d8d0-4c74-ad29-58f587d1460a");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "deaa9e34-6d81-4818-bff3-4c0f04252578");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1e626333-9b53-406e-9828-ede233ae1d81");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "674e90ac-e620-4f48-a02d-a9547d1be2ae");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "497018e5-526c-480e-948a-48c1d3829b77");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7cedf89e-c89e-4167-9915-ff71d0013204");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "df6ae304-6d08-4134-87bc-73fe422517d7");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "60693c7b-598f-46bf-805c-c6db63345277");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c1c4109f-e8f6-4715-95b6-4ee818277438");
                outDataRow[col] = dataRow[col] + mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "2604d370-8fbc-41c8-9acb-fe63d20374c7");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "55f8dc47-734d-4f48-8eba-8b9c0ae134c2");
        MatrixUtils.checkSubtractionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "50a8322a-5794-461c-a10c-ea881cda4e06");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "972ec6e3-505e-4773-bfd9-ac8340182a2c");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "925fe504-72dd-40df-b169-ff02188e97cb");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "87bd944f-db65-43a0-af09-e1b3aa0fa6ed");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ebb6bd14-5dac-4112-b409-793ed81304f4");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c76e556c-c248-4b1d-8597-f3fd1e372139");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6c0bdb4a-0e45-40e7-8b79-4086ce7a787a");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "587f18f2-f705-426f-a35a-3cfb56b6ce07");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "24525a18-9165-4adf-85ac-f1efc31b143a");
                outDataRow[col] = dataRow[col] - mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ded6c17a-8d9b-48de-a3d2-89d2da7ca804");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "84dbeafd-a609-4e30-81ca-e0ecc11dac48");
        MatrixUtils.checkMultiplicationCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "d1d9532e-bdfb-4d87-bef6-fd461bb53d34");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "281a26dd-cbc7-4f30-889b-88e88fec34fd");
        final int nCols = m.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "d15513fc-5326-4fa3-92d2-1d9b9637ec98");
        final int nSum = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "57e055c2-ad06-4277-b491-81a5aa05633c");
        final double[][] outData = new double[nRows][nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ed35b284-fcf9-4388-a40f-b4fc0410bdee");
        final double[] mCol = new double[nSum];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "81c57f0b-6f82-44d8-9403-2932fc3cf563");
        final double[][] mData = m.data;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "dfa60a59-4e2f-43ef-90b7-c0feecee4162");
        for (int col = 0; col < nCols; col++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ebedd29a-6594-4568-b83c-9f5a5a189d51");
            for (int mRow = 0; mRow < nSum; mRow++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b3c40450-b5ed-4f0a-838c-0c032717a26b");
                mCol[mRow] = mData[mRow][col];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "3330e308-9cc1-43da-8a43-198cfc0bd662");
            for (int row = 0; row < nRows; row++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "e27dcb27-dd6b-470c-80c9-463533f3786b");
                final double[] dataRow = data[row];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "4fb2d028-2f45-4ebf-957b-ef0d22194933");
                double sum = 0;
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "a5579add-8b0f-449c-89df-c3da74e80585");
                for (int i = 0; i < nSum; i++) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "773e47e7-09c8-4735-8c1a-080ea4422338");
                    sum += dataRow[i] * mCol[i];
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6290a308-f769-439a-ab28-97fc761dbfd8");
                outData[row][col] = sum;
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "a8840bb9-7c53-4d52-95f2-3aaac3aaa9f3");
        return new Array2DRowRealMatrix(outData, false);
    }

    /** {@inheritDoc} */
    @Override
    public double[][] getData() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ef1f1daa-9c2b-46e3-be69-82f59e845a56");
        return copyOut();
    }

    /**
     * Get a reference to the underlying data array.
     *
     * @return 2-dimensional array of entries.
     */
    public double[][] getDataRef() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "bf25a125-a515-47a3-8709-0ef5a0de0438");
        return data;
    }

    /** {@inheritDoc} */
    @Override
    public void setSubMatrix(final double[][] subMatrix, final int row, final int column) throws NoDataException, OutOfRangeException, DimensionMismatchException, NullArgumentException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "a9e3c4b7-b9e3-4210-8abf-bf497ae35af8");
        if (data == null) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "42b6ad12-8d7e-442b-b1b5-ea852f2c89bd");
            if (row > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "fb9534cf-b01a-4c60-aa4d-77a3bf59f9ad");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_ROWS_NOT_INITIALIZED_YET, row);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "76e38ba1-6cf3-40f3-8a8f-a03dab0d93a4");
            if (column > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5811325d-d428-40f2-9d53-e0b09c650d3c");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_COLUMNS_NOT_INITIALIZED_YET, column);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7ece2810-84c7-4e4c-9d6f-9863d3c15f36");
            MathUtils.checkNotNull(subMatrix);
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7fa06110-f735-45fd-8554-5be3afea7769");
            final int nRows = subMatrix.length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "f3c0043a-60a1-4df1-b817-70da55a30e9a");
            if (nRows == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5841cc8c-f8c3-4f9a-aa06-b9c6373ebbc5");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b2e9c850-1890-4a36-a8c0-89bf1becf98a");
            final int nCols = subMatrix[0].length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b9197991-2f30-406c-a617-9585b4566790");
            if (nCols == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c2b65b1e-e868-4add-a79b-83e11f892096");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1df476b3-6209-4753-b026-1c872fbd2cf0");
            data = new double[subMatrix.length][nCols];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c397978b-e54a-4496-adfd-9128110fe4a9");
            for (int i = 0; i < data.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "47d18e00-29a8-4861-9a50-6cee4aa00cce");
                if (subMatrix[i].length != nCols) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c0fed269-0be4-46e9-a4cc-5455f955df94");
                    throw new DimensionMismatchException(subMatrix[i].length, nCols);
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ed74cd1f-461d-4b2c-89ad-a39ebb91aa1c");
                System.arraycopy(subMatrix[i], 0, data[i + row], column, nCols);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "67ef101a-af08-4ffd-bd21-bdd5c183457d");
            super.setSubMatrix(subMatrix, row, column);
        }
    }

    /** {@inheritDoc} */
    @Override
    public double getEntry(final int row, final int column) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "515c2e3e-814f-4784-aaba-6fb7bc86d6a4");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "a4d5be27-d161-4afc-9d67-e07edb43fb4a");
        return data[row][column];
    }

    /** {@inheritDoc} */
    @Override
    public void setEntry(final int row, final int column, final double value) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "13c40aa5-6a00-4265-aa98-8f4788c0cf75");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "8c21c7b5-d065-4233-b2f3-40c778ae98b6");
        data[row][column] = value;
    }

    /** {@inheritDoc} */
    @Override
    public void addToEntry(final int row, final int column, final double increment) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "d052f8fd-48f5-4843-b7dd-307264f09f7d");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "cf67d023-c948-4572-8c74-ac72044420e1");
        data[row][column] += increment;
    }

    /** {@inheritDoc} */
    @Override
    public void multiplyEntry(final int row, final int column, final double factor) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "0f36ca66-17bb-4671-9191-070b0c6ae85a");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7e400d95-a18e-4acd-bd0e-82159242f01b");
        data[row][column] *= factor;
    }

    /** {@inheritDoc} */
    @Override
    public int getRowDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5915cff9-5781-4d62-a1fb-687eafa29ce4");
        return (data == null) ? 0 : data.length;
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "bfb85481-d666-46a3-a4a6-1a1f4b418eca");
        return ((data == null) || (data[0] == null)) ? 0 : data[0].length;
    }

    /** {@inheritDoc} */
    @Override
    public double[] operate(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6d4e18be-3a34-4fc6-a83f-09b67f77221f");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6cd8fd10-f93a-48b0-921a-d1b712540d1f");
        final int nCols = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "0871431c-89b7-4844-af9f-590b869df659");
        if (v.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "9281cffe-9256-4bb5-9c41-8635e987a82f");
            throw new DimensionMismatchException(v.length, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b3d191bd-404f-4c2f-89c4-10496539dd6b");
        final double[] out = new double[nRows];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "627308e3-3165-4afc-bb62-1619f0661e34");
        for (int row = 0; row < nRows; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "bbfcfa90-3973-464f-9203-5f495f53ceee");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "e9fe7daf-e345-4964-adb2-c3f7fd5b5632");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "d254db74-70aa-4446-8ace-846dcdada35f");
            for (int i = 0; i < nCols; i++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "2f394b01-21d7-492c-bddd-a319a9078ee6");
                sum += dataRow[i] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1c8c671a-8e40-4feb-b7c5-545a705f78af");
            out[row] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1042877c-0e48-428d-99e7-af332666d7a0");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public double[] preMultiply(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "80377f65-e57d-4ba5-a44e-53ced5f2b6aa");
        final int nRows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "42f3a94b-30d1-49f6-8297-4f6432345c06");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5649f1b2-56d3-490e-8437-3a02ea8a824a");
        if (v.length != nRows) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "289e5874-af58-495f-b548-005aae4aad30");
            throw new DimensionMismatchException(v.length, nRows);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "f4deb9dc-8fa5-478c-97a2-2349280364e5");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1658bd5b-3a3f-4027-87c2-7f46067bb2bb");
        for (int col = 0; col < nCols; ++col) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "97c0876c-0b7b-4ad3-86e1-227bbac6f633");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "54256def-044e-45d2-a9dc-fd0b58818345");
            for (int i = 0; i < nRows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b30905f6-54e3-489b-a4a8-7b1b914d2f54");
                sum += data[i][col] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "2388190d-b2fa-4019-811c-50e60d5bbbdf");
            out[col] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7501ad43-d00b-4521-b293-192dd34f848a");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix getSubMatrix(final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "f62e9fce-b06f-4cdb-95d1-33070fd02046");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7934ae17-7dd5-4106-8a35-b3a8be1a1af0");
        final int rowCount = endRow - startRow + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c6e62e30-1911-46c6-a6ff-99244c83c337");
        final int columnCount = endColumn - startColumn + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "a7f24c8c-adbe-4b1b-bd68-b58980ca979f");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "8f7c791f-ef45-47b4-8d1b-24cba1d92a7f");
        for (int i = 0; i < rowCount; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "9d889822-9e03-4d8b-b24c-2f85f448482d");
            System.arraycopy(data[startRow + i], startColumn, outData[i], 0, columnCount);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "9537ab12-c746-4dae-aba0-3586f671c046");
        Array2DRowRealMatrix subMatrix = new Array2DRowRealMatrix();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "ae5019f6-e92e-44c5-aa2a-73d625b3f927");
        subMatrix.data = outData;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "84c4ca25-b34f-4beb-8790-0b111b6e89af");
        return subMatrix;
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "4c313b09-f2d5-4b25-af45-9457737d67db");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c6765741-993c-4cea-ab95-2a852fb7f6d1");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "2db362ca-3ccb-45e5-b9c6-ac8471a122a1");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "02bf77c0-cff0-4a2b-a80b-2195a34f475b");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "95910490-3044-48d6-98c6-142ec126fec7");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "8692490c-88b8-4b28-aaac-a66ce948abc7");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "e596be48-5ae5-4200-8491-975512056cdd");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "4eb7d45e-9b3a-4def-acec-4100934e6996");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "20ea7092-ba60-4474-a63a-6a2d637f211d");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "67341d19-a209-4d1a-94e0-dba0b81c397c");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "2e36e29d-ddf4-4a99-b2d6-b92b8cf17e7a");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "dc4e9693-f36c-4ffe-b8cb-aab3484b97ff");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b74b6225-37b8-4152-a3cb-51d874184b30");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1b3f1fa0-e717-48db-86f5-b4bd950ce245");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "aee3be01-144f-425e-bb9e-96f30bfab466");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "79386fbe-81cb-4c21-aee6-5f4ec5b42381");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "95002ce1-8d72-4b45-a6e7-55f53e9ad296");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "16e358c0-1f60-4c89-b8dc-e7fd96ea72f0");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "9c28ea21-05a3-4469-830c-3cfd5f3169da");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "0a2f9563-58ea-42fe-91be-49bdb2373352");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7837a79c-bfc4-45ad-906e-9b642b556b9f");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5f504e36-e250-4aad-ab2f-698ff58be417");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "21ca1982-d7b7-4980-8053-3d5d8d369c83");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "56299429-5d1a-4e01-b66d-f44295368caa");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "3efefd62-dc4a-4ef2-8f9a-90a119567806");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6b4b7d55-18cb-4dc5-b406-fc82e8ca73ec");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "74d1c90e-ca59-4a0b-b30e-3c4ba7633c05");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "f94fbe48-a617-4713-91a4-fa55b7d986cf");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6c3b96d5-7b60-4b2e-bfc9-ddd7ffd714b3");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "3804dc05-3bab-4934-9444-3a8f3b298868");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "bd45daf5-9c42-4bb1-b4d5-647e54c423ee");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "3bc46c27-f959-488a-b522-1e0e3c101098");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "96d46a22-9ba4-43a6-8e12-c48bd4c0dde9");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "58a23a56-fd0d-463b-b814-8cb3cb299c8b");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "d81cece7-b2b3-4a7e-a181-d0412e1f5e91");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "94def0f1-2181-4495-a6f3-fc62741323c7");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "728746d6-0e0b-43a7-90cb-e49f6e68f5e0");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5cbbdf0e-aaa0-403c-bdac-220003838c74");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "f8426d67-e8af-42f4-9d50-cb5c0b3e8895");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c2537012-a45c-465b-98b2-e2df40ea9ee5");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "aaa0469a-b765-4874-97f3-d4240a90e0bb");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c2014e8d-e03d-41c8-9892-63f0b4d2f207");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "c7c18531-af39-4c8b-a2dd-096243f9955c");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5755581c-9190-4f31-a12e-2a55eff35e0a");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5bb23d43-a556-4875-a925-557585e77042");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "8646ae62-7182-4722-a5ac-8fc143ca10a8");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "3b410c2c-ba40-4103-9186-ea1e7b80e756");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "0a9d0460-7a74-4ad3-86a8-77f772e1ddb5");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "660a3f57-f81b-4313-8218-d4937470f4b1");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b51323fc-9b7f-4448-8a15-3cf0245a6eae");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "0c2aa41b-1e99-4bed-bb4f-ad15345571e9");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "573a5758-384c-4ca3-94e4-9f59070db256");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "61d7038d-94bd-49c2-a692-8d8ffb0fd4cc");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "267d9e36-3b25-4260-8de2-5d37eddcfb17");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "0446ff96-512c-4871-aaf7-d952af986cd3");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "820604f3-2046-47a5-ba64-ae7a54ffcc3b");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5eade730-78d0-432a-a827-75b456d46757");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "cc4ecfb3-83ad-44aa-bf24-e3d8604ac7ec");
        return visitor.end();
    }

    /**
     * Get a fresh copy of the underlying data array.
     *
     * @return a copy of the underlying data array.
     */
    private double[][] copyOut() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "543ef2f9-92c6-42b3-b101-93340305609f");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "29b9d16f-a433-4ff0-ad6e-478957e6eb73");
        final double[][] out = new double[nRows][this.getColumnDimension()];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6e261277-04a3-41f2-9d65-3a06c69e3de7");
        for (int i = 0; i < nRows; i++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "18560228-2928-4b96-a753-8d4d54c97de7");
            System.arraycopy(data[i], 0, out[i], 0, data[i].length);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "2c20ee6e-5cc7-4928-a202-96c4a220a1f2");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "b100ee37-0822-411f-bd3a-a066b58296cd");
        setSubMatrix(in, 0, 0);
    }

    /** {@inheritDoc} */
    @Override
    public double[] getRow(final int row) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "fa1474bc-0c77-436c-ab95-17f8917b3d92");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "10da8197-1f44-4b6f-bd24-0e3226c533b9");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "6d11d790-3628-44ed-bfae-a4c31c65268b");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "5c1fe449-8a17-410b-965b-e8ed746797e9");
        System.arraycopy(data[row], 0, out, 0, nCols);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "e005063e-4cd3-45c4-a0ef-2f33067b4a1f");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public void setRow(final int row, final double[] array) throws OutOfRangeException, MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "779bd92a-a5e8-4618-b636-7b18a0a82a8b");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "9f23f30d-ac68-4496-84f7-857bdfff4026");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "7a638c61-a24c-4cf6-925c-2b9c123831cb");
        if (array.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "9e702bc7-5923-4a2c-aadc-f659827fa041");
            throw new MatrixDimensionMismatchException(1, array.length, 1, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_1_10.coverage", "1f8bb854-6232-42bd-a714-1ad23655acb7");
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
