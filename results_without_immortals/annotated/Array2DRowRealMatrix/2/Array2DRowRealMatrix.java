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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0a81fae0-f2fa-4333-9260-e7831a4c4b23");
        return new Array2DRowRealMatrix(rowDimension, columnDimension);
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix copy() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "9a5915bb-de75-440e-bd55-d38b9f4056ad");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "a7820f5e-2b93-4ec3-8582-b0f5f4999be2");
        MatrixUtils.checkAdditionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c64aa721-525c-463b-b446-2224ee54fe3a");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "bd14543a-8741-4c56-94a1-8932d81792df");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ed3c4751-2d91-42ba-85b6-7877b7f21826");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "fc4c2654-97da-4a2a-9c66-b343989a245b");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "46a21946-f4d4-4633-b1ab-5b34b47db468");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ef08c6bf-3b73-49dd-b0d1-20b2fb4ceab4");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "12047a87-f6bd-4364-87e7-86190fc5b68e");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8bb5d620-6b81-452c-8f41-37dff020f062");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8d0e6f59-c7c6-495a-ab12-131481dc3b34");
                outDataRow[col] = dataRow[col] + mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c7f0db49-096c-4bad-97bc-a604bbbd1808");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "be39ee94-9bb2-480b-a0f1-87bfa652fbf2");
        MatrixUtils.checkSubtractionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1f03d941-a9ce-44ef-8e21-0a92951389cf");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "4f58f0e0-2e36-4c70-a180-37d061ff5a4f");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c9dc266a-6a82-417f-bb6a-e99642fe2298");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "95b68dca-7e5a-43a9-ab74-b7a3a38022f7");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "57f9c6dd-1c19-4d1b-8996-0009c8d9d943");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5e34c44e-44f8-4503-881b-a328ab66d8a4");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "22119411-6737-44c9-a3e1-2c3ddf578a22");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8e98c39e-9cb4-469e-b6bb-db7ded74db8c");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0077f8c8-9df1-457e-a3b3-88f20e4a540d");
                outDataRow[col] = dataRow[col] - mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e53c41a8-6ff4-440b-8274-35aa178dd9e5");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e4f22a97-a1e3-47e7-8c28-c58309e9889d");
        MatrixUtils.checkMultiplicationCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "b57cf383-5b58-435d-8860-b39c4fd4dd9b");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f83a9fb0-f820-4700-8767-d45e58b4f0c5");
        final int nCols = m.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "be5902a2-0629-48a1-9e3a-3f9678747efb");
        final int nSum = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e991fd41-0510-4273-960a-2eece24c8fa1");
        final double[][] outData = new double[nRows][nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d8e1d033-05e2-4b58-bec2-86dce1726b9e");
        final double[] mCol = new double[nSum];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "85597169-f1f2-4aaf-b0e4-2a83d9a00a17");
        final double[][] mData = m.data;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d9d172a6-857c-4b76-b641-5aa52f0907e5");
        for (int col = 0; col < nCols; col++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "b5a5c155-fe5d-4062-8534-d0a1490d41d7");
            for (int mRow = 0; mRow < nSum; mRow++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d4f4a55a-cc3d-4fd8-8d5c-94646d8bd826");
                mCol[mRow] = mData[mRow][col];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ad1a10fa-797a-4757-a9e7-bb529b0e16c0");
            for (int row = 0; row < nRows; row++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "eb675a17-42d8-4efd-8218-48cd0724f7e3");
                final double[] dataRow = data[row];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "9a6b4347-29fa-498e-ae81-fae26a6c4809");
                double sum = 0;
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "74893190-32c9-46ce-adac-97404cedce5c");
                for (int i = 0; i < nSum; i++) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7fa38830-c55d-4dbc-8858-b1a3d1c4aaf4");
                    sum += dataRow[i] * mCol[i];
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "9e4d6beb-86db-47de-af4c-a752ce614589");
                outData[row][col] = sum;
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f351bcd2-b271-4419-a0dd-6d2ad557a26b");
        return new Array2DRowRealMatrix(outData, false);
    }

    /** {@inheritDoc} */
    @Override
    public double[][] getData() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "6465ed8f-63c2-4e1b-8843-db84cda05c63");
        return copyOut();
    }

    /**
     * Get a reference to the underlying data array.
     *
     * @return 2-dimensional array of entries.
     */
    public double[][] getDataRef() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "56e8d8e2-60a7-48a8-8689-bdf977d8ed40");
        return data;
    }

    /** {@inheritDoc} */
    @Override
    public void setSubMatrix(final double[][] subMatrix, final int row, final int column) throws NoDataException, OutOfRangeException, DimensionMismatchException, NullArgumentException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f3a9965d-e9ed-436f-8e3a-388eb6a4d63f");
        if (data == null) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7cd85724-0b01-43c8-894a-92fcbe3d112b");
            if (row > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "b9bf9764-00b5-47a4-8384-3a4e89a8c4dc");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_ROWS_NOT_INITIALIZED_YET, row);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0588f836-582b-448d-a8b1-445dfd37f554");
            if (column > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "4bf300ae-e956-48a9-bcc7-1d61eacbbe67");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_COLUMNS_NOT_INITIALIZED_YET, column);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "517e8373-1fd3-45b2-8a88-d3d4f332bf01");
            MathUtils.checkNotNull(subMatrix);
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "11b2ea2d-c55c-4642-80ec-da528242da39");
            final int nRows = subMatrix.length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e150267e-5d18-43d9-99e0-ea876bee0d36");
            if (nRows == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "db330a84-f279-49a4-ba05-ab9faa5b1791");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "3f071b3d-2572-413b-acfb-cd85ce94cab1");
            final int nCols = subMatrix[0].length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "fba14616-aad5-4641-bbe8-b36979332476");
            if (nCols == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "26e62061-53e2-4257-aa06-4cfbf8a49180");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "aed05c7a-9923-4cf2-91f8-425ac61e38c9");
            data = new double[subMatrix.length][nCols];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7b186196-f726-44fd-b3d3-a13d58818c16");
            for (int i = 0; i < data.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d0f321dd-69a6-43f3-b3ff-04bbfc9b387c");
                if (subMatrix[i].length != nCols) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1f4abde2-ef99-47f3-acdc-2c155ef9c1a5");
                    throw new DimensionMismatchException(subMatrix[i].length, nCols);
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f7716b8b-7c40-441c-8165-9d198b302f82");
                System.arraycopy(subMatrix[i], 0, data[i + row], column, nCols);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e7471c80-4e7c-41cb-a32f-a04fd81caf53");
            super.setSubMatrix(subMatrix, row, column);
        }
    }

    /** {@inheritDoc} */
    @Override
    public double getEntry(final int row, final int column) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d0a69daf-cb3a-4f2b-a8f8-dd654707da04");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ddc09e21-0721-46b5-b81b-27a5d917a6ca");
        return data[row][column];
    }

    /** {@inheritDoc} */
    @Override
    public void setEntry(final int row, final int column, final double value) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8565df7f-6e27-4693-aecc-52aba0220c4b");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8ea120d8-337d-491e-a3cb-cb5089087f70");
        data[row][column] = value;
    }

    /** {@inheritDoc} */
    @Override
    public void addToEntry(final int row, final int column, final double increment) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0fa9944f-1d65-4320-830b-5780474b49e8");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "cf59ed72-e447-41ba-9730-07da9048f7cb");
        data[row][column] += increment;
    }

    /** {@inheritDoc} */
    @Override
    public void multiplyEntry(final int row, final int column, final double factor) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "fb7b73ac-9cac-4276-bd14-ff643778f118");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1c3fdaa2-995c-4b56-813a-6a07cfa59c17");
        data[row][column] *= factor;
    }

    /** {@inheritDoc} */
    @Override
    public int getRowDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5991712e-cb01-4b65-af97-98e4eefbe08a");
        return (data == null) ? 0 : data.length;
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0c6e4de6-1e92-4228-a052-c263f2c25fdb");
        return ((data == null) || (data[0] == null)) ? 0 : data[0].length;
    }

    /** {@inheritDoc} */
    @Override
    public double[] operate(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "579c5789-5542-4415-bc4d-9bab4b378cb5");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f79c6a82-f5c6-4756-8402-8d0bea123ac6");
        final int nCols = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c516b2a7-cf5f-4c88-b492-62b1911318a8");
        if (v.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "b559ff59-cd29-4371-8fa6-71cee60917f7");
            throw new DimensionMismatchException(v.length, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "9b374d7b-ef7b-4ffc-adfa-84609acdddd7");
        final double[] out = new double[nRows];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "4256f963-1c0e-4625-85b1-d39e5d622010");
        for (int row = 0; row < nRows; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ef63d82a-45dc-4535-85c3-0aa061d1b58e");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5fc4239b-7020-43c6-8ad2-a195ce5b8cc9");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1da665a0-5f83-4eac-b4ee-043b37cc2d7e");
            for (int i = 0; i < nCols; i++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1f7201b9-ce98-452a-9923-287224d43099");
                sum += dataRow[i] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "b68d85c4-76a1-4556-8762-473bec77a6af");
            out[row] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7add404e-58c6-4fad-8b10-770f1ebf8d14");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public double[] preMultiply(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "14a4ec72-d9aa-4334-9aee-4c6c3dd0f810");
        final int nRows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "6c9e1e07-f4c0-4252-af0a-7acacdd6c135");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f4dfef6c-e6f6-4bc2-80a2-fb363f25ef4d");
        if (v.length != nRows) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e6ead37f-b2b8-4554-8547-7a335becc3f3");
            throw new DimensionMismatchException(v.length, nRows);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5ffca5fc-0872-4a99-ac48-df3919d06abf");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "76722ed1-92d7-472a-bde0-2d6eba153a48");
        for (int col = 0; col < nCols; ++col) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c345d761-fdee-447b-9a38-384e2a2b858c");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0bea102d-5349-4d55-83c0-c3160f371fba");
            for (int i = 0; i < nRows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e3ad9043-a596-4803-991f-db69ec567f06");
                sum += data[i][col] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8643d75b-67ec-4c16-9d19-621dd20121b7");
            out[col] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "4a54eed6-9650-4081-9a60-93525c244eb7");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix getSubMatrix(final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e26135c6-3e14-4ddd-9338-1a257ff1d349");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d65a6c35-c858-47a6-84fc-08954fec9be9");
        final int rowCount = endRow - startRow + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5dec0ebd-5e42-4c54-a8fc-fbd09367f717");
        final int columnCount = endColumn - startColumn + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "451fab48-f0e4-4ef6-b090-acc43b82003a");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "cce19eb8-ae13-4161-893f-c17a9bd66dfc");
        for (int i = 0; i < rowCount; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "94f5466d-52f0-4db0-8e29-0beaea9c19fe");
            System.arraycopy(data[startRow + i], startColumn, outData[i], 0, columnCount);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "136e5b8e-1d4d-491b-8d08-09cb1a96ff50");
        Array2DRowRealMatrix subMatrix = new Array2DRowRealMatrix();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "a480bfdb-0876-4fbb-8f37-1e2ace44bcfb");
        subMatrix.data = outData;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "644ea973-3fd9-4a42-9bb9-93c4d3eb7cb4");
        return subMatrix;
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "73b390e4-5a65-4843-924b-1ef8f606c762");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "92469931-5ff2-4f24-8f2b-e2f08f0f6001");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "03caeede-d09c-4f45-9ed0-e6b5c23dab77");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "9d81cb2b-7c4b-4723-ab10-7d64120ee244");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "070f7ee8-ae82-425a-a723-167a630390bb");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0775e600-2b81-4b88-abf3-53459f9996a2");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e1091b77-c3fe-4050-8761-7d2eca2d5e66");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "26983431-ac44-4eb2-b444-8b78238d81fd");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "dd39611e-2e7d-4c18-bb78-40afb971a3d3");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0229a210-29b6-4a4a-91cc-0c8c3e02183c");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ef5a5aa4-d32d-4772-b527-4da32c53458e");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "e348f484-d726-42cc-8f86-72628d53a57d");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f8d4adac-07ce-43c0-9915-3e8897c909b5");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "db50213a-4e6d-4b61-8ede-4c5c96fa674a");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "699d0941-7eac-4ad1-94fa-cfeb1ce8414d");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ca427a59-bcef-47bb-91d6-87f05b8c5921");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "3a6179df-039f-4747-ae1a-06a8d4cbca28");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "28f6eec1-0ab1-485a-b423-133c4f743a07");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "16685651-4e60-4af3-9d5a-0efa69d58f37");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "eaf60dd2-2c62-4bc7-95af-bf60f6ec76f5");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8d231392-2793-4803-b2ae-a45ae07a21c7");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ed2ce0bd-dd22-4476-b8f9-9590caca767e");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c646301a-e26f-46e0-b50a-372a99781a99");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "75ef0e3e-a18d-4f2f-a8aa-e5be5b0a494a");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1bf262b7-6db1-4719-aa83-617e105a06be");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "25f477c3-d178-4e43-b603-514280a7c45f");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7a43e586-b395-4007-8c22-2d5b6b0fcd11");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "2cf355c3-f67b-448c-84b8-7c6c1727844e");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f89f7a9c-5cf4-4671-9988-e0ce019d06ca");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7faa9d15-016e-4efe-bd78-9a9442770f6f");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "7c4b7cd0-ce86-433d-8477-0ff1af2fc8a7");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "cb146175-a93f-4915-8bb2-66375eb474f2");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0aed70c7-ed93-4a77-9173-8155e423db6a");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0a3e4d84-17ce-41eb-a871-a36f8e2fcaa9");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1dc8e994-a60d-47ab-a742-d62970c2ba93");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "67f708aa-85d4-4f6b-8dc6-a04d1a9ba95e");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "a3af610e-316c-4108-841c-04cc7eeeec77");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "fee3f33e-2180-454c-8d14-ec612ef93141");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "ee1bdb12-7f6f-43bc-a854-f5a82b446c2f");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "75173d56-a8dd-4254-b5db-e9201f893b30");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "bb892180-4b9c-4901-9b1c-98261ca3b726");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8f65cba5-d571-47e9-a443-2f0731479cb3");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "134b3ab3-77a2-4b72-a18c-c6d629b375f3");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "489aa0d8-2a3c-4791-994d-ba246cf7aea3");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "232c81a6-dc75-46e4-a88a-469648b4707f");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "b6af4c1b-22e5-4955-a634-87b223b1bd2c");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "2b04ce1a-9b56-4d52-9863-72249fbfa1c7");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "27f17f58-bfa2-4195-b011-18531460aa4c");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "80bf7982-a048-488e-959a-8dba48254f67");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "a4ea5f99-c71e-41ba-a585-50ea8bee8380");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c09047db-ae86-4f56-a260-ed1e63c18b19");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "39c4cb0e-5a54-4182-a03b-d49700ec0e86");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "1940c425-c2d5-4c1c-a341-1b0dfcf173b4");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "d169815e-cfce-4e10-ba1b-a2368b0386b5");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "804bdd81-4217-45cf-b7ca-2ec66c6fbe05");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "c238d517-ffa5-41cf-a176-ac82929b8045");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "8571fd47-dcea-409c-bd29-4dbdf0864254");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5aeb4895-6730-49d6-9f0b-a32fdf16471a");
        return visitor.end();
    }

    /**
     * Get a fresh copy of the underlying data array.
     *
     * @return a copy of the underlying data array.
     */
    private double[][] copyOut() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "001f7b5b-c1fa-49a8-aa50-c069aec5a2bc");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "cce4dcf4-7a13-4542-b1b5-7f81bf4bd770");
        final double[][] out = new double[nRows][this.getColumnDimension()];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f6a0cf77-8c9e-4730-9d9e-34f79536847d");
        for (int i = 0; i < nRows; i++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "3d67ba95-a605-4f41-8761-461a3eac102d");
            System.arraycopy(data[i], 0, out[i], 0, data[i].length);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "dfd2813f-4207-4169-bba9-6186f10d69eb");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "636db49e-8c37-459e-ba62-c1cb8ee0fc26");
        setSubMatrix(in, 0, 0);
    }

    /** {@inheritDoc} */
    @Override
    public double[] getRow(final int row) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "da888c0a-08e2-41ed-86e8-a0344f93a1ba");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "0989e8ff-1612-45c6-b362-fda6ac11a3e3");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "a4842303-1c9a-4cc3-af61-535cdcf2b4f6");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "621234b6-b61a-4ab3-b8cf-65bc051ab4d4");
        System.arraycopy(data[row], 0, out, 0, nCols);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "5e2ecb17-e9a0-4442-9d76-e9d4efcf7a94");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public void setRow(final int row, final double[] array) throws OutOfRangeException, MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "efd03ed8-2835-400d-93eb-630de09be698");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "f3dd0e9b-142c-4150-adec-9f17b85a46cd");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "3c85a801-022a-4e15-aa8d-435f3fb7a4e2");
        if (array.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "73721c8b-9ed5-41a2-bbc1-9ad8825e90b9");
            throw new MatrixDimensionMismatchException(1, array.length, 1, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_2_10.coverage", "55d2f74e-ecae-435e-a908-207fceaaf468");
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
