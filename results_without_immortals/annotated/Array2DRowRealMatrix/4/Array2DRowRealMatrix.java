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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2efa7461-af94-4cf3-a802-94438205f989");
        return new Array2DRowRealMatrix(rowDimension, columnDimension);
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix copy() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e7a22e27-1fff-4f5b-90ad-306e00a20ec9");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "569f78f1-acc1-49a1-aab0-6350b4dd9731");
        MatrixUtils.checkAdditionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "6e37729c-0560-4f82-91a9-646ae4d1cdc4");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e8c55038-ea5a-47b1-8783-3fdb5f09950a");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "03f2783e-c915-48c0-af6c-57dbca1d6cb7");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "765dd6d3-1777-4228-b00a-f507c270acf9");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1a94fe99-5f4d-4de2-a61b-b62b5ba3f519");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "7bc738f5-d2af-4be3-8206-ba7b4b7fa259");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1bea481c-f27c-46df-a4ac-ea503af085c9");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c47eec0f-79f9-47c8-b25f-e9d5d7a43f88");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "cda2fafd-f429-4947-9442-c4476579214c");
                outDataRow[col] = dataRow[col] + mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "0c70f6cd-a643-4a9f-849d-67da84fc1463");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "78141e75-ea98-4a0b-8606-fdede79b29eb");
        MatrixUtils.checkSubtractionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ee50ef95-a634-40d4-9fae-c8c67557d03c");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "596912e0-6bf6-4121-8192-9d024f4913d0");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2e030d39-a830-4b7b-b43f-cce4fe21a570");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f8c65b82-c73c-4a30-bebd-f67a7860310d");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2305aaae-fa7c-428f-9eaa-c6c691db1f51");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c71deec2-deb2-4b1a-902e-eeadba239cc6");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e98b3eaf-3050-48d5-8c7d-38c3a9097c39");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "323a7555-5b64-4ef9-a1e6-694ccc8443cf");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "4e093d07-fd8a-4648-a9c5-da0aedfcf8a8");
                outDataRow[col] = dataRow[col] - mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "5f3621d9-a8b5-44f7-a63b-7a35f0d33802");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a9263aec-e8aa-41c7-8d0b-5f2f8d038d64");
        MatrixUtils.checkMultiplicationCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f3152b6f-b8ab-41f5-a1ac-b0bba223d8ec");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c7531025-ba6d-49b3-be51-7176d5d6105b");
        final int nCols = m.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "115a20d2-bfbc-4e46-ab31-5735437cb5e7");
        final int nSum = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "9ffa3bb8-e3de-4752-87c8-f1fde460e696");
        final double[][] outData = new double[nRows][nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "03c08981-d8c5-4eb2-ac42-b479084d20bf");
        final double[] mCol = new double[nSum];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c62931d7-69d0-4285-a97e-36ed00535e5b");
        final double[][] mData = m.data;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "23b0b5eb-587d-44d6-92b0-48e7182cace6");
        for (int col = 0; col < nCols; col++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "dd42c8a8-0798-4a91-8fd1-8491de921206");
            for (int mRow = 0; mRow < nSum; mRow++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "71a0354e-17e0-43a5-8d97-32b73b45db36");
                mCol[mRow] = mData[mRow][col];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ace9ba73-a72b-492d-af84-6a87b20e36f4");
            for (int row = 0; row < nRows; row++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "cc9ebf6a-1372-435f-b9a8-b565893b768c");
                final double[] dataRow = data[row];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "878958ba-b418-41f9-8f2f-ec78785b7aa5");
                double sum = 0;
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "3741789c-05f7-49d2-9c88-6533f69626eb");
                for (int i = 0; i < nSum; i++) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "fb0789c5-ef30-482b-95d9-48eb1349e9b7");
                    sum += dataRow[i] * mCol[i];
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "45372d0d-6df1-4696-bab2-ec6d0371f82c");
                outData[row][col] = sum;
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b56215c4-fa3d-46d1-b199-956a43902368");
        return new Array2DRowRealMatrix(outData, false);
    }

    /** {@inheritDoc} */
    @Override
    public double[][] getData() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "d9ad0127-b7c1-406c-a4b0-1c6838a2515b");
        return copyOut();
    }

    /**
     * Get a reference to the underlying data array.
     *
     * @return 2-dimensional array of entries.
     */
    public double[][] getDataRef() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b520b704-fe64-4524-a983-8376cd7e856a");
        return data;
    }

    /** {@inheritDoc} */
    @Override
    public void setSubMatrix(final double[][] subMatrix, final int row, final int column) throws NoDataException, OutOfRangeException, DimensionMismatchException, NullArgumentException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "0c85e41c-3b85-4252-bef1-87dd8d15b5c2");
        if (data == null) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "698f7e71-76fa-452b-a071-7d04cae3969f");
            if (row > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c4788471-7586-41fd-a347-4d244da1cdd2");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_ROWS_NOT_INITIALIZED_YET, row);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "70c32663-93dd-4cbc-ac1d-c1065fd0756c");
            if (column > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a9b7d03f-3d46-455f-8854-c1949465d7e8");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_COLUMNS_NOT_INITIALIZED_YET, column);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f0a4d9da-a528-460f-8ad4-70dc5ca892f5");
            MathUtils.checkNotNull(subMatrix);
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a1858ba3-23df-4c9b-8f29-136bb03563c7");
            final int nRows = subMatrix.length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "3804038f-2a80-4328-8834-0b478241f716");
            if (nRows == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a3b36e92-29c3-42f7-9194-998899a86799");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "dfb02e48-fd1d-4f8c-b814-2356d78cbb5b");
            final int nCols = subMatrix[0].length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2f971fc6-1dcb-457a-8d88-9df2a8e6c989");
            if (nCols == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "d8d1e895-3266-462d-bcbd-7756aee1ef93");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "4ff623d3-1bb2-44e6-bcab-4ef36ef56c0a");
            data = new double[subMatrix.length][nCols];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "572ea965-663e-4737-82e0-7cb538cd26a8");
            for (int i = 0; i < data.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "d2402955-c61e-4f5f-9c3b-d83690f61d88");
                if (subMatrix[i].length != nCols) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "d0ae7cb3-bfb5-46a9-9f45-1f69aa0e9099");
                    throw new DimensionMismatchException(subMatrix[i].length, nCols);
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a3a74ea1-9e7a-4c5d-bf76-cbd08a20580c");
                System.arraycopy(subMatrix[i], 0, data[i + row], column, nCols);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "7df16f7d-e1c2-4f4d-9e61-6aea46122166");
            super.setSubMatrix(subMatrix, row, column);
        }
    }

    /** {@inheritDoc} */
    @Override
    public double getEntry(final int row, final int column) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "9fd58cc4-d625-4859-881c-ffd686b83702");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "efb26d63-b7fd-42cf-9822-645ee4448ffe");
        return data[row][column];
    }

    /** {@inheritDoc} */
    @Override
    public void setEntry(final int row, final int column, final double value) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "414a965e-a841-4750-821f-4da217b0db4b");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "377bd9ec-d470-41c5-8c09-edffc802d034");
        data[row][column] = value;
    }

    /** {@inheritDoc} */
    @Override
    public void addToEntry(final int row, final int column, final double increment) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f53f28aa-0ba4-4a31-b5e9-c40bdf5e23ea");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ef53205a-df9f-42eb-8bbb-224335592c72");
        data[row][column] += increment;
    }

    /** {@inheritDoc} */
    @Override
    public void multiplyEntry(final int row, final int column, final double factor) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "29ad9e60-a910-44f4-866e-a1654bbee35b");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "002e8693-a730-4d9f-937f-88532921d47d");
        data[row][column] *= factor;
    }

    /** {@inheritDoc} */
    @Override
    public int getRowDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "83d325f9-d080-4769-84a0-3ffecd06676d");
        return (data == null) ? 0 : data.length;
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "6ab55112-b0c1-4fa3-bc3e-432f6cf345d6");
        return ((data == null) || (data[0] == null)) ? 0 : data[0].length;
    }

    /** {@inheritDoc} */
    @Override
    public double[] operate(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "616e608e-d908-4523-bc68-8059238ca404");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "439e047f-d70d-4640-ad4a-4c480543fada");
        final int nCols = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "48cc128c-ca29-46d2-b32b-b6dbe4c87aa6");
        if (v.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "d81529fa-51aa-43b1-aa48-1162c3107847");
            throw new DimensionMismatchException(v.length, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f019aa68-d65e-4b29-9f05-101a4e517773");
        final double[] out = new double[nRows];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c5b965a3-7e55-4f14-8824-e8920c754657");
        for (int row = 0; row < nRows; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e251b1b3-f177-44c3-9f38-ec809f468d8e");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b9a23e3e-67ee-4830-969f-ac83ab37480c");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "51d30e1b-90dd-41bd-982f-9ef7ed8e95cb");
            for (int i = 0; i < nCols; i++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ff19669d-aa3c-4d51-b10d-68b7a125a34b");
                sum += dataRow[i] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f3cd5647-d479-4bc0-8104-103e85e99889");
            out[row] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "361eab7e-186f-4ef7-8ad0-e26d7a89b653");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public double[] preMultiply(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "8fc91faf-c36d-4193-aad5-37c139d0bc24");
        final int nRows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "73b91515-808b-42ed-b27b-56d2489c2ee8");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2d7052a9-acec-4602-aaea-d3559c4e21cb");
        if (v.length != nRows) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1586b8ce-8900-40eb-b952-284984a34bc3");
            throw new DimensionMismatchException(v.length, nRows);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "dc72e247-9780-4da5-95e8-c9a08ffb4c64");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "262d4457-2c6d-47ca-be58-2da57eda5a7f");
        for (int col = 0; col < nCols; ++col) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "4b6babca-51f5-4043-b252-2fc816aec4ae");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a4f80594-9d17-42cc-ac8c-8e0e717cce41");
            for (int i = 0; i < nRows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c5fbdcac-d6b0-472d-9cfe-6a3ab940885e");
                sum += data[i][col] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a630fc76-68d9-4f14-af24-7e5e0251944f");
            out[col] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1760ce51-93c5-4b9f-a33e-2b9c5a9e513e");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix getSubMatrix(final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b2464534-d419-4dd3-9b41-91742c709a8e");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "613b1dea-33cd-44c0-8b7b-b535b8db147b");
        final int rowCount = endRow - startRow + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c6f88e66-657b-4429-b260-d30a365acf20");
        final int columnCount = endColumn - startColumn + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "0a778487-2242-4ff8-a038-9a5c8dd4a556");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e9adbf45-7626-4a96-8341-6acb461830cb");
        for (int i = 0; i < rowCount; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c7696b4e-4c94-4e10-a2bb-24a21d64637d");
            System.arraycopy(data[startRow + i], startColumn, outData[i], 0, columnCount);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f8bd71c6-281b-4c6a-ae73-bdbfe07c9e43");
        Array2DRowRealMatrix subMatrix = new Array2DRowRealMatrix();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a73c7dcf-86c8-43e8-ba45-951634202785");
        subMatrix.data = outData;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "7675ca3e-7c8c-43a6-91d6-76da22194173");
        return subMatrix;
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "8af7b5c9-d390-4575-871a-82d188d350fd");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c354464f-6521-4774-8644-2dc5586f1d33");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "3c375eb0-11b4-47bc-84f4-c376d74355a6");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2a4435b1-0338-4aaa-a28a-08af646d4f59");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ebe0ceb9-1b0a-44b2-a9fd-151c6caad2ec");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e150a9af-a632-4633-8166-40b9ebc3f746");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b07b6e06-8ed9-4a3b-8cb1-c00fb00746c0");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "fa5490da-6488-4558-8056-b68d65a34836");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "0945cea5-f510-4c23-8cae-a5182559149d");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "605145be-b9aa-46d7-ab59-271e84bae047");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2deae4a1-98a0-4a09-b380-839644241b94");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "097c6fff-68e3-4485-83f2-8cc6935a5f0d");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "6e27161b-b80a-4fab-908e-093a0b7446e3");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "fb928380-2a20-429f-bfb7-23f5171d1a97");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "41631898-d93a-48a6-afbb-fe05970ea934");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "5871ebd2-fb1f-4341-86ab-05e8f96fd386");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "00cd6347-1347-4837-b378-c0d0c34132a5");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c56ab929-684f-4022-bce4-dd0a947c5f19");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "186dbc80-74f7-4c01-b70d-71f93e68ee68");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "fe672e74-8aee-4dda-9c6d-1612c85d9f34");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "6d67a6a1-1881-4621-8d41-23b89e9cec6a");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "feaba4a7-f0f3-4cdf-9610-69514639e243");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "6d9da171-70b8-4985-b0b5-b37d0b6959fa");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ec3795eb-5479-4c2f-94ca-eccd4f122d98");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "05c1f5c4-e6a9-4ba2-b2c8-90d3bd46ab9c");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2f21fce9-7b95-4e6a-a15c-945db39c5e3f");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b0550e41-574a-4a04-a7a7-2a894eb4719f");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "649b042b-53c9-4edd-ac1d-f5c6e007b0b6");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "124cfea1-f163-477b-acfe-8306c81fe40f");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "3ecd1bcf-ab76-44a1-bb74-7b975e33af12");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c897978f-a5f8-4ad7-a7db-863940757e0a");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "40a541c1-2b9a-4642-9a33-9e054979d149");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "667e56db-0a2a-4bcc-866b-590beb1b2b84");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1c06ed4a-a381-4d35-bd87-d76a1dc515c2");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "6ed93317-8a12-45a1-b82a-66c132c35c84");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "52b3cbc0-97b4-436f-8ea8-94dba2ce2a2b");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "8e49e295-8d21-4aff-b87e-ce41701fd610");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ca003645-3cc0-4ba6-82c1-c72eee266eb0");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e5985622-2e01-4fb6-8282-cc7f6336c417");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "0b993e1c-916f-4d4a-b6b6-9cf2aae136b8");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "62c87e3f-b6b7-4235-ab89-1a5ca20f845b");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a906e6d4-c4a2-472b-995d-b26591ddb9d4");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a689214e-448f-414c-be5a-e473cf542e03");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1f192848-9cf5-4350-b6ea-365f69d0221a");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "64d800ed-2a95-43b5-811b-fcce64a422da");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "87077c26-4f63-4e0f-9d34-88e89ede6f92");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f80ca437-a509-45cb-9c89-b51c4eff4e68");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "9557ff55-da06-474c-ac97-940b57a88efa");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "0daf3430-74fe-4672-970e-83fde56b3a2e");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c9d8c67c-ab1c-46e0-b3d2-c444e151f0ee");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "903d26b5-4ed8-4409-89a3-afe8edfd9a94");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f565495e-807d-43fc-86d9-bf5b048f4b28");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "8ce21cdf-cc57-477d-8e5d-2a1c165d7e08");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b25676fa-750f-4314-8db6-7fe5bdea58f5");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2744556f-ed76-4d1f-98eb-b738571a0fa0");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "b4f9bc51-130c-4baa-a8a8-43885eb6cd40");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "52886376-af0b-4be4-aede-3245d01e52b8");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "dedc0810-1e4c-43d8-b17d-c0cb14810186");
        return visitor.end();
    }

    /**
     * Get a fresh copy of the underlying data array.
     *
     * @return a copy of the underlying data array.
     */
    private double[][] copyOut() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "c9cd86e3-bfc2-42b6-9e81-4cb085b19c04");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "d873d66f-cf6e-4332-a112-63ed3a6ec50f");
        final double[][] out = new double[nRows][this.getColumnDimension()];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "2e1529ee-d423-4dc3-b247-5bd5b7721311");
        for (int i = 0; i < nRows; i++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "cc6e8b3a-b6b2-4b91-b6cc-10c4141b6270");
            System.arraycopy(data[i], 0, out[i], 0, data[i].length);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "efb8561d-54a6-4cf1-a961-4855bd5bb8c6");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "a31ee321-443a-4023-8dd3-f35312ec1a4f");
        setSubMatrix(in, 0, 0);
    }

    /** {@inheritDoc} */
    @Override
    public double[] getRow(final int row) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "ddf17d0b-589f-4cd3-8f24-7a4c28505fb6");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "5acb46d8-ea0c-40b7-a4b8-5091d5729fb5");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "82e01b74-bb2f-47c1-8ba5-244066a22a06");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "1043ac7d-abb3-4a12-b55d-7e2a8ab87286");
        System.arraycopy(data[row], 0, out, 0, nCols);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "e7718f34-ca9f-423e-b874-9261f843f913");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public void setRow(final int row, final double[] array) throws OutOfRangeException, MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "415afb0d-1b04-4256-843d-84ab6312081f");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "f634c219-c9ca-4a89-bd08-2a910dbed004");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "5a4a9b04-e4b8-4bc9-951a-6b802b106e47");
        if (array.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "936fc1b6-ae6e-4e07-ab60-895c72c710c1");
            throw new MatrixDimensionMismatchException(1, array.length, 1, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_4_10.coverage", "8314e3f9-8780-4b75-98ee-98c33f644732");
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
