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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "775febb4-e8d5-4c0d-9e5e-60cc3ec67135");
        return new Array2DRowRealMatrix(rowDimension, columnDimension);
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix copy() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "1907aac6-3746-45ce-9aba-aa657210bf88");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "bd3b066b-da8e-40bd-957c-b6f405d89006");
        MatrixUtils.checkAdditionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "20fbf385-7093-4996-adb6-943c0ba0f815");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "40902531-e21b-4457-b92e-77927f213442");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8c040765-0d2d-4d09-811c-a0d9daebc99f");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "0d394542-3179-43c6-8546-a7556815b006");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f9435707-dd51-46b4-a8c7-4b735f95a908");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "ddc220da-5985-4691-b2f5-2fe2174d0787");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "d6e7c20c-fa8e-445c-8432-518820aa9f70");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "1a7a7989-3d19-4b33-9f76-c9aaece98224");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "fbb04ac4-77d2-4456-872a-1ce85542661c");
                outDataRow[col] = dataRow[col] + mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "995b7cca-28fe-48a4-80e2-b2711e17624f");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8be2099c-757c-4e65-8de9-f8f45ff11ca8");
        MatrixUtils.checkSubtractionCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "01111cd5-448d-4354-b148-8c12897c6bdd");
        final int rowCount = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "336328c4-1f66-4898-95de-333c696b0a99");
        final int columnCount = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "72971564-dc48-4aed-bd7e-a2b816018430");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "69508985-7b9f-47af-9fbc-2cf852750a39");
        for (int row = 0; row < rowCount; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "5dc02791-b114-412b-8d61-f8382e18272c");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "e1aac362-79b1-4cc0-9fd1-f760cb11aa82");
            final double[] mRow = m.data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "55566b27-d90c-4fef-bce8-4b80e834f51a");
            final double[] outDataRow = outData[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "1b3d4d07-58dc-49b4-b3d7-4656b88c4184");
            for (int col = 0; col < columnCount; col++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "37b7f42f-ff89-4c55-94c3-572ebb5c9705");
                outDataRow[col] = dataRow[col] - mRow[col];
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "14d4b2b8-cbc4-4561-993b-1c09d34a2c30");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "fe4812f3-f76d-425d-a5fb-f2720f4eb0cd");
        MatrixUtils.checkMultiplicationCompatible(this, m);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f3709fa6-c8ca-4d5f-904b-0e9cbc03fe74");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "048de5f0-6ff6-462f-b3b2-777fb8b9843c");
        final int nCols = m.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "c0a2f4ab-6fb6-4093-bc34-9f77f250ac9b");
        final int nSum = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "48b1d5f9-43cb-44bf-96df-cdaeb9acd961");
        final double[][] outData = new double[nRows][nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "69edc674-ff62-4c83-aade-9889be2f39a7");
        final double[] mCol = new double[nSum];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "4afecc4b-110d-41e8-819c-bf1db84a1a5a");
        final double[][] mData = m.data;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "99834091-ee1e-4e72-bdf4-1f199b1c8cc6");
        for (int col = 0; col < nCols; col++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "73ce0e0b-e76e-44f2-888b-9c5dc712fe57");
            for (int mRow = 0; mRow < nSum; mRow++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "25c12e23-7424-41a5-a8c1-7c48679e347d");
                mCol[mRow] = mData[mRow][col];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "cebd5211-49f0-4d73-bb1a-d906166b29c3");
            for (int row = 0; row < nRows; row++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "4cefce79-72a0-4167-b715-df4fbfdb224b");
                final double[] dataRow = data[row];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6cdf8d33-6d40-4467-94e0-3ef1d84cd065");
                double sum = 0;
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3a50a656-aabc-4ecb-8077-492236fed510");
                for (int i = 0; i < nSum; i++) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "d23df34f-c924-499c-b5c6-a6c14524bd17");
                    sum += dataRow[i] * mCol[i];
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "76c84216-7df9-48c2-8596-8b342db63303");
                outData[row][col] = sum;
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "e11c468f-c980-4bca-a783-2f4c66d7c3f6");
        return new Array2DRowRealMatrix(outData, false);
    }

    /** {@inheritDoc} */
    @Override
    public double[][] getData() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "a82e9367-c8f4-4964-a9ca-122749954138");
        return copyOut();
    }

    /**
     * Get a reference to the underlying data array.
     *
     * @return 2-dimensional array of entries.
     */
    public double[][] getDataRef() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6a4d310a-4e29-41be-8469-b29b0a300671");
        return data;
    }

    /** {@inheritDoc} */
    @Override
    public void setSubMatrix(final double[][] subMatrix, final int row, final int column) throws NoDataException, OutOfRangeException, DimensionMismatchException, NullArgumentException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "5bfc40bc-602f-4059-8548-b99ccfaba73d");
        if (data == null) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9e22ad78-f29d-4925-a179-291f42140f6c");
            if (row > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f5968594-06b4-4153-8b39-12150b0eb4e1");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_ROWS_NOT_INITIALIZED_YET, row);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "da71840c-2123-4ad3-9620-4cbf04c1a326");
            if (column > 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9ffa884a-05c5-45c5-a00e-5f9771b5014f");
                throw new MathIllegalStateException(LocalizedFormats.FIRST_COLUMNS_NOT_INITIALIZED_YET, column);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "43bf81a7-8ce5-4d2d-8880-f31dd3e9e5aa");
            MathUtils.checkNotNull(subMatrix);
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "824a97a3-73ae-4fe1-88aa-0e1af67c334d");
            final int nRows = subMatrix.length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "35aece55-3627-40a9-b9c1-3a796e786f53");
            if (nRows == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "90221aef-44f2-45fa-840e-c8a3c54580d0");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_ROW);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "93846c75-2872-4c8d-990f-67ae987eb372");
            final int nCols = subMatrix[0].length;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3fe30efa-c1a1-4808-916d-36bc8f71e7d6");
            if (nCols == 0) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "63b5c64b-b7f3-49b1-85d3-a5551ee4cfb6");
                throw new NoDataException(LocalizedFormats.AT_LEAST_ONE_COLUMN);
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8507d52e-ffa9-489e-9b63-4d2e5b796272");
            data = new double[subMatrix.length][nCols];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7ed00561-3574-4f6b-a683-ccec90baf8ee");
            for (int i = 0; i < data.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "35b014ed-69a0-4eeb-9358-64bbf575d7a3");
                if (subMatrix[i].length != nCols) {
                    writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f777a6ea-f0de-428b-8baa-4b0445f19803");
                    throw new DimensionMismatchException(subMatrix[i].length, nCols);
                }
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "a3fafd3c-84a3-41fc-9c34-5599e8c54185");
                System.arraycopy(subMatrix[i], 0, data[i + row], column, nCols);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9dcfe482-84f4-4c09-9aef-31cbb1e1650a");
            super.setSubMatrix(subMatrix, row, column);
        }
    }

    /** {@inheritDoc} */
    @Override
    public double getEntry(final int row, final int column) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "176704be-facf-401e-b93e-bc34544ef0bd");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "c1a7b557-ead2-4015-bc59-5d83422036d7");
        return data[row][column];
    }

    /** {@inheritDoc} */
    @Override
    public void setEntry(final int row, final int column, final double value) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "39152de5-ebe4-449f-95db-8412447a71ad");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "513aa54c-d12b-4fca-82d0-dd81698df78a");
        data[row][column] = value;
    }

    /** {@inheritDoc} */
    @Override
    public void addToEntry(final int row, final int column, final double increment) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "86409048-7e40-44cf-a317-8205258f124e");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9a0d00c4-cdc5-406a-af58-c678bd29a211");
        data[row][column] += increment;
    }

    /** {@inheritDoc} */
    @Override
    public void multiplyEntry(final int row, final int column, final double factor) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "a6820e58-e991-4200-b912-2ddd4840e82e");
        MatrixUtils.checkMatrixIndex(this, row, column);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6446102a-e2a4-4a88-a317-5fd82de2ffde");
        data[row][column] *= factor;
    }

    /** {@inheritDoc} */
    @Override
    public int getRowDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "e6f14bf7-d070-406d-b71e-0e3973e39660");
        return (data == null) ? 0 : data.length;
    }

    /** {@inheritDoc} */
    @Override
    public int getColumnDimension() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7fdc93bf-ccd0-4104-a58a-b4b419313ff9");
        return ((data == null) || (data[0] == null)) ? 0 : data[0].length;
    }

    /** {@inheritDoc} */
    @Override
    public double[] operate(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "4aa6dbd5-55dc-4b84-a8b3-90ace72264ab");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "118effa5-36f1-403f-9d6f-2bb94545bfa9");
        final int nCols = this.getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "d4c55183-10f8-4dc5-9eec-5f82c0749f60");
        if (v.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "2a6dd0a6-3bf0-4462-9a5b-a46a65ae9af7");
            throw new DimensionMismatchException(v.length, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "98cb9168-e736-4305-b264-b15887760b14");
        final double[] out = new double[nRows];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f31dccca-d7b1-43d1-a590-ef9d14197edb");
        for (int row = 0; row < nRows; row++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8a2f937a-0f10-4d04-9aef-96a1e5b3eaae");
            final double[] dataRow = data[row];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "fa55ed6f-08b9-4000-b52d-0dd9cf227d7d");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "c17d1f88-e3a7-4fc1-87ad-d09c310780f4");
            for (int i = 0; i < nCols; i++) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7c5aef93-b9c0-4007-8774-8589726885a5");
                sum += dataRow[i] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "5395566b-4124-46cc-8bbb-2b67d98f5090");
            out[row] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "510390cd-3dcc-4d88-981b-51ddcebec1a8");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public double[] preMultiply(final double[] v) throws DimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "36b0fa07-600f-447c-846d-2951c8943850");
        final int nRows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8a68c751-22e6-4425-b6ef-3040d1d361de");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "30d3186a-3497-44da-a9e7-da82242fe04a");
        if (v.length != nRows) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "0c30cf09-6226-4a6b-b645-08f55cc6998a");
            throw new DimensionMismatchException(v.length, nRows);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "aa522707-38a8-48f5-b41b-bb8b1d6e197e");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f6f36590-b16a-41de-953a-57adf7f2590b");
        for (int col = 0; col < nCols; ++col) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "94b822da-edf6-4a7f-bcd4-e203a6ed7538");
            double sum = 0;
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "1c672f50-8c46-40c2-bf70-d8d81ecf03e0");
            for (int i = 0; i < nRows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "e9994351-e3b6-4a7a-93b0-c13118c68c9b");
                sum += data[i][col] * v[i];
            }
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "648ebf1b-4a26-4cec-83c2-e1ccf40a8cde");
            out[col] = sum;
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9117ea59-bc5c-4d72-afd7-a2b43c9b3efd");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public RealMatrix getSubMatrix(final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3d50d77f-5729-4267-98ae-8a464ea84965");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "d3044774-ca74-45af-8990-54b1ef194b58");
        final int rowCount = endRow - startRow + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "178b2684-bea5-4975-b017-b6f2198e7867");
        final int columnCount = endColumn - startColumn + 1;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8dad2c3e-6553-44a4-a2d8-7ff007d9c519");
        final double[][] outData = new double[rowCount][columnCount];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "078125af-f8b6-4277-8d6b-c60edaa3c1a0");
        for (int i = 0; i < rowCount; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3e4bf8fd-3cb2-4c3f-b679-955997c927bd");
            System.arraycopy(data[startRow + i], startColumn, outData[i], 0, columnCount);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "ea4923ce-0ae1-477b-b8eb-dccef28a92b3");
        Array2DRowRealMatrix subMatrix = new Array2DRowRealMatrix();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "538791dd-e6bc-466f-882f-1f26bb9a8f81");
        subMatrix.data = outData;
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b9699ee6-24ae-404a-80f3-4ec8ae776f9d");
        return subMatrix;
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "0ed8bf10-4ec4-48b5-bbf5-2ed2075a9050");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "cf0b0022-32af-4be0-bbaa-4654a56cb433");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "75e66a22-7f9c-4d7a-ab05-3b8ed6063f28");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "2bc330b6-742f-45a4-ae9f-efb5525b76fb");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "0786163a-faa6-4c73-a379-543ceaf16140");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "43012fb7-2b61-4897-8ff0-d8af4dc34f0c");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3e4c3367-3537-4f2e-b678-141a156cd89b");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6a8057a7-95f8-411e-8409-3fc39bee576a");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6da14b3d-83b3-4f8e-baf7-dd3632a53b1e");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "17e05fe6-7129-41c9-8aaa-04aba014d442");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "2976664f-eb19-4d7a-9a07-7d0782a4aee8");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "646df6c3-9505-4e19-8484-a0f83f3ddc6b");
        for (int i = 0; i < rows; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b85d22ea-35a5-42f2-90b0-986968a8f101");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "59630d6c-6b3c-4fb1-a57d-2a152f5b24c5");
            for (int j = 0; j < columns; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f9cb8fad-86bc-445c-b7c9-d2f7caf9a212");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b6afee06-098b-49d1-ae1f-8eb6ac91493b");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "0bd6eb9e-3dba-4a46-bf52-d1ea637d77d7");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "ce3a3517-4bbb-4c00-a6bc-109e894563de");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "00726232-3891-474d-a5f9-38f06550b435");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "39b8c84a-1a10-4fef-8bca-bdab1d1684aa");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3c7bfb34-98b9-4ff3-a4fc-7e4172131786");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "c54827a4-dc3d-4eda-8a64-501a2eaf62ac");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "5d4d98dc-a0a6-4b7c-900e-e26d3d8baad9");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInRowOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b7490b3b-fb0f-49e4-8cb9-93a00f95c28d");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "dac190c5-bde4-403d-81a4-da04865e60e0");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6f699b66-9262-465b-ba89-a0b3e7eabcdc");
        for (int i = startRow; i <= endRow; ++i) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f9bd8edd-129b-4b76-bd25-23b645104ae0");
            final double[] rowI = data[i];
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "1a19f34e-9df5-4ba7-8f22-94f813a26ad2");
            for (int j = startColumn; j <= endColumn; ++j) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "ff0ab307-de46-44e3-ae2f-70b5362945cc");
                visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7adb515e-6e4c-413a-bca3-9b238a5dd6aa");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "39edea99-b583-4cd8-a14c-5772d9579cd2");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "572636d9-42ab-424b-9f48-1c976e0d554b");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "98529f14-0a44-42e6-bbcf-4d4068389168");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3f2a5cbe-284f-409c-9f50-60b24a7aab94");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "4ee9354b-2e38-4193-ab64-85f16242f178");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "56e5769e-b9ee-4bbf-be76-6aa410dece34");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "729bce6e-c525-4d8a-aeac-89986f57f54a");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6823805a-9117-466d-8713-65e028ed6127");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor) {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "3af2f3f1-3bb8-42b4-83c3-debf07773e48");
        final int rows = getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "634ea872-7f5c-449d-be5b-6241a18ed012");
        final int columns = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f3166977-21b3-49cf-b5e8-c20cbfcc801e");
        visitor.start(rows, columns, 0, rows - 1, 0, columns - 1);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "0d316259-a64c-4e00-8c5d-561f118be4aa");
        for (int j = 0; j < columns; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b09fb67e-6521-441c-a328-4da7471c2803");
            for (int i = 0; i < rows; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6964ef02-0c7d-41a1-95e5-8865d5f0214b");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "5b8566ce-852c-416e-854a-3858b0d32450");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixChangingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7ff16b88-0fbc-46b7-8ff3-f5318af93aaa");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "93a532e3-360a-4b4e-93d6-5198aea9abd5");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b2d67cdd-090c-4100-811d-8dab3cbc0562");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "170c23cb-ddbc-4310-a63b-759dbb8ede46");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "2d0788cd-36e7-41cc-a35c-5c4a3c98cea8");
                final double[] rowI = data[i];
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "59963674-30a6-44b1-904c-abb23e3f71db");
                rowI[j] = visitor.visit(i, j, rowI[j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f45f690e-57ce-4c5e-acc4-c2b78d07c87e");
        return visitor.end();
    }

    /** {@inheritDoc} */
    @Override
    public double walkInColumnOrder(final RealMatrixPreservingVisitor visitor, final int startRow, final int endRow, final int startColumn, final int endColumn) throws OutOfRangeException, NumberIsTooSmallException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "61a36cdd-0e97-49da-a659-6dcca0027296");
        MatrixUtils.checkSubMatrixIndex(this, startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7c842d9a-a4d9-487b-b062-31c248175aef");
        visitor.start(getRowDimension(), getColumnDimension(), startRow, endRow, startColumn, endColumn);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8cff425d-f3b7-4676-b9c0-2a3e9b8239da");
        for (int j = startColumn; j <= endColumn; ++j) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "b7f011f5-c0a1-46e7-8530-afbcf1d7962e");
            for (int i = startRow; i <= endRow; ++i) {
                writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "dbd27c23-afaa-4e99-810e-5178ca8959da");
                visitor.visit(i, j, data[i][j]);
            }
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7cde698f-dd76-4e1d-a81d-c4996514defb");
        return visitor.end();
    }

    /**
     * Get a fresh copy of the underlying data array.
     *
     * @return a copy of the underlying data array.
     */
    private double[][] copyOut() {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "562d8b45-ecfe-4eb0-a053-159347bb1c05");
        final int nRows = this.getRowDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "7c6ef679-f5a4-468f-8e7b-68afe08c596d");
        final double[][] out = new double[nRows][this.getColumnDimension()];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f0c8afc0-f306-4c05-b746-b21ed6779dcb");
        for (int i = 0; i < nRows; i++) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9a80ffd9-5e3f-448b-a5cb-49d28221145c");
            System.arraycopy(data[i], 0, out[i], 0, data[i].length);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "71caabd8-b8a4-4a41-afb4-b2828654fc0d");
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
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "d704f158-a942-4500-a218-6a05ea33b951");
        setSubMatrix(in, 0, 0);
    }

    /** {@inheritDoc} */
    @Override
    public double[] getRow(final int row) throws OutOfRangeException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "aab471ca-6c8b-4e79-8922-058fb316c3a8");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "c486dab4-7f98-485d-89f7-852e2c6c3f8d");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "f84379de-7a4d-4558-a844-6f4c3b4b664f");
        final double[] out = new double[nCols];
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "4f4a5c8c-b737-4077-9e16-f512ae55e360");
        System.arraycopy(data[row], 0, out, 0, nCols);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6de06a56-c648-45c8-8eb0-c806fb585263");
        return out;
    }

    /** {@inheritDoc} */
    @Override
    public void setRow(final int row, final double[] array) throws OutOfRangeException, MatrixDimensionMismatchException {
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "e7723e03-575a-46ce-a6d2-819425315303");
        MatrixUtils.checkRowIndex(this, row);
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "9f7c32e4-6490-4f48-bfd5-6524a17b6f0a");
        final int nCols = getColumnDimension();
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "843ad15a-8467-433e-8bf8-1a803ade0ba8");
        if (array.length != nCols) {
            writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "8e76abc0-05bc-453e-ab30-965d555f8d00");
            throw new MatrixDimensionMismatchException(1, array.length, 1, nCols);
        }
        writeline("/home/ubuntu/results/coverage/Array2DRowRealMatrix/Array2DRowRealMatrix_5_10.coverage", "6003f49c-08ed-44f7-9123-d1a1c2f704ec");
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
