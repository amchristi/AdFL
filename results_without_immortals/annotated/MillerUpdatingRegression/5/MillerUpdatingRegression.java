package org.apache.commons.math4.stat.regression;

import java.util.Arrays;
import org.apache.commons.math4.exception.util.LocalizedFormats;
import org.apache.commons.math4.util.FastMath;
import org.apache.commons.math4.util.MathArrays;
import org.apache.commons.numbers.core.Precision;
import java.io.*;

/**
 * This class is a concrete implementation of the {@link UpdatingMultipleLinearRegression} interface.
 *
 * <p>The algorithm is described in: <pre>
 * Algorithm AS 274: Least Squares Routines to Supplement Those of Gentleman
 * Author(s): Alan J. Miller
 * Source: Journal of the Royal Statistical Society.
 * Series C (Applied Statistics), Vol. 41, No. 2
 * (1992), pp. 458-478
 * Published by: Blackwell Publishing for the Royal Statistical Society
 * Stable URL: http://www.jstor.org/stable/2347583 </pre>
 *
 * <p>This method for multiple regression forms the solution to the OLS problem
 * by updating the QR decomposition as described by Gentleman.</p>
 *
 * @since 3.0
 */
public class MillerUpdatingRegression implements UpdatingMultipleLinearRegression {

    /** number of variables in regression */
    private final int nvars;

    /** diagonals of cross products matrix */
    private final double[] d;

    /** the elements of the R`Y */
    private final double[] rhs;

    /** the off diagonal portion of the R matrix */
    private final double[] r;

    /** the tolerance for each of the variables */
    private final double[] tol;

    /** residual sum of squares for all nested regressions */
    private final double[] rss;

    /** order of the regressors */
    private final int[] vorder;

    /** scratch space for tolerance calc */
    private final double[] work_tolset;

    /** number of observations entered */
    private long nobs = 0;

    /** sum of squared errors of largest regression */
    private double sserr = 0.0;

    /** has rss been called? */
    private boolean rss_set = false;

    /** has the tolerance setting method been called */
    private boolean tol_set = false;

    /** flags for variables with linear dependency problems */
    private final boolean[] lindep;

    /** singular x values */
    private final double[] x_sing;

    /** workspace for singularity method */
    private final double[] work_sing;

    /** summation of Y variable */
    private double sumy = 0.0;

    /** summation of squared Y values */
    private double sumsqy = 0.0;

    /** boolean flag whether a regression constant is added */
    private final boolean hasIntercept;

    /** zero tolerance */
    private final double epsilon;

    /**
     *  Set the default constructor to private access
     *  to prevent inadvertent instantiation
     */
    @SuppressWarnings("unused")
    private MillerUpdatingRegression() {
        this(-1, false, Double.NaN);
    }

    /**
     * This is the augmented constructor for the MillerUpdatingRegression class.
     *
     * @param numberOfVariables number of regressors to expect, not including constant
     * @param includeConstant include a constant automatically
     * @param errorTolerance  zero tolerance, how machine zero is determined
     * @throws ModelSpecificationException if {@code numberOfVariables is less than 1}
     */
    public MillerUpdatingRegression(int numberOfVariables, boolean includeConstant, double errorTolerance) throws ModelSpecificationException {
        if (numberOfVariables < 1) {
            throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
        }
        if (includeConstant) {
            this.nvars = numberOfVariables + 1;
        } else {
            this.nvars = numberOfVariables;
        }
        this.hasIntercept = includeConstant;
        this.nobs = 0;
        this.d = new double[this.nvars];
        this.rhs = new double[this.nvars];
        this.r = new double[this.nvars * (this.nvars - 1) / 2];
        this.tol = new double[this.nvars];
        this.rss = new double[this.nvars];
        this.vorder = new int[this.nvars];
        this.x_sing = new double[this.nvars];
        this.work_sing = new double[this.nvars];
        this.work_tolset = new double[this.nvars];
        this.lindep = new boolean[this.nvars];
        for (int i = 0; i < this.nvars; i++) {
            vorder[i] = i;
        }
        if (errorTolerance > 0) {
            this.epsilon = errorTolerance;
        } else {
            this.epsilon = -errorTolerance;
        }
    }

    /**
     * Primary constructor for the MillerUpdatingRegression.
     *
     * @param numberOfVariables maximum number of potential regressors
     * @param includeConstant include a constant automatically
     * @throws ModelSpecificationException if {@code numberOfVariables is less than 1}
     */
    public MillerUpdatingRegression(int numberOfVariables, boolean includeConstant) throws ModelSpecificationException {
        this(numberOfVariables, includeConstant, Precision.EPSILON);
    }

    /**
     * A getter method which determines whether a constant is included.
     * @return true regression has an intercept, false no intercept
     */
    @Override
    public boolean hasIntercept() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "efe3c262-7191-4ba8-8787-cea5b294c2f8");
        return this.hasIntercept;
    }

    /**
     * Gets the number of observations added to the regression model.
     * @return number of observations
     */
    @Override
    public long getN() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "bc9a934c-a86e-481c-a392-320cbec06711");
        return this.nobs;
    }

    /**
     * Adds an observation to the regression model.
     * @param x the array with regressor values
     * @param y  the value of dependent variable given these regressors
     * @exception ModelSpecificationException if the length of {@code x} does not equal
     * the number of independent variables in the model
     */
    @Override
    public void addObservation(final double[] x, final double y) throws ModelSpecificationException {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e53618ef-5ee8-4fe4-95cc-0e48297e90e1");
        if ((!this.hasIntercept && x.length != nvars) || (this.hasIntercept && x.length + 1 != nvars)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "80b7a0d7-8c6a-4ad3-ad2e-86d3335fec7d");
            throw new ModelSpecificationException(LocalizedFormats.INVALID_REGRESSION_OBSERVATION, x.length, nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "89758e0c-41c3-4d04-9c34-09285954d0d0");
        if (!this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f9089187-0a30-46e5-8c00-1b76c7f2424b");
            include(MathArrays.copyOf(x, x.length), 1.0, y);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "19a31c4e-60fe-4d8d-a400-901f05e317bf");
            final double[] tmp = new double[x.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "188334bb-d71d-4dc3-9c02-37f8e8deeb7e");
            System.arraycopy(x, 0, tmp, 1, x.length);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "cd5896b9-8f99-4446-a0af-89a7cd61145f");
            tmp[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4bcf567a-d854-41c0-8460-57829021215f");
            include(tmp, 1.0, y);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a688bbd4-ad90-476b-8900-16cc34e646ff");
        ++nobs;
    }

    /**
     * Adds multiple observations to the model.
     * @param x observations on the regressors
     * @param y observations on the regressand
     * @throws ModelSpecificationException if {@code x} is not rectangular, does not match
     * the length of {@code y} or does not contain sufficient data to estimate the model
     */
    @Override
    public void addObservations(double[][] x, double[] y) throws ModelSpecificationException {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ac9a0a59-943c-49ba-b9ea-8fa6f6df26d4");
        if ((x == null) || (y == null) || (x.length != y.length)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f9fb8776-0bf2-4805-9ec3-74d8ad7c5d9a");
            throw new ModelSpecificationException(LocalizedFormats.DIMENSIONS_MISMATCH_SIMPLE, (x == null) ? 0 : x.length, (y == null) ? 0 : y.length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ace68f16-357c-4416-8d7e-310c111fed54");
        if (x.length == 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b73a8e04-cec0-47b4-bf15-3398b3772ff8");
            throw new ModelSpecificationException(LocalizedFormats.NO_DATA);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "61ee60cf-a2fb-4f46-8927-78b98493bb2f");
        if (x[0].length + 1 > x.length) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d3ce44a0-a830-48f5-bcbf-f214f8729567");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, x.length, x[0].length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a7e4c6ba-aacc-4df7-aa6c-0738b72291df");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "dbfbce77-e042-4181-ae00-9aa9c1c962ee");
            addObservation(x[i], y[i]);
        }
    }

    /**
     * The include method is where the QR decomposition occurs. This statement forms all
     * intermediate data which will be used for all derivative measures.
     * According to the miller paper, note that in the original implementation the x vector
     * is overwritten. In this implementation, the include method is passed a copy of the
     * original data vector so that there is no contamination of the data. Additionally,
     * this method differs slightly from Gentleman's method, in that the assumption is
     * of dense design matrices, there is some advantage in using the original gentleman algorithm
     * on sparse matrices.
     *
     * @param x observations on the regressors
     * @param wi weight of the this observation (-1,1)
     * @param yi observation on the regressand
     */
    private void include(final double[] x, final double wi, final double yi) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "586b444e-06df-4be6-a7e6-1b174aac01ee");
        int nextr = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8196da61-37e4-451a-9bf9-4e83628c4255");
        double w = wi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c2cf3466-a326-4482-84d9-923e3e7b26e1");
        double y = yi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "024cffcd-72b3-4416-aeda-841c57db705c");
        double xi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1fdbd056-5657-4895-af0f-974941e31825");
        double di;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4d2b4863-44d9-4207-81fe-2694e28517fb");
        double wxi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "86008a5b-9564-4d7f-af8a-f204f261bdda");
        double dpi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "71cc88a1-1e82-4de4-accc-4c4d15b95484");
        double xk;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7ef0885d-0260-454e-98a7-b29841952e9b");
        double _w;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e4a6e8c3-9a1f-4f46-8c93-3eed24dba8bf");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e40bb209-352c-4701-90a8-b0b6380a8c39");
        sumy = smartAdd(yi, sumy);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9b902a90-40c6-45c5-9ed9-d666954206fa");
        sumsqy = smartAdd(sumsqy, yi * yi);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7a05ac7c-6d2f-4c35-8747-d84be9a95c9a");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "728a8979-675f-489e-87f2-c955ea955e4f");
            if (w == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "cd0f971f-b670-4a2a-a85b-832281b115ca");
                return;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "137e8390-a248-4d3d-b9fc-38430c985c38");
            xi = x[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ba90b749-cfbc-422d-9476-a929b1ef6561");
            if (xi == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3e5bfc73-0fa4-4297-8a62-64ed4b8f54b9");
                nextr += nvars - i - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "cbde5442-6a96-4526-8668-0a1b5914a55e");
                continue;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "26f5ed3d-dc3f-418f-8747-552c9679615c");
            di = d[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d8288b56-f245-4f3a-9b4b-d9f66bf0250c");
            wxi = w * xi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "447fe30b-e9ce-457e-9cca-766063b19785");
            _w = w;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2e642308-f351-4e05-97b3-df709f71f2c2");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "dc76f7d1-9964-4f5e-8f9d-df0e075318f8");
                dpi = smartAdd(di, wxi * xi);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "56b55008-353c-4037-9be3-5d348f686380");
                final double tmp = wxi * xi / di;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ba5d14e4-868a-477e-9da4-2726388e3df4");
                if (FastMath.abs(tmp) > Precision.EPSILON) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "58af5791-9dea-465b-ae2e-58a8e7377347");
                    w = (di * w) / dpi;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "bc78fb7a-6073-4b25-b1d1-70d5831f2bfa");
                dpi = wxi * xi;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6ba90d0d-fa19-478b-ba40-5d13bb535f95");
                w = 0.0;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "42fa3487-4f2c-4d98-83af-255b0499aa33");
            d[i] = dpi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d57e5ef6-dacc-4284-8091-98445b4308da");
            for (int k = i + 1; k < nvars; k++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0b918aaf-4ba1-457e-9ad3-00c912c950aa");
                xk = x[k];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "405c802b-3f8c-4343-ba27-6d789795b353");
                x[k] = smartAdd(xk, -xi * r[nextr]);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7f719268-a441-4691-96d7-3cf571318fbb");
                if (di != 0.0) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4358459e-11f8-40d3-a4ef-75a4858baed5");
                    r[nextr] = smartAdd(di * r[nextr], (_w * xi) * xk) / dpi;
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d893da42-53db-4e0f-80bb-48382172a631");
                    r[nextr] = xk / xi;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "82168d75-6fc7-4dd1-b89d-c1ecc6d49d6b");
                ++nextr;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c18c7e31-b2e5-4728-8173-d218c6e02421");
            xk = y;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "474845a4-7f95-4a40-9e64-c2159f210aa2");
            y = smartAdd(xk, -xi * rhs[i]);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "558c691b-ad25-4a00-b2ad-207c9f8e112d");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8410f99e-7a6d-49e2-9354-b3ffc76b37d3");
                rhs[i] = smartAdd(di * rhs[i], wxi * xk) / dpi;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a478a3a2-c423-4894-880f-52aa508b1d31");
                rhs[i] = xk / xi;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "39fff50a-135b-4a82-a64b-026e54a71bfe");
        sserr = smartAdd(sserr, w * y * y);
    }

    /**
     * Adds to number a and b such that the contamination due to
     * numerical smallness of one addend does not corrupt the sum.
     * @param a - an addend
     * @param b - an addend
     * @return the sum of the a and b
     */
    private double smartAdd(double a, double b) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "473df6a5-4e1c-424c-8ef1-260130163a24");
        final double _a = FastMath.abs(a);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "73b5b2e2-7a24-4f58-bb23-c1d6dea704e0");
        final double _b = FastMath.abs(b);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b2254ce8-f815-4e5a-a0cd-9db8b11e1eb7");
        if (_a > _b) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ae84cb04-580a-4654-b089-9149fae371ce");
            final double eps = _a * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d4f05627-0962-41d7-bbc1-c5f6e2a15342");
            if (_b > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b8ddd31e-8a67-4e03-b649-1e04a0fe28c1");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "207dace5-86a3-43f5-8799-1a56ad47bd85");
            return a;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8b1df92b-da5e-4447-b12d-44de4143e212");
            final double eps = _b * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "18ec7750-71a6-4e15-bffc-3d0441049867");
            if (_a > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4ed34e0f-9d9b-4b25-aa54-dd077d701231");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c3851109-4a17-4e1c-bf0f-ad56604660eb");
            return b;
        }
    }

    /**
     * As the name suggests,  clear wipes the internals and reorders everything in the
     * canonical order.
     */
    @Override
    public void clear() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "af375a2e-8bbf-4196-8242-d53e41904402");
        Arrays.fill(this.d, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c0038490-e973-4bf2-b78a-2b6ce51514ef");
        Arrays.fill(this.rhs, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7a7898d0-d99c-45df-8e74-8d817d6edf9a");
        Arrays.fill(this.r, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8b3fb852-1915-4974-9c16-54a5152fa0cc");
        Arrays.fill(this.tol, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "01656685-4b4b-4d3e-9a10-907baecf49ee");
        Arrays.fill(this.rss, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c71d7a2f-a3f8-48dc-bb63-8e26cc7a19e5");
        Arrays.fill(this.work_tolset, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7c77a566-ce21-4ade-a8ab-a04cbe5f08ba");
        Arrays.fill(this.work_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fea7b5c0-3ed6-451b-a76d-a577049bc154");
        Arrays.fill(this.x_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "eef8ec20-3470-4906-8198-18f27d11d260");
        Arrays.fill(this.lindep, false);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "edceada4-7c06-4505-89aa-9dbae25bfea9");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6d467d7e-5a21-43c1-8678-37c05ffd5be5");
            this.vorder[i] = i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "51a7228f-712b-4e59-b847-e73a6315bf75");
        this.nobs = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0cd1cd14-b96f-4b00-a03c-943b8a8c610f");
        this.sserr = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e5ffa0cb-7bab-4e9c-8efd-b41a5381c7d9");
        this.sumy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "edffa315-ccdb-472b-b316-35ba1bf8dc4c");
        this.sumsqy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3749042b-8dc6-4730-a826-a1d86a84659d");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1e1ee5e8-7717-4d9b-8a99-25e4cefc5d5f");
        this.tol_set = false;
    }

    /**
     * This sets up tolerances for singularity testing.
     */
    private void tolset() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4dbbc735-f3d0-4b14-87a5-8012b149a6bf");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1ffaf97b-5232-4b56-aa0c-9ac6a9a49fd7");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2116d03b-415f-44ee-819d-ef3f3a037527");
        final double eps = this.epsilon;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d3df1b27-ac9c-40db-895b-fdeb7636775f");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "bd54c49f-b490-42a8-97bf-e3f694ff9be4");
            this.work_tolset[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f5eb17e0-92e7-469a-a4dc-72a2fb0bc85f");
        tol[0] = eps * this.work_tolset[0];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "aad146bd-cb22-46ea-9965-6f935cfe0ad4");
        for (int col = 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0aa9d362-5953-4eb5-a0af-0a5e5ff59f66");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e7b69c88-c570-4954-a849-ef459ddff7a4");
            total = work_tolset[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f283e5b1-90df-4332-ac6b-47e2362019e8");
            for (int row = 0; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f43be819-45da-433b-96f7-260b6e58a397");
                total += FastMath.abs(r[pos]) * work_tolset[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4709a757-e487-4730-8aff-4cbfa5475f8f");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a2d5d7c0-72a4-4401-842a-56e1dc1a8a4a");
            tol[col] = eps * total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e5b41bd4-24ab-4251-a455-51a1bc29e221");
        tol_set = true;
    }

    /**
     * The regcf method conducts the linear regression and extracts the
     * parameter vector. Notice that the algorithm can do subset regression
     * with no alteration.
     *
     * @param nreq how many of the regressors to include (either in canonical
     * order, or in the current reordered state)
     * @return an array with the estimated slope coefficients
     * @throws ModelSpecificationException if {@code nreq} is less than 1
     * or greater than the number of independent variables
     */
    private double[] regcf(int nreq) throws ModelSpecificationException {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "691c83aa-e54f-4791-a4dc-5edeab1a15ff");
        int nextr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9e5c606c-9d43-4133-b644-2a0658461a04");
        if (nreq < 1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d96b4966-5832-44a8-abee-00df721ffd57");
            throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "eb0946b0-cec4-4690-b10b-99839de16003");
        if (nreq > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a18c1635-a286-4cf6-8ce4-596034f72f20");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, nreq, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0b22d669-2346-4fe9-b043-71138f14f93f");
        if (!this.tol_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7d952210-ef49-4fa3-80ba-d5fa20f10c29");
            tolset();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7fb2ccdc-d504-4aa1-960f-96277311eca3");
        final double[] ret = new double[nreq];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "289dbb51-e088-47bc-8464-2abed428ed8a");
        boolean rankProblem = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "362e062a-2058-4c93-b4ea-d1659123df7c");
        for (int i = nreq - 1; i > -1; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "104f56ed-ea1e-44f3-ae85-6e1429c4eeea");
            if (FastMath.sqrt(d[i]) < tol[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f8d4a4bf-41bb-4540-95cc-14b2ae871159");
                ret[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5c86a6bd-2945-4619-ae7e-00de1a32d06c");
                d[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c41e1039-ce8d-4339-812b-3ada9b59145d");
                rankProblem = true;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "72008c70-8a9e-4829-add5-038915297f6f");
                ret[i] = rhs[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4ee2a60d-d32f-4565-9f21-a4fb6dc584dd");
                nextr = i * (nvars + nvars - i - 1) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fcf655d5-80be-4b45-a8b9-b665d2a67331");
                for (int j = i + 1; j < nreq; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d7af8e09-ea9e-4a74-9a08-c241c1bd83ce");
                    ret[i] = smartAdd(ret[i], -r[nextr] * ret[j]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0d7bf38c-409b-4567-935c-08b2ea4a6d1b");
                    ++nextr;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4aa06e36-9fcb-48c0-9ed9-1482498f5a69");
        if (rankProblem) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6174188d-97c7-4ead-a9b9-3cc627bdefe5");
            for (int i = 0; i < nreq; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5f7ae8d0-b1c5-40e8-bbe5-d184c1c028dd");
                if (this.lindep[i]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b4da6bc3-367c-4936-95c0-a7c5a94996e3");
                    ret[i] = Double.NaN;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "31d6517d-a5da-4cd4-98bb-9c195791d2ca");
        return ret;
    }

    /**
     * The method which checks for singularities and then eliminates the offending
     * columns.
     */
    private void singcheck() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "468f7492-1572-441f-9ecd-b29a7914e1f7");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f04a0809-d6e0-4939-989d-a563ef6f7a67");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c8ece005-813f-457e-ae4a-ea6697a4c847");
            work_sing[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "baa18cac-0234-4666-9585-4497baf6060c");
        for (int col = 0; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9fbcf589-28eb-4a4b-a15d-41975fa7bc96");
            final double temp = tol[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "62a1c6f0-fd65-4098-9ec3-94a23268bc52");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0fb28bbd-9917-4d0e-8eaf-18f42e6dab76");
            for (int row = 0; row < col - 1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7cced770-2997-4129-9c73-b6978652adfc");
                if (FastMath.abs(r[pos]) * work_sing[row] < temp) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5a600f7e-9b6a-4447-8534-5ba7cdbc9c79");
                    r[pos] = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "269e5634-dc53-4574-9bfb-d9adefc778fc");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fa50a54e-6301-40af-bf8c-e08e83d1ae0b");
            lindep[col] = false;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0c1a0c0a-45c6-46b2-8b89-04a61e3be443");
            if (work_sing[col] < temp) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "73304ba1-9cf9-4f7c-ba48-f2161740fd74");
                lindep[col] = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b5d17579-7e73-420a-bedc-b5d5b84ac9dc");
                if (col < nvars - 1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "45563931-c0c5-4361-ae4d-006cac0147f9");
                    Arrays.fill(x_sing, 0.0);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8b5e9fad-99bd-460d-a3c3-b8d8ab51abff");
                    int _pi = col * (nvars + nvars - col - 1) / 2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f6fecab3-69d2-4420-9cc8-76b86722e3bd");
                    for (int _xi = col + 1; _xi < nvars; _xi++, _pi++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4d242aac-d293-45b4-8ecf-ecdc9fca0504");
                        x_sing[_xi] = r[_pi];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "937447d7-f6ec-40d8-b2c1-e1701266bceb");
                        r[_pi] = 0.0;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8bcad62c-6193-441b-b6e7-52c993619bd7");
                    final double y = rhs[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2d14d7d5-d7a7-4284-81a3-1844e6444f37");
                    final double weight = d[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ccd4ab67-cffd-4aef-bc0e-2c4562b6534e");
                    d[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "55fdfec5-d5c2-49ee-b62e-f04538dcc63d");
                    rhs[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "96e59696-8e2d-4867-9d44-6cb2005d65e1");
                    this.include(x_sing, weight, y);
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0b4a2cce-5caa-4c11-9d94-fc443591b9b1");
                    sserr += d[col] * rhs[col] * rhs[col];
                }
            }
        }
    }

    /**
     * Calculates the sum of squared errors for the full regression
     * and all subsets in the following manner: <pre>
     * rss[] ={
     * ResidualSumOfSquares_allNvars,
     * ResidualSumOfSquares_FirstNvars-1,
     * ResidualSumOfSquares_FirstNvars-2,
     * ..., ResidualSumOfSquares_FirstVariable} </pre>
     */
    private void ss() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b0fcfb46-221d-4780-b486-d80676f6ccf1");
        double total = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "945c2474-919c-45b0-b990-0e1103f505c2");
        rss[nvars - 1] = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "33aa4203-3488-481e-a277-5f0ba8dd9dcd");
        for (int i = nvars - 1; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1794747d-3dff-4224-8666-ce91a7f78c7c");
            total += d[i] * rhs[i] * rhs[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "10d0f577-5870-4458-bc69-054d23c63ba8");
            rss[i - 1] = total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1919357c-bd32-453c-b323-9d7f9e401e3f");
        rss_set = true;
    }

    /**
     * Calculates the cov matrix assuming only the first nreq variables are
     * included in the calculation. The returned array contains a symmetric
     * matrix stored in lower triangular form. The matrix will have
     * ( nreq + 1 ) * nreq / 2 elements. For illustration <pre>
     * cov =
     * {
     *  cov_00,
     *  cov_10, cov_11,
     *  cov_20, cov_21, cov22,
     *  ...
     * } </pre>
     *
     * @param nreq how many of the regressors to include (either in canonical
     * order, or in the current reordered state)
     * @return an array with the variance covariance of the included
     * regressors in lower triangular form
     */
    private double[] cov(int nreq) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7c19dcb1-af1a-42f8-931c-33407479d2f8");
        if (this.nobs <= nreq) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a3dc79f8-a539-45f4-98a3-970ef97519ee");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a65a1036-e335-4d01-bc23-2b7ad1cdc945");
        double rnk = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "46d5bfde-54a5-4b2c-afd2-34759921e925");
        for (int i = 0; i < nreq; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "262fec92-f965-438e-89b7-57bdc2a0fad3");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8d707302-172d-495a-a09d-ebd0febaaa17");
                rnk += 1.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ff9de073-dbc4-4365-b24f-a249ec71c5a9");
        final double var = rss[nreq - 1] / (nobs - rnk);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c0925727-afec-4863-b440-5ddb2fa53fda");
        final double[] rinv = new double[nreq * (nreq - 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "bd405f27-d9d7-4df1-8128-d41810a25604");
        inverse(rinv, nreq);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8108da5b-9dc9-42b9-be81-968608d36342");
        final double[] covmat = new double[nreq * (nreq + 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "72b57abd-691d-4549-918e-ce87658ef86b");
        Arrays.fill(covmat, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "af30476d-5f57-470d-b912-c581a6efc9d9");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8f4947d9-603c-4180-b404-4d1987315558");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3a4c7cf0-b80d-4745-9b41-ef5aa813d4d0");
        int start = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fdeaa2ef-ad8e-4a04-a342-b2a7ee9a3999");
        double total = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c169392a-4cfc-43b8-83ef-10ae12519fc0");
        for (int row = 0; row < nreq; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "78b1bb93-2ef6-4468-b2d2-8d8d97e30b04");
            pos2 = start;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2d81dfd9-96b6-4bed-bfa0-192dd9d6b9fc");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "05159e30-e044-4d9f-bcbc-59f41976f18e");
                for (int col = row; col < nreq; col++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7a4f6cd1-f515-47e5-a149-9c9fcafd0658");
                    if (!this.lindep[col]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "703d39c2-06bb-4f10-829b-1f717a62f7cc");
                        pos1 = start + col - row;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "61e269af-8f15-422a-80be-9fc9c1924d3b");
                        if (row == col) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3665b005-a12c-4565-9043-7e3d4c97f9ea");
                            total = 1.0 / d[col];
                        } else {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "88d9ec78-e337-488e-a9ea-0f38d772a314");
                            total = rinv[pos1 - 1] / d[col];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "99047cb7-34cb-423e-830a-b304d4abf842");
                        for (int k = col + 1; k < nreq; k++) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3f5fbe9b-dbb8-4bb4-a33e-d292348bcffb");
                            if (!this.lindep[k]) {
                                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6345f469-1e00-4e4a-bb1b-cfdaa096074f");
                                total += rinv[pos1] * rinv[pos2] / d[k];
                            }
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "486f212a-5dea-4d81-b66b-3daf88c86f79");
                            ++pos1;
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1f1b041c-c3c2-47e1-9c27-1671c5f7c686");
                            ++pos2;
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8456411c-b2dd-4731-9efb-b81139f7a591");
                        covmat[(col + 1) * col / 2 + row] = total * var;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a32fd7ee-5399-45bb-a0ca-61485bd72c40");
                        pos2 += nreq - col - 1;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "07592feb-59bc-47c0-824d-97a8fdf5c2f6");
            start += nreq - row - 1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "795667f5-6897-4e3a-bb59-83704add907b");
        return covmat;
    }

    /**
     * This internal method calculates the inverse of the upper-triangular portion
     * of the R matrix.
     * @param rinv  the storage for the inverse of r
     * @param nreq how many of the regressors to include (either in canonical
     * order, or in the current reordered state)
     */
    private void inverse(double[] rinv, int nreq) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "22af4fad-2b27-4753-b1e9-88bae3ef00ed");
        int pos = nreq * (nreq - 1) / 2 - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3eae0967-c83c-4aa4-9510-cb0636c69081");
        int pos1 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "25f45236-16df-4bab-8b77-944ef52e2d17");
        int pos2 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b9132069-2b72-4ef0-a15b-3ac943e75e7d");
        double total = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b80d1f1c-83ab-4a0b-a1b1-5d0c7a3785a6");
        Arrays.fill(rinv, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e50de292-ffd3-44ef-ade8-2c4a20305b5e");
        for (int row = nreq - 1; row > 0; --row) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "aad66c43-1cc2-4e28-b765-12d3c60b2ad2");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a0d3eb7d-1fe2-419b-bc03-6f43c5bfc442");
                final int start = (row - 1) * (nvars + nvars - row) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "843ce12e-59e1-4067-872f-521ba75bb7a6");
                for (int col = nreq; col > row; --col) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2faa5e92-56b9-4119-9dae-bd87eb22a8a5");
                    pos1 = start;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3d648097-7537-4711-ba51-fb1b5630175f");
                    pos2 = pos;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "80047414-18b5-4432-92a5-6810cabcd6cd");
                    total = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ee0d7fae-3bed-4425-b499-5b1a4577275a");
                    for (int k = row; k < col - 1; k++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9a557184-29d5-496e-a226-68afbc95f411");
                        pos2 += nreq - k - 1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ca921860-341d-4847-945a-942f7dc2484a");
                        if (!this.lindep[k]) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "715e1b5d-25d0-4c6c-9b8e-37ff221c67d3");
                            total += -r[pos1] * rinv[pos2];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b435062a-9353-4251-876e-c2f897897e8a");
                        ++pos1;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "425bf225-c1fa-4938-9742-a4b92f03a41a");
                    rinv[pos] = total - r[pos1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c6a9133e-b1ea-47be-908b-56b74cc88c94");
                    --pos;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d2ada5f2-ea21-431c-9a8b-1d99f3359c19");
                pos -= nreq - row;
            }
        }
    }

    /**
     * In the original algorithm only the partial correlations of the regressors
     * is returned to the user. In this implementation, we have <pre>
     * corr =
     * {
     *   corrxx - lower triangular
     *   corrxy - bottom row of the matrix
     * }
     * Replaces subroutines PCORR and COR of:
     * ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 </pre>
     *
     * <p>Calculate partial correlations after the variables in rows
     * 1, 2, ..., IN have been forced into the regression.
     * If IN = 1, and the first row of R represents a constant in the
     * model, then the usual simple correlations are returned.</p>
     *
     * <p>If IN = 0, the value returned in array CORMAT for the correlation
     * of variables Xi &amp; Xj is: <pre>
     * sum ( Xi.Xj ) / Sqrt ( sum (Xi^2) . sum (Xj^2) )</pre>
     *
     * <p>On return, array CORMAT contains the upper triangle of the matrix of
     * partial correlations stored by rows, excluding the 1's on the diagonal.
     * e.g. if IN = 2, the consecutive elements returned are:
     * (3,4) (3,5) ... (3,ncol), (4,5) (4,6) ... (4,ncol), etc.
     * Array YCORR stores the partial correlations with the Y-variable
     * starting with YCORR(IN+1) = partial correlation with the variable in
     * position (IN+1). </p>
     *
     * @param in how many of the regressors to include (either in canonical
     * order, or in the current reordered state)
     * @return an array with the partial correlations of the remainder of
     * regressors with each other and the regressand, in lower triangular form
     */
    public double[] getPartialCorrelations(int in) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d9c95383-6e69-454b-b71e-c7c676710e18");
        final double[] output = new double[(nvars - in + 1) * (nvars - in) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2a479385-4201-4c8a-94b8-da4aee3aa467");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "feffb4c2-9a3c-446a-b386-5437f4ef8d6a");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "51609815-6113-4673-8f1e-ab695018fc91");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0e72c3b3-2c87-41ca-b207-fd7ed1bc1741");
        final int rms_off = -in;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f5bcabd9-be9d-43b5-b565-3f6b5241e234");
        final int wrk_off = -(in + 1);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1cd29c09-cf1a-4f18-99b8-d285e3a78aeb");
        final double[] rms = new double[nvars - in];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "044c9705-b314-474a-9151-2aa23f18f0b9");
        final double[] work = new double[nvars - in - 1];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "796612ac-b022-4f33-a33b-c3609bf6dfbe");
        double sumxx;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "eee3d89d-7199-45a3-9a38-e327d4beafcb");
        double sumxy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2061e51e-96c4-43b8-9a80-cf1e23ea1fac");
        double sumyy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ba897e00-47ab-4c4e-9079-98d077a9e9a1");
        final int offXX = (nvars - in) * (nvars - in - 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e241cb0d-b0a6-4ed7-8e14-a2b61e488f7e");
        if (in < -1 || in >= nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "989ed4d7-39a7-4ccd-b708-2b68b40f9e57");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1a7f276b-3f70-47f0-8e74-907c1f989635");
        final int nvm = nvars - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "847408b7-1f7e-47ac-a777-cebcf1f07f7c");
        final int base_pos = r.length - (nvm - in) * (nvm - in + 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "885e2ecb-7b45-4be0-b399-654c9c65479c");
        if (d[in] > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "25862f1b-23d9-4b14-982a-54f95a5ede55");
            rms[in + rms_off] = 1.0 / FastMath.sqrt(d[in]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d64da1ac-2756-43ca-be55-50ded4ea16dd");
        for (int col = in + 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1ad71f3b-3f3c-46c3-9735-3b159a1809fb");
            pos = base_pos + col - 1 - in;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "344716cc-b283-4d9a-9bbf-ffa5c991af9e");
            sumxx = d[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "26968ca9-28e6-4e93-bea3-8d163579e64f");
            for (int row = in; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fa227183-a5e3-4ed1-9778-fb398fa22ab4");
                sumxx += d[row] * r[pos] * r[pos];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7a85eef7-1e8d-44fa-8ab5-82b091cd63cd");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f1f0ca9f-88ef-4985-805d-802f51632acb");
            if (sumxx > 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2c01220f-4b9f-438e-bb0c-a3f8641365b8");
                rms[col + rms_off] = 1.0 / FastMath.sqrt(sumxx);
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "129768ab-be5f-4990-8d18-49d165403673");
                rms[col + rms_off] = 0.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "60835f9f-a191-4c09-8726-0b5528ebe218");
        sumyy = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "83fa6843-3742-4adb-9bb7-41bef4195f21");
        for (int row = in; row < nvars; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c4a9f653-629b-4228-abc3-12bcc39a18a0");
            sumyy += d[row] * rhs[row] * rhs[row];
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "af8b5bc8-7326-42c8-b8f1-f598dcf1c35f");
        if (sumyy > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9546cef3-cea3-471f-8623-ccf3f1f5ca1d");
            sumyy = 1.0 / FastMath.sqrt(sumyy);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "eca90064-1c29-4067-9aec-00b0a6c659fd");
        pos = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4d6bbfbe-dbba-49c2-a82b-11f13f07a3fa");
        for (int col1 = in; col1 < nvars; col1++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8a7ab739-36cd-4aa3-b678-85714489da09");
            sumxy = 0.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0e6c53df-02c2-4fd5-b0a7-444a28a5b9cc");
            Arrays.fill(work, 0.0);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e8b7e8e6-43cc-4002-b742-c37f5657573e");
            pos1 = base_pos + col1 - in - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6053e927-2a82-4877-addc-767c6ea6ccab");
            for (int row = in; row < col1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6343ab1c-f681-4b7d-9ea1-45dbf90157da");
                pos2 = pos1 + 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "47a9227c-4531-4784-9a6e-0c7e6ceb9c1f");
                for (int col2 = col1 + 1; col2 < nvars; col2++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "10a70191-55bd-4da5-8ac3-a4e9062173ee");
                    work[col2 + wrk_off] += d[row] * r[pos1] * r[pos2];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "575a9218-23fd-4daa-99fb-1c26ae8e7412");
                    pos2++;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "65ad3469-af46-4f1b-8ef4-13bef323cc75");
                sumxy += d[row] * r[pos1] * rhs[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "68babf07-ac98-4d4d-9d20-54047aebef93");
                pos1 += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "65576283-78be-4a3e-a7b3-55ef566f296a");
            pos2 = pos1 + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e6d3016a-3b19-478a-b49f-0ded6f8d4dc9");
            for (int col2 = col1 + 1; col2 < nvars; col2++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c70ac20b-6c63-4e09-98bb-934369287e27");
                work[col2 + wrk_off] += d[col1] * r[pos2];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e83d54ab-fcf5-4d21-959b-70312b8b0043");
                ++pos2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5b0d69fb-ffae-4372-bdb0-3a71459bc6e8");
                output[(col2 - 1 - in) * (col2 - in) / 2 + col1 - in] = work[col2 + wrk_off] * rms[col1 + rms_off] * rms[col2 + rms_off];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f54c5bbf-1471-41a2-8193-8e76f65fe095");
                ++pos;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0776cb65-1e50-4f7d-b665-a4426c8cc893");
            sumxy += d[col1] * rhs[col1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "91bebdb9-7fa5-4704-96cf-4ab28a821e74");
            output[col1 + rms_off + offXX] = sumxy * rms[col1 + rms_off] * sumyy;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "eee3ab2d-a64c-45c2-833f-d8010bed5cd1");
        return output;
    }

    /**
     * ALGORITHM AS274 APPL. STATIST. (1992) VOL.41, NO. 2.
     * Move variable from position FROM to position TO in an
     * orthogonal reduction produced by AS75.1.
     *
     * @param from initial position
     * @param to destination
     */
    private void vmove(int from, int to) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "afbc3e84-1137-4452-bf8a-a4d2adea6ac7");
        double d1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8e258813-5f1c-4053-84ef-e7201869c2c8");
        double d2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6a1c12f5-2185-4948-a74f-ed00c3de28e0");
        double X;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7724692f-7beb-444e-a374-8380af48831b");
        double d1new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "570a26c2-2799-4284-a24c-a45171a7334d");
        double d2new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6a3be39f-46a2-492e-ab77-27d2b2e08455");
        double cbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "37116f4c-0617-451d-b344-29b19fe70678");
        double sbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a2bbede2-d486-42c5-bf18-5650c6a3a369");
        double Y;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "00803644-7204-430f-994c-8d3540911b02");
        int first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c06230ad-b5a5-457c-a1fe-b084fe9bf8f5");
        int inc;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e80e439f-45fd-450b-b33c-159fc9dc4d59");
        int m1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "878fccc7-ab38-4ed0-a942-6d3a4088f40f");
        int m2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e938794c-1958-497d-ae44-6c7dc24e9433");
        int mp1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "98765f94-7b4c-4d5c-93d2-0da080820af4");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7f3e7c7f-f5b0-4da9-99d7-181616b7ff2d");
        boolean bSkipTo40 = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5f00efbc-0ebb-4b80-bb36-6a6c9e3e34c1");
        if (from == to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "979fa396-e74f-451c-a941-74c1acbc5d20");
            return;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "14fbd635-9460-4999-bd73-ba38a87d4ac9");
        if (!this.rss_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ba8102e2-5846-42fb-8d38-457cb9da7b17");
            ss();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "76c45b04-62fc-4fdb-9770-ccc056c88c4d");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "33ef64b8-c2d2-450c-9630-e28ee0ec3ee4");
        if (from < to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ab83a31a-5e11-46f6-bca7-d720d6130da7");
            first = from;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "26fe30fe-b985-4c1c-8f04-a99d0e0da77a");
            inc = 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f918359b-c2f8-43ae-a3e9-854c1a3d98fe");
            count = to - from;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c8c173c1-071c-4015-802c-8c30599c0f0a");
            first = from - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0e294912-f489-4b0f-8c69-fd82fe129b74");
            inc = -1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4a699975-2e94-4064-b0a0-67361f83cb57");
            count = from - to;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f7f27a1d-f2f2-4614-a20d-701d0458d86f");
        int m = first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1a4fa76b-b869-4c77-a874-f1c56ff3fbcd");
        int idx = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "162141bc-01af-47f7-9738-0cceb42325c1");
        while (idx < count) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1ce9c89c-7906-4f99-8ab1-b068c2ec3a96");
            m1 = m * (nvars + nvars - m - 1) / 2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "92b9e3ca-af8f-4af7-b6cd-47736a2d7d93");
            m2 = m1 + nvars - m - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "3e52e8b7-2206-4f92-abf4-4ec3df3e7ce2");
            mp1 = m + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e83a703a-baea-490c-8363-9c139d24976c");
            d1 = d[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "92a1ae41-9bb9-4f85-baf7-2e1c5a788bac");
            d2 = d[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d56428cf-8372-4cde-8c29-6147d81e4dfa");
            if (d1 > this.epsilon || d2 > this.epsilon) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "77b4fb19-c42a-4363-880e-79c31c6d72f4");
                X = r[m1];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "bd91af17-2df1-4341-8cc5-d77432769317");
                if (FastMath.abs(X) * FastMath.sqrt(d1) < tol[mp1]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "93de2778-bf56-4043-b496-c26efc106894");
                    X = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "af5559f0-e2b8-46fd-b51b-6863e3e6a207");
                if (d1 < this.epsilon || FastMath.abs(X) < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "73aaa3de-c2d6-4613-9702-7f1b5f06272d");
                    d[m] = d2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "30976720-a535-40bc-a404-af10465ac794");
                    d[mp1] = d1;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "265cf2f7-9db8-4d35-a527-b9e9784a5b94");
                    r[m1] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "61c04ed7-c2f0-49d9-8690-9586ef0320bd");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9234a7e2-6b9d-4ad9-b2fc-78928b52030b");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "dbff2fff-a9ff-4ca0-ae47-c5461ad8569e");
                        X = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "af782c9d-17f5-4be9-8a13-4ef272de2144");
                        r[m1] = r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d16636f2-0d7e-4dab-8821-8128f8523ba2");
                        r[m2] = X;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b8504732-39bc-4ae9-8c7f-6b657d31acce");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a3907255-5457-46f4-85d7-8303a7831624");
                    X = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2f2150e8-586b-402e-9918-b5a6e78227ba");
                    rhs[m] = rhs[mp1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0cafda0c-129b-46b7-aaac-01f097563385");
                    rhs[mp1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "bc580e2b-43be-4ab2-8515-6b262fab2460");
                    bSkipTo40 = true;
                } else if (d2 < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0aa95fec-c5d5-4421-9f2f-6565008fe9f5");
                    d[m] = d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b3f7bc01-19b6-4251-a8bd-fdd7c7d44010");
                    r[m1] = 1.0 / X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "55ed92f3-9263-4d55-874d-24295091eb10");
                    for (int _i = m1 + 1; _i < m1 + nvars - m - 1; _i++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "cfd3d8c5-d246-44a4-a9dd-c38d20f02b6b");
                        r[_i] /= X;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4ad60fa6-6a6a-4188-b8c4-1af813068ed4");
                    rhs[m] /= X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1b12e87a-be0f-4334-a0f2-12169d825be6");
                    bSkipTo40 = true;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d8be45e8-4b55-47af-9c99-ca4f045a8cfb");
                if (!bSkipTo40) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "331ab269-1f6e-4b74-b675-9cf25b3663ce");
                    d1new = d2 + d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ecaa6f63-a8a3-4e26-b2c1-62cc34223f00");
                    cbar = d2 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8cc1858a-babe-4d8c-84d7-ffa90d3fb63c");
                    sbar = X * d1 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b80fd401-95fe-45d0-9d7c-789804b353bf");
                    d2new = d1 * cbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "913cb80f-78d0-4ce8-accf-cabe68b3ffc0");
                    d[m] = d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5ed997b9-e5b9-445d-81be-07544d40b869");
                    d[mp1] = d2new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b90751d0-2526-4003-a308-49b9bc7a9608");
                    r[m1] = sbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "21785d01-17fe-40c3-bc46-eb25744b77c9");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1925d3ee-cac6-4008-be1e-18e14b27995e");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c22c1a57-36a7-4eaf-a1f3-091f14d42e50");
                        Y = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2ceb3db2-c2e8-411e-a299-b9dd1b9d4c10");
                        r[m1] = cbar * r[m2] + sbar * Y;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "337f1f81-531c-4da4-baed-387b6b474989");
                        r[m2] = Y - X * r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "259451b1-caae-41b1-9323-12fb0e9d9d75");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e127b320-3d66-4497-87b2-ff86267a111b");
                    Y = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "dcd9e1ef-c977-4c71-9229-ea1abb009501");
                    rhs[m] = cbar * rhs[mp1] + sbar * Y;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "275860ff-1f07-4496-b904-d23034ca2caf");
                    rhs[mp1] = Y - X * rhs[mp1];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8602015b-22cb-4754-b943-e129ebdf66d3");
            if (m > 0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f1cd0345-0b0c-4466-b109-516088a3f6cf");
                pos = m;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c3bc8178-016a-4865-ba17-63c55deaaffb");
                for (int row = 0; row < m; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e4025c5e-c45f-4ff3-b611-b43091c4c58e");
                    X = r[pos];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c17644ff-c1e5-473e-b74c-0f0c2c80deea");
                    r[pos] = r[pos - 1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2656cf24-fab7-421a-9ed4-448a3e4d5eb8");
                    r[pos - 1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "259db9cb-28c7-46fc-bb98-86c032bce14c");
                    pos += nvars - row - 2;
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f41dfa60-73fa-4e8a-a35f-f70b6cd051fd");
            m1 = vorder[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "44d5e749-50b1-43cd-b6ec-9fae36be5125");
            vorder[m] = vorder[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7778689d-0882-44e2-bdee-35e6c9bd730e");
            vorder[mp1] = m1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "424ed969-bbd0-450d-8b8b-1ede6211eebf");
            X = tol[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1c0c4510-19be-4002-964a-38a893dc1ff6");
            tol[m] = tol[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7d88b3c1-ef11-4da4-a2b2-52d79ed8a6eb");
            tol[mp1] = X;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1d1ab209-b4af-4967-86c5-d9ccdde64c12");
            rss[m] = rss[mp1] + d[mp1] * rhs[mp1] * rhs[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "50fe66aa-7e31-4bf4-ae33-51f222b4da97");
            m += inc;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "16b9d883-a3d9-4bc0-8be0-e7eafdbab5db");
            ++idx;
        }
    }

    /**
     * ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
     *
     * <p> Re-order the variables in an orthogonal reduction produced by
     * AS75.1 so that the N variables in LIST start at position POS1,
     * though will not necessarily be in the same order as in LIST.
     * Any variables in VORDER before position POS1 are not moved.
     * Auxiliary routine called: VMOVE. </p>
     *
     * <p>This internal method reorders the regressors.</p>
     *
     * @param list the regressors to move
     * @param pos1 where the list will be placed
     * @return -1 error, 0 everything ok
     */
    private int reorderRegressors(int[] list, int pos1) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a6dd125e-281d-4223-9bff-0b17b9a97d99");
        int next;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a26bd248-557f-46c0-befe-4f5dea27e5a7");
        int i;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5efba2b9-c9ad-48b3-89c2-a2ef01444dc9");
        int l;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "53e13f45-ba98-4f02-9936-2facc04668ea");
        if (list.length < 1 || list.length > nvars + 1 - pos1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "51b40a2f-1ecf-4e3d-ac8e-005609d35be4");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ed2f977a-79a6-43a3-a4cd-05d1df217e25");
        next = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "00cc0ab6-262d-4362-8e5d-376d213c367f");
        i = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5736f189-26eb-4596-95b9-b9fbcd04ac77");
        while (i < nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "099705e3-43ae-41b0-8dc7-bc735be21494");
            l = vorder[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1b2dd247-cdca-4553-931d-e552770fe581");
            for (int j = 0; j < list.length; j++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "656a5c5e-e529-4ad6-8be2-9e8c358d19fc");
                if (l == list[j] && i > next) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1916a240-1b64-4cf8-8b2b-867c4ef97314");
                    this.vmove(i, next);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "82f2de9a-3e56-4218-b06b-cfcfbd64382b");
                    ++next;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ca655219-83b7-4323-90fc-aec47c22f340");
                    if (next >= list.length + pos1) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "26b40546-11f7-4ca1-af1a-ab7ca23ec010");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "53cca894-8ef8-4669-840c-a97877d76344");
                        break;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9a5cbf8d-14af-439b-90fa-54265b078d05");
            ++i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "632e776c-8093-44e5-8ac4-eaaab51b7a3b");
        return 0;
    }

    /**
     * Gets the diagonal of the Hat matrix also known as the leverage matrix.
     *
     * @param  row_data returns the diagonal of the hat matrix for this observation
     * @return the diagonal element of the hatmatrix
     */
    public double getDiagonalOfHatMatrix(double[] row_data) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5ba21694-cb44-4e00-8e16-9673d120eebb");
        double[] wk = new double[this.nvars];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6e8af02d-57b4-4b84-a2e4-dbf23209d5e9");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6b668e59-c7b4-44d4-ba02-d8c61789e874");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f2da6669-bc88-40e9-b33e-1368712b32d1");
        if (row_data.length > nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "56b14761-6015-4304-a777-ee1a0f554d73");
            return Double.NaN;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b8391197-626d-4d95-b976-8b650fa3fa27");
        double[] xrow;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "81f1644a-cd9e-4618-a710-3e1241d7e6e7");
        if (this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "578d5ab5-5668-4221-ab24-b16ff52277da");
            xrow = new double[row_data.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b0d86f71-859b-4243-9f78-5bcd3f2df4e3");
            xrow[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0b319c66-821c-4502-b0cf-8888d7df2fff");
            System.arraycopy(row_data, 0, xrow, 1, row_data.length);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7deeab85-7fcd-4edc-b6c3-1e9a2e811fbb");
            xrow = row_data;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "977074cb-04f3-4d6f-961a-f4ba92b14258");
        double hii = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6ea281e7-b78d-4165-be50-928f6613b791");
        for (int col = 0; col < xrow.length; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f11e2143-e509-4089-b03d-6b0186ae4779");
            if (FastMath.sqrt(d[col]) < tol[col]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4c86c516-b971-49b3-878b-8855548dc660");
                wk[col] = 0.0;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d07d9a7d-6f99-4e57-bffd-541986888ce6");
                pos = col - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "25f27473-3d4b-4e96-8a7c-20564e28b3cf");
                total = xrow[col];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5c727d98-3bac-4eba-93d8-d018f4d67dd6");
                for (int row = 0; row < col; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "de6656b1-f442-4599-98a8-26dbcacb04b9");
                    total = smartAdd(total, -wk[row] * r[pos]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5dfd242f-8cbc-4b26-83dc-b9f10e09db11");
                    pos += nvars - row - 2;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7302a68e-a6b2-4f19-bfbd-d0bb1a37ad41");
                wk[col] = total;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5986b665-d3a5-4237-a413-e681c386d1d8");
                hii = smartAdd(hii, (total * total) / d[col]);
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "25a2b982-8a8c-4964-b291-dd7af805d928");
        return hii;
    }

    /**
     * Gets the order of the regressors, useful if some type of reordering
     * has been called. Calling regress with int[]{} args will trigger
     * a reordering.
     *
     * @return int[] with the current order of the regressors
     */
    public int[] getOrderOfRegressors() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7fd29d45-5229-4797-be63-f75dd9052377");
        return MathArrays.copyOf(vorder);
    }

    /**
     * Conducts a regression on the data in the model, using all regressors.
     *
     * @return RegressionResults the structure holding all regression results
     * @exception  ModelSpecificationException - thrown if number of observations is
     * less than the number of variables
     */
    @Override
    public RegressionResults regress() throws ModelSpecificationException {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "33ad2a46-20ea-4018-8853-2409649be0b6");
        return regress(this.nvars);
    }

    /**
     * Conducts a regression on the data in the model, using a subset of regressors.
     *
     * @param numberOfRegressors many of the regressors to include (either in canonical
     * order, or in the current reordered state)
     * @return RegressionResults the structure holding all regression results
     * @exception  ModelSpecificationException - thrown if number of observations is
     * less than the number of variables or number of regressors requested
     * is greater than the regressors in the model
     */
    public RegressionResults regress(int numberOfRegressors) throws ModelSpecificationException {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "973ddf66-f03a-40c5-aecb-924bc10bfcf9");
        if (this.nobs <= numberOfRegressors) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "80a953ac-0f6e-4fe0-877d-bb5afd10356a");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, numberOfRegressors);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9cdf82a1-eb79-45aa-b1cf-3d4ad783df92");
        if (numberOfRegressors > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4207af2a-ecd3-4814-83f4-913dd5704af5");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, numberOfRegressors, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ff2d610d-db15-4163-bd74-9c87be805b86");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0266ee6c-8c76-4e6d-a27b-c14d50525fee");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f91f469e-8d7e-4ad9-8f1a-f8af4c17a368");
        double[] beta = this.regcf(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "511653f1-e668-4309-900a-053d2a14e926");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b229aa68-78c8-4cee-b738-21552d0f7d2d");
        double[] cov = this.cov(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4081dfc1-e52b-4799-84f6-0c2b259506a5");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1161c7f6-6d9a-4fce-a8da-862075af9acd");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "dbc9a2ea-e51b-45e9-8504-01a586783717");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6aec65cd-f844-4d46-9227-26307d5e775d");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a4ace528-1e5f-4b43-909b-d08cc8bd09e0");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e5b7fb47-9b8a-4f84-8342-b2d7c5d1009d");
        for (int i = 0; i < numberOfRegressors; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "94571855-160d-4678-973d-96330646deca");
            if (this.vorder[i] != i) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "6d24d95b-97c6-41db-b180-2a14116625cf");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "44b06b3b-59b4-4a1c-8819-0de6931c20ad");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "74db0af6-23fd-4a40-8ae7-bc33c8dc32a1");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fe9dced4-2aa1-46da-bc6a-f81d4f8a17f0");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e0531435-afb7-4bdc-9578-f9dfec7d62af");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a2659079-1da2-476d-9b26-4bfb960c42ed");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d0f443f9-d34c-4601-afb3-dcc2eea3197e");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c85a1a9d-b581-4c9f-8f2e-c47e992acfcc");
            for (int i = 0; i < nvars; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b7f1c291-dfa4-4a36-94d8-297c5caeeda3");
                for (int j = 0; j < numberOfRegressors; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1cf57ba3-7dcc-40f8-9456-c62fc11e8921");
                    if (this.vorder[j] == i) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "188fae53-f44d-47ae-9e38-4a6973f2139e");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a4b10d28-39b4-434a-a477-164a5fef139e");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c7c70c9f-4d4f-4657-bf63-a2f6d6d47f9d");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "005a8a53-325a-4f94-a15f-59d2f0627b5a");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c24b5900-52be-4b41-bd0b-5de99bd08287");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5916f179-896d-4987-a11c-f6d9fbbcd9db");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a784cf7f-468b-47f8-ba51-fb4a76c2b5eb");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5f9de7ac-b38a-4ce5-8d4b-01ad5581ffe9");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "54519a23-50bb-443c-960b-4284cc9999d6");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f84ae5c2-e1c2-4fc4-af7a-af9461867c2c");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "50dc44f2-1c0c-410a-8b14-409f48527922");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8f3fac1d-c82c-4c54-abd2-9c800e353f0b");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "584ab66f-b310-4287-9ad6-4d175106a43f");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d7de5a32-eb86-4f41-8d60-4ae49d6bbd4e");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "005f0286-2a33-4e0e-b49e-95ca9610720d");
            return new RegressionResults(betaNew, new double[][] { covNew }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        }
    }

    /**
     * Conducts a regression on the data in the model, using regressors in array
     * Calling this method will change the internal order of the regressors
     * and care is required in interpreting the hatmatrix.
     *
     * @param  variablesToInclude array of variables to include in regression
     * @return RegressionResults the structure holding all regression results
     * @exception  ModelSpecificationException - thrown if number of observations is
     * less than the number of variables, the number of regressors requested
     * is greater than the regressors in the model or a regressor index in
     * regressor array does not exist
     */
    @Override
    public RegressionResults regress(int[] variablesToInclude) throws ModelSpecificationException {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b58b2441-e425-4500-8543-43b22c4b876e");
        if (variablesToInclude.length > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c6cb2784-f659-422d-98d0-00c356acd481");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, variablesToInclude.length, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d3531439-4002-469b-bfc7-4187cee6e4ef");
        if (this.nobs <= this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "465ba1ae-4b64-4fa3-85c1-a71184307f43");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e126c53a-424a-41d5-818c-493a7a3abbce");
        Arrays.sort(variablesToInclude);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d1fc2869-1a82-4527-a6ec-92fda019e395");
        int iExclude = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9d020c3d-4f85-4a5a-855c-9345e7bf6c6e");
        for (int i = 0; i < variablesToInclude.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "fe9d60d5-9bb2-46a0-8cc6-20c2b81d8400");
            if (i >= this.nvars) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e09930e8-b4c6-45f2-92cb-b1d3d43ebbe1");
                throw new ModelSpecificationException(LocalizedFormats.INDEX_LARGER_THAN_MAX, i, this.nvars);
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "822917e1-81b8-400a-89a7-c9f53f76ef69");
            if (i > 0 && variablesToInclude[i] == variablesToInclude[i - 1]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "30ad2306-5ad6-4a2c-85dd-3b655fc1330e");
                variablesToInclude[i] = -1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "b71664ab-5cc2-4f3d-bcc8-6daf9a1a89cd");
                ++iExclude;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f6921df3-6d20-46e4-ada8-639f43f0612f");
        int[] series;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "f1f0c62c-1492-47bb-97d4-fb4615389317");
        if (iExclude > 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "33c02b1a-e231-48e9-b7ba-fca534a57047");
            int j = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "85ac4cb8-269c-49d7-9829-9173cc55f40d");
            series = new int[variablesToInclude.length - iExclude];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e13646be-dfcf-4532-997c-374df1d70372");
            for (int i = 0; i < variablesToInclude.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "11628f23-f983-4f8f-9522-4f7d16e06b5b");
                if (variablesToInclude[i] > -1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "4580ec95-a3f5-42b1-9537-078858cca984");
                    series[j] = variablesToInclude[i];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "725de731-739a-4e7e-928c-47aef2b2277f");
                    ++j;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9652ce6b-ed67-4239-a4ef-b9b4dd6448ad");
            series = variablesToInclude;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "8bfd0604-aba9-4f7f-963d-291a2bbabb0b");
        reorderRegressors(series, 0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "a567e2fa-5db6-4c88-aeb1-b2c9c4d5b2f4");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "62f1dc43-c7a0-4bd1-a764-572dc8251ec1");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0f1521d7-a7c2-45e2-bbc9-570388c8f7b3");
        double[] beta = this.regcf(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ab356c03-de44-4567-a800-f826c38cf39e");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "0e61f3b4-47c3-4463-8213-bd6187ad62de");
        double[] cov = this.cov(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1858cb70-0e2d-49d8-b0f7-d705d9b59a14");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "609892ee-289d-4fa6-b49e-673d62639f72");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "00f08dbc-bbe4-4f32-9e92-7cede2a65253");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "19e473be-7797-4888-a92a-c3de57d493ba");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d422844e-c33e-4d50-9699-05689c9a14da");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1532e657-9d3c-4017-b6c3-ff24d8afa7c9");
        for (int i = 0; i < this.nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "910cc7f2-0d0d-4245-acda-8a38a2447a6f");
            if (this.vorder[i] != series[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c8d7b309-6cc8-4dc4-9539-8034116df988");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e92165a9-5173-4a59-ac7f-cfd12993e421");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ac5c2c3e-2ef8-4488-a621-14177bbe67f4");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "9a5c4fa2-558c-45ee-ae7c-5e4e8858e839");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "c2e39456-612b-48d8-a376-fcc52faf9a9c");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ad3d1a25-204d-48e6-8560-409314b7f2bb");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5051fa88-4dca-4a55-acb4-d6ef9f685495");
            for (int i = 0; i < series.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "96ccd4cc-b624-46cb-82c2-76a4753b5976");
                for (int j = 0; j < this.vorder.length; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "d42123a8-f908-43e7-8302-0bdb1624e9d9");
                    if (this.vorder[j] == series[i]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1a7c61d5-6ec5-4112-808f-bd29104903c4");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "5b1d592d-41b3-4ca9-8ed7-465390889f93");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ff971852-5e51-42a8-9e16-260d8d21a5fd");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "aae2768f-9871-44ce-a1aa-c3c8938f6fa0");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "e9bb4451-1f6c-451f-a877-c13e48caeb26");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "07648269-e6d2-4d09-9c9f-6b38d2b94db6");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "335eeb65-3da8-4c80-9caa-78926c5194fa");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "35850434-d4a6-4d02-85c5-e7c361de4e85");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "2463a44d-08ba-41c7-bdc9-5c9f04bb05b2");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "eb3e0617-7fdb-44a3-9326-8cf8d80c543a");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "932a4281-1545-431e-85aa-6e208f4b88f0");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "ee953a74-9f5f-4ae6-9ee6-a06dcbe22066");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "69c66a77-9b09-40b4-b1ba-377bac2c9443");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "081ac522-2420-455f-97bc-343c4b159dd1");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "7fda48ee-e9c8-40b8-b5c4-918e6ae66fa8");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_5_10.coverage", "1d68934d-d1f1-4eff-85b1-4e6df4357392");
            return new RegressionResults(betaNew, new double[][] { covNew }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        }
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
