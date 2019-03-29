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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "73d90143-b5de-47bc-87d0-3b5b91d8b54a");
        return this.hasIntercept;
    }

    /**
     * Gets the number of observations added to the regression model.
     * @return number of observations
     */
    @Override
    public long getN() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "59e6e162-1ba3-4134-98d8-042f4467bedd");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b06e7245-24ce-4a13-a44a-c730e633dab2");
        if ((!this.hasIntercept && x.length != nvars) || (this.hasIntercept && x.length + 1 != nvars)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fe6bed17-d2eb-4020-b6ae-4f52f6756504");
            throw new ModelSpecificationException(LocalizedFormats.INVALID_REGRESSION_OBSERVATION, x.length, nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "63d46108-c0b8-4bac-b1b8-c7a410ebf359");
        if (!this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "be70038c-5652-408c-8ff7-4f47ad336a66");
            include(MathArrays.copyOf(x, x.length), 1.0, y);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3cb75c88-8f6b-4606-b984-4ab19c819f7d");
            final double[] tmp = new double[x.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6d23cb3e-0256-492d-a038-c02f2b9353f5");
            System.arraycopy(x, 0, tmp, 1, x.length);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b1afa169-93b1-457d-b762-c2aafd48ab8c");
            tmp[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3683a220-0fcf-461e-84e6-40fb91614e58");
            include(tmp, 1.0, y);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "98bc02d0-e5bb-404c-96c1-1d2f0d2fc6b2");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "600bba61-a04c-4766-aa2d-e86df221379c");
        if ((x == null) || (y == null) || (x.length != y.length)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "be41e9bf-b80c-4241-89d2-fbd5a5e307b9");
            throw new ModelSpecificationException(LocalizedFormats.DIMENSIONS_MISMATCH_SIMPLE, (x == null) ? 0 : x.length, (y == null) ? 0 : y.length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c704bbf0-6c51-49d6-a410-c9708b073ccb");
        if (x.length == 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "69140e31-9dd4-416f-9099-a4d256d473a3");
            throw new ModelSpecificationException(LocalizedFormats.NO_DATA);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a75fa6b1-6760-4608-86d4-3c24cd0f3af4");
        if (x[0].length + 1 > x.length) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "95d3211e-b724-4397-a610-f44a49f57d80");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, x.length, x[0].length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c1267ef8-fe62-4197-b54e-b3b33d3d8fe9");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a829c4d4-ebc5-4f16-9fae-1c209850ebff");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ff9e4612-4eda-411f-b69c-5dc132ac2494");
        int nextr = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0dbe4081-4916-40d2-a2ac-fcea3a1346fb");
        double w = wi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6863e4bc-bf11-465b-9a5a-2c60b66e7329");
        double y = yi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2f65d706-9ffb-49a4-819c-61f6d7aa0099");
        double xi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a975599a-df6e-4efe-95cf-9b5f76076cec");
        double di;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "233fdd37-72a6-4e1f-81fc-3d518fe8c2a1");
        double wxi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "eac91d30-fd15-4a4b-a329-90e80b85ddc7");
        double dpi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f0046bad-ec57-4131-b2ed-fa47d6be4290");
        double xk;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ca4dcaf5-9772-4fe0-8dfb-b05358004b40");
        double _w;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2677c50b-b631-4ad4-819e-0835fd8b5923");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3876604a-22a2-4c8a-a1f2-4786f4823feb");
        sumy = smartAdd(yi, sumy);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0954e138-bfd5-452a-84e2-c714f302b532");
        sumsqy = smartAdd(sumsqy, yi * yi);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c7f550f6-6241-4704-b477-db3dc97af6f2");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "341df812-41f2-48d4-b4be-90e74d999be1");
            if (w == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ae612009-3447-4fef-a46c-45557d93f0c1");
                return;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9cfd6a8e-4c51-4045-9727-c3497cd0a213");
            xi = x[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8222e1f2-ada5-49bb-9fe1-43bf532936fa");
            if (xi == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9eff3a00-7e22-42ae-9a72-78ca577ae6b8");
                nextr += nvars - i - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "06130d73-91e9-4882-93e8-2f5719028eca");
                continue;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "51f092fd-ea00-4ce7-9dad-491ed1c10bf0");
            di = d[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "343484ba-b867-42d1-bba9-be49347879b4");
            wxi = w * xi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9d645fe2-19ea-459f-96cf-8ec1e09c76de");
            _w = w;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "163d4e9b-36fb-4fe5-9483-5b7d172e28bc");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f2e962c8-2474-4817-9cd9-df8ecda71638");
                dpi = smartAdd(di, wxi * xi);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ea87528b-383b-4416-8d58-98bfa5d6d37c");
                final double tmp = wxi * xi / di;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "17bcb51f-5a5c-4e05-8fd7-490169598a62");
                if (FastMath.abs(tmp) > Precision.EPSILON) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9e38b3e1-3179-424b-b1bc-7555f5f9f69f");
                    w = (di * w) / dpi;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0ac73881-b5b3-461b-a0cd-84b68d927743");
                dpi = wxi * xi;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dbb9bf91-5e5b-4e66-945d-024e1dd6b336");
                w = 0.0;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "afcf3bf6-b3b4-4360-ab33-6de503704539");
            d[i] = dpi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "281f351e-a7e6-48ff-864a-191b53e2b1c9");
            for (int k = i + 1; k < nvars; k++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7b45f6a7-f9b6-4686-add7-d7eb78f379f1");
                xk = x[k];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2c885efb-f463-456c-aaf1-d79eaf645546");
                x[k] = smartAdd(xk, -xi * r[nextr]);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7fd458d0-aab3-4e89-9431-a649c63ac9b2");
                if (di != 0.0) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2d5aba78-1f14-46b5-b3f3-44114777ba8d");
                    r[nextr] = smartAdd(di * r[nextr], (_w * xi) * xk) / dpi;
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8399893b-7509-44a5-b6b7-30227bce7fba");
                    r[nextr] = xk / xi;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ab1fad7e-e1c9-4b00-9608-f45b421caf11");
                ++nextr;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2fd4360a-4adf-4129-bd8d-1ec7f3a64786");
            xk = y;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f1702fdf-96ac-453e-9645-999420390e42");
            y = smartAdd(xk, -xi * rhs[i]);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "29ffa7a5-9e69-4d9e-a17f-4964f647843b");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "54194b3b-4b6c-4202-9cff-b92d33e8fb8b");
                rhs[i] = smartAdd(di * rhs[i], wxi * xk) / dpi;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "10a023cf-5582-4639-b7cb-d392f9c93398");
                rhs[i] = xk / xi;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fdd60285-a20e-4b91-9425-f1ff97bb5475");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b69dcae9-1b70-4f6a-b5a4-64d0a80e2e69");
        final double _a = FastMath.abs(a);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "75cc952c-b6f9-45e9-bb25-337320467f2f");
        final double _b = FastMath.abs(b);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7c9d93a2-2601-4d1b-8edb-b3ff0a1adfd9");
        if (_a > _b) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "13e10581-6a72-4940-b867-feee6c227cab");
            final double eps = _a * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a6e98508-044e-46e3-9b9b-922d13b46e22");
            if (_b > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "554faf14-5b90-48cf-8e45-60de7989bfa6");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b102208e-52c8-48ee-832c-d7af5ae86e09");
            return a;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2cb3cac2-d04e-4862-8353-9edd258a9167");
            final double eps = _b * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7cdb429e-b28d-40e7-824b-604035c98391");
            if (_a > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "86cfb21d-68f6-48c2-a97c-9ee1d017f0c2");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "769ec78d-614c-4c09-8a64-d27cf53104b3");
            return b;
        }
    }

    /**
     * As the name suggests,  clear wipes the internals and reorders everything in the
     * canonical order.
     */
    @Override
    public void clear() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c96d0c46-2d8c-4ddc-8e7d-fad354367ee2");
        Arrays.fill(this.d, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f9f2af59-3fb3-4721-898e-383e6e22730c");
        Arrays.fill(this.rhs, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "63348248-c4a6-4c72-bf52-d39cd48d0176");
        Arrays.fill(this.r, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ae7f4ffa-7623-4826-9bd7-805ac850d419");
        Arrays.fill(this.tol, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "41a51e9d-b4c8-4984-9c74-b545657e921c");
        Arrays.fill(this.rss, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "38ae9dac-337e-4d6a-927c-5726af6d69c8");
        Arrays.fill(this.work_tolset, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4164bca4-ed0e-4e1b-9ece-48a89ff8c67a");
        Arrays.fill(this.work_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9ab88515-a852-4e5c-aefa-fbf758e07929");
        Arrays.fill(this.x_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "59052273-0a8d-4a86-b7d9-dcfd7c120447");
        Arrays.fill(this.lindep, false);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "607cd9cc-4749-4e54-8405-f17d8c8ae252");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9f2f5128-d7b9-4ffe-88f3-36bc5e0ca396");
            this.vorder[i] = i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "405a8ef0-b6c3-403a-8ce4-f6ce1fdc0775");
        this.nobs = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "37aad0ff-b885-435a-89f0-09a0af7abfd4");
        this.sserr = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1d00cdad-d02f-4cd9-a3b8-33f0d51017e7");
        this.sumy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "35871ba0-878e-4a70-b2e2-45d2393550b1");
        this.sumsqy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ccd850b5-907b-450d-bb2e-3654c262e29f");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ca4ff082-0f41-449e-9535-b9e6f7c96a26");
        this.tol_set = false;
    }

    /**
     * This sets up tolerances for singularity testing.
     */
    private void tolset() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "df2259d0-3d23-4410-8826-348947c9fd7a");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f6899816-f949-4815-a1fc-3ee5e4e5514d");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "af715893-8154-4260-8329-84d42956eb5c");
        final double eps = this.epsilon;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e54d5ddc-7948-4350-9eed-54872241b96e");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "bbc1eef0-b808-4770-b5e3-e60e2b33cbbb");
            this.work_tolset[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6df67886-22ca-4c10-8c66-8232cf2df485");
        tol[0] = eps * this.work_tolset[0];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "200e2533-9218-457f-bad0-6aee02916bbc");
        for (int col = 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "88c4db73-d61e-4695-b74e-9819f9176adb");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "722d9c82-43e7-4f51-b020-c9889b322dd3");
            total = work_tolset[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6dae193e-31c8-44f1-ba15-217385906338");
            for (int row = 0; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a3a0cecf-3414-4b54-b068-adc5fbe7f811");
                total += FastMath.abs(r[pos]) * work_tolset[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f3fbec8f-7297-44b3-bfdf-17ff8f8d2212");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "77977eb0-47f2-4765-82f5-749e20c303eb");
            tol[col] = eps * total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e6047e16-d72b-41f9-905b-ce38d56cb788");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "63372c0e-0d39-4bf9-b1c0-e13fff7b6b75");
        int nextr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cfff7626-9cbd-45df-99bf-eae76c605c80");
        if (nreq < 1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "de0e1788-801a-4a87-89d0-67898eae3ef7");
            throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6e42c10c-6878-4c46-b2e4-8f6a098035ba");
        if (nreq > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "67af4290-f776-4562-b2f7-7103692152aa");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, nreq, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "68a54333-53b1-4e2b-b104-35dc4bc4ddc3");
        if (!this.tol_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fa0e0ca8-5037-4b5f-bc73-3d5da5d0c4d9");
            tolset();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "15b4adad-b04e-4376-b6f2-aeeb2eb50b3d");
        final double[] ret = new double[nreq];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8ba02c9a-bb33-4bcd-a035-ae75f7c1ec9d");
        boolean rankProblem = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9d4b36f8-ad2f-4b14-a712-e6d8efd5bd10");
        for (int i = nreq - 1; i > -1; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0a1c6830-1af2-46d7-a7d0-e7a3007b5f7d");
            if (FastMath.sqrt(d[i]) < tol[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7b3d9f29-5e65-43a5-8140-d8fb9f1e90cc");
                ret[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "eaffc11e-62ec-43f6-9b50-4f61cb39b8e7");
                d[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5a840060-bd6f-4e8c-83e3-6c8676c950d2");
                rankProblem = true;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1cc73aa8-1ad4-4243-b8e9-c431f42c6a7c");
                ret[i] = rhs[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5891e522-a6fb-409d-a7c1-b3577eb32bad");
                nextr = i * (nvars + nvars - i - 1) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "70065c32-0e70-4336-b0cf-2543587ee457");
                for (int j = i + 1; j < nreq; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c39a6999-61b5-4b00-b1b0-4249bc07b27b");
                    ret[i] = smartAdd(ret[i], -r[nextr] * ret[j]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d99e670c-f691-417c-a9d6-d6dc9dd5b55c");
                    ++nextr;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7be92e7f-cd2e-4d15-aa67-d2476cf05af8");
        if (rankProblem) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "df5c17ce-9fcc-45a1-83f0-ca75b7f212b4");
            for (int i = 0; i < nreq; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "51aa8d15-28e6-4863-8cd6-4be777140168");
                if (this.lindep[i]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f4350acc-adcd-4208-9342-48ac1af5309a");
                    ret[i] = Double.NaN;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c028ca9f-cdf7-4e6d-8d7c-e055675dd5fe");
        return ret;
    }

    /**
     * The method which checks for singularities and then eliminates the offending
     * columns.
     */
    private void singcheck() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6fdbdbfd-affd-4676-baf5-04659392861e");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2d38b62f-14dc-4cdb-985b-d017b95158cb");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fcf0a011-f266-43d8-ba37-ac40a77beebf");
            work_sing[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1bb875be-6db2-4a02-8883-ef7948a57ad3");
        for (int col = 0; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "56633f8f-5c30-4d6b-99e5-1287a063f213");
            final double temp = tol[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "af91d500-7716-4210-a44a-b91a8ad2b36a");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7aa07ac1-0da8-47c6-b221-8ffcde5caeaf");
            for (int row = 0; row < col - 1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f1c96b41-1c2b-4229-a4e5-96d1a433337c");
                if (FastMath.abs(r[pos]) * work_sing[row] < temp) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "eab95841-9784-4912-808d-7be82cd861fd");
                    r[pos] = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7839612c-926a-4b8d-b3c7-b5368e4e4a05");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8e393062-ad13-4348-a86c-6e14b2f3c107");
            lindep[col] = false;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4d1966e2-ef9b-4208-acda-0d1a5221bfdc");
            if (work_sing[col] < temp) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "21ebfbe4-597f-4713-a423-4c370addf2ce");
                lindep[col] = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3db533f5-77aa-4136-9364-0d3e25abec00");
                if (col < nvars - 1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0d092d42-1e70-41b0-81ae-e62650c25c5e");
                    Arrays.fill(x_sing, 0.0);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "977bc79e-7fec-4240-8481-d454fd541b30");
                    int _pi = col * (nvars + nvars - col - 1) / 2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "85589bb7-0314-4a42-851c-71edc95a407d");
                    for (int _xi = col + 1; _xi < nvars; _xi++, _pi++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ddb247ee-ac80-4616-b2e5-4e98a3a37293");
                        x_sing[_xi] = r[_pi];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d2c83754-94d5-466f-9541-b29949be7768");
                        r[_pi] = 0.0;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a346fbf0-bd5c-4b14-b233-39f8ee628df7");
                    final double y = rhs[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b11a81f3-b45a-48fa-b7b2-e8429ba137db");
                    final double weight = d[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1dc2cc91-35cc-45de-a522-b1bebca54cd2");
                    d[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e11cc298-fbd4-4c0c-aec0-dafa04e7a584");
                    rhs[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ee8e0ec0-ef7a-44bd-9454-9ff3082a089d");
                    this.include(x_sing, weight, y);
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ed37d309-224b-428b-adca-d914245adb1a");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9ec55dca-595f-43c4-9708-263dbcbfd91c");
        double total = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b251c84b-69a8-4b48-97c4-d12ae784a5ae");
        rss[nvars - 1] = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "49a82197-4026-4670-8669-80752cde5cd2");
        for (int i = nvars - 1; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1c065caa-f5f4-452f-a624-4ad303b7db29");
            total += d[i] * rhs[i] * rhs[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5ad1ad21-45c1-49e8-b413-b87378f541fd");
            rss[i - 1] = total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "26edaa3e-ee01-4223-8c1e-e17a8149385b");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7d310a2f-a5ee-4e9e-a43b-6cce7400ccaf");
        if (this.nobs <= nreq) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c230e1e4-7f5b-469e-bd3c-924cae0e0f58");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4d4e06a9-6365-4178-9fae-e522703a828b");
        double rnk = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "43d65775-c790-4f7f-b9e5-9850cea5c43e");
        for (int i = 0; i < nreq; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1fa05ed5-426e-43c8-8efb-6bd51f5060e8");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "086c21c2-298a-425d-a178-683c1d65168b");
                rnk += 1.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "182bad9c-148a-4554-8b92-053f61c45a55");
        final double var = rss[nreq - 1] / (nobs - rnk);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "080259b4-3559-4c95-8735-76fd435145fa");
        final double[] rinv = new double[nreq * (nreq - 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ba1d3ee5-de9c-4166-8fc5-149dde38ddc8");
        inverse(rinv, nreq);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "52958954-bfd9-4c95-ba2d-450b12dbac7d");
        final double[] covmat = new double[nreq * (nreq + 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3dc7f5ff-dd96-4b83-95a1-87134e4a4423");
        Arrays.fill(covmat, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "30496620-f782-4e4c-8c95-04523aade7c2");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "89e6b787-6553-4b79-a9eb-0ce341a188e3");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4a132ad6-9692-441e-8f1d-caed451edd7c");
        int start = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2b977b4e-f4fd-489d-9438-c160c540dee6");
        double total = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e51b2457-2062-4a75-8465-06a692a473a8");
        for (int row = 0; row < nreq; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dff9c874-639e-4a18-bb89-30564a234c3d");
            pos2 = start;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ef4fdf34-f75d-4bf1-ae70-039da6989ffe");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1c725ef9-c596-45f9-b931-ab6a7651e224");
                for (int col = row; col < nreq; col++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1af598e4-5917-419b-870e-ece676248124");
                    if (!this.lindep[col]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d0dcb3c2-429d-4534-81aa-9a6ac5febdab");
                        pos1 = start + col - row;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "50422364-072d-46b6-9d05-5abbb25af504");
                        if (row == col) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "00fbd896-2339-46b8-bad0-b6c48790a90e");
                            total = 1.0 / d[col];
                        } else {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b71778bc-92c2-45b3-bce1-fa4a95c72b23");
                            total = rinv[pos1 - 1] / d[col];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "47ecae32-8ce8-4412-b416-3f21c8731582");
                        for (int k = col + 1; k < nreq; k++) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4d2272fa-6c66-4a86-a0bc-d8f6145fb36a");
                            if (!this.lindep[k]) {
                                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3a4539c5-da37-4f05-ae70-e26608c2e158");
                                total += rinv[pos1] * rinv[pos2] / d[k];
                            }
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4d746977-86ed-4a40-9048-03946114bd57");
                            ++pos1;
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a6b42ef9-5668-4d7c-99e3-4c8286027606");
                            ++pos2;
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "31d6ef9d-33f8-4bcb-b0e0-48817fe22c8a");
                        covmat[(col + 1) * col / 2 + row] = total * var;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "675c9523-3dbe-4dd5-9a9e-a8c44b192ba5");
                        pos2 += nreq - col - 1;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6e8f350a-f4fa-4644-94a1-9353b64ec284");
            start += nreq - row - 1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "aa7edb57-1b8d-4a6d-9690-66daeba0df4b");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7086eb09-28e8-4b7d-ab94-78abce40e7fd");
        int pos = nreq * (nreq - 1) / 2 - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4cd7eb9e-fb35-47ab-8654-5950613e64a9");
        int pos1 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "02be0d63-4f27-4ca5-bf66-272bb6ffbe90");
        int pos2 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "282d2026-041b-4ecc-8b28-197846fc1302");
        double total = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e31c61b7-1235-436e-85d5-18a1dde4178f");
        Arrays.fill(rinv, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d1755e64-fc70-4f1e-9775-32210960cfb6");
        for (int row = nreq - 1; row > 0; --row) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7257cf24-8edc-4d63-81de-2cc9657beb9d");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ddfc67b6-ff76-467b-be6f-6ea62edd151e");
                final int start = (row - 1) * (nvars + nvars - row) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cf0ff2a7-326d-4cd5-988e-3c08d6c51c40");
                for (int col = nreq; col > row; --col) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "74d74977-cf4b-4457-ab78-451c87ca363c");
                    pos1 = start;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "50cded7f-f024-4d0b-b4b1-1dd8681e022a");
                    pos2 = pos;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8855fc50-6a08-43df-95be-3fc7f2dfa664");
                    total = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "65f8ad66-bf8b-4059-83af-66a0776cddc1");
                    for (int k = row; k < col - 1; k++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9f84159f-0961-4331-873e-a2c965f6ef1c");
                        pos2 += nreq - k - 1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9a2c74d0-854c-4316-8384-f6006be61fcf");
                        if (!this.lindep[k]) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dd55beb9-c834-454b-9a75-932329c2eb4c");
                            total += -r[pos1] * rinv[pos2];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "611728cf-80d9-4453-99ef-7bcc0102c78d");
                        ++pos1;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "970178d6-de62-4b30-9b1c-2438d45a56a4");
                    rinv[pos] = total - r[pos1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "bcb82748-327e-4840-974c-c08a882bf5ab");
                    --pos;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "649805c6-2c8b-4f38-8b99-673365d273bc");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "70af2b10-e712-4980-bd72-2da5a9c75646");
        final double[] output = new double[(nvars - in + 1) * (nvars - in) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "27967f91-929f-49df-aec0-95c1acd3ea18");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "207c8dc9-ed7d-4f85-ac89-5837bee7be9b");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5d40394f-5925-4233-ba4c-fee84f2fda60");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dae9cf33-d44c-4cc2-b52a-8f6528ff7c37");
        final int rms_off = -in;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a8ca351e-8fad-498d-8dc8-b422c2cd30ce");
        final int wrk_off = -(in + 1);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "21b76baa-13fc-49bc-a8f7-e1bc84be90f2");
        final double[] rms = new double[nvars - in];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "70b5e40d-c1a8-48f4-9590-e5a4b6b7ce9b");
        final double[] work = new double[nvars - in - 1];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0ffcc174-9322-44ef-a03f-426c9b1a03e4");
        double sumxx;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "457f7ed7-6066-4a19-ae60-f1c3fcb2c0a2");
        double sumxy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a4bb3d8c-bb92-400f-aba2-f2db77ccac39");
        double sumyy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "66e88a12-68bd-47b5-8156-32fa3b267040");
        final int offXX = (nvars - in) * (nvars - in - 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "20f0cebb-31d2-430d-885c-90610b22b7a2");
        if (in < -1 || in >= nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ab12fb07-07ae-4163-b4e4-012a8cb8955f");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "eefa8b94-f1a4-4b2c-ba94-ba7434b41fd2");
        final int nvm = nvars - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "23bd1745-d3df-4749-a3c2-a5de2987186f");
        final int base_pos = r.length - (nvm - in) * (nvm - in + 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9fd7e311-9280-43de-8c5c-8a5b757287e4");
        if (d[in] > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "438c632c-d0df-494b-a9f6-552cd04ba0c4");
            rms[in + rms_off] = 1.0 / FastMath.sqrt(d[in]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "65adbea0-9e83-45e1-a9aa-1de0cce742e8");
        for (int col = in + 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3609502d-5ec6-46d7-b86c-94f8b3e181f6");
            pos = base_pos + col - 1 - in;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5b56622f-66a8-49c8-aa6c-054a728c2262");
            sumxx = d[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "04f55d31-71d6-466f-a6bb-2ac90c3e3c54");
            for (int row = in; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cf219067-4c43-42ce-b076-8bc4f7e0eecf");
                sumxx += d[row] * r[pos] * r[pos];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "84a6cc12-2688-4679-8091-f44f04161a3e");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7f0e8b87-5510-46a2-af26-8cb73944dc06");
            if (sumxx > 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2433df63-5658-49f2-9525-9921b81954e1");
                rms[col + rms_off] = 1.0 / FastMath.sqrt(sumxx);
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "737fb56d-cd68-4adc-9828-b3b9fe026fb3");
                rms[col + rms_off] = 0.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "84675d99-a1d6-40d8-9c5d-822cd10c72c4");
        sumyy = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "64837b5f-88e2-423a-bf87-76a163e3f14a");
        for (int row = in; row < nvars; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4ce1c7ac-cf09-4126-9185-771264eceaed");
            sumyy += d[row] * rhs[row] * rhs[row];
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "aba85cdd-8d59-46a1-8fef-571d30a28d47");
        if (sumyy > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "097810e1-9952-48f8-a603-6e8421b64055");
            sumyy = 1.0 / FastMath.sqrt(sumyy);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "77c10e45-c0b5-45a8-b5b2-195486d5ef4f");
        pos = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a3db7031-3050-4d03-adc8-2bcd56a4505e");
        for (int col1 = in; col1 < nvars; col1++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "78d89f8a-e4b1-4e91-9dca-08bf578e26ad");
            sumxy = 0.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "90e919be-e20a-43f3-a581-d6f329336726");
            Arrays.fill(work, 0.0);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a2cf769f-0469-4db9-aeff-87f1499d9a72");
            pos1 = base_pos + col1 - in - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f6f5270c-b371-4415-8fc8-5cd0137b7769");
            for (int row = in; row < col1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "60504ff7-7d71-4f24-88f0-a294fe3a64a4");
                pos2 = pos1 + 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3f322eab-db35-49af-b61a-f2e5bc95047b");
                for (int col2 = col1 + 1; col2 < nvars; col2++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "50bd789c-05bf-41cc-8268-add38312924a");
                    work[col2 + wrk_off] += d[row] * r[pos1] * r[pos2];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a0045851-4656-4895-9321-2a5fc880e131");
                    pos2++;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ebadcd9e-2ad9-4163-b8b0-7507261bbb13");
                sumxy += d[row] * r[pos1] * rhs[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ac011bef-e103-4a01-84a5-797e7985fdce");
                pos1 += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7ffdcf3c-8194-46d8-9991-bd360ec37c68");
            pos2 = pos1 + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c68c8364-2d7f-429f-9ba1-69b55ed6387e");
            for (int col2 = col1 + 1; col2 < nvars; col2++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "65a2ea51-0889-4471-a7eb-ddaaa6d5f30a");
                work[col2 + wrk_off] += d[col1] * r[pos2];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a01dcad4-97e8-468e-9372-3635a71d9bdd");
                ++pos2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "554186d2-29cf-4f83-88e8-6fddf850a345");
                output[(col2 - 1 - in) * (col2 - in) / 2 + col1 - in] = work[col2 + wrk_off] * rms[col1 + rms_off] * rms[col2 + rms_off];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a259cec0-7b78-4ba4-9420-a0a5096871c9");
                ++pos;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2b3d9e7a-0267-4806-ad26-c4dd3c801b92");
            sumxy += d[col1] * rhs[col1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e9a6e492-1533-42c5-bfb5-7bc6c32f9c10");
            output[col1 + rms_off + offXX] = sumxy * rms[col1 + rms_off] * sumyy;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "85fb1b19-cd71-4e97-a382-bf9c92fa9f41");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5279b860-093a-436c-986a-efd46462eb3e");
        double d1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "006f2450-e4bf-462c-b647-6e22fb341bf6");
        double d2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6a6d714f-3517-4e7b-b255-da72039bebf2");
        double X;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "171d369d-233e-45e8-868a-358d6f7eb61f");
        double d1new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "07f961ca-683c-42a2-b745-fe3bc1380875");
        double d2new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3d50bf93-ca76-42a1-b2d8-2ddfc7ee2c02");
        double cbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4dfd8769-3579-4227-9437-ce7bce038b0c");
        double sbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3437dffb-b7c8-4cd0-a570-7726569b230e");
        double Y;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ed85a19b-b53a-4592-8103-6ced8755a3a8");
        int first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c317dda5-e7f6-44e8-905a-c04bf01c1bfc");
        int inc;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "abc7cb4f-c8fe-4f2b-bddb-8d61ad4f858f");
        int m1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2d658351-6123-4378-9598-de3c2ec81e79");
        int m2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3627e360-1bff-44c9-8d9d-821e3615e9ae");
        int mp1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ef96d45d-c4ed-4eba-ac8c-701a7317f0a7");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "703dddec-e13b-4f7b-880d-f4b0425d927e");
        boolean bSkipTo40 = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "15979ad9-494c-49da-8fbc-ea77c92deb54");
        if (from == to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "605a3f1b-844d-492a-a6d7-a4767726c660");
            return;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9a5e275f-924b-4c44-be1c-573be086efb7");
        if (!this.rss_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "97d697b8-8158-4573-9a2b-b5b25acffafa");
            ss();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "82bdd66e-45fb-45be-984c-04ba151d7fb4");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "58aa0b17-8f32-4b49-acae-9e0ae0b69b48");
        if (from < to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d7ec5c50-76a2-48ad-a1c2-699224315395");
            first = from;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "03aeec1e-6916-439a-a1c8-2dd572c111f4");
            inc = 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "02fbbd7a-2166-4515-b3a8-0b4b36542e0e");
            count = to - from;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7121a01d-58d4-46a9-824b-b709e1db328d");
            first = from - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ca3db85a-2599-4426-97a3-7d875d3f21ca");
            inc = -1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9d05ec7c-d24f-47bf-a895-942e69a30d1f");
            count = from - to;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d71aefff-b0c4-4f47-a979-337fd3a9f56a");
        int m = first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "09686890-d460-42ff-99ad-56450edad5ef");
        int idx = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7ff228d3-272d-42ac-95d7-e4b30e686cc5");
        while (idx < count) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "bc38258f-651e-4bf8-aa2f-d055ae627d24");
            m1 = m * (nvars + nvars - m - 1) / 2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6b85a852-3a74-46d9-b562-fd7dea602612");
            m2 = m1 + nvars - m - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "38272d3a-27e7-4e68-ad17-488782e06a45");
            mp1 = m + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8c4221f2-3d29-44a9-af70-6ca9c1541e32");
            d1 = d[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "16c787a9-1161-4443-b2fc-ca4e3e6c28d6");
            d2 = d[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a6e9d704-c101-4e6b-bf17-f0ab6e58ca23");
            if (d1 > this.epsilon || d2 > this.epsilon) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b879d05a-d3fa-4d01-bf41-4a3c296edc8c");
                X = r[m1];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "eb7eae8a-edd2-44ae-9c69-e07ead8d17e0");
                if (FastMath.abs(X) * FastMath.sqrt(d1) < tol[mp1]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "852ff975-1dcc-4b91-b709-05d7d3d9a62f");
                    X = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "76e4e0cc-af51-4265-b8eb-0d713a443c75");
                if (d1 < this.epsilon || FastMath.abs(X) < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "71371974-baa2-4009-b1c3-332e00aab913");
                    d[m] = d2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "876faea3-6303-40e5-9be3-a3fdadf4a684");
                    d[mp1] = d1;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6a995a35-50b6-44af-85ee-944759ea7fc3");
                    r[m1] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "61aa74a7-1bfb-4a9a-b475-b5d098d662f2");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "72edcef5-737a-4b6a-b3fe-1598be1b9cdd");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c4f12c41-6092-45d0-a2c8-58ac71b88fea");
                        X = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "86a5bdeb-9b45-4517-a529-cce2f6a7cc6f");
                        r[m1] = r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a8e5a338-37c3-4969-8902-2da346242043");
                        r[m2] = X;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "184e81c2-4753-4cef-bb6d-055e8e58b968");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e91ab384-6e6c-4447-9d9e-8773fd596ef2");
                    X = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "510459a9-b159-4a3c-a9e6-9a8343f33694");
                    rhs[m] = rhs[mp1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6ec02890-014c-410b-b0b9-2c5feecd380d");
                    rhs[mp1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dbf9a87a-cd41-41b5-8d0a-18a26908e9d6");
                    bSkipTo40 = true;
                } else if (d2 < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "286ac309-6328-49ae-9b7a-63ae632fa620");
                    d[m] = d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "87ad8dfa-7ff7-4216-b176-f12f166d1401");
                    r[m1] = 1.0 / X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0f3f2f77-ac25-405f-a1aa-bb4f7f7979db");
                    for (int _i = m1 + 1; _i < m1 + nvars - m - 1; _i++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7d178d50-74cf-4751-ab60-7fdeb75d9677");
                        r[_i] /= X;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5c44d065-286b-4f29-a759-dc0e4d696ab2");
                    rhs[m] /= X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fdb05272-c945-4980-afaa-8f8c346319a2");
                    bSkipTo40 = true;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "05311412-0809-4c8c-9861-3a29052819ab");
                if (!bSkipTo40) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5489c930-670f-45e3-baaa-00a5cc1fa47b");
                    d1new = d2 + d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4d1cd3bc-6e3d-49cd-ae82-c13413d1ccc4");
                    cbar = d2 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1a02a8cf-98e8-476e-8b7e-8278564a1d39");
                    sbar = X * d1 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4381c16c-abbb-4598-8396-61813221410b");
                    d2new = d1 * cbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7ab08bd9-d4ad-4c23-8f30-6c1676d7b80d");
                    d[m] = d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "83494eb2-6449-4fa0-a9cd-019a3591d954");
                    d[mp1] = d2new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d744ca54-c4d6-4b44-9d1b-5fec8f6d45a0");
                    r[m1] = sbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "59e3b515-41c1-42a9-94f2-020c982b314d");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cc50ce69-c64f-4064-abe8-83b4db34ec69");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "973d77d6-746f-4365-b74b-fc6a090c262f");
                        Y = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fbc0fe4f-bbf0-4d47-bc60-7e5e93e0c4b8");
                        r[m1] = cbar * r[m2] + sbar * Y;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b98ea101-96d6-4ec0-b7bb-1fbdd4341ef3");
                        r[m2] = Y - X * r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6ddcb156-9575-41f4-b2b0-386074f278b9");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8ae484aa-6c56-4883-b981-9c2452afc769");
                    Y = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a30c1439-9c0e-444c-b806-f39f8aee7754");
                    rhs[m] = cbar * rhs[mp1] + sbar * Y;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3f337bf1-15f3-41a3-b7fe-d25f99b73361");
                    rhs[mp1] = Y - X * rhs[mp1];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "04a9a56d-bc5d-4aab-b780-24d634032cd1");
            if (m > 0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4e8789f0-60ca-4e67-9e21-aae472a745b7");
                pos = m;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0942596c-17ae-4e53-b984-ceb4c71bbb24");
                for (int row = 0; row < m; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1ec1138a-836c-47e9-a48d-2ae78dc0f391");
                    X = r[pos];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "96bf9026-73a7-421b-a57d-654d954a18cc");
                    r[pos] = r[pos - 1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e3deadd1-a9d5-4d5a-a349-67425b53ecb6");
                    r[pos - 1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fc34eb1a-5b5a-435a-954c-1507642af6fb");
                    pos += nvars - row - 2;
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2d19b70f-5ece-4859-ba4d-f44c46911f11");
            m1 = vorder[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "26f41c75-9e54-498e-8587-d9f772f7b82d");
            vorder[m] = vorder[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d43f446a-b5b3-4d25-929b-4515c66db6cf");
            vorder[mp1] = m1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9d2ef3e8-3019-4b44-a502-445394cc962b");
            X = tol[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cc80aea9-db13-4a29-aacc-e4aa193da66b");
            tol[m] = tol[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "015550e9-aeb6-4e56-b94b-592dbdde6358");
            tol[mp1] = X;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "70e2b142-2ac9-4db8-9751-5f860623a8c9");
            rss[m] = rss[mp1] + d[mp1] * rhs[mp1] * rhs[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "77509c1a-e821-49fd-9ee2-f3c0315b7d00");
            m += inc;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0882322f-4103-4ff1-aa80-a3f1d3a51aea");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0c5b4b19-4f9c-4e6c-8905-0dc12b096041");
        int next;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "60d77f1c-15a3-4081-8122-730c577601fd");
        int i;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "05c18329-b609-4333-b67c-77627d65f0a1");
        int l;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cf9f68e0-314f-45e8-a68a-877ee0f879d3");
        if (list.length < 1 || list.length > nvars + 1 - pos1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fbb22313-cafd-46fc-b48d-9bf4f0f93fbc");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fbe817d7-2872-4d5d-a693-bd64282e09e8");
        next = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d990e80e-2ef1-4511-92ee-c14d43d70235");
        i = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a4c06d6f-a29a-49a2-b3c2-65e68eadff7d");
        while (i < nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6ce94101-8c52-42e7-b3e7-4ffa884d581c");
            l = vorder[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "725866a3-e7c1-4df2-b7ca-0c22e293c532");
            for (int j = 0; j < list.length; j++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "40b88c49-4a73-4880-9c14-e2748f2b209d");
                if (l == list[j] && i > next) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b86c457b-b09c-42ef-9026-e7c0d67bdbe5");
                    this.vmove(i, next);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0cc15543-f8fc-44e8-a888-a19335b8d1fe");
                    ++next;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "920626ed-7d71-42e9-a3bf-39bda8b6b200");
                    if (next >= list.length + pos1) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cfd73888-8450-478e-8f64-2b70ab77a2e8");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1d781bc3-be36-44f8-aa82-1b5769e101f7");
                        break;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e6f0b6be-7d43-457e-86fd-2bca7f05b0a0");
            ++i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2b8c3033-cf95-4de2-a146-f7ed2b18281a");
        return 0;
    }

    /**
     * Gets the diagonal of the Hat matrix also known as the leverage matrix.
     *
     * @param  row_data returns the diagonal of the hat matrix for this observation
     * @return the diagonal element of the hatmatrix
     */
    public double getDiagonalOfHatMatrix(double[] row_data) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2a4f5fcf-f606-4854-89d9-a09e0ea0b824");
        double[] wk = new double[this.nvars];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cdbeb0da-c301-4a03-98f3-0d9669928e7b");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c7ccebb3-ec65-4642-b817-bdd80da6706d");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "49427b65-a23d-4283-bfa0-50799090b6c9");
        if (row_data.length > nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c38c349d-50df-44e6-bf8d-70c3da6d8278");
            return Double.NaN;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6f3277ff-8c53-4735-ab06-ad72916220ee");
        double[] xrow;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e772d053-3d81-4277-b152-a52ca63ad035");
        if (this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c4b93cbf-c657-4738-a574-2fea2fceffd4");
            xrow = new double[row_data.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ad6f4ecc-eb1e-435c-ba05-3341884e82ed");
            xrow[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3c0573ad-ebac-4312-b9cd-dcb85a8991f9");
            System.arraycopy(row_data, 0, xrow, 1, row_data.length);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "88e3a0b4-729e-4898-bf74-57f3aa27f312");
            xrow = row_data;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "017f9795-b293-42c1-9f8a-2d171cacfba8");
        double hii = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7a60f38d-c92f-4748-a4b7-67bad5ac5a83");
        for (int col = 0; col < xrow.length; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c0641287-e9e0-4b57-a31c-d22698551fab");
            if (FastMath.sqrt(d[col]) < tol[col]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fe93bf28-dd57-46e3-af95-9ed98e5cb26e");
                wk[col] = 0.0;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2fd3ddb0-5c5d-49a1-9a23-46585ca7ad3a");
                pos = col - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "62b35b8b-0cda-4fe8-88fe-f86f4fd59cde");
                total = xrow[col];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "23594087-b0db-4a57-a091-ea33d393db1e");
                for (int row = 0; row < col; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ade3714f-d918-4b87-a728-a5e71eb355b6");
                    total = smartAdd(total, -wk[row] * r[pos]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cb2b8cca-d4fb-4800-b667-d5ec4c283d9e");
                    pos += nvars - row - 2;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b2d15d29-f06c-42ec-80f6-11cccc883529");
                wk[col] = total;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "6b66a2b5-129a-482b-85bc-41a665d91827");
                hii = smartAdd(hii, (total * total) / d[col]);
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cc6d963d-8d1a-4383-9be7-59c534b7c981");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1d2a0fbe-ec7c-477b-b959-cadd6194c4de");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "03a2eb8c-fcb7-41bf-a9c1-2d09a71fbdc7");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "265e4b44-2ab3-4463-9f7c-a2f06e81d934");
        if (this.nobs <= numberOfRegressors) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1c26aaa5-5dd3-4e1b-9293-e11c451bb3cd");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, numberOfRegressors);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "78186348-ed8e-4d12-beba-a74681da9e17");
        if (numberOfRegressors > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d6035cc7-42eb-41cd-8be0-04b162fec0eb");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, numberOfRegressors, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2b666876-dcb2-423a-9108-5c1929be4162");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dc3269d2-9d6b-4c51-b9c1-bb707b3a5c15");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c45722e5-60c6-4412-905c-e23c394a1aa6");
        double[] beta = this.regcf(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "758b56f9-67d5-4335-bcf4-62b7edc57518");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4040cfb4-3e39-4891-9c66-7cd47f72ef99");
        double[] cov = this.cov(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c9efb80f-2b1e-4592-8575-fd5afeed7bff");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "40091f3e-b6db-46fb-8614-9873fbcec7ac");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0472c668-bcaf-4f7e-8b4a-00402fbf5bb3");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8f44e859-d015-467d-a1a5-0cae42bd1419");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8cecf4ea-adcb-4c33-88e1-efb65da7596c");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4006bb2f-f947-480f-8746-21f5cb449dbb");
        for (int i = 0; i < numberOfRegressors; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5cb7b809-696a-4d4f-856b-b9dca3baf2d9");
            if (this.vorder[i] != i) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f71fe3c1-a362-415d-be00-a22c63a9bf63");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fab8f03f-c1ce-488b-b54c-6066e98ac7b7");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d2d1c088-19e5-4191-b36b-4a3651e8d3d4");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "bab9bbce-ad94-4f7d-baeb-4d038e792761");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f6759844-ef05-426e-ad95-382685a64bb7");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9125af1a-caed-43bc-8b3b-e16469f066e9");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "834c9b74-88d5-4d35-9f63-3f7f5c9d9958");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e3852da2-039b-4c46-b199-f86014ef2b9a");
            for (int i = 0; i < nvars; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9dd8025c-ecb7-4bef-994d-c783695b5e01");
                for (int j = 0; j < numberOfRegressors; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0b10662a-ff0e-4906-a5ec-8be7b0765592");
                    if (this.vorder[j] == i) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "96afebe7-0edf-42e1-83ec-175b87c473c3");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "edc16229-7b91-4687-93da-e4e227261d03");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "83798037-1d0b-439d-bef6-2bf971827b38");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "efeedf31-888c-498c-85d3-e88d73242272");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e1b626da-477e-40e5-b469-195353b8842a");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "922c75f8-8851-4e17-8e3c-e6609d8d72c4");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e76da118-65c8-459f-a98a-44b3f43f939f");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "2ac2d784-6378-417a-8800-7164527cc8e2");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "aa11ea4e-c4ee-448d-bb45-1be050ed1de7");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f4a0ffa0-ca79-46b1-a127-dee342c1df2d");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b83af3a8-e200-4561-938d-e05dd9259b7d");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0083e2f6-dca8-4623-9f83-eae1e6e70b89");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c681ee4c-f2fd-44e8-a8c2-f1a13473d491");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dd834520-1b82-4e4b-bb78-94e839c851ff");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "403c5302-cb1a-496e-b450-70cd4950ff99");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "ae1352af-12c0-40ec-a824-2ec8867b1988");
        if (variablesToInclude.length > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "96246f0a-a134-4f72-b147-afbe46c967d9");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, variablesToInclude.length, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "76f605a0-2356-4976-82ee-df3785f9b57a");
        if (this.nobs <= this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "be6b9fb2-d08f-4317-9ce1-3d6e8870e0a8");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "691c7002-a92f-44eb-8833-a274974e545d");
        Arrays.sort(variablesToInclude);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4c648586-936c-4561-abd1-0b1c2efb77fd");
        int iExclude = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "82d74c92-d4b5-4f38-bf51-d10ecd036f2e");
        for (int i = 0; i < variablesToInclude.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c6b2480e-49aa-44ca-86c9-92ce16699878");
            if (i >= this.nvars) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "33a19a57-e458-4c40-8767-edf80da8b89c");
                throw new ModelSpecificationException(LocalizedFormats.INDEX_LARGER_THAN_MAX, i, this.nvars);
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0612a485-2e64-43df-a5bc-e530c224ddd9");
            if (i > 0 && variablesToInclude[i] == variablesToInclude[i - 1]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c9e374ae-d24e-403c-aa4d-d59e7a08b4c7");
                variablesToInclude[i] = -1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "61bcb2e9-b3ff-4ce7-b4bd-3141ea9aab18");
                ++iExclude;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "67137c97-6426-4a0a-af97-4d84c01a3466");
        int[] series;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "d3a13774-91b2-4c42-b757-4da78d3cbb2f");
        if (iExclude > 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "76128126-8104-44ef-9c3b-ec12549e4e66");
            int j = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "57551978-6e46-4b48-b9f0-3f6c86dee941");
            series = new int[variablesToInclude.length - iExclude];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c11fdc98-88a3-4c81-8f01-003a74d69342");
            for (int i = 0; i < variablesToInclude.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8f4e6c20-b52b-4e0d-b79f-d06c1d299c05");
                if (variablesToInclude[i] > -1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1af15af9-ade6-4eb0-928c-e01679d4f74a");
                    series[j] = variablesToInclude[i];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3a9fe314-23bc-437e-9de2-9c02ad29bf82");
                    ++j;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e55a844b-460a-41b5-b212-c3b4e1999870");
            series = variablesToInclude;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "774f2235-11b7-4ace-8fb7-054fef9f22c5");
        reorderRegressors(series, 0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1dec65cc-8c92-4aa8-bdea-467f757b2692");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "fdcde4fd-77c5-4021-90b0-eb3ba5d61b5d");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "266da567-fa3d-43e5-b48d-a21c30114796");
        double[] beta = this.regcf(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9967f45f-0ba3-46d6-a4d8-f00997e28a55");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "671bf61e-e641-4838-850a-bf1bfd03fc02");
        double[] cov = this.cov(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "cf0c259a-4266-4da4-b153-0c1c73c400ac");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4eb6b9b0-b606-4ab4-90d9-c01617d12078");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "af6fe3ef-7244-4cbe-b407-ff0644c765cd");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "a8b36311-c3ac-4021-922e-f9c8e6f8e6fa");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "f64de1a0-c794-40ed-8114-b61566f5390b");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b91e8915-a97f-4a69-b133-083442a1af65");
        for (int i = 0; i < this.nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "5baac5d4-ee86-41ca-a955-2b980023fbd2");
            if (this.vorder[i] != series[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "adfaa6fc-4156-475c-8f1d-6de1f1479ad1");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4a17876a-92f9-4bb1-8fc6-269a57fa076f");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7e6715de-5055-4941-95ea-2c670f031727");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "96f5653b-fd48-4c4d-aec8-0fd5f736c58f");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "428f2141-30aa-4ba9-bb02-0eb4c3a50429");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "7a6934b3-6109-4c2a-9570-be85bc1d8a0d");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "8582a050-fbf0-4546-8a20-8fdb91cf3d0c");
            for (int i = 0; i < series.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c075ba63-b3a9-4b35-a88a-39821293b8fd");
                for (int j = 0; j < this.vorder.length; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "18a56ae8-b251-4809-a2ce-97ed834ce8dd");
                    if (this.vorder[j] == series[i]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3966a909-ff61-4957-a0a4-64e4821f1a10");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "e51db6c3-cdf9-439a-aae4-9090ff07d511");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9a3a67ce-26ee-4b86-89cb-607442ae1b26");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "9ea4622a-c25d-4a51-9dc4-79b90ff989b7");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "94aca35c-d9fb-4733-87c2-ffa042392b3a");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "b6635884-962b-4d40-8b65-0b863f75c866");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "0f181065-6f48-41d1-ae1b-1f1a75b406f9");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "1978347b-1127-4a2c-b3b7-a303297f8bc5");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "46810371-bc04-44d0-b100-8275850beba2");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "05377bc8-015a-4e40-a25f-21ab7f982e9c");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "38d0ff35-ef97-4487-ba09-1254cf45eff1");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "c873ef03-daef-4897-a9b1-9e895ecd87a7");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "dbd6e1db-68d2-436a-8a72-04640107297f");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "4ccc44c9-0cd9-45c7-bd61-9da64ed0a949");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "3c02061a-1853-4b44-884b-95ac1ce455ee");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_3_10.coverage", "00478d9f-1ea8-40d3-be7f-33ec7ad37cde");
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
