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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "192e4674-8e51-4d4f-9cd9-d6a308cb5000");
        return this.hasIntercept;
    }

    /**
     * Gets the number of observations added to the regression model.
     * @return number of observations
     */
    @Override
    public long getN() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1cf33471-0123-43ab-b7a8-1ea9b7abf951");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c5729fd3-b848-40e8-9453-0389e3901b95");
        if ((!this.hasIntercept && x.length != nvars) || (this.hasIntercept && x.length + 1 != nvars)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "94355ce9-a0d5-4d3d-8ac5-cf1c6d985345");
            throw new ModelSpecificationException(LocalizedFormats.INVALID_REGRESSION_OBSERVATION, x.length, nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5b918951-fa5b-4f45-a6a3-832796c6fcdd");
        if (!this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "edb95870-86bd-45c6-905b-d7a4997af761");
            include(MathArrays.copyOf(x, x.length), 1.0, y);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2a599092-cd65-4f61-a4b0-485aa9baa72d");
            final double[] tmp = new double[x.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e116623f-eb2d-4a97-9079-45378a9ecc92");
            System.arraycopy(x, 0, tmp, 1, x.length);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ea29282d-9b15-4a6a-9152-61707bbc68bc");
            tmp[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "203b46f9-93ab-4dd7-a64f-684c4e18ad32");
            include(tmp, 1.0, y);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ca4a6d95-379f-4ee7-a644-968435425a4c");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a6e845ec-7e17-4ea0-aece-aba5c080c1b5");
        if ((x == null) || (y == null) || (x.length != y.length)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "04eb0860-b9c4-41c7-9fbf-231413b669c4");
            throw new ModelSpecificationException(LocalizedFormats.DIMENSIONS_MISMATCH_SIMPLE, (x == null) ? 0 : x.length, (y == null) ? 0 : y.length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3c9526b3-2b08-48e4-829e-e0a03fe6f474");
        if (x.length == 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "87248fbf-f8ac-499e-bfb8-c4f6aa198491");
            throw new ModelSpecificationException(LocalizedFormats.NO_DATA);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c0543689-8b78-45c9-ba4c-f601292f773a");
        if (x[0].length + 1 > x.length) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "201baa5c-4faa-437c-a9a6-bcd2d65abd69");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, x.length, x[0].length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f3d991dd-7544-4769-81b2-098ed33cbd99");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "601d1c88-5071-44e2-a8e3-9809678875a5");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c75712bb-65d3-4063-9c08-176ed228461d");
        int nextr = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6398f497-bcb4-4949-902a-d08a53b3873a");
        double w = wi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "eaac1121-8484-46bb-9ef7-b72fc9eeb069");
        double y = yi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b09442d4-afa8-48ef-8b33-fab06951600a");
        double xi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9c8aa6f3-c22d-41f1-89fe-917ce73f1228");
        double di;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d747667d-177b-48bb-b0ab-a9970b24a4c1");
        double wxi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b14b2983-f6da-4bd8-a22b-0383d869f8ec");
        double dpi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6a445b61-2199-415c-9c69-bdebad091608");
        double xk;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "44ae5bc0-9852-441c-9357-ba408dddadaa");
        double _w;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "79445c43-c2f8-435d-8f67-00ce8f869936");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c9fa1c1d-3542-4fb9-9980-2d63013525fa");
        sumy = smartAdd(yi, sumy);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ab18d265-1127-4f06-92c3-67dcf6467108");
        sumsqy = smartAdd(sumsqy, yi * yi);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4493bfef-4279-4ddb-b188-01c5adb4bf9f");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b0e9c5b4-909b-4c3b-985c-ff7f7457cadc");
            if (w == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ee864028-958b-46cb-b73a-c2f02461e4c6");
                return;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ecc2b056-4ff8-4039-8015-29db83bd0ef3");
            xi = x[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a36bdda3-7750-416f-b083-cf04cb1b6ab7");
            if (xi == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c2cc068b-91bf-4a96-ac92-00fcc3dddef8");
                nextr += nvars - i - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d34740bc-e905-4e06-8ce7-fd72cdcbc636");
                continue;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "31408769-8cb5-4552-af30-d458e3921789");
            di = d[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "850bb149-00bf-4867-b97d-ebc75ed95758");
            wxi = w * xi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2c626080-9b15-4d82-8451-987a94d9b2aa");
            _w = w;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b65835dd-99de-4d2a-ad94-d145a4a0f5f5");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "225adf81-ca8e-4ed4-9f2d-5939dd907e3f");
                dpi = smartAdd(di, wxi * xi);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9a63a3d3-1217-4fd9-915a-6ae978e2e92f");
                final double tmp = wxi * xi / di;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3409ef1d-1337-4802-9e23-3757e6cef919");
                if (FastMath.abs(tmp) > Precision.EPSILON) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "316e1b77-13cf-453c-8bc2-164d4b93308b");
                    w = (di * w) / dpi;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "86c23325-0a78-48ef-9a7e-896aa44d18d8");
                dpi = wxi * xi;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "48fda036-fd5d-4ce5-b11e-533f028fb851");
                w = 0.0;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6c2b6311-3200-444e-8b90-b64710eb38a5");
            d[i] = dpi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "26e3f0d8-2e92-46f5-b7d4-b9e867073715");
            for (int k = i + 1; k < nvars; k++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d0c8590c-50b0-46df-a3de-cfacf6b84e57");
                xk = x[k];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7edda96c-2fd0-4206-94b5-52d0333b3b3f");
                x[k] = smartAdd(xk, -xi * r[nextr]);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6251bd23-759c-4c50-a272-3c6015681aea");
                if (di != 0.0) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f9a14697-74f9-4146-8227-abbd6869d54c");
                    r[nextr] = smartAdd(di * r[nextr], (_w * xi) * xk) / dpi;
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "efc9ca3b-aa4b-4847-bcf9-c5b4296727a8");
                    r[nextr] = xk / xi;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f5ba5ecd-557a-46d5-8b51-6af5d349d73e");
                ++nextr;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9f8a7aa6-6e98-4785-9076-6d91e95f969d");
            xk = y;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "673278e6-3b88-4e95-a381-d3efd3ebe167");
            y = smartAdd(xk, -xi * rhs[i]);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1c10f489-1b19-4ea8-8451-401251409e2e");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1598158b-e38f-4ca5-afd9-c06be2ba476b");
                rhs[i] = smartAdd(di * rhs[i], wxi * xk) / dpi;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "eb0e9e39-8171-4d69-984b-cb2c3c823b71");
                rhs[i] = xk / xi;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c6eee12d-c625-42c1-b658-8c230209a72c");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "675c9405-9a8a-4e86-b7c5-b0e56dcaa67a");
        final double _a = FastMath.abs(a);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f5884b9d-1ba5-4c15-be51-30dde0b0dd09");
        final double _b = FastMath.abs(b);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e69c9daa-5051-46bc-b93b-7d4e42f70e61");
        if (_a > _b) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "569d6430-ff7d-468c-8a8f-94b2304f92a4");
            final double eps = _a * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e72bfc69-1367-4aaf-9413-017025778e63");
            if (_b > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c590d22e-c869-48e3-8da3-59efb24da8a1");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3911c30d-583e-4ba7-b5cf-d42776c4a2b0");
            return a;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "91d17da4-1837-4a51-bee1-0d5faf531ea6");
            final double eps = _b * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "874c4350-1db3-415a-a3c7-41e6fc1ba7b8");
            if (_a > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b5f598ed-e32a-4c4f-8b47-616024893388");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d3777cff-dc09-4b9a-be80-5c7c94eb4001");
            return b;
        }
    }

    /**
     * As the name suggests,  clear wipes the internals and reorders everything in the
     * canonical order.
     */
    @Override
    public void clear() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "80f05cb6-3978-4918-86c3-b1b85763e421");
        Arrays.fill(this.d, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "29d3a142-aa2b-4cce-86a4-1bdc6cabce11");
        Arrays.fill(this.rhs, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "98e09ee3-69a3-4b37-83cc-3faf150eebc8");
        Arrays.fill(this.r, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "99e18a9a-4f2c-4727-ae29-fd69fcdf98a9");
        Arrays.fill(this.tol, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c83655e6-e70c-4895-a9ca-2aa83faa0d5c");
        Arrays.fill(this.rss, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fd138946-3c87-4b73-855a-fd0a9bbcb72f");
        Arrays.fill(this.work_tolset, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7324ff3b-819a-4865-861e-c6c6d1124245");
        Arrays.fill(this.work_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "97c33c3d-487d-4970-a11d-fd5776552645");
        Arrays.fill(this.x_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f61127a5-a386-4d3f-91c5-01a5d2109e13");
        Arrays.fill(this.lindep, false);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8994397d-3b31-48d2-b425-8a8b102dc4c1");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "52064574-4b95-4c4d-9cf5-201b5849f2cf");
            this.vorder[i] = i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3a7fb828-646f-4b8d-8ae7-24e67d7d5ca5");
        this.nobs = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f350e798-ca31-4e3d-9154-1178bdbdec6c");
        this.sserr = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "28b1d1f2-ebba-4971-8f90-61a5cff8edbc");
        this.sumy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2d1decdc-d0d3-41a3-b4b4-5d99a36453f0");
        this.sumsqy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "78080b23-b4f2-4004-b2f5-9597c3c420a8");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1aa34d8e-2e3a-433b-a174-fe983349908f");
        this.tol_set = false;
    }

    /**
     * This sets up tolerances for singularity testing.
     */
    private void tolset() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fda74369-b457-46f8-bfb5-5d51ebbdf338");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b70a1538-c2f6-4d99-a312-94473eedd3d6");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "38fbd5c1-89ab-4041-8fe8-00051c78946f");
        final double eps = this.epsilon;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a1241cda-1d5b-40db-a2f9-840a56e61b1b");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e72c7831-ca28-47ac-91d3-038fe9e4dc82");
            this.work_tolset[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7b1166b8-770a-4b6c-8313-c6ca468a0c88");
        tol[0] = eps * this.work_tolset[0];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "511e179c-2b5a-47fa-bc2f-25baee78ee4e");
        for (int col = 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "730d87e5-f073-417b-a8ae-43357024a07d");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "95e6de36-1106-47ce-89a8-de5b49a3679e");
            total = work_tolset[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d51dcc92-5309-4da7-8ce3-a3be0ac8df2a");
            for (int row = 0; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0ca01a61-07bd-4c23-8227-2fe6ea8ab16e");
                total += FastMath.abs(r[pos]) * work_tolset[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2b852ad5-b06a-4e75-9989-e1592ffb7585");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "48d232d0-a23d-4361-bb79-c7b9c5fabad8");
            tol[col] = eps * total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "30c49be1-a0d7-409c-9504-53a52d43c124");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cd94a5bf-b6a7-4965-b044-fed3cdc1d090");
        int nextr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ff8bbc64-ef1f-4635-8df7-b0833c40b309");
        if (nreq < 1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fd520913-99cd-49e6-88e5-f2ff8541a3ba");
            throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dbe448bf-47e9-48e0-a145-630141308685");
        if (nreq > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6cea9333-bbea-4505-9d73-de7abfd3b29a");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, nreq, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "41e3aa0f-0680-4b6d-b577-7bdb9b60ea12");
        if (!this.tol_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bfc6d9af-beba-4d84-bd24-7432d1e22c6d");
            tolset();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4c33ce80-51c7-446c-bde4-4164870f61bc");
        final double[] ret = new double[nreq];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "65dd2512-c2bb-4f2e-87e5-ce115ce0b268");
        boolean rankProblem = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "029b0486-187e-4cf3-949c-979597e1550c");
        for (int i = nreq - 1; i > -1; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7487dfe7-ddd9-48e5-8d8d-591d976209f8");
            if (FastMath.sqrt(d[i]) < tol[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "287df55a-34be-4106-b87f-7c3a4705b037");
                ret[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9311f2f3-28d2-466a-b538-03f6888be735");
                d[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dc2acdec-d7e5-4ea6-9661-f55e8ff14246");
                rankProblem = true;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a91fcf86-9d5c-496a-8c83-b8599beff159");
                ret[i] = rhs[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "919f8045-2c3b-418b-aa8e-48e59d5e01af");
                nextr = i * (nvars + nvars - i - 1) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "999d61a9-1c6e-432e-8e00-869327420637");
                for (int j = i + 1; j < nreq; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4900a235-f3f9-4a27-b3ab-080295f5bef0");
                    ret[i] = smartAdd(ret[i], -r[nextr] * ret[j]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "624694db-9540-40e9-93de-36f09950b166");
                    ++nextr;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "92ca5177-9cb5-480f-86d0-c2dd02051580");
        if (rankProblem) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4ad222b0-c537-492d-84bc-351319cae16e");
            for (int i = 0; i < nreq; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "569047ff-518a-4a85-8895-fc78d22cfb1b");
                if (this.lindep[i]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0073a1ed-2f10-40c1-beec-4718be6a89d8");
                    ret[i] = Double.NaN;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6c347c57-0dab-417d-b61a-17ac99f92576");
        return ret;
    }

    /**
     * The method which checks for singularities and then eliminates the offending
     * columns.
     */
    private void singcheck() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "30b6c4fd-5031-48f5-a732-15bce4b2d499");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "759e7b6c-0d57-4247-bffa-4fe612a83ec1");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "59efbd9e-7deb-4d60-9585-02e8af257fde");
            work_sing[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d9b1de3a-5455-487d-a119-d2b2f48eaf18");
        for (int col = 0; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "30a0767a-c744-4cde-84f3-c7687792189b");
            final double temp = tol[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5386886e-bcc8-4c4f-80fd-eef0684edda3");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "726545d3-175b-43f2-80ec-9b32516b3fda");
            for (int row = 0; row < col - 1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cd406c43-0091-4ea5-9648-cce26eb273ed");
                if (FastMath.abs(r[pos]) * work_sing[row] < temp) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1f952fec-d3c4-47f1-a095-522fc60c0d5d");
                    r[pos] = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f927754f-7dab-43b0-a9c5-5f0ecac01163");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2c9507d1-2d7d-4d2c-9da8-f08aa1fbc729");
            lindep[col] = false;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "aeb8794b-c3c2-44b5-8531-8074c1ebe8d5");
            if (work_sing[col] < temp) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "48b7a35c-28a5-464c-8b3b-9bfddb624c17");
                lindep[col] = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0645dfce-96ca-4594-aab2-028b5871ae5d");
                if (col < nvars - 1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4fd13e15-239d-470f-89e4-bb26da88b311");
                    Arrays.fill(x_sing, 0.0);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "577a00ed-9af6-4c78-b92b-103ed7b76525");
                    int _pi = col * (nvars + nvars - col - 1) / 2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "71ef8c80-639d-44bb-a87d-980640e86c50");
                    for (int _xi = col + 1; _xi < nvars; _xi++, _pi++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fdfd8011-4d0e-4d1b-bf2a-27ff980b7788");
                        x_sing[_xi] = r[_pi];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c3a3f345-2e29-4180-a1c2-bd0d39943631");
                        r[_pi] = 0.0;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "63b96638-0ecd-4545-8717-aac3dc28ae18");
                    final double y = rhs[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8118b0c9-ae2c-4875-b3f7-da8a6be89aa8");
                    final double weight = d[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "263c7247-45c8-4455-baff-fae1bbe509ec");
                    d[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6482d95c-e30d-4d6b-bfab-a3048de779d9");
                    rhs[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d81420fe-b084-40b3-9f20-a7f9834c35a4");
                    this.include(x_sing, weight, y);
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "168696d0-2970-4511-a40d-dfe60e114105");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "27e92c1c-11df-4897-b457-38807c38d0cf");
        double total = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b07aa368-05c2-4e23-b038-a31ba5153f7e");
        rss[nvars - 1] = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "02957ec8-9a2d-441e-9e34-896cf73e1a18");
        for (int i = nvars - 1; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2b0e8b0d-a149-4c9d-8aed-afd93e52a7e9");
            total += d[i] * rhs[i] * rhs[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4367257e-a45a-406f-bc55-450bbf50c0cb");
            rss[i - 1] = total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d5e00a15-6747-48fa-980a-e6ba8305d473");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e4adce0b-1b6a-44a2-8118-b47a6b9304c2");
        if (this.nobs <= nreq) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b1061286-cb73-4e7e-81a9-c59c415dc91c");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d0418dd8-f9ee-4a4b-a637-f9b431557f89");
        double rnk = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fc9dedb0-6363-4686-b6d6-91785039695b");
        for (int i = 0; i < nreq; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "58df1069-eb94-40bf-8725-edbb5c245950");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "99ce8093-5f14-4298-94ab-32bfc73e30d4");
                rnk += 1.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "15bce78d-99d5-450d-bd7f-b876f921376f");
        final double var = rss[nreq - 1] / (nobs - rnk);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9d330281-fdff-4139-ba7e-f4e78031a1c5");
        final double[] rinv = new double[nreq * (nreq - 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "45869ba5-db70-4e5f-b0a2-60b6ce826ce4");
        inverse(rinv, nreq);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "422fd6a8-19f0-4f36-91c9-05fab305064e");
        final double[] covmat = new double[nreq * (nreq + 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a08da48f-8a32-4ed8-bb56-5a102c908843");
        Arrays.fill(covmat, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d3c787e6-7b61-450c-8810-a1b9265c7f5b");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "40284bb4-dca8-481d-939c-ef5a4b578757");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8a242895-aba2-45ba-98a6-8e0f5cbe36fd");
        int start = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8d9cdaf5-c2f3-4dad-a0f0-5a675301c72a");
        double total = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9d7a3483-52f8-480f-aeec-e2b187d7d4bf");
        for (int row = 0; row < nreq; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bd432f3a-de14-4aa3-82f2-86a4fa73d905");
            pos2 = start;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "eba0b942-71f2-474a-a159-01078c9c2b7b");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "82da2d68-2d8a-4379-9521-fa5b6a3a9986");
                for (int col = row; col < nreq; col++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "12873f2a-5a8e-4198-8fa8-ff3937c08893");
                    if (!this.lindep[col]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "82eeb1ae-bd3b-40f9-bc11-920d80be6635");
                        pos1 = start + col - row;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8f3757a9-a839-49dc-ae7e-27b89f3c1770");
                        if (row == col) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a1c89de2-ccbb-4e91-9c56-e6626115baa5");
                            total = 1.0 / d[col];
                        } else {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1fcab391-dc68-4f19-a2ed-f8c609ca0361");
                            total = rinv[pos1 - 1] / d[col];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c3761786-0414-4bcb-80d6-66718d178519");
                        for (int k = col + 1; k < nreq; k++) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0a22f1b2-53f3-4fa8-9db3-e27df67652c6");
                            if (!this.lindep[k]) {
                                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "66054577-9cc1-4a90-bb3c-58229ebd0520");
                                total += rinv[pos1] * rinv[pos2] / d[k];
                            }
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "305789d9-c341-41ad-be59-d018f4d9d7a7");
                            ++pos1;
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d9d0d20f-02b4-4ca5-9afe-8540ed39cbb6");
                            ++pos2;
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a7628542-f275-412a-8711-db71cb235e03");
                        covmat[(col + 1) * col / 2 + row] = total * var;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b4f854a4-71a1-4767-9b5d-1ccfc80e4537");
                        pos2 += nreq - col - 1;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b97ca65d-de3b-4fe8-b14c-9b1e1557c81c");
            start += nreq - row - 1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a6e6e511-934c-44d0-8a2c-47fb58d6f741");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ee6be526-0722-4737-9798-235af8ca1e99");
        int pos = nreq * (nreq - 1) / 2 - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "596685c8-2d95-48fb-8710-a17d5a1a8a58");
        int pos1 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d153096d-3487-4d28-ae0c-9976aea22253");
        int pos2 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2cff39e6-d5cd-4bee-9862-a703623245aa");
        double total = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6d84346f-5acf-4ab2-aed6-5e3deb640651");
        Arrays.fill(rinv, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f20e8e4b-efb3-47ff-8b55-91cc3c40479e");
        for (int row = nreq - 1; row > 0; --row) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5e2757e3-b50f-4532-8617-bd062ec5fa73");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1564a445-ff95-4a17-9527-988694896aea");
                final int start = (row - 1) * (nvars + nvars - row) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b555ce98-131d-450e-8649-28ae1293d740");
                for (int col = nreq; col > row; --col) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a9e1e020-22ed-4e2b-8db6-dd729e099ac1");
                    pos1 = start;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ef96e6bc-b5bf-442b-b261-8c7fcdf1dd53");
                    pos2 = pos;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8203fddb-1a22-4f46-8d16-e16945ce6720");
                    total = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5faf3818-5cb4-4d0d-badf-da95b683e250");
                    for (int k = row; k < col - 1; k++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b205be3f-6a4d-4893-b61d-dcbdd41e991d");
                        pos2 += nreq - k - 1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "75cec1ff-1b53-4a59-8704-bd768c57ae18");
                        if (!this.lindep[k]) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8716188d-c2cf-4d8b-bdd0-b4117010158d");
                            total += -r[pos1] * rinv[pos2];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4a71e536-80e9-4074-919b-2599bcff7083");
                        ++pos1;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "64a8a0b7-b856-464a-a775-4c78a914a91e");
                    rinv[pos] = total - r[pos1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "12b654a8-81ad-46d2-bc77-c5ccc7646d4b");
                    --pos;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "97632c7e-f38c-4aef-8467-baeb769d19bb");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "88affc9e-5d43-45f3-ad9b-26cecab42f43");
        final double[] output = new double[(nvars - in + 1) * (nvars - in) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "66801d9c-6806-4cc4-9bfb-9e92903343b0");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "20303bae-f7da-406a-b2f7-3f173df83a36");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e18eea13-ff93-4717-8b82-3ca4fa9e52e0");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9e2caafc-7ab5-4953-88dd-962c5ea92f58");
        final int rms_off = -in;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e63d74f4-9f0c-4e71-a4d3-215b8a18ae29");
        final int wrk_off = -(in + 1);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b32cad3a-d72c-4e0b-ac32-5780286f8a46");
        final double[] rms = new double[nvars - in];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1b419837-3390-4e17-8f12-26f228909cbb");
        final double[] work = new double[nvars - in - 1];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a44c0ade-8e3b-43f8-9573-87bf6a739878");
        double sumxx;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e03f876f-0f51-475c-8213-67b896456807");
        double sumxy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6540f889-bc48-4cd6-94b0-cd53accd42ed");
        double sumyy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "22e439aa-f5be-48ca-9d45-02675617e70a");
        final int offXX = (nvars - in) * (nvars - in - 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f32cbe7f-75f9-44ee-91c0-7bcec5e26379");
        if (in < -1 || in >= nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4c76bb5e-d401-4096-88b8-f2acdcad9fcd");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a3ff13b5-eab7-4d75-b942-1f8ab4035bd9");
        final int nvm = nvars - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "57405613-a11e-469f-9c98-6367b7ca427e");
        final int base_pos = r.length - (nvm - in) * (nvm - in + 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b88ba14a-1419-4aa9-95c1-8131a111d1d6");
        if (d[in] > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "90d7c022-bf38-435f-ba47-9d7dcc55c162");
            rms[in + rms_off] = 1.0 / FastMath.sqrt(d[in]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0528b843-59fd-4c0e-b54a-a0d5409a876a");
        for (int col = in + 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7a2d7b0a-0b95-41fc-89de-ea6d34207576");
            pos = base_pos + col - 1 - in;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d6f9bbd4-59ed-4985-9635-ec06514830fa");
            sumxx = d[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "171d9ae4-d9bb-4e4b-853b-7e8029fc5f80");
            for (int row = in; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "46dd8451-6441-4bb5-b0e3-03c5accbecad");
                sumxx += d[row] * r[pos] * r[pos];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9eb3300b-019b-488a-a3c4-249a8119b9c3");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d03d8a19-f03a-41c6-a67b-18a142c2b927");
            if (sumxx > 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "93859a7d-69f7-41af-baf8-8066eeb63fb8");
                rms[col + rms_off] = 1.0 / FastMath.sqrt(sumxx);
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "421e46a5-764f-4d98-a04f-c6ccce5ec127");
                rms[col + rms_off] = 0.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "df4f2617-c780-451d-9e37-0ed1289c986e");
        sumyy = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1106110d-8540-4b13-974e-a97328afe351");
        for (int row = in; row < nvars; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7dddb1ce-2a54-493d-9f56-a3982926a8a9");
            sumyy += d[row] * rhs[row] * rhs[row];
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1dca789d-c018-48d8-b9a2-591b4f7f802f");
        if (sumyy > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a522ba82-1ce6-4c5e-8220-0e247e9ff9a1");
            sumyy = 1.0 / FastMath.sqrt(sumyy);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ff6173dc-219e-4a70-b5b1-cbfe3876566c");
        pos = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9d72463f-aac0-4bad-acf3-32e0480c2e48");
        for (int col1 = in; col1 < nvars; col1++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a2170b3e-5647-42f6-a417-c86964f1b65c");
            sumxy = 0.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8c084966-80f7-4384-9a24-205fbc8cbc6b");
            Arrays.fill(work, 0.0);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "79685369-ab93-41d0-872e-f881f5085bdd");
            pos1 = base_pos + col1 - in - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e5afeff8-32e4-4b15-8f70-4d295c523c22");
            for (int row = in; row < col1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "94132546-4707-4c71-9840-cee712b27872");
                pos2 = pos1 + 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "51f76484-1a0e-44e6-a1a5-7523cc14eb0f");
                for (int col2 = col1 + 1; col2 < nvars; col2++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "016a817b-00dc-4e13-ab8f-fee95a82ca19");
                    work[col2 + wrk_off] += d[row] * r[pos1] * r[pos2];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "57f7d1a5-142d-46d1-97d2-161e9cac7d52");
                    pos2++;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "75d59f78-0592-4758-925d-f994345cee9a");
                sumxy += d[row] * r[pos1] * rhs[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2356ecc5-0f39-439b-b392-de17490c50fb");
                pos1 += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b10a8537-0346-487c-a143-f786939d823a");
            pos2 = pos1 + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "abd49b7c-f8ce-45e3-87a5-0789ed40e0ec");
            for (int col2 = col1 + 1; col2 < nvars; col2++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ea7f0505-ac2d-4efd-9433-b7650dbbcfd6");
                work[col2 + wrk_off] += d[col1] * r[pos2];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "34f0480f-8889-459d-85d4-764de37e8a3c");
                ++pos2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "31463191-a297-4764-96db-764c54eb327a");
                output[(col2 - 1 - in) * (col2 - in) / 2 + col1 - in] = work[col2 + wrk_off] * rms[col1 + rms_off] * rms[col2 + rms_off];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "206aa71e-8519-41b4-9cf7-7bb6981030eb");
                ++pos;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d7620ccb-c040-4416-a025-65c55d0645da");
            sumxy += d[col1] * rhs[col1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c1d10cac-b51a-4d9c-b564-11374cd8f23a");
            output[col1 + rms_off + offXX] = sumxy * rms[col1 + rms_off] * sumyy;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c0f203a7-41de-4d36-82ca-80d6919fe86a");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6bd3c2ab-4bf1-4f2b-a593-20e11740ebef");
        double d1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e44e3251-fdec-468d-b784-fbe6182d70b5");
        double d2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "73e07b0c-1bc8-48f2-af6f-b0b3e2310877");
        double X;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e9ac8d8b-9ca2-4cb7-b505-3b4189dc9b76");
        double d1new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "961de8c4-b45a-43b7-8248-7351d0edcb3f");
        double d2new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3c59da1f-4e69-46d1-9498-da2c71fd7671");
        double cbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "382bff64-8c0c-472c-85ca-c857c38400db");
        double sbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3887091b-8be3-4c69-88e2-34aa942637bb");
        double Y;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0f807944-8f66-40ec-b36e-33d7b04fcf13");
        int first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ea14cbb4-784c-42ff-890c-9413120b570c");
        int inc;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7c5cbe8a-3fe4-438a-88be-a8ab2c32331a");
        int m1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7787bbf7-32f6-487e-a6d9-51319a61bf71");
        int m2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "352dc03b-e183-46df-91a6-1b1335d77ab7");
        int mp1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bb478c7b-14bf-4e34-8d44-230105bad568");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a292d9ad-e14f-4c00-964c-50881a31e1f0");
        boolean bSkipTo40 = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e586f2d4-988a-44ac-ac25-0363661e0c46");
        if (from == to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dad5ab46-fc73-4760-ba80-7ac5c90887fe");
            return;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e1d03bc7-2269-4bbf-935d-b677a0aabd6f");
        if (!this.rss_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d6108e0f-9302-400c-8e16-fdb446eb8a8a");
            ss();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cfde7858-a69f-40a3-bbcf-350deca51e6e");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f3060b40-e487-497d-bd0f-01233e4a9bf3");
        if (from < to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "37ff1d17-9731-4c01-bd81-eb8c9d138d49");
            first = from;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c3bdcfeb-e229-4615-9931-ccba37fdacdd");
            inc = 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "50a24492-24cb-4733-955e-7700a2aee626");
            count = to - from;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6bc7840b-251b-4882-85ef-91d6484ce59a");
            first = from - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5034baa9-b7db-4d0c-bde4-73482c4a5f0c");
            inc = -1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d9312ae6-d012-4a73-9325-b23683403029");
            count = from - to;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3da4f6ea-3a65-4cb6-866a-d789e5271862");
        int m = first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "26ad4de0-1369-4e7f-bf3b-83b1f3a86c00");
        int idx = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "237dd071-c490-40aa-84f5-1d45694197c1");
        while (idx < count) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7278ed8e-f264-4120-a61e-8b9c582d283c");
            m1 = m * (nvars + nvars - m - 1) / 2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "56e18e5f-5256-4d44-bad8-0fd87f5542bc");
            m2 = m1 + nvars - m - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9c3117c8-9938-46db-9bf1-5bb02b753b79");
            mp1 = m + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "893062ff-27dc-43f5-9715-a07dd03b9a5e");
            d1 = d[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2732c06a-52a7-46d1-86ee-5853f188efde");
            d2 = d[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b23ec6d1-a8aa-4fe8-b577-e1580bff1474");
            if (d1 > this.epsilon || d2 > this.epsilon) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d5f50c78-029c-4d5a-bdaa-c25e15cc3444");
                X = r[m1];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "54af6c7d-d26f-457d-9ddc-c8d7c754fa2d");
                if (FastMath.abs(X) * FastMath.sqrt(d1) < tol[mp1]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "80a0d5d8-592e-4aa6-982b-9d81c19fcc19");
                    X = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dbaf7f1d-4944-4e79-8413-1b1f70d82866");
                if (d1 < this.epsilon || FastMath.abs(X) < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fa11b187-da51-421d-a4b2-a1dd4c6b6bf1");
                    d[m] = d2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "33051c6c-1205-4629-ba3b-0799812a351b");
                    d[mp1] = d1;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bdb415df-ca1a-49f2-9134-add6fd0715ae");
                    r[m1] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "eb53b419-aa67-4371-8bb1-c5eb5811da70");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "14aacc39-7ca9-4ab3-a135-8dedc0b02b7f");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "789cc60a-cde1-4a60-9d51-7c5181fdc3a8");
                        X = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0d790be4-3ece-41e0-9396-3956cbfe7187");
                        r[m1] = r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bb8ab273-b4c8-4a41-91cf-76b16633f38b");
                        r[m2] = X;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "39ce4b3c-837e-4a91-94ec-247154d7c09d");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "137c2163-5b21-44d4-b1cf-6d4f18225f10");
                    X = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2715ca55-27c6-480e-9037-4b956047be66");
                    rhs[m] = rhs[mp1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "81a9c76b-a58d-4145-b112-2132037195f1");
                    rhs[mp1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1b48c5fe-abff-4b94-b8aa-f84a065913ad");
                    bSkipTo40 = true;
                } else if (d2 < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3f41a70c-f4e8-4739-8693-39047e4ade52");
                    d[m] = d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dfc32555-7967-4bc6-8bd5-0dcdba77b73a");
                    r[m1] = 1.0 / X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "54738a17-48f5-45dd-a9a7-2e74199bb22e");
                    for (int _i = m1 + 1; _i < m1 + nvars - m - 1; _i++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6b6c7e0b-dd89-41d7-9092-65228312c5fa");
                        r[_i] /= X;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "76a520e2-95e9-40bf-998c-7b8a93dfe2a3");
                    rhs[m] /= X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "67ad0ca9-ce95-4e17-a053-ba47f798d506");
                    bSkipTo40 = true;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3855867e-c96e-4057-9e0b-88b216b1bc5f");
                if (!bSkipTo40) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "209adaa1-ed5b-4f89-ae14-781aacca6188");
                    d1new = d2 + d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dfc97edf-3c20-4260-98d4-d40c8d00642d");
                    cbar = d2 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c41790c7-c9bb-413b-b453-577fefcec2c9");
                    sbar = X * d1 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "95d193fd-91a4-4445-b02c-6fccf0db880c");
                    d2new = d1 * cbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6002e8f0-bab5-48f3-b9c5-42c5e0470078");
                    d[m] = d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "caf7b2d3-7615-49be-9454-9c6b49c1e38c");
                    d[mp1] = d2new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b55f6160-7b6d-49a9-86c4-8f42c6826d23");
                    r[m1] = sbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e781f87b-8fd7-404f-ac81-576b7a7f540f");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f5d98aab-0225-4b25-bed7-38ca3295e284");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "86b79e55-ce02-4ec0-8dbe-65a4a9025b2b");
                        Y = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dfd33fb7-23ee-4435-b795-324ba326daf1");
                        r[m1] = cbar * r[m2] + sbar * Y;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7126bd8f-08c0-4ac8-946c-fb63b612b078");
                        r[m2] = Y - X * r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0bb23bd8-dcd0-4787-a359-b05f6b6b6405");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9d3934b3-9529-4b0b-bcac-fa158ce13656");
                    Y = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7a4505f7-ba23-4731-9818-26eb6a31aa8a");
                    rhs[m] = cbar * rhs[mp1] + sbar * Y;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "120229e8-9892-4910-b8d9-18b1cc3da8a9");
                    rhs[mp1] = Y - X * rhs[mp1];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8857c578-c7ba-4477-95dc-beddf6c637de");
            if (m > 0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "495ed554-6b45-4900-b927-85237122f4db");
                pos = m;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7e071af7-70d8-463f-981c-84eeb19581d9");
                for (int row = 0; row < m; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "55fb7282-5432-41a7-af7a-9fe1fb0071fc");
                    X = r[pos];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7e032ce8-ff65-473c-a639-f549257df80e");
                    r[pos] = r[pos - 1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "322868f0-8ea8-4a3e-96ef-cffcf833c9e9");
                    r[pos - 1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "11928846-2537-46ac-a72b-acc42360f46a");
                    pos += nvars - row - 2;
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e0bccaf5-b2ce-46ea-a9b4-f4b1f01a6b2b");
            m1 = vorder[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f35d88c5-612e-4a22-8d5e-239f0b45882a");
            vorder[m] = vorder[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "902a84d9-77c3-4f0c-9c8c-38d70f2e6584");
            vorder[mp1] = m1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9c8d7925-dcf9-4080-b9d6-dc5da75d6b70");
            X = tol[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "33f7e3b0-2d0d-45f5-a610-b162e94fa0dc");
            tol[m] = tol[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a0b78d17-6a66-4653-a8bf-cda99c2ea93a");
            tol[mp1] = X;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f003b855-ce5e-4dd3-9c50-eac23140ac73");
            rss[m] = rss[mp1] + d[mp1] * rhs[mp1] * rhs[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d5c0bb86-35c6-4ca5-b3b2-9fae79b47c85");
            m += inc;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c536e739-4c03-4132-bc76-a04da1be41d0");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "71401855-9295-4402-8d0a-bcabaae20721");
        int next;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c74f0762-ffe2-4d8f-9373-cce3e5489f6a");
        int i;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8db36bc0-0a83-472c-9695-c65e19e6e5b9");
        int l;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "788513f5-d363-4254-aebc-a8c49c744437");
        if (list.length < 1 || list.length > nvars + 1 - pos1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3085f1f7-84fb-4bca-97a9-ed5e6c7bcc47");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "aab31b21-ea24-4fa3-b82b-1a6ca63404e5");
        next = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "dd337b23-e717-45d3-af36-c3b06d7083a0");
        i = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "47c759ae-f616-4024-8e4e-a098029ad65e");
        while (i < nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bdf323e5-694f-452e-9045-546c9a693c1c");
            l = vorder[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e1cc1666-9789-4e73-87b5-75d95c19f84b");
            for (int j = 0; j < list.length; j++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "47d02e4f-6e25-4b76-b866-8c97ccca0525");
                if (l == list[j] && i > next) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "74383d98-34ac-4de3-b0b6-609a3d6d9fba");
                    this.vmove(i, next);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1dd88904-2156-46d3-a460-6163ee166e5c");
                    ++next;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "73e3e115-bd25-42fc-aa67-d18530f42bee");
                    if (next >= list.length + pos1) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ca8ac0b9-2924-4b4d-9f1a-841831d2c84d");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "96e82cd0-8c6d-45af-b30a-e45cb77e1182");
                        break;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "86878d0a-56c8-47a1-9d97-080f2c6d3126");
            ++i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "65417750-a0bf-4bf2-908e-fa592d684627");
        return 0;
    }

    /**
     * Gets the diagonal of the Hat matrix also known as the leverage matrix.
     *
     * @param  row_data returns the diagonal of the hat matrix for this observation
     * @return the diagonal element of the hatmatrix
     */
    public double getDiagonalOfHatMatrix(double[] row_data) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "097f91f6-9717-4257-800c-b802237cfdd3");
        double[] wk = new double[this.nvars];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7e904a2c-4b04-4f37-944c-e9bde6714e56");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5bee62cc-7351-48de-953d-dba1f21877e1");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b5bfea01-adc8-4a73-a438-484012c85a8f");
        if (row_data.length > nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "aa43ba5a-4ea3-47ba-8fa2-c8f9dc7f84ec");
            return Double.NaN;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cdae6aa1-5125-4cc1-b951-a6ba2e353994");
        double[] xrow;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "15406d6a-ea63-4450-8efa-ddca89011536");
        if (this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "6f54d6c8-a611-4c86-97ec-a381cc01a6e3");
            xrow = new double[row_data.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1dfebff5-7271-4279-af03-07eefb177685");
            xrow[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "925c6a8c-ab81-46cc-803f-52f1642370d0");
            System.arraycopy(row_data, 0, xrow, 1, row_data.length);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5b7f203d-9863-4845-a3f4-2728392348ea");
            xrow = row_data;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e950981b-a466-4ca7-b305-6fe02f16d4e0");
        double hii = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d44eee78-1cf6-49cf-b2e9-bf07ebdd70f7");
        for (int col = 0; col < xrow.length; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "88d1a924-2e27-4c18-8e18-45811a5c74b5");
            if (FastMath.sqrt(d[col]) < tol[col]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "852eb4a6-8594-4d91-9b57-98a74dc9f7e1");
                wk[col] = 0.0;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9de178d9-5a30-425b-afc5-5808c36c4efd");
                pos = col - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "440370b4-5215-4868-8bf3-1c578feb7a22");
                total = xrow[col];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "46453268-5cc6-4ea7-ad98-11c3552061f7");
                for (int row = 0; row < col; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "08cb3e10-0576-496c-9781-d1e3aad8365a");
                    total = smartAdd(total, -wk[row] * r[pos]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2186dac8-8c44-4430-867c-847b7e1f8949");
                    pos += nvars - row - 2;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bcefa2b1-555a-44c7-8510-0d76fc5c674a");
                wk[col] = total;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fdfca515-e508-4d5f-93b3-b997a4fa55f4");
                hii = smartAdd(hii, (total * total) / d[col]);
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0c2b1a8e-00e7-4a5b-9a30-71da0f204d41");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0a7a38dc-5a5f-4879-b61d-df9309792251");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "75ecd5b7-3c44-4371-9fbd-20c0d9cbdef9");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f35afb98-b483-4fa6-b99e-c2dda8db4362");
        if (this.nobs <= numberOfRegressors) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "01a4a9db-ebdf-4296-a985-e7d06963b49e");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, numberOfRegressors);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "73612782-93b9-4101-8a4e-9215282c247d");
        if (numberOfRegressors > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b1cf0a1d-b177-4aaa-b5ab-2aba7503782c");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, numberOfRegressors, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7e7460f3-c128-405c-bc56-e3877d480a1e");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a4866600-3dc2-4602-82aa-2b89ee2a8960");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2030d14f-6ad7-475e-a0f4-6079e5a3baca");
        double[] beta = this.regcf(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e6750c00-51cb-47cd-b879-4b0534ce93b9");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4ff6377d-b83c-4395-8cf1-5ced943f3e73");
        double[] cov = this.cov(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "71025979-a3ac-4140-b97b-d1763ab0c5dd");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b054f0d7-a7bf-4800-8f52-d2e844ef2b4b");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c4779e1b-1471-4275-8c5a-c15b5a63278d");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d261e61c-eb67-4e50-9712-3e035ac10163");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cda99bf7-db30-40d5-ae42-8497c4b9a7f4");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "622da001-26f9-4cd4-8f40-1c0f7e6c729e");
        for (int i = 0; i < numberOfRegressors; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "52e69e49-7574-4bc1-8bb3-a4e38c8d8998");
            if (this.vorder[i] != i) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ca4f3ec3-a869-487f-a69e-c4664f45c66b");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d352de6b-b84a-4a3d-bbff-a4dee3483c92");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ada56940-09d2-400b-ae34-45f3b7e96cff");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "791381b2-2513-49c9-b70e-ab7fe6d5aaf9");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "727a7196-e22c-4011-8088-8e7748ae18b0");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "392b7e86-f72a-41b2-9b57-885dfe736901");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cd0bea11-4024-437d-8219-c2315f157ed3");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4b823fee-a875-4eee-a5e0-f01582975203");
            for (int i = 0; i < nvars; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "843fb280-b4e8-448d-879c-3d798edad59b");
                for (int j = 0; j < numberOfRegressors; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "77bb52ba-1f02-4264-9031-d88e1e673d59");
                    if (this.vorder[j] == i) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3fe9cb34-61ee-4275-84c0-077105d4fb4a");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fcf36143-461b-45ec-a3af-f46f259655fb");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c105174d-6918-4ae8-83de-5cdf914101e4");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d5ad707f-9f81-4f33-862b-f22072fa6c4c");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "48e07b7d-f877-4f87-9375-15254016dc57");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "4ad0d782-55bc-46b7-bbbe-bc839c4c5335");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b1ae7fed-543d-4268-9110-8d3f28d0eec8");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d75cbb7f-2948-4344-8070-5c98b92daa66");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "665e7643-5d6c-4693-b430-5e37faf51def");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "54bfd666-c83c-43f0-a49f-70c643fb9ae0");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1d6a3f58-1093-44ed-b1fc-8602d1c818c5");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "c0484019-306e-451c-9d51-c5863c8af209");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "63210b77-ed84-444c-9ffb-b619d50b2fe1");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "78df13fd-fd58-416f-8f99-06b0198b7a57");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f0949ac8-f3db-4168-996d-9c82f9e19b27");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fdc92348-4a50-420b-9ec1-0919053a8a28");
        if (variablesToInclude.length > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "25f8ef88-bf25-4be5-9c6c-30a4c4bcd779");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, variablesToInclude.length, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5ea32c6d-9433-4e3f-b66e-9ca35dce38db");
        if (this.nobs <= this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0f896338-59ad-4206-a693-5d7e7c0dd4bd");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "afbbb73d-bf43-428a-bb0f-3833f26ab8bf");
        Arrays.sort(variablesToInclude);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "10df67c8-9fd0-4aa7-9fb0-378ff084773f");
        int iExclude = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a68bce22-f885-469c-8de1-f426b7139ff0");
        for (int i = 0; i < variablesToInclude.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3976b2b3-7515-41a7-8481-c96b7f9556e4");
            if (i >= this.nvars) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5b7cfbff-650b-4d84-9dbb-7c01d7657de6");
                throw new ModelSpecificationException(LocalizedFormats.INDEX_LARGER_THAN_MAX, i, this.nvars);
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e27b2fed-40bd-4873-b2d8-9df2a1a4a5fc");
            if (i > 0 && variablesToInclude[i] == variablesToInclude[i - 1]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "900665c6-63a5-4dba-bfac-70d8cea0bb0e");
                variablesToInclude[i] = -1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "cda14957-da88-4239-8339-16bc18c53bef");
                ++iExclude;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "54a5ebc2-fb93-4b0b-b903-15d0121134e7");
        int[] series;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1a8c6046-ac24-4a5b-b632-7453b854ed26");
        if (iExclude > 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "38fe77c8-ff31-41e5-9783-9ad5c23ddc16");
            int j = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7f937d8a-8f91-48c1-a31c-8a5219c25ac8");
            series = new int[variablesToInclude.length - iExclude];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "fdbbad23-89ac-44e9-844c-4699e20388b0");
            for (int i = 0; i < variablesToInclude.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8755fe57-fd1e-4182-b5fa-021aeddd82e5");
                if (variablesToInclude[i] > -1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2448eceb-be78-43aa-8e5c-467315d556cf");
                    series[j] = variablesToInclude[i];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "b4f3d312-1889-477b-a6fe-8d3ef4b01e11");
                    ++j;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "13b60ea3-072e-4270-8d2b-24bf88a26b8d");
            series = variablesToInclude;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "043c979c-4366-45f3-948a-b2606f868c3d");
        reorderRegressors(series, 0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "0d6efa87-ce61-40fc-8aac-143e89f9465a");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "bac25716-67cc-4598-82e5-e60418207908");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e604c4b5-9c51-4018-b422-fd0d1224f4fb");
        double[] beta = this.regcf(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "2b5cef5b-a94f-46b9-bc6d-7c90315d023e");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "453b1c37-8da9-4a7b-a2be-7c171d127ce7");
        double[] cov = this.cov(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "aea44526-e9a7-4fdb-beb7-9c2de490249b");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "23d7d5df-bd68-497a-9b6d-5caccb464753");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ec114909-fb9e-4c96-a278-82320309105a");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "577bbd3f-59b5-47cb-8e73-3b43b66d2aec");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e2e64130-eb05-4c98-821c-ceaddd86d122");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "3ccd1ddb-d6dd-4ab4-a7c7-20c34a883c65");
        for (int i = 0; i < this.nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "5006fdba-28e1-4598-9fb9-3b2b3238535d");
            if (this.vorder[i] != series[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "75b881fb-93d5-4022-8d7e-9ea7165ed926");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "08bb5644-9562-482d-8809-be9587b7c81a");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "32d784f0-9285-4744-a808-f27c72b65cb8");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e234905b-5fbe-474f-be4e-40f8679788e1");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "692ce1ea-3d6b-4077-a507-ae6f39a226b9");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "890ec218-2de4-47a0-9f3b-37414cfda2fb");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7708638a-8ec1-4736-b67d-1b522de81161");
            for (int i = 0; i < series.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7a118d84-45ec-40a8-9b15-3e833d268a42");
                for (int j = 0; j < this.vorder.length; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "ca911d52-fb6a-41ea-ab64-2242121e0706");
                    if (this.vorder[j] == series[i]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "728909f4-fcc8-47bd-bc1d-59511f0aefdb");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "f0876b52-be68-42e7-9366-bbe2549a5cfe");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d992dab9-8087-4777-8337-63434c5d726c");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "036006e4-1f28-4cf7-bf00-2b044c5833f4");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d02f847d-1fc3-4ce1-acb9-0901bb201838");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "d9c04eeb-ab97-4673-924a-4020afeacc36");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "8d4e5d24-7b5c-4ff2-9cf4-331122ccceef");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "76e0a116-8672-4474-a306-8501d3789b4a");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "1b7b68f9-7e91-462e-beb4-f4fee8d9eb90");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "78fbedb9-0add-40e7-a506-bb5e5a41b795");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a5dbb51b-4a62-4f4c-88a6-934009938b47");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "e5939737-c4a7-4e88-8cb7-c78236d1855f");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "7bce805d-4c3b-42c3-ac0c-23f658a45890");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "9285e21c-7b94-4ec8-99d2-f50f7783a68b");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "a078a4eb-8419-4dad-a745-7b40f290fb29");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_1_10.coverage", "387b263b-0e58-4460-9c31-273efbf03e06");
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
