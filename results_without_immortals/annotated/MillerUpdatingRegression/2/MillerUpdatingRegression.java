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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0720196a-cb82-45a1-b350-48b2016549c1");
        return this.hasIntercept;
    }

    /**
     * Gets the number of observations added to the regression model.
     * @return number of observations
     */
    @Override
    public long getN() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4f0fb569-b76e-4693-a1bc-939a689b8ac4");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1d72f304-8f90-4b41-b14c-f03486215260");
        if ((!this.hasIntercept && x.length != nvars) || (this.hasIntercept && x.length + 1 != nvars)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b40a3db4-aa98-48e9-8045-55fc3fef5ac3");
            throw new ModelSpecificationException(LocalizedFormats.INVALID_REGRESSION_OBSERVATION, x.length, nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5b1d2722-7945-480a-ba0a-afff9b8fc746");
        if (!this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "654e632e-cc60-4828-9644-cdcc1b2a80c5");
            include(MathArrays.copyOf(x, x.length), 1.0, y);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7808fd08-f21e-46d5-9062-14aa9d6b68e2");
            final double[] tmp = new double[x.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "12e798c1-08ff-49bb-8c9e-0fc770f90a3b");
            System.arraycopy(x, 0, tmp, 1, x.length);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e3a8f12e-ba87-468e-9619-9072dd1b69fc");
            tmp[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f3556959-5722-4872-b225-92b6a375aa02");
            include(tmp, 1.0, y);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "901a9f8d-5097-4641-a755-3e63347f7b7d");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c156cda8-a9b5-4044-83d7-e835098a10db");
        if ((x == null) || (y == null) || (x.length != y.length)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "30b2d352-7d92-4576-9d75-12651bcc719a");
            throw new ModelSpecificationException(LocalizedFormats.DIMENSIONS_MISMATCH_SIMPLE, (x == null) ? 0 : x.length, (y == null) ? 0 : y.length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c1fb8ed2-794b-4d7a-b0cb-7d81ebc57133");
        if (x.length == 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4567c1dd-0dab-4b9e-8a3e-8e6285ca522f");
            throw new ModelSpecificationException(LocalizedFormats.NO_DATA);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "dc595cd4-e197-40b1-a4a8-eb3e5103fc2e");
        if (x[0].length + 1 > x.length) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ebfe906a-4bb8-4704-bb8b-15068ddb147e");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, x.length, x[0].length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "51e52e32-293d-4f2c-b828-8a93d9cc8679");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5c184b24-59da-44b2-9aaf-d904cc8bb8f5");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c614fba2-2b47-45b8-8217-1f9c4b2c5ec3");
        int nextr = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7b2bb2da-20ef-498d-b2f3-a86561abdbef");
        double w = wi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b2ffb011-70c6-4b30-b10b-97453a1cd464");
        double y = yi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5d6e10cd-0a00-4fe4-8741-f46c578580a7");
        double xi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ec197013-47cb-47a0-89aa-3900932536bb");
        double di;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a173baa4-cf30-48ba-b934-b45573e9e261");
        double wxi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c454651b-1fed-48c0-ba1f-dcb88b506176");
        double dpi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3448bbea-5bc3-44f9-b95e-0d8e6c2f5786");
        double xk;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c24f16cb-fb88-406d-8c1d-adf38f307190");
        double _w;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "def39143-ac3b-436b-9095-f0f2bdb8d698");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "32c3af37-4ead-42f2-818d-3d82ba4dec33");
        sumy = smartAdd(yi, sumy);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9e67cab7-f059-48d9-95e7-5ee70ee3d1bb");
        sumsqy = smartAdd(sumsqy, yi * yi);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c28e4450-01e9-4f2e-a8b3-3a92122e3721");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b089ddf9-a330-4171-894d-6c45746163f9");
            if (w == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "69e0f871-9ce7-4d9b-8f2d-6313bb62c7b2");
                return;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "529b9372-0843-4f86-84e3-f1957685e97a");
            xi = x[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0e00d266-4163-410d-a8b2-a4aca1790293");
            if (xi == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b386dbaf-5ed5-4311-8aca-2d592cae5bd4");
                nextr += nvars - i - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "86f48574-79b4-4b99-a2f8-162d9a0a832f");
                continue;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "158a0b0a-69b6-4c70-8622-213ff8804c5b");
            di = d[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3d5fb19a-41cd-438c-90d7-cbde91fc02b7");
            wxi = w * xi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c256cc81-f758-40bf-8029-d076b99db919");
            _w = w;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "fa8df0d9-7b49-4686-a247-8ee0cf1c2b93");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "95f4704a-9b61-4958-adf9-4bda5ca80c7a");
                dpi = smartAdd(di, wxi * xi);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e5f3ec49-e30d-44d7-b49d-62f579ec2e59");
                final double tmp = wxi * xi / di;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8688c94c-38bd-42e7-892d-23d35c82e686");
                if (FastMath.abs(tmp) > Precision.EPSILON) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7cb69729-948e-4e43-9dd6-310087832da7");
                    w = (di * w) / dpi;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f72d15f2-5b67-45c9-91df-3eba47b6b209");
                dpi = wxi * xi;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8d607646-592a-498e-9319-a62e99f98f49");
                w = 0.0;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8dbfd706-3dfb-4279-bde7-44c16f8a2f46");
            d[i] = dpi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e9271ef6-7604-4828-a4a6-d67c776d32b6");
            for (int k = i + 1; k < nvars; k++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4c4f2237-a1c5-4e8c-8f4d-8ce3ea690247");
                xk = x[k];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c6f2829c-4fac-4c90-9a23-67f1659cfdeb");
                x[k] = smartAdd(xk, -xi * r[nextr]);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "10d24680-7b34-4608-be0a-681d02e0e602");
                if (di != 0.0) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3284e3e6-8682-4173-bd88-2b34e0f28787");
                    r[nextr] = smartAdd(di * r[nextr], (_w * xi) * xk) / dpi;
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ba788d8f-040c-4358-86ab-0c08ab19dcb9");
                    r[nextr] = xk / xi;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e0a66e3c-c13d-4ac2-bb51-46d102a1c903");
                ++nextr;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8b88b674-74ac-4e8b-b28f-04cbd61ee834");
            xk = y;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c1598b96-3efb-4d74-968a-25a883a22e70");
            y = smartAdd(xk, -xi * rhs[i]);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "33bd207f-322d-4da1-a7df-07cccdb8eeb7");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f5c3cf03-9f13-451b-8fc6-36d414577e9c");
                rhs[i] = smartAdd(di * rhs[i], wxi * xk) / dpi;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "90399214-d737-480c-889a-8c937e458bf7");
                rhs[i] = xk / xi;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8479f35f-72e7-44ad-a9b1-4904ad30f0a9");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1b78da5d-3169-4e7c-8f09-33613742ea64");
        final double _a = FastMath.abs(a);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a19a0a47-288b-4898-8e44-7d2926705fdc");
        final double _b = FastMath.abs(b);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "24a8f94d-9b82-4157-aaac-6c3eb2318ef1");
        if (_a > _b) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "88b8ec7d-e312-4ab2-999e-52eb571ddb51");
            final double eps = _a * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0a1e18fc-3c1c-46a4-a7e8-b2a57a754052");
            if (_b > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d3860d85-6b57-4c86-be7c-7ffbacec020d");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4ed10d9b-f239-434c-b4b0-6b6ec4f07403");
            return a;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "70b73d27-8d65-40e3-914c-2eed3075794a");
            final double eps = _b * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e8589909-68f3-477b-8322-736ea071f179");
            if (_a > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "dffeaa52-8202-40ed-9dd4-b6ef27182edf");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "82c3c7cb-47d6-4e6e-b733-96e30d744a4a");
            return b;
        }
    }

    /**
     * As the name suggests,  clear wipes the internals and reorders everything in the
     * canonical order.
     */
    @Override
    public void clear() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "819aa058-2918-4d96-a998-7224e9503928");
        Arrays.fill(this.d, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "db088e9f-51a6-4d65-aac6-772c056ee3d4");
        Arrays.fill(this.rhs, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "80457ad3-6918-44f5-a364-efb63d8b65d3");
        Arrays.fill(this.r, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b9afc016-6963-4a5b-b241-dcb9dbb8cad0");
        Arrays.fill(this.tol, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "31d6c392-655b-402d-a393-8ee21b7e14fe");
        Arrays.fill(this.rss, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "98a8d4bd-972d-45c0-a7e0-92208087e322");
        Arrays.fill(this.work_tolset, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "33fe8eae-4f5d-4bee-bc30-8c5caac3db9a");
        Arrays.fill(this.work_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "422a2337-ad8b-4fb5-b77a-297155db457c");
        Arrays.fill(this.x_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5682caa2-a9f3-4cf6-b81a-818643b2c91f");
        Arrays.fill(this.lindep, false);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "47980bf7-3609-4c88-b917-11b483eb5373");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f144b934-a451-41dd-9463-4919ae060294");
            this.vorder[i] = i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "909f4fd5-a39e-40a4-8fb7-0c5731f8b818");
        this.nobs = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "97ff789a-169a-42cf-bab3-d238d0a4545e");
        this.sserr = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cc8f5103-5c72-4fb3-8575-22b74967e599");
        this.sumy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f975ab52-ca15-4270-ae4c-9c1ddc698b36");
        this.sumsqy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "759d5fb4-67de-434c-b4bb-d689f21e53d0");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ca50f990-4dd6-4006-8203-97d13ded1003");
        this.tol_set = false;
    }

    /**
     * This sets up tolerances for singularity testing.
     */
    private void tolset() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ab049755-57c9-4a2d-af03-43a8ef794cef");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2a90aec9-b26c-40c7-8c88-227e1a0b9528");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cf0b0225-a0e6-410a-9b1b-9cae075a5acb");
        final double eps = this.epsilon;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cd283915-d14f-4681-b2b3-4bc16f5315fa");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "24124f73-97c7-422e-9961-2cf692f1a36f");
            this.work_tolset[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2f8fc3f3-df9e-42ee-a5e6-c3176d1d7b41");
        tol[0] = eps * this.work_tolset[0];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6f198901-b498-4bca-bb7e-3cb13209f111");
        for (int col = 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bc990313-6486-4aa7-962e-b13123114730");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "82e2da51-867e-4509-8197-cb396a62d042");
            total = work_tolset[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "67f48286-bdfb-4460-85c6-bf38c91080ed");
            for (int row = 0; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bb35a404-f74a-446c-b53e-04fd174376b9");
                total += FastMath.abs(r[pos]) * work_tolset[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7ec4ef9e-2f7a-4ddc-8b76-fd65c027af4b");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5c26cc27-6e5a-4bd0-8999-4b9a559e5484");
            tol[col] = eps * total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "09e58520-a25f-4516-9135-bacef3e33002");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "92adc8c1-f0d1-4472-8d26-8385eee9b2df");
        int nextr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e6146b08-069a-42cf-bcd3-54be64885a33");
        if (nreq < 1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f2d9dc70-4329-46da-bca3-ade309562b82");
            throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b2c18daa-5126-4791-b3c6-b0245136e88c");
        if (nreq > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ebbab354-2a5f-4975-9586-20cb525f0f99");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, nreq, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a7871cb9-dbc2-4e24-8e83-c794a04671aa");
        if (!this.tol_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6231dddf-f318-4b2c-a44b-3b14e9939603");
            tolset();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "393667f3-3253-430b-861a-16e3ae85bba0");
        final double[] ret = new double[nreq];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "73fd5f8a-683c-47b7-b751-a4cee941c2ae");
        boolean rankProblem = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "366581b7-2697-43ab-93f1-65d448b088ac");
        for (int i = nreq - 1; i > -1; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0da81d6f-392c-42a6-9be8-de16a3d42d79");
            if (FastMath.sqrt(d[i]) < tol[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b2e625ac-a785-4918-bde9-afdecacd19ee");
                ret[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d411c3db-cc20-44dd-b4d4-22d2160e8385");
                d[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7ebcb642-4e56-4d6e-9dfd-89c6920b1af2");
                rankProblem = true;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "222f62d2-1438-4b05-8cf1-0c95e6a93c90");
                ret[i] = rhs[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b1a94668-2c14-494f-bb82-9ef65c0e6b97");
                nextr = i * (nvars + nvars - i - 1) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "fefb15ba-06e5-4e50-9aec-43ca34557b18");
                for (int j = i + 1; j < nreq; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f56afec0-1617-445d-a93e-27db55c40ab9");
                    ret[i] = smartAdd(ret[i], -r[nextr] * ret[j]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "26d16f41-8ee0-43e7-bb5d-1d674c3d0429");
                    ++nextr;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "aeb61399-b660-4741-8b3e-cf6b1749a9de");
        if (rankProblem) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0c73e510-a94a-4859-875c-52e4ad7af22b");
            for (int i = 0; i < nreq; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a6f17b94-000a-4e99-b9ae-72ad2616f499");
                if (this.lindep[i]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "51382573-e12c-4081-b0e5-8ec87d0745bb");
                    ret[i] = Double.NaN;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "115da062-7c7e-4c6c-807b-6cea0ecf72c5");
        return ret;
    }

    /**
     * The method which checks for singularities and then eliminates the offending
     * columns.
     */
    private void singcheck() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "497699af-19b9-4199-918d-ed9e10e72011");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cd4451aa-90e3-49a1-b149-d459cf68a552");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "fdb9fca5-ab2c-41d4-b05e-3ed1ecb08eee");
            work_sing[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cfbb0dbb-70f5-4b90-99aa-d3139b386a2a");
        for (int col = 0; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c3ea1d76-5cc6-451e-ab1d-69a128813ad6");
            final double temp = tol[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "00819f46-0134-4920-842c-211074c4e1bc");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9d17c737-c525-4c68-ba74-857e3fe57bc3");
            for (int row = 0; row < col - 1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f3aa8f52-a333-4d6a-86af-8dc0350f8470");
                if (FastMath.abs(r[pos]) * work_sing[row] < temp) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d2747be0-6093-461b-a3e4-876674395dc1");
                    r[pos] = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ccf7fd5b-2568-4ef9-9e9e-a55c647ff9af");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ac73ca6b-7f51-4078-9b60-ed7cf699588d");
            lindep[col] = false;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2a845cbd-6982-474a-a41a-ff13a5a7e127");
            if (work_sing[col] < temp) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "74d0d3bc-c4d2-4e5c-8688-ce578b32a411");
                lindep[col] = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2de1438d-2b76-4107-8621-3a9f3ef30d4a");
                if (col < nvars - 1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1f14103b-e407-41be-a824-400fb1d7ddf7");
                    Arrays.fill(x_sing, 0.0);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b4296583-7b2c-4b1d-8bd6-413cec28f162");
                    int _pi = col * (nvars + nvars - col - 1) / 2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "43ea850f-db04-4ad1-a308-39f2b0e80fea");
                    for (int _xi = col + 1; _xi < nvars; _xi++, _pi++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "34d38f60-c50f-4483-9d88-cf147aa19046");
                        x_sing[_xi] = r[_pi];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "62ad3fa3-6be6-42e5-8559-b143aa1b990a");
                        r[_pi] = 0.0;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cdfbdea6-1a5e-4430-849b-1deb4fc34555");
                    final double y = rhs[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "97660a7f-c0d7-4748-a22b-515b7c02e131");
                    final double weight = d[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9ad12bab-a20f-40d2-a2a3-08cff8233023");
                    d[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ca510008-cc2e-4660-a2d0-a7de3d4bc179");
                    rhs[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b8626d1d-ad5d-4521-8643-84beda04734b");
                    this.include(x_sing, weight, y);
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "05e7c8ba-2427-4086-8caf-dcb4008a4922");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "dec9069d-74c0-4954-a172-ab9d1af2da4c");
        double total = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f779a812-4a72-4ca9-bf71-c135bf6930c3");
        rss[nvars - 1] = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "04766485-8d72-49dc-b392-d186d49171de");
        for (int i = nvars - 1; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2665c8d5-d895-47d6-97fb-a1f413d5163a");
            total += d[i] * rhs[i] * rhs[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "edb17011-30fc-4d1e-94d4-6a10ab24d1a2");
            rss[i - 1] = total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4c82d6b0-1628-4293-b2e5-fbe5c2ced638");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ddde4fff-0e20-4cd9-b4d4-7b8ffc174023");
        if (this.nobs <= nreq) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4e0aecb7-1ad8-41bc-b204-c39698e7aeef");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "aee78b90-cf25-444c-a2f2-2665e386bb67");
        double rnk = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9cc968c4-64fb-4a1d-a1c0-2eada2276732");
        for (int i = 0; i < nreq; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6ef02565-5dbc-4442-9cd5-1b69b9aa0822");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8342f9e0-9ff8-450f-8e26-9a025258cc24");
                rnk += 1.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "744614a0-78ed-4026-af32-4291d67e16b2");
        final double var = rss[nreq - 1] / (nobs - rnk);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e702ecd4-2a91-4e25-82bf-801445d75e21");
        final double[] rinv = new double[nreq * (nreq - 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "597cbe6d-2317-4525-923d-7f7944e39044");
        inverse(rinv, nreq);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "49333685-9e82-4525-b5b7-afc7259762f1");
        final double[] covmat = new double[nreq * (nreq + 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c13f4479-7562-4920-9c5c-dade3284113e");
        Arrays.fill(covmat, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5421ad27-4562-4efb-a4c7-226eba07bdc4");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3cf81c76-4270-4979-aef2-b3dc158c7bd4");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "48d376d7-5a07-4382-a7ff-d46576fbfecc");
        int start = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c95e4d65-bdaf-4b7d-94ee-0988d6b50d38");
        double total = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d27e03c9-5ff4-4697-9ea5-2df1c414d485");
        for (int row = 0; row < nreq; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "fe34056e-ec16-4d65-b26e-0a4e04e5c901");
            pos2 = start;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "abe7c200-12d3-4093-a9d0-264c9660ba14");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "aeb7e923-b18b-4c5e-b4f0-69bd7a4000a6");
                for (int col = row; col < nreq; col++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a4f370e5-2818-49d7-8b34-6522f311a405");
                    if (!this.lindep[col]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "39baf43d-f9bd-420b-935e-a9ae5423a466");
                        pos1 = start + col - row;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "31a72b81-3be1-4418-8444-e80d8ead8cdb");
                        if (row == col) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9b10f652-fd7c-44cb-a845-8eb936d6c6c7");
                            total = 1.0 / d[col];
                        } else {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "033b9f26-7760-483e-bac4-2f9453ed3e43");
                            total = rinv[pos1 - 1] / d[col];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "da6e35d3-3881-446a-9973-488cc1b90bba");
                        for (int k = col + 1; k < nreq; k++) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "581d86dd-0e9f-4e99-a14d-1d09a1abb9a3");
                            if (!this.lindep[k]) {
                                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "80e23d51-918c-473c-8230-fcb37b26e418");
                                total += rinv[pos1] * rinv[pos2] / d[k];
                            }
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "722a4738-7bed-4baa-8887-84edbd226214");
                            ++pos1;
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "72a62739-3b05-43fb-a3ea-12bace00f2ed");
                            ++pos2;
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "24fbd378-f0df-4ef9-80b7-bf559e658d69");
                        covmat[(col + 1) * col / 2 + row] = total * var;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4444511e-7667-477a-851d-26de4a495549");
                        pos2 += nreq - col - 1;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e4cc0fd3-85b6-42fc-a460-e9ea0f8b2f11");
            start += nreq - row - 1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "703a276a-b0eb-42f3-be52-d40171e86978");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c6fddc24-b891-4e50-a1de-9370e7bd3efc");
        int pos = nreq * (nreq - 1) / 2 - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5784ab41-af89-4caa-8bcd-a3a5586a102a");
        int pos1 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c51db075-bc2b-4d1f-8ab6-443d389cb98b");
        int pos2 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7eedb8a9-8664-4032-980c-5271126e36eb");
        double total = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3464e126-c50d-44d9-811a-26aeff5dcd45");
        Arrays.fill(rinv, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2f7005ba-59a7-459d-b520-06e99f5f65ac");
        for (int row = nreq - 1; row > 0; --row) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e3bf0b0e-c0e0-4300-aa5f-f252c26d5ee9");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "48caab66-b5a4-4d5b-a0c2-b746c70fab26");
                final int start = (row - 1) * (nvars + nvars - row) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9044c51e-a5d2-4fc2-b338-f6c7aacd6d17");
                for (int col = nreq; col > row; --col) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "35c7853d-c0cf-4fef-8939-aeef44ef438c");
                    pos1 = start;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "46834e6f-b648-476c-9d84-c4b85747f604");
                    pos2 = pos;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a5402bb4-d9fd-4155-84b4-e6aa2def9126");
                    total = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "598d5865-0e3c-406a-a045-a2742efbb08b");
                    for (int k = row; k < col - 1; k++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "586f870e-4075-4473-b884-71e378ad21aa");
                        pos2 += nreq - k - 1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3814a548-936a-4c32-a674-d5e07cb25efb");
                        if (!this.lindep[k]) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "04abc61f-dcab-4192-abe9-fb1e520679c6");
                            total += -r[pos1] * rinv[pos2];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bb52cccb-79b4-4f75-838f-2110cdb4a59b");
                        ++pos1;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c0d030e4-844d-409b-9d10-3720cbc660af");
                    rinv[pos] = total - r[pos1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0a381430-9b8f-4633-aded-c7ba1a53551e");
                    --pos;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "89888aad-894e-4391-b09d-8babbc450c1f");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1ec41192-d754-45fd-9b36-3d72aa2a55ec");
        final double[] output = new double[(nvars - in + 1) * (nvars - in) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c68f2ef4-74f8-4e09-a585-483b5ba717f5");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "197503d5-5cf9-4e7f-be16-c54d3a65000a");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "aa1a40d8-fca4-445c-9b3b-526035b64551");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d4c5c9eb-1a62-43d9-b510-d6145dbd9a2e");
        final int rms_off = -in;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "99742035-4c52-4003-b9e3-fd4710fdf247");
        final int wrk_off = -(in + 1);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bc424144-629c-4e05-bb80-3969348400aa");
        final double[] rms = new double[nvars - in];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "986a79f0-ebb9-4452-958b-b56ad79c36c9");
        final double[] work = new double[nvars - in - 1];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "090117c1-6406-470b-8938-42281ff94aad");
        double sumxx;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c596c56b-7056-4087-ae35-d7426dd4162e");
        double sumxy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d35e1644-928f-49cc-a4b9-2e3fe266b0e3");
        double sumyy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ee1b3697-f4ef-49b8-9f4a-ddde04e00106");
        final int offXX = (nvars - in) * (nvars - in - 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a8af0a4c-1d69-4c73-a1b3-952da8404c28");
        if (in < -1 || in >= nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bdb49b04-ba1b-4d67-a90c-689c18e6d63a");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2dc92fab-b0e5-429e-951a-f75a126318b7");
        final int nvm = nvars - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7d8ca677-fc73-4971-a8ad-020823f678be");
        final int base_pos = r.length - (nvm - in) * (nvm - in + 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f2a581ac-d9b8-45ae-b8e1-f1b0fa24dac5");
        if (d[in] > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1438f3f3-1b25-479c-9252-f6d98cc09904");
            rms[in + rms_off] = 1.0 / FastMath.sqrt(d[in]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b8b9a5f9-6bf6-4c19-8e7b-474db8c83f8b");
        for (int col = in + 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "190d0ef2-3b77-408b-8bec-49e840c4d3ac");
            pos = base_pos + col - 1 - in;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "92af064c-6512-44e2-9d07-ffc6be16cc2b");
            sumxx = d[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5557f620-8b95-499e-a068-8c920c112cdb");
            for (int row = in; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0c09f2b0-fb17-44be-ba8d-d3c83f8bc2f7");
                sumxx += d[row] * r[pos] * r[pos];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6fbcacf7-53d7-4518-9add-3ffc88aed8d0");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d439812e-36d4-41a9-bf91-a5de0401a12b");
            if (sumxx > 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "096cc0da-1b32-425a-9563-3065e67e82ba");
                rms[col + rms_off] = 1.0 / FastMath.sqrt(sumxx);
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "87b6c201-13b7-406d-8edd-18367e0271b9");
                rms[col + rms_off] = 0.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "47d4886e-7ca2-4f3c-b071-86c696853e8b");
        sumyy = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1c373ce3-c9c1-42ff-aeed-e8ece6b6e3d5");
        for (int row = in; row < nvars; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c0736f7a-1bac-4d66-98b0-7a99e2bb9d1b");
            sumyy += d[row] * rhs[row] * rhs[row];
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "31a55fd9-fb00-46c0-8886-3cc70cf9f4c2");
        if (sumyy > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "50d633cb-5b2a-4d7f-a105-78027a580682");
            sumyy = 1.0 / FastMath.sqrt(sumyy);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d33e6460-ec8a-42f6-af3b-fa187a70706f");
        pos = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8e9a6f22-8d40-4887-8a3e-ef9da9e7f9ec");
        for (int col1 = in; col1 < nvars; col1++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "da6777fd-01cd-4e4f-9be3-914fbd9512ed");
            sumxy = 0.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9feec0c8-0daf-41bb-96c1-60820c367253");
            Arrays.fill(work, 0.0);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6f2d1aee-b32b-416d-a165-4fa3db7dc7f9");
            pos1 = base_pos + col1 - in - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "85b04424-44ac-4859-950b-325544384288");
            for (int row = in; row < col1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "07947fc6-f89d-413a-9f09-2c849fe1e0fa");
                pos2 = pos1 + 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "975e4f7a-862e-424a-9c8f-b7b7bc895c0c");
                for (int col2 = col1 + 1; col2 < nvars; col2++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "018882ee-10f4-491b-aed7-69885a5b54d3");
                    work[col2 + wrk_off] += d[row] * r[pos1] * r[pos2];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "68beb90e-e6e8-407c-8a7e-25cc5662b84e");
                    pos2++;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c95cfb67-7cfb-4b7e-a5a8-fddd4b752843");
                sumxy += d[row] * r[pos1] * rhs[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3165b6f5-62db-4a41-a5c0-753f3d1b3ab8");
                pos1 += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b4d913e6-3079-4908-a845-334cabcbc29f");
            pos2 = pos1 + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4bcd81fd-c9a6-4bc6-b389-33cbbc19615d");
            for (int col2 = col1 + 1; col2 < nvars; col2++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "348ccb2a-3bcb-4aad-85f9-402554335e30");
                work[col2 + wrk_off] += d[col1] * r[pos2];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d7ca3957-a8ea-463b-b4dd-ca7982097ff5");
                ++pos2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1a7339e2-21f7-4ecc-9191-50839a566e8d");
                output[(col2 - 1 - in) * (col2 - in) / 2 + col1 - in] = work[col2 + wrk_off] * rms[col1 + rms_off] * rms[col2 + rms_off];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1a57eee7-0ac1-43dd-9040-f18b7811248c");
                ++pos;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "37b3c475-e051-408d-8b3e-2c797fdeff86");
            sumxy += d[col1] * rhs[col1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3ab4dfbb-c9ac-4a94-9edf-3f70bdfbbb9c");
            output[col1 + rms_off + offXX] = sumxy * rms[col1 + rms_off] * sumyy;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4f46e1a1-a9e9-4f55-bd6e-5b85a5a9c530");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8167d1a8-0af0-44ae-bc6b-c7de01df46bb");
        double d1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "56381258-cd25-4e3f-a901-e36f39f8082c");
        double d2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f0769c1e-af33-48bd-b8dd-b6a92e3d016d");
        double X;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "48e896ed-e47b-48bf-b2b2-feddcbf9f68c");
        double d1new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e007ee7a-7036-4eb3-b2d6-189df0d6e7b8");
        double d2new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "66808637-de98-4177-806c-72145ae9d4b2");
        double cbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "fb17a51e-b930-4237-a602-5edd2ff88ed0");
        double sbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7368afaa-699f-418d-8b9c-b1b38f2289f5");
        double Y;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e4aab7cf-2e50-4747-8ea4-786a619753ad");
        int first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "dc5260df-b8b6-4242-bbfe-8a212e89f19c");
        int inc;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "226f7442-4572-46f2-ab42-bf4dcb46a9bf");
        int m1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "832928ce-db72-41fc-98f8-d03478c1110f");
        int m2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "58b4c9a9-7fd8-4f41-9596-5fbdf626d1e2");
        int mp1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cd446084-3b9f-424c-919c-429a7449d0a9");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e83a1c51-178f-4c13-8134-b66678948941");
        boolean bSkipTo40 = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6757400b-f429-442b-89d4-14143ac0a3cf");
        if (from == to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9a596f70-0f52-4462-929f-a1369b41c3bd");
            return;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1ed873fb-599b-4b1a-9efb-f454c363fb18");
        if (!this.rss_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "41ccd201-5f17-41cb-8ef1-fb3acc0713b5");
            ss();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "361b6536-cf56-4e53-aaab-dc86182dd415");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e9251ad0-3378-41b3-a447-971ea9f97f7e");
        if (from < to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "eb3bdc56-a5ed-4593-b168-05b4ad459f38");
            first = from;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f2114aed-0490-4172-9b85-358260f4a804");
            inc = 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7966a25c-2f06-4c60-a853-c6ba7e580550");
            count = to - from;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0498b188-729e-4874-ada6-1dba0170c7fd");
            first = from - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d3773fe2-f70a-4382-981a-4c107775c023");
            inc = -1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e14d650c-bb33-43ab-bb01-2ba9b3a02014");
            count = from - to;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e81c3d1d-1881-4639-87bb-e90c7515a244");
        int m = first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "009cf68e-f2c9-440e-b3b8-33efd4c0d7b6");
        int idx = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7e973d44-d4b9-4db8-b726-8356d97dfbd7");
        while (idx < count) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ada3377e-2bee-49d7-bf32-185f60b94cb1");
            m1 = m * (nvars + nvars - m - 1) / 2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "77b3f090-a149-4363-8974-8dceba1f70b4");
            m2 = m1 + nvars - m - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d9666b31-2839-433d-b9d1-a1bb709e9207");
            mp1 = m + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "38cc6d9b-efdd-41a3-80f7-43108ca3c76c");
            d1 = d[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "73f703e9-679a-4e2f-849c-1d9b79ecab37");
            d2 = d[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9b213e5a-f6af-46cc-8b61-48098380e1ef");
            if (d1 > this.epsilon || d2 > this.epsilon) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "340e217b-a3a9-400c-b6ca-131a6cc3fda7");
                X = r[m1];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2c618aee-9253-40fc-936a-a26ca7de0dc4");
                if (FastMath.abs(X) * FastMath.sqrt(d1) < tol[mp1]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "aed9b64d-ec0f-45af-85cf-2605712c7d03");
                    X = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d80fdad8-9a85-4219-94bc-a24970dfb621");
                if (d1 < this.epsilon || FastMath.abs(X) < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8a36c7be-3ae3-4814-ba1b-fe7553189f81");
                    d[m] = d2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "311992af-15ec-4438-9d37-57504a2d735a");
                    d[mp1] = d1;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "36949665-fdaa-48d2-b1a4-b58a42c46c71");
                    r[m1] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bc602c5e-d4b8-414b-954c-45bfb75225b9");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "976af9ab-8048-4cde-9809-3128648272e4");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a4b0ef71-23ec-4ebd-8c6f-076566f5dc97");
                        X = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "77dc55fc-e6f0-4571-b276-0171a76f3f40");
                        r[m1] = r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "03acc0e9-7eb7-4cb3-8a17-b25962fd5197");
                        r[m2] = X;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2377986d-b32f-441e-b867-ff5604c52136");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9554626a-5f3f-4c36-98a4-25a0e834a965");
                    X = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "492c3241-35f0-42cd-9548-ce745c69de79");
                    rhs[m] = rhs[mp1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "95cd3653-e079-42b2-ba29-0ca66cd3bb7e");
                    rhs[mp1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "02d54475-18b5-4373-9deb-2e996edb7ee9");
                    bSkipTo40 = true;
                } else if (d2 < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a949b992-dd71-4eb4-92a8-667ab003ada7");
                    d[m] = d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b2aeab40-bb23-4f2f-b1fb-db9868735748");
                    r[m1] = 1.0 / X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ee6c7391-32b8-4e59-a6ad-2a702af7ce8b");
                    for (int _i = m1 + 1; _i < m1 + nvars - m - 1; _i++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "44133cd4-4cc7-44d5-969b-96d8693d76b9");
                        r[_i] /= X;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e62cddb2-a500-4d82-bfff-a56f885abd76");
                    rhs[m] /= X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "00b84366-268e-40a3-8ff8-f9a3d070fe3f");
                    bSkipTo40 = true;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7f01af81-2097-4a0c-aed4-9a4df15cb82d");
                if (!bSkipTo40) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7be17c34-3ce5-4a41-abfc-ea99a99ce0dd");
                    d1new = d2 + d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a6da76a1-a1aa-4048-bc87-55cb753364aa");
                    cbar = d2 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8d4441b3-1fe4-4de8-a813-bfb3e5ef9646");
                    sbar = X * d1 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a5aa6f6d-1bd9-4181-b9d5-538cd0fc7b64");
                    d2new = d1 * cbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e320d275-f7b0-4cf0-ae7b-cec41b9d8417");
                    d[m] = d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6ff95286-00b0-4916-9934-49ec644f6ac8");
                    d[mp1] = d2new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7d307dc8-4c1c-4b89-a6ee-a421754ac73b");
                    r[m1] = sbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "190a0753-63de-483e-a065-2f5f9726118a");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0b29ea42-dfa2-44e5-93cf-1c52848ddab8");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "658ddb3d-98a8-4367-8599-ea48fa63df40");
                        Y = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "44fa754f-1d23-48a2-b09a-5750a703e96d");
                        r[m1] = cbar * r[m2] + sbar * Y;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0597bf6e-979d-401f-a5dd-d3fba0cabc27");
                        r[m2] = Y - X * r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7aeb0bce-291c-4035-9fab-2820e593426f");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8f1e69a3-ab93-465f-89ac-8ea462ca3348");
                    Y = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "aa2089bd-9572-425f-a474-ba1c5286ec44");
                    rhs[m] = cbar * rhs[mp1] + sbar * Y;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "76269455-c0fd-49d5-8da4-a05f1f2f335f");
                    rhs[mp1] = Y - X * rhs[mp1];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e775c32d-7980-439a-9ab9-500624e41ebd");
            if (m > 0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b271290e-ddd1-40bc-8c9b-0147e15c8943");
                pos = m;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "248f603a-26b6-48ad-9831-15b6613495ac");
                for (int row = 0; row < m; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6e389ddd-f7bb-41cf-b592-7f51b4375eba");
                    X = r[pos];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5e02224b-b41e-40e8-a1e3-5c898fba5441");
                    r[pos] = r[pos - 1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4f047f73-fdda-4e68-8a31-3b1791dcd52e");
                    r[pos - 1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6fc6e9cd-5c52-4594-999d-5ef74fa6ccb6");
                    pos += nvars - row - 2;
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c6511254-4b95-4f94-9cb0-44f63cc0a8c6");
            m1 = vorder[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6612b181-f8d8-4da5-a87f-80e6ff2b3d73");
            vorder[m] = vorder[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "eaf73f34-3a2b-4747-92cd-4226d12c1286");
            vorder[mp1] = m1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a5b10d81-769f-45d8-af6a-92f036880f3e");
            X = tol[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6619b372-8b26-454e-9410-7393cde9d49b");
            tol[m] = tol[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0648162b-3198-4936-b42d-47eac0bb6939");
            tol[mp1] = X;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5cb2c16d-68da-4dd1-812a-f9e0e76a9f23");
            rss[m] = rss[mp1] + d[mp1] * rhs[mp1] * rhs[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1eb0f7e4-9149-4255-9940-1c7d637e30da");
            m += inc;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2ca2a732-0f7f-4ffe-bf99-7a17337ff5b4");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5c2e6f37-eb5f-41f4-84ac-4940864ece8e");
        int next;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5618e05c-a8e0-4bb5-bb18-7ccfe703f9af");
        int i;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "28b27480-a233-468b-9395-4e7c34467c5f");
        int l;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ac3bab10-785e-45c4-a14b-dc527b6f0f79");
        if (list.length < 1 || list.length > nvars + 1 - pos1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7c013fc8-9207-451a-9cb3-b7d170fd59ae");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a193a89b-a84b-4b26-b4e2-c59d63ed1335");
        next = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2c6447c0-c902-4776-bef7-fd5c30dbe4e2");
        i = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8628dcdc-6338-44c8-8320-645ffc9d918b");
        while (i < nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7766f2ae-5bc9-49ce-aa5c-8e8d38254cf8");
            l = vorder[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7e81214a-aec5-4444-93f9-77ddd63a9356");
            for (int j = 0; j < list.length; j++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "fd889527-f6dc-4a3d-a38d-770f852b14ff");
                if (l == list[j] && i > next) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "165dd2e5-c1d0-4b96-86e5-1657097a6443");
                    this.vmove(i, next);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "73d10a09-deb4-420e-b549-51f528cde1f1");
                    ++next;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8c7511bb-15f4-432d-879c-8a4397f8c8e8");
                    if (next >= list.length + pos1) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0e547dd8-3495-49c2-a38e-3303ce996cb5");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b8096104-bb38-490d-9549-2222d5c157cd");
                        break;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7588b5a4-8c98-4447-9590-6fd1e4b9a1d6");
            ++i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "544cdfd4-f44f-4615-b955-a38c96f72041");
        return 0;
    }

    /**
     * Gets the diagonal of the Hat matrix also known as the leverage matrix.
     *
     * @param  row_data returns the diagonal of the hat matrix for this observation
     * @return the diagonal element of the hatmatrix
     */
    public double getDiagonalOfHatMatrix(double[] row_data) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8f866947-3a01-4890-8cb6-048e32587522");
        double[] wk = new double[this.nvars];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f278e105-b1e9-4e8c-9f3c-396bd1e77e54");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "86dd0d63-7cec-4579-8642-b242757a6038");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3cdd6df2-7b23-4b9a-8768-287c895c5ea8");
        if (row_data.length > nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d5c67fe1-033c-4e74-8935-54592e1564b2");
            return Double.NaN;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "33c8845c-054c-4af2-9cf2-a9900ff81c6e");
        double[] xrow;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a1307fb6-4b2e-4abb-a5c7-9b0cb1f8a650");
        if (this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "885d01a5-9a06-4b0a-9320-729e73742c9c");
            xrow = new double[row_data.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e60c97cd-c573-43a4-88c3-8432dca4ecee");
            xrow[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "04f80f3b-9239-4f93-9030-6e0e1cd0bf94");
            System.arraycopy(row_data, 0, xrow, 1, row_data.length);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a4be826b-8aeb-450d-9622-68b5edf0b303");
            xrow = row_data;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9e781707-471a-4c2a-9821-3a9dee165db9");
        double hii = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c9356223-7fe6-4693-bf1d-d28063c74b8b");
        for (int col = 0; col < xrow.length; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d10aabcf-3aab-4030-89b9-06d7a72ab81e");
            if (FastMath.sqrt(d[col]) < tol[col]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b50ea384-0bf8-4bd8-b5ff-a2b4316a0d4d");
                wk[col] = 0.0;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4df77932-1a5d-4010-bc5d-b5ef44dbb539");
                pos = col - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "16e11860-36fe-4cd4-93be-1d0a9ee0173b");
                total = xrow[col];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6311c729-9169-4414-a8e0-759b5f06b57a");
                for (int row = 0; row < col; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5c4b499e-7c46-4d35-9a7a-bbe6e8e893f0");
                    total = smartAdd(total, -wk[row] * r[pos]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2776ae66-d5a4-4d3d-b783-2548a10d0202");
                    pos += nvars - row - 2;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b75074bd-e643-45ea-bf57-3d56d5f82cbb");
                wk[col] = total;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "19bf322b-ae75-441a-a32c-93e27f64260c");
                hii = smartAdd(hii, (total * total) / d[col]);
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0ebc9b20-f2ca-43ac-b934-c4a529d611b1");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1c09a325-a904-4153-8234-f0721a463a6b");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "626d1fa6-a666-46ba-bd32-35fa64dfddfe");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "02fce1fb-2ddc-448c-b295-66dd8557aa63");
        if (this.nobs <= numberOfRegressors) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "29bfe45f-f592-443d-8a15-40409f19f4df");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, numberOfRegressors);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "75cd5291-6d94-449a-b8d8-114590cb0282");
        if (numberOfRegressors > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a5141b2b-9aa4-401c-ac49-cb1192ca1816");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, numberOfRegressors, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "086aa34c-2f3d-4b92-9e74-cd9e97c41e91");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a9041cce-2811-41a8-a34e-6fbc4dc0df13");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e80ebcd7-98e4-4f97-b7b4-8df459806889");
        double[] beta = this.regcf(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f8a064cf-4f62-457d-ac5e-3883dedeeee3");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ffb00dba-6d96-4772-a9af-6342cf8ed65d");
        double[] cov = this.cov(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3a7c2d8d-f342-4338-aa34-65c9dc42188a");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0de28a8b-273b-4067-baf8-a4f3e84f80a0");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "caed9e8b-ba39-49b5-8bdc-793b598d3dfa");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "94a21c82-9a0c-4926-9af4-7c1011b33547");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "31862a24-af05-4118-ad91-8c2dd488b426");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "22f6f770-2cb5-4e69-a8cf-dd3d51cf40ee");
        for (int i = 0; i < numberOfRegressors; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "66432da9-4a75-4c4d-ad2e-8078bff6cf42");
            if (this.vorder[i] != i) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3bcd1935-b245-49cb-a70d-4ff0ba568e89");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "37d64a1f-ba15-4698-9d1a-91c9727919e9");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9845d877-0555-47f6-aa35-7ca34eb15e80");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d068ad5f-712b-46e2-8f70-32ed88fdd94f");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c0866b8f-0ea4-489a-aa1c-50cb122cd34a");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d5570bad-ac93-464f-b0cf-71fc35e44d83");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e58910e6-c813-4ecc-bd95-b88448cd6db0");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2c8f46ba-dad0-4d96-964a-329aecfd60be");
            for (int i = 0; i < nvars; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5c37a1fa-d602-4768-b8e9-9f6437668cc3");
                for (int j = 0; j < numberOfRegressors; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8b659c63-15c4-4a29-95b1-d1d399fc94e7");
                    if (this.vorder[j] == i) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "08d9bbcc-279f-4729-b3a0-3f0164f410a4");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2e7b1464-8cb8-459e-b0fb-3a88cb64e3a1");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "8092e03a-7df3-4bbc-afca-6b33ee44b6d2");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c87923a6-ad9e-4091-8f00-7280c21d11dc");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "89c733d9-2a1d-43b6-991d-7d99473b008f");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4a441864-2065-4c09-b7e9-87f8570233d6");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "16a047d9-ebc7-4e76-8491-7bffa29c8196");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d21b9f40-0512-495b-b837-f918a4979198");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7d109d87-ef19-4d11-b0cd-02f3f649cdc5");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ea07ada0-f7f2-4789-93ee-2c5fd6c050fd");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "13e039b2-6525-4f3b-af88-2daacd6a182f");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "351019e8-00ba-42f7-97ee-9f2be8081320");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ba196cd0-e32a-45d3-8104-615a67cb9085");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "95b26079-eb29-490e-8b6c-ee26e6b5f8d5");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bbe243f5-3be4-4129-9897-f2bcfbf81e54");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b1713f8e-8a91-43e4-ae5f-c2f340e20f31");
        if (variablesToInclude.length > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "697615fb-9a65-437b-810f-68825f79dcdb");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, variablesToInclude.length, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "58caa29f-54fc-4055-abfe-3e574b89286a");
        if (this.nobs <= this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "786935b4-f3b5-485b-b97f-bfa939849c0e");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7376bccf-d223-48e6-b17b-236619fcd91f");
        Arrays.sort(variablesToInclude);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "eeb8fe56-763d-425a-90d9-91b82ba0851c");
        int iExclude = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "da980ea8-f4ec-4e67-8e70-7235bd6424b7");
        for (int i = 0; i < variablesToInclude.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1d400db4-d28e-4e71-985d-5b0b4ee8b0f4");
            if (i >= this.nvars) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "177bab97-fdf9-4e6a-82a5-5f323ab53d7d");
                throw new ModelSpecificationException(LocalizedFormats.INDEX_LARGER_THAN_MAX, i, this.nvars);
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ae046fa9-8cc6-4466-b227-66cce2155cf7");
            if (i > 0 && variablesToInclude[i] == variablesToInclude[i - 1]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "f15acf2f-25ad-4a1b-b0ae-daead7d0d5e4");
                variablesToInclude[i] = -1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "10cebdef-1629-4897-b716-34edaab0ebbb");
                ++iExclude;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4ae3a4b3-90b9-4c3a-8663-37839b0af2d5");
        int[] series;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b242f081-0842-4b11-b356-1f7f006e4653");
        if (iExclude > 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "6a117a90-65dd-4653-b6cc-d1623e2368f3");
            int j = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cf979d15-b54a-4bd2-9723-1ffac5b2d282");
            series = new int[variablesToInclude.length - iExclude];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5a876076-45a5-4e19-9b63-5cb88be252cc");
            for (int i = 0; i < variablesToInclude.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "c25097e7-87bc-4425-bfac-66759741b1fa");
                if (variablesToInclude[i] > -1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5e156ac4-db25-47ec-9e0a-1911af54cb2a");
                    series[j] = variablesToInclude[i];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "1ebf2baa-fc7c-40d5-aa46-7e32531f77a3");
                    ++j;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "70560c57-6fc5-420d-878e-675f7b9233aa");
            series = variablesToInclude;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2e3df5a5-143d-4a50-853f-cd6fecc2093c");
        reorderRegressors(series, 0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "d9710c89-0d94-4c1c-a4a1-5e0e54d8f8a1");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "59ab7e84-9ac4-4dea-af8b-7a4ae2f4cb92");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "4b9d147b-89b5-4652-b532-6bb262440c5f");
        double[] beta = this.regcf(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3c66e606-3c0c-4a48-8b47-a60efab978ca");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "e16a65c6-6020-423d-ad3c-e7d05966bc06");
        double[] cov = this.cov(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "20c1111f-72eb-42e4-9ca8-fd6c1a48937c");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "a2d97953-a36e-4d6f-86b6-1203bc4b5987");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7eb5824a-2f2f-4544-96e5-ae6f93f89584");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "41e2cd38-7312-4f7e-8e82-512cc7291a52");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2d535b61-a029-4b97-ac4c-a3d09b5d1314");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "85e283f0-581f-4dbd-a91a-721ed180dd8f");
        for (int i = 0; i < this.nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "5357819e-d70a-4ca1-854c-f37d9072f5f2");
            if (this.vorder[i] != series[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "9285cfd8-5dad-471b-8395-c470f3be273f");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "689d8f81-27a2-4b90-9889-283420acdc06");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "37d9377c-c4c0-4358-bff0-bda3ea2dd4b8");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "78716b74-89bf-4b11-bb5c-03ef4a2d0f3e");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "92e27640-6f19-499f-8681-d70d114aa1c1");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "150cb024-9a73-43bb-8fe7-665d5bbdd471");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "bf50b1c6-3e53-4bd6-a977-0de4a4e672a0");
            for (int i = 0; i < series.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "857b7524-f2de-448f-a42c-03128494dc4b");
                for (int j = 0; j < this.vorder.length; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "dc03fef2-2292-48f6-af8d-c75b3973d4e8");
                    if (this.vorder[j] == series[i]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "214a1ac1-dfc7-499e-a949-57ecff3140fe");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "36260fdf-494e-41a6-b3f3-894a2030ae58");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ccb5d6eb-94be-4490-8cf7-212ad0b3d063");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "0eac091e-28c4-4844-b1cd-2638f91bbad5");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "be385604-1d3e-4d25-8ece-c27daa29dd07");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "03d41478-9e60-4143-b005-cef1e82ff1cd");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "b0e3359d-7599-43e0-a472-32ac0e9f11dc");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "cc199de2-7d7c-43f1-bb50-a2fe4a9b5f12");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "3ca2fa58-ef4f-4370-abbc-7af86c62ca6c");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "553b50c7-7fd7-415a-9461-1316c885be9c");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "379280e9-f99a-4933-8bd0-a129f423381f");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "2ef9c79d-b44f-444e-9ce2-69a0c3698316");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "ebd95fb6-5f41-496f-b795-1aec3f48eae1");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "86e1093f-0647-4245-bfde-3a9076c40ece");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "7e8b7de3-0bfb-4a42-9c49-46bda34effbb");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_2_10.coverage", "512da724-b93a-4efd-8475-8edb18b862f1");
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
