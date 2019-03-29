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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7c0bb1bf-c18f-4e3a-91d4-f8bb68cd2a42");
        return this.hasIntercept;
    }

    /**
     * Gets the number of observations added to the regression model.
     * @return number of observations
     */
    @Override
    public long getN() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7f5ac4ee-fdaf-4616-9ba2-2e437cd29b2d");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3c378cf8-d979-4560-8dfa-2ad08a281fc4");
        if ((!this.hasIntercept && x.length != nvars) || (this.hasIntercept && x.length + 1 != nvars)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "88ab63d5-d3f8-446a-935d-089e150caff4");
            throw new ModelSpecificationException(LocalizedFormats.INVALID_REGRESSION_OBSERVATION, x.length, nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f12d97ee-faba-4911-8a60-d56ac9fe4792");
        if (!this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9de654b2-b993-4629-8b5c-635b3d19503f");
            include(MathArrays.copyOf(x, x.length), 1.0, y);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9d5d0b71-5fde-4a22-b5c8-328eca802b3f");
            final double[] tmp = new double[x.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "db75d70b-5d24-4700-83af-d0b4454dfe52");
            System.arraycopy(x, 0, tmp, 1, x.length);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2878b23d-9191-4ac9-817d-6afe474ef739");
            tmp[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f1acb518-992f-4d99-8fce-92c25e702624");
            include(tmp, 1.0, y);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ed432289-a363-4d57-b6fc-98ca2e8997c4");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b3dfdb63-df90-4bda-8c13-7c539b22f1cd");
        if ((x == null) || (y == null) || (x.length != y.length)) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8f3b8048-d942-49ad-92d5-ffab03b227ed");
            throw new ModelSpecificationException(LocalizedFormats.DIMENSIONS_MISMATCH_SIMPLE, (x == null) ? 0 : x.length, (y == null) ? 0 : y.length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d77649f0-a57a-4863-9a76-800dd26c3963");
        if (x.length == 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6a5e746d-e470-44b6-ab22-45c1aff39bd6");
            throw new ModelSpecificationException(LocalizedFormats.NO_DATA);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ead8ea61-ee8e-417b-bdde-92b4d1c35927");
        if (x[0].length + 1 > x.length) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2b6d5f34-8b9a-460e-a768-a1fd33619105");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, x.length, x[0].length);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cae1c5fe-bb6a-4ce7-a48d-38fa80b59b24");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2ba74917-b5be-45e5-a4bc-9ca41431469c");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "166e005b-d919-44a3-9e83-d40fbce7372a");
        int nextr = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9f87d739-6080-4044-b841-a1a904a17e47");
        double w = wi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5440a7c9-b66a-485a-9d38-c92ae5e6d29d");
        double y = yi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c3b670ef-f4c6-49c4-bf3f-90f50d028570");
        double xi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "190843b0-ef4f-4644-9054-4d2bad6cb3bc");
        double di;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2ceaefa5-5cbe-4211-8f41-18c19d3cb265");
        double wxi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "747d6bd7-d263-4347-b7a7-de27028a5719");
        double dpi;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "80aee8e9-66f4-4886-baaa-c2d2867e4a38");
        double xk;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "432924a1-9603-4428-82eb-0c49ee4d631c");
        double _w;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "501ccf2c-77d6-487f-9a25-0fa4658702e7");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "12854408-a726-4353-b0c3-3330f5cd8f6d");
        sumy = smartAdd(yi, sumy);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3cac9795-d8e7-4dee-b090-2cb4e3b49ee2");
        sumsqy = smartAdd(sumsqy, yi * yi);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "77a8ea1f-46f3-4443-84fb-dd94e34a6973");
        for (int i = 0; i < x.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "544e1565-2cd2-432e-805f-419ad8cf7939");
            if (w == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9826b675-6c9b-42b6-b722-b8f6534a9369");
                return;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a5562700-894f-44aa-879f-e0b93d2131f2");
            xi = x[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4b611f3f-23c1-46d8-aba9-0527156f9226");
            if (xi == 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bcb57e16-b375-4eaa-b3fe-6374d491c60e");
                nextr += nvars - i - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e2a2ba5b-7685-45b6-907f-58a52a0da715");
                continue;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "13626ce0-451e-44ae-9869-adb6b832604a");
            di = d[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d359c6d0-052a-4587-8ea8-456b0dfcc41e");
            wxi = w * xi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "696237df-aa84-4af4-9b38-54c83aa3793e");
            _w = w;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c0c37057-8f23-4560-8a40-28128914a0ab");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5fe795fd-e9d4-4aba-83f6-668e9deaabf5");
                dpi = smartAdd(di, wxi * xi);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "233cc363-72f9-4473-92cb-cdad35a18d31");
                final double tmp = wxi * xi / di;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "69fd53c7-8587-4883-983a-5dcacb4caee2");
                if (FastMath.abs(tmp) > Precision.EPSILON) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "44ba2295-e43a-4e45-b939-302214419f17");
                    w = (di * w) / dpi;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c366300e-0c1a-47cb-9e9e-06d13c4de79a");
                dpi = wxi * xi;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c7d928d2-9228-43b2-9afb-3d18a2cfbfea");
                w = 0.0;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a6e2d273-1c38-47a6-8cb8-70977a60de84");
            d[i] = dpi;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2353c6fe-8c4d-4463-b909-af3f7765bdd2");
            for (int k = i + 1; k < nvars; k++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a9e649d1-1587-45cb-a29b-43effcbb0fa8");
                xk = x[k];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "90b2ef33-fd76-497e-8c9a-5a06e9779e44");
                x[k] = smartAdd(xk, -xi * r[nextr]);
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a220ddd6-4b00-4b26-8fe7-228f3d005177");
                if (di != 0.0) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "685b3b34-5a8f-46c8-a87b-4b1d72c3f2a1");
                    r[nextr] = smartAdd(di * r[nextr], (_w * xi) * xk) / dpi;
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "831b8125-fb1c-4b6e-bca7-6692a1c32f66");
                    r[nextr] = xk / xi;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0f0fad7e-60d1-43ec-806a-733c2cb7a30a");
                ++nextr;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8ae44a0e-9bdc-460e-aa47-f82b00bc02db");
            xk = y;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a6de783a-6a4c-4cb6-af19-929db2bd36cf");
            y = smartAdd(xk, -xi * rhs[i]);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "98e68d0c-188a-4043-b075-a8b7630c6eeb");
            if (di != 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "01074abc-6196-4bb7-8c53-7650d039f5b4");
                rhs[i] = smartAdd(di * rhs[i], wxi * xk) / dpi;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2c285c9b-5c12-459e-b180-ea08cd9fca5f");
                rhs[i] = xk / xi;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c307f3c9-ea93-4082-82ac-e286f2f4a731");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b8fa7e66-ac54-44ff-b173-339b57645afd");
        final double _a = FastMath.abs(a);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "87173780-c028-4f8f-bd14-2b49ce3b0f4c");
        final double _b = FastMath.abs(b);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f836b8b1-f60e-422b-a1cc-4f528d9b6afb");
        if (_a > _b) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ea97117e-d0e7-459c-a247-04e209ffb38e");
            final double eps = _a * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "428d1cd6-00e0-4416-bbc0-b1d1fad079c4");
            if (_b > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9e807b65-0a52-46c6-a444-1759c47a11d0");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fca777a1-a735-4561-aab9-59287af0a948");
            return a;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7056abe0-dd17-4bea-bc9e-8f935d699dbd");
            final double eps = _b * Precision.EPSILON;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "31135c8c-b2e3-4640-bb76-689d85773342");
            if (_a > eps) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9c8708b7-77f7-4c51-8397-c9aed1fb1a81");
                return a + b;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f9340cb9-2823-4935-bc43-41d6be6c0139");
            return b;
        }
    }

    /**
     * As the name suggests,  clear wipes the internals and reorders everything in the
     * canonical order.
     */
    @Override
    public void clear() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0ac0c799-64cc-4eef-b50c-eae255881860");
        Arrays.fill(this.d, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7eaa43ad-e6cf-4026-88d0-51fdac7eafb5");
        Arrays.fill(this.rhs, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b7f9b659-18a2-4767-bc0f-7e7910534205");
        Arrays.fill(this.r, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8b7fb624-7fde-45ee-a25e-5e44df3e029b");
        Arrays.fill(this.tol, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2d98f7ba-6d07-4f40-a717-239b3662e950");
        Arrays.fill(this.rss, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "dd57ac0b-589b-47b6-9b70-de01328eeb35");
        Arrays.fill(this.work_tolset, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a985e389-209b-4155-905f-f645bb48fb3c");
        Arrays.fill(this.work_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2e9fdf56-20d6-4fa2-b87d-2b3559ca60ba");
        Arrays.fill(this.x_sing, 0.0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bd5a79ff-0a00-4f17-9c85-0cd82d26356e");
        Arrays.fill(this.lindep, false);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9e6f2f0b-4647-448a-921f-982a8c402852");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "aa16ebbb-97b5-45a9-9293-f5464d15124b");
            this.vorder[i] = i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "56636074-dddb-4505-b9c7-8a2789ef488a");
        this.nobs = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "704e5b35-c610-46f7-b0bf-691f6227b063");
        this.sserr = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e727a9a2-317c-46e0-b927-0dab7eec7b6f");
        this.sumy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "270cf9c6-29d4-4d8b-9c0b-7ee776dce0fb");
        this.sumsqy = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "00c4b9a1-7b34-459e-bfa7-9936997d36ed");
        this.rss_set = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "92d47c29-9672-4b21-a670-1011da73ec8f");
        this.tol_set = false;
    }

    /**
     * This sets up tolerances for singularity testing.
     */
    private void tolset() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2eaff37f-8cfe-4d40-85f3-cf9da495d747");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5f8401e6-d341-418b-99b3-cce7ba46e857");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e968376f-86a3-488f-8bf7-c30e35c77782");
        final double eps = this.epsilon;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "829dab46-20ad-4176-99f8-bac94db7213e");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7cfd0313-21e4-4618-bca8-1f9eb166037b");
            this.work_tolset[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "977e3bd4-6815-4835-ad0d-362382db3ac8");
        tol[0] = eps * this.work_tolset[0];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "08b78bfd-bb67-4a9f-be27-6c15e48a6792");
        for (int col = 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2f2c25f3-926c-4751-89d5-ba0ba01d9a19");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e11cce46-7df2-4ba7-b86d-0cdb49c282f0");
            total = work_tolset[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b7521d18-f5fc-451c-8302-989cd89abdf4");
            for (int row = 0; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "824dbe45-0342-4d70-b6db-13ff2edaff9b");
                total += FastMath.abs(r[pos]) * work_tolset[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0023d287-6b5e-491f-9147-69fc30b5af98");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b797fdb2-c5df-49e5-aace-bbf29eb161d2");
            tol[col] = eps * total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6791299b-f86c-41b3-882d-b507a9a0964f");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "059ff973-d380-4671-8b61-30208a5c7f34");
        int nextr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d5b4c448-2c5e-451e-9047-34f7fc385416");
        if (nreq < 1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ad360165-f591-4b06-94fb-26d3849418ea");
            throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0b5533d3-9b32-425a-b117-c8705fca0e46");
        if (nreq > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9e097e2f-890d-4b8c-b5fc-37f8e965b44b");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, nreq, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "af29f398-486c-4c2d-bc78-c4ca4204f33f");
        if (!this.tol_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4f97bb0a-a54a-4660-8cc8-17589bc9fc6f");
            tolset();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1923d8cf-c449-4764-bacf-1829d3df7c21");
        final double[] ret = new double[nreq];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f591e98b-32ab-4a7e-8633-f6e20f20bb77");
        boolean rankProblem = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "dc9f50c6-8da5-4284-b999-2554cd89b5cb");
        for (int i = nreq - 1; i > -1; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "761b4c21-232b-4bd0-9e20-63ea3c71c4d6");
            if (FastMath.sqrt(d[i]) < tol[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e739bd1f-9aa8-481d-bf5b-6ac8bd854672");
                ret[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d4fafe78-9f90-4667-9caf-f8e2ed741db5");
                d[i] = 0.0;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9b477927-9f3b-44f8-82ab-830ddb4192a7");
                rankProblem = true;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "08d5cd93-7313-40cd-a775-959684ba669b");
                ret[i] = rhs[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b1a12f37-c7f9-43da-b5db-fd994abd6e15");
                nextr = i * (nvars + nvars - i - 1) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "15ab9e4c-f732-4854-8930-486ebfb55f16");
                for (int j = i + 1; j < nreq; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b9d7e1ab-c257-4451-a304-ff2e9d86c017");
                    ret[i] = smartAdd(ret[i], -r[nextr] * ret[j]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "58751abd-fa55-4e2d-bc80-4e05af51430f");
                    ++nextr;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bcb55886-4400-498e-8e77-5ff8be46a730");
        if (rankProblem) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1d23097f-711a-4839-aaca-b902e5e5b282");
            for (int i = 0; i < nreq; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bdf3fb7d-c314-452e-a92c-7e0de8538fe7");
                if (this.lindep[i]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1ddf7953-cbdf-4d74-a4e6-a263cf2c65e7");
                    ret[i] = Double.NaN;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e8ae2894-dc94-4f95-9b39-6692ddaf9548");
        return ret;
    }

    /**
     * The method which checks for singularities and then eliminates the offending
     * columns.
     */
    private void singcheck() {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d5c69c73-5f16-4383-83d1-7756e868eb04");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d0bfdf9e-7bbd-4de7-bc4b-7881a4d8494b");
        for (int i = 0; i < nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ac96aae6-57c1-4bec-9e28-40120edf00ee");
            work_sing[i] = FastMath.sqrt(d[i]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "20195284-110d-4972-bbbf-70bf0b560442");
        for (int col = 0; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e956e575-d0fa-45b6-988a-93ebf07644a1");
            final double temp = tol[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0010eaed-698a-463f-9274-fad4702595e5");
            pos = col - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d84ed5f5-fe7b-48ed-86de-19ed3aa0b695");
            for (int row = 0; row < col - 1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "73110b7b-4050-400e-8ba5-413c8037724f");
                if (FastMath.abs(r[pos]) * work_sing[row] < temp) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "30d439b1-877c-4262-99d6-76f455431006");
                    r[pos] = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ecafd15e-d5e4-4bb6-bc1c-ec4193e83cd8");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7b6ea7ef-e6a1-4c49-a0df-df48183c3849");
            lindep[col] = false;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "65bdbccf-2337-4185-9dc6-21c07b91368d");
            if (work_sing[col] < temp) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "787a4655-b71a-47da-8aa3-6f8784bc9e4e");
                lindep[col] = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "554d36f8-9c9f-49d0-98b1-4022a5ec7b9b");
                if (col < nvars - 1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1be2468c-d98d-4526-ad25-94b0f1107283");
                    Arrays.fill(x_sing, 0.0);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bb735d53-6ecd-47fc-ae83-48a7d0147e26");
                    int _pi = col * (nvars + nvars - col - 1) / 2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bce27633-a3eb-40f6-90cb-cc01bbca36ba");
                    for (int _xi = col + 1; _xi < nvars; _xi++, _pi++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e7bbfcde-99c0-4b23-8f9f-6a3bd2696d3a");
                        x_sing[_xi] = r[_pi];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "677ad4c4-213b-4bbb-8aed-2f959c87954a");
                        r[_pi] = 0.0;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1a23dd96-7c65-4cb7-b0d6-e1c53b338946");
                    final double y = rhs[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "83b67565-5820-4ce3-aa00-b235579aa936");
                    final double weight = d[col];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b658d4cb-15ee-407d-a408-8d3f2db05130");
                    d[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "68e31def-9e68-4853-b92b-1ef928e862ef");
                    rhs[col] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3c8a5192-48b0-44ca-acf9-30de39c21d39");
                    this.include(x_sing, weight, y);
                } else {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9b39be6e-26f4-4ce1-abb4-8cbfeb3624b2");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "18fb33bd-5404-4c1a-aeb3-c50f206c3336");
        double total = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1c1f1c0a-1d4f-4ca8-be32-f3248a367eab");
        rss[nvars - 1] = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d129c87f-4062-4af2-b72d-0654c71aaa30");
        for (int i = nvars - 1; i > 0; i--) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f731d49c-cd76-4b47-8de7-9bb68e43e6c0");
            total += d[i] * rhs[i] * rhs[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9633489a-f6d5-4aba-b913-da662415e8d1");
            rss[i - 1] = total;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cc219786-d86c-4529-a88d-cd4b6e4e765e");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "31529292-2e74-4bfe-8f99-3d8dd0f05149");
        if (this.nobs <= nreq) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e42ed00a-1e37-421f-a3d7-7a1cc19f7c4b");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5fecbfd6-e9fb-44c1-9c9d-e8a78c796270");
        double rnk = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ada7998b-daf8-4df6-9de1-787e2e5bbf7e");
        for (int i = 0; i < nreq; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d02aaa6f-ad2f-4118-8b20-3f46ac5d24c3");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "63761ea5-6567-45ac-8ecb-92b42bc5b3f6");
                rnk += 1.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "93d95a8c-ea5a-4b0e-951e-83d4d92ea831");
        final double var = rss[nreq - 1] / (nobs - rnk);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c8df471e-da67-45ef-8e90-14fcdc842ede");
        final double[] rinv = new double[nreq * (nreq - 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e6cc50df-20f3-433a-a44d-f0957a8282e8");
        inverse(rinv, nreq);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "40bb6d1a-6aea-45fd-baac-1778a052e592");
        final double[] covmat = new double[nreq * (nreq + 1) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a7cebcf9-0c01-482f-9097-f370b8c56179");
        Arrays.fill(covmat, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f76f9a5a-681c-451c-8370-d92aba5d02bf");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2f1032c6-ca3c-4a15-989c-5479f5111018");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2a839e26-8e03-4748-8326-b7d8f0455f1d");
        int start = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "566d076a-2eeb-40ed-85bb-1c6c8e4ab727");
        double total = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "26182924-df34-416b-961d-becdd9694b87");
        for (int row = 0; row < nreq; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "daf69c9b-11c5-4955-9575-430157d70909");
            pos2 = start;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "98534512-d000-4ca8-b846-593df4d7b7ee");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "acadb323-5e98-4dd5-be9c-5485eec14a65");
                for (int col = row; col < nreq; col++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d31451f3-d280-44b5-956d-c8200856ce15");
                    if (!this.lindep[col]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "072e7e56-8c79-4b5a-b3da-327b1138bfb0");
                        pos1 = start + col - row;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5320a863-18ef-4bc0-a09c-e5170918403e");
                        if (row == col) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a7483f9d-0567-4c12-8dd1-61c96879bc7a");
                            total = 1.0 / d[col];
                        } else {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cb5ef11b-61f9-4716-b4ef-644d321c1771");
                            total = rinv[pos1 - 1] / d[col];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7aecc01d-ef36-4b1d-b960-ef6824a71b0c");
                        for (int k = col + 1; k < nreq; k++) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e61eb488-7333-4904-9375-f45e4fcf67cb");
                            if (!this.lindep[k]) {
                                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "866df0f4-284a-41ed-8d50-0cce6e693fc8");
                                total += rinv[pos1] * rinv[pos2] / d[k];
                            }
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f0aa3eb0-f649-4929-ae16-ad497947066d");
                            ++pos1;
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "02bfe62c-a9de-4dea-abfa-1cab788d4b04");
                            ++pos2;
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e98d9e32-32ac-4712-bd78-2d829ad6fc5c");
                        covmat[(col + 1) * col / 2 + row] = total * var;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "49d7f5ba-0830-4187-92e5-a7db8fbf3362");
                        pos2 += nreq - col - 1;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a9997594-d7f4-4edf-a6c0-e5be580dd6d8");
            start += nreq - row - 1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7fb03f9d-f9cb-4a5a-829c-997a42d6f371");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e0167c02-c71d-4e11-8ab5-b2a00dd446ee");
        int pos = nreq * (nreq - 1) / 2 - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "54518726-5aed-4291-8e6a-8a5adf2539a2");
        int pos1 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6acf0a91-c857-42c8-8cc1-cbd7feb848db");
        int pos2 = -1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "745295d3-2de7-4b3b-96a1-b72625ab63e1");
        double total = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2a94c711-a334-4548-b186-b415d1f74fd1");
        Arrays.fill(rinv, Double.NaN);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "419e2b41-4da3-45fd-a734-e64aa946a7ab");
        for (int row = nreq - 1; row > 0; --row) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "35820a76-8ffc-4f32-b342-502b2829608e");
            if (!this.lindep[row]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fe885d91-dda7-471b-91d5-24686c6321cb");
                final int start = (row - 1) * (nvars + nvars - row) / 2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e41fb450-8c25-413f-9783-acc45e6319d0");
                for (int col = nreq; col > row; --col) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e5e733ae-4873-4459-b431-659f7ac4d93f");
                    pos1 = start;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d043799d-31c1-41f2-9d69-886196142c3d");
                    pos2 = pos;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7e67c436-9911-4d9c-94d9-cb8419dbd378");
                    total = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4e9a2743-d2ae-404d-ab93-82b4954f9c76");
                    for (int k = row; k < col - 1; k++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "35545c76-eaa6-4dc3-a07b-5d3dff56aca7");
                        pos2 += nreq - k - 1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "49a88260-2659-4191-87a7-6e5cb5ccce7c");
                        if (!this.lindep[k]) {
                            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "93d76ccd-ba4d-41e0-ad4f-a474decbf47f");
                            total += -r[pos1] * rinv[pos2];
                        }
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "19e44cb2-942f-4043-95b9-5bc7caf4ccc2");
                        ++pos1;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fe2e8d87-3d07-44d8-8bdb-71a8a276ab6f");
                    rinv[pos] = total - r[pos1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1eeefff0-909f-4c4a-bbfb-0b78e02b7d8f");
                    --pos;
                }
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e03834f4-744f-4d42-ba22-b4c55a490bc9");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5f1edcba-4cfe-4b35-9196-9df6d6ace1ad");
        final double[] output = new double[(nvars - in + 1) * (nvars - in) / 2];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b2f7fee3-d303-4746-ad09-4c114308ab85");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "47a72c6a-7b59-46c0-a297-4d8fb82f2f12");
        int pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a0107183-e3eb-4379-8898-e40082f56c10");
        int pos2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "31867e22-2639-425e-a994-95139116716d");
        final int rms_off = -in;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9841bf32-e033-4f2e-a1b3-a8aa9380111f");
        final int wrk_off = -(in + 1);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "85d2eaa4-f7fb-4c04-b574-6acd9104e096");
        final double[] rms = new double[nvars - in];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b58a5442-7604-469f-93e1-5b8c259c7338");
        final double[] work = new double[nvars - in - 1];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1420bc8e-0838-42c8-ac6f-f6684cde0e13");
        double sumxx;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6a7307b3-5655-4caa-88d0-dc3e2f839ada");
        double sumxy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e41d0e1c-3122-4a6d-ab79-dff1f4afb760");
        double sumyy;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "867db8f4-69a2-411e-8c6f-853ff9eeae98");
        final int offXX = (nvars - in) * (nvars - in - 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8ab81e8f-ee48-4696-8ba7-58a99de67ba8");
        if (in < -1 || in >= nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "35f76f8f-93ac-43f6-9d93-a4750829e819");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fb2f0f8c-1b9d-4436-95bc-7f433734955c");
        final int nvm = nvars - 1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "951701b0-ae96-46d0-a56e-cf6d57f8cdef");
        final int base_pos = r.length - (nvm - in) * (nvm - in + 1) / 2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "790e6bd0-1e0b-4ad4-b085-5f744a159abb");
        if (d[in] > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "df03837e-6ed2-41c0-96e6-3a62fffe74c5");
            rms[in + rms_off] = 1.0 / FastMath.sqrt(d[in]);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8c5131ec-7158-4bd6-bd23-56da5b8f0f6a");
        for (int col = in + 1; col < nvars; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "748e8796-1c7d-4bbf-9164-ffd464bb2ea7");
            pos = base_pos + col - 1 - in;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0dda6435-a9f1-430f-8e3d-32f07416da4c");
            sumxx = d[col];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e8d4f084-0c4f-4de6-b888-c96d6d74672c");
            for (int row = in; row < col; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d1c8d858-2b50-449c-ad4f-8cf3e0d3612f");
                sumxx += d[row] * r[pos] * r[pos];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "feba54d7-17bd-4854-85de-26b052c15368");
                pos += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6c93f6b3-bbc8-453a-829d-e55f3906a15d");
            if (sumxx > 0.0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "55f70c77-7654-4aa0-b5e6-33b4b2ca89d9");
                rms[col + rms_off] = 1.0 / FastMath.sqrt(sumxx);
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ff989ed9-4612-4a50-a598-78278789d1f9");
                rms[col + rms_off] = 0.0;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bc6e34f5-57eb-446d-b16c-00dce458f1b1");
        sumyy = sserr;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "38e425ff-1397-4b67-a497-36f7a3b4a809");
        for (int row = in; row < nvars; row++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "781c9a02-924c-416c-87e4-9b726dadcc78");
            sumyy += d[row] * rhs[row] * rhs[row];
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0742e0da-d902-48ee-ab9b-3bfaa2353252");
        if (sumyy > 0.0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7ab3bb63-54da-4120-bef0-71531193c3ca");
            sumyy = 1.0 / FastMath.sqrt(sumyy);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e50e0e89-0944-40b1-b606-f1b30c7e6db1");
        pos = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b6dd1309-29c0-4ca1-bca8-6f561100ca52");
        for (int col1 = in; col1 < nvars; col1++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "93f8d3eb-81e9-4e32-a49f-f234d53dc2c9");
            sumxy = 0.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c8ee93c1-29c9-4c49-916c-c8200a8e872c");
            Arrays.fill(work, 0.0);
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "826aa0b6-5db1-4ccd-b267-55df714bdf2b");
            pos1 = base_pos + col1 - in - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4fa320a0-34d8-4706-bb18-66e0eeb21128");
            for (int row = in; row < col1; row++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cfb8313f-ffc3-4d48-9d91-b97331a414b3");
                pos2 = pos1 + 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "337e2bca-45a1-491a-ad34-c05d2cfde8a8");
                for (int col2 = col1 + 1; col2 < nvars; col2++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3b207cb6-03ea-4460-beb5-0a5113d4a46e");
                    work[col2 + wrk_off] += d[row] * r[pos1] * r[pos2];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "df01d93f-c8ea-4c84-9a99-f6012ca97904");
                    pos2++;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "259a6b66-1859-437f-af92-45686963e3e8");
                sumxy += d[row] * r[pos1] * rhs[row];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "205f15de-0d02-42ca-8798-6b8ece477fac");
                pos1 += nvars - row - 2;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e2eae795-254f-47ee-9cde-23f27e1a1358");
            pos2 = pos1 + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c01e8578-53b0-40e2-b213-60dbdede8b1b");
            for (int col2 = col1 + 1; col2 < nvars; col2++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7d54084a-40e9-47d3-b056-a1646e6089c6");
                work[col2 + wrk_off] += d[col1] * r[pos2];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5804e5b3-e091-4a4c-8b9d-f26c88cd124f");
                ++pos2;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3f53b5fe-6d78-4e75-96cc-3096768065c2");
                output[(col2 - 1 - in) * (col2 - in) / 2 + col1 - in] = work[col2 + wrk_off] * rms[col1 + rms_off] * rms[col2 + rms_off];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a9347b76-56c6-4709-b01e-bdd947100a5d");
                ++pos;
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7f2d7d13-fa22-4070-8f8b-d1c342c5109c");
            sumxy += d[col1] * rhs[col1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fca3a35f-834a-4123-addd-318dee136de9");
            output[col1 + rms_off + offXX] = sumxy * rms[col1 + rms_off] * sumyy;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8aeea34d-8df7-46b2-b1e0-5162d2b12e06");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "91ab068e-74d9-4e20-a74e-989a449d3bef");
        double d1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "338129af-d340-46f1-b89c-4ae1c000b107");
        double d2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "67d0588d-d2f9-4977-903d-9589aa77c4af");
        double X;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fb1c8e57-c92a-4318-a2fd-033100f3f050");
        double d1new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b45e9164-21a8-4e7b-a30a-5a4058361539");
        double d2new;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c782c747-313a-46ac-a1fe-e9db02121228");
        double cbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2119bda2-0a96-420e-ae60-9db78bd6ade1");
        double sbar;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bb7b4ba0-9241-470e-a57c-2684d3234d8e");
        double Y;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ea5815c5-cc80-405b-9bf4-76f4364eeeae");
        int first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1a08d275-d119-4b15-b0d0-776785a95eee");
        int inc;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ab055c7f-c7ec-4a11-9728-22d090e8eb0c");
        int m1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "692d5ec9-3374-4314-afbe-c8b90552fe8c");
        int m2;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9e06e205-7196-4ea6-b34d-cbe3458b589d");
        int mp1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7da1dbbb-a846-4654-923e-bb3157511672");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4adbb88d-4669-49d8-9d93-be2ceecfd486");
        boolean bSkipTo40 = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "459f2181-3610-4197-9f36-0f937f43ffa6");
        if (from == to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "38ca90f5-e0b7-47aa-a769-7e7af159a28d");
            return;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "de7b7a7d-8f96-4104-962b-71fb12613ebc");
        if (!this.rss_set) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "229d741c-6740-47ae-81dd-5c654e40c97e");
            ss();
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d31097fa-7abb-4298-b1da-3689ee838778");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2aeaad3a-c37c-4d9a-b0f5-9741c1127793");
        if (from < to) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9c9dc383-9a92-4359-ab6f-efbcd5146057");
            first = from;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8a21562d-969f-407d-82de-cea448da45b1");
            inc = 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cfd859a4-f476-41f1-92b6-e74147ce2726");
            count = to - from;
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1cc00a16-e73f-471d-85c8-892bc8e830a9");
            first = from - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6c6c55e0-2cf2-4fcb-9f66-60cbfb0518e7");
            inc = -1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d0b7af20-f04b-4b43-8228-a5840539f248");
            count = from - to;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b4386ad1-8769-4aea-80b5-e56d0602fe1e");
        int m = first;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "df899076-1506-4601-97db-6d2178d6eef3");
        int idx = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a1ae98ba-cb38-4a96-bdfc-d16d01cdb751");
        while (idx < count) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "40f29a58-ff2b-4b15-986f-6d0e55d58e36");
            m1 = m * (nvars + nvars - m - 1) / 2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3ab4289b-93f0-45a8-be9e-930495769e62");
            m2 = m1 + nvars - m - 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3318aa10-5f9d-4262-a194-79983a22cb1e");
            mp1 = m + 1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "390e3efa-5a9c-4826-9e72-bf60c4dd4273");
            d1 = d[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "04dd142c-1f0d-4244-bf5d-3438c6b08a2e");
            d2 = d[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "01a22eb0-7851-4c8d-a548-5bc6b974c51c");
            if (d1 > this.epsilon || d2 > this.epsilon) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d44dea9f-e2bd-4d7f-9dfb-168290cc07ae");
                X = r[m1];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8c8db884-aeba-4a77-8c9f-eb06cbbfce9c");
                if (FastMath.abs(X) * FastMath.sqrt(d1) < tol[mp1]) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5fdab724-8744-485a-a988-56ab0c70097e");
                    X = 0.0;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bec1bc72-b9c1-4c1c-a358-c7c7ee153e24");
                if (d1 < this.epsilon || FastMath.abs(X) < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "eaec7c60-e282-4142-acc7-4e7aaf6495b9");
                    d[m] = d2;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "46169cb1-b04c-409c-b6a0-2da1e2a1ff63");
                    d[mp1] = d1;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5e178004-0499-4200-a069-60758a22148c");
                    r[m1] = 0.0;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "524acccb-024e-44b0-b6dc-ad34e17310b0");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a7e676e3-1024-45da-9e43-6825ff83b689");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "60730570-8e41-4337-beb6-823437038fda");
                        X = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e8234a0c-549a-469a-9525-ce82b7a75db3");
                        r[m1] = r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f19cef7d-ec9f-4e92-956e-6a7e5fc121f5");
                        r[m2] = X;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3e33b786-bd27-41d5-bd13-4da9e0548800");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1106c65b-b139-4271-aa88-46a91131ed5d");
                    X = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9f9f448b-0abf-4a8c-96e1-20bef6613335");
                    rhs[m] = rhs[mp1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "86357f70-a350-454d-9043-eca5dcf2e421");
                    rhs[mp1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c2966eb2-8d15-46e8-a56d-7a285dcf24ca");
                    bSkipTo40 = true;
                } else if (d2 < this.epsilon) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e0c6375b-5aa7-4a31-a75f-47e7bcf10085");
                    d[m] = d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5f2ba91c-3f9b-421c-9773-210e9598792b");
                    r[m1] = 1.0 / X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d571fdae-0ce9-479e-97b2-ecdcf4887608");
                    for (int _i = m1 + 1; _i < m1 + nvars - m - 1; _i++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2e951b31-14bd-40be-afd9-e9810a6ffbee");
                        r[_i] /= X;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d73eefce-2fd0-4e22-aa71-12a3a02edd5e");
                    rhs[m] /= X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "dd32d407-83e2-47e6-97ab-2bf82af3df6b");
                    bSkipTo40 = true;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f0224722-8751-4469-86d5-a35fa90f3cc1");
                if (!bSkipTo40) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f0542551-d335-4d42-b8f6-8b4307126fe9");
                    d1new = d2 + d1 * X * X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "293f0dc2-520d-4c56-8ee6-b73113e9636b");
                    cbar = d2 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "13b91cdd-f3c2-409e-9d78-7a099c36d58c");
                    sbar = X * d1 / d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "89bffee9-2d89-4ca1-9b3e-f7549e031572");
                    d2new = d1 * cbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6ff40355-2cb2-462f-a54e-0f9ece812422");
                    d[m] = d1new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "94cf7ddc-839c-4757-b51e-2420331b1c6c");
                    d[mp1] = d2new;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "53ddec1e-43d4-4707-99d6-f2d6d1588fc6");
                    r[m1] = sbar;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2ff0d596-ae72-4211-bea6-3c5e561e53dc");
                    for (int col = m + 2; col < nvars; col++) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f67dc462-b1a9-4eee-9984-4a3460ccde73");
                        ++m1;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3072121c-c8e1-41e5-a617-5652a0617156");
                        Y = r[m1];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "dc4ded58-4f82-401e-a250-693f2392aea3");
                        r[m1] = cbar * r[m2] + sbar * Y;
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7babae4a-e5f9-4545-bb95-98483df3ac81");
                        r[m2] = Y - X * r[m2];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c7429d41-8bed-499c-b7c9-dda8daa4e803");
                        ++m2;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "26133f76-1a38-47c2-9771-30a3481ed762");
                    Y = rhs[m];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7dc56cb9-e197-41de-943d-81c662739188");
                    rhs[m] = cbar * rhs[mp1] + sbar * Y;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7bde0691-de59-403b-8904-9595611593c0");
                    rhs[mp1] = Y - X * rhs[mp1];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3646fb3d-67e2-4fed-bdda-0d3b7ad3e772");
            if (m > 0) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8a14130e-d6fa-4cee-8673-03163c8a5a82");
                pos = m;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "709c254c-b978-4204-8b3e-2b2fc7d11482");
                for (int row = 0; row < m; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b3327100-85cf-44c3-a917-2957fa8729e5");
                    X = r[pos];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "52ec788d-d279-452c-8d8f-4abb82b8d32e");
                    r[pos] = r[pos - 1];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7486acde-ba17-4bea-8525-459020cba4a5");
                    r[pos - 1] = X;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cb25e733-7a08-48b6-9e3d-ebe610581a9b");
                    pos += nvars - row - 2;
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f581ad8e-f614-453d-b0c3-89791627a9c1");
            m1 = vorder[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cecd5c3f-6a53-4268-bf41-fdfa43f6f1cd");
            vorder[m] = vorder[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "637f047a-8020-49dc-9cc8-a13f4a14643b");
            vorder[mp1] = m1;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cd628bfc-f85b-49e7-869a-71171fc0820c");
            X = tol[m];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "18156289-7d28-45d4-a0c1-ade83d177afc");
            tol[m] = tol[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bc3463f6-ee80-4667-b821-5a0919838795");
            tol[mp1] = X;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5a0151d6-66b1-4011-85f4-62b6559dc63f");
            rss[m] = rss[mp1] + d[mp1] * rhs[mp1] * rhs[mp1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "cfe1b2d6-3cc0-4fc0-a5c4-cd0d3ba62712");
            m += inc;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fe30464b-dc71-4542-8a18-b0f6b6919ae8");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6c3c7c1f-e931-4ba5-9638-2333fa1176b3");
        int next;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e8585dad-48ba-4365-91b7-8f9a916027e7");
        int i;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8422d7bc-8865-492e-a0ba-cabeb61351dd");
        int l;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "70572099-bb39-4c80-aa49-18b7e7c312c6");
        if (list.length < 1 || list.length > nvars + 1 - pos1) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f26ffb23-c565-412e-9a15-d3a25f299382");
            return -1;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9f905f14-a8f7-40f2-a42c-4e2424efc963");
        next = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "88951db1-f547-4f83-beeb-f9b091046bc6");
        i = pos1;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "72c8292e-e5e7-438d-bc69-12c67be2dcd2");
        while (i < nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "176db0b6-0501-43b6-961f-90b01958ed53");
            l = vorder[i];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "74f816ee-44a0-4eb6-beab-5b8bfbcd15a2");
            for (int j = 0; j < list.length; j++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ed92c64f-a9f7-43e8-9cc1-66da784b0f07");
                if (l == list[j] && i > next) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "db45c672-2353-4012-8284-9af4b98588a5");
                    this.vmove(i, next);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "92c6a357-5092-47f4-843a-7e064dd6c1ff");
                    ++next;
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "675c22b4-418d-4a38-9b84-4fdf84cc3e10");
                    if (next >= list.length + pos1) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "dc7ecdf3-2fa1-438a-aa0f-bcbdaf482cc7");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "337a9ec0-6ccb-42c6-abdc-3cbb2a2d050e");
                        break;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0fc3acb1-fcc2-4c91-8c2e-5d8f34c7e9c1");
            ++i;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "58927c72-df13-4a56-aab5-0381318c7bf0");
        return 0;
    }

    /**
     * Gets the diagonal of the Hat matrix also known as the leverage matrix.
     *
     * @param  row_data returns the diagonal of the hat matrix for this observation
     * @return the diagonal element of the hatmatrix
     */
    public double getDiagonalOfHatMatrix(double[] row_data) {
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b6cd8d03-c6a6-4df4-ba2a-664cdc367268");
        double[] wk = new double[this.nvars];
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "73c791e3-f2e0-43e0-b98c-7408c5141ff2");
        int pos;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "79d5a72b-ee4c-4977-b454-a24be5f431c1");
        double total;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "50df5082-0256-4691-8b64-3f563a030c16");
        if (row_data.length > nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f2e546aa-8639-4ab2-8c1c-07654929884e");
            return Double.NaN;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1cb84ec4-c9ba-4081-8a19-528b3e958c59");
        double[] xrow;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7dbdd7e3-a60e-425a-803b-add290691921");
        if (this.hasIntercept) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "29b36bc7-9286-4bb4-9cf9-fb50f866e805");
            xrow = new double[row_data.length + 1];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8ef3441e-8b9a-44d0-bc0b-322aa430fd60");
            xrow[0] = 1.0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ad7b060d-2c85-4a0f-b42e-bf86bcf09fa2");
            System.arraycopy(row_data, 0, xrow, 1, row_data.length);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "31646e0a-3329-4cef-ab6b-80d3a4a59bd6");
            xrow = row_data;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "16ac749c-d247-4693-b688-f6e7a4bfd36a");
        double hii = 0.0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3b1705eb-c134-4dfe-bd97-89a49c123f78");
        for (int col = 0; col < xrow.length; col++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8fb00f95-2f67-4815-829c-7e26aeb7a939");
            if (FastMath.sqrt(d[col]) < tol[col]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fb913df6-3090-45a8-91a6-ed3a155b02a9");
                wk[col] = 0.0;
            } else {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8843b6e0-19f8-431e-998c-a7ccc963bfaa");
                pos = col - 1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b092e479-1625-41f9-97fc-c19a1dc9b747");
                total = xrow[col];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "99108825-6a96-4dc7-93cd-fe8e4cc19ec4");
                for (int row = 0; row < col; row++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d50daacf-7754-4667-8c05-8acab86e0ff0");
                    total = smartAdd(total, -wk[row] * r[pos]);
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a2dc1536-954f-4538-a248-8561404e056e");
                    pos += nvars - row - 2;
                }
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "051b6450-1998-49e3-a867-b3e3d852a37c");
                wk[col] = total;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "12d61687-44d8-4efd-adfa-553610aaef84");
                hii = smartAdd(hii, (total * total) / d[col]);
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c68ba0c3-74f8-4602-a246-36a3bd53dd5e");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "661a3ceb-ab66-4397-bce7-25c8ad4009b5");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fc8c7002-c170-4299-844b-8e1c55bf168e");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3d7fe4b4-5585-44bc-8aac-1a75169ac8fd");
        if (this.nobs <= numberOfRegressors) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7a1397ff-7318-4b5d-8684-0b5ba1f2dc38");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, numberOfRegressors);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5c094c1b-3bd0-4b55-a533-7542bfe9de80");
        if (numberOfRegressors > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "c5b17c13-9e6e-413f-b08e-605581c6ff80");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, numberOfRegressors, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "af56d452-1c0e-4668-a63b-1abb2617d330");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ef53630d-4afd-4bde-bf3c-d1c0c0a60e27");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "04b53a08-55e0-4577-b892-c9a878c44a37");
        double[] beta = this.regcf(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8f72a4db-6b5b-431c-921a-93c88feafe5f");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "92d5e281-f6a6-4b51-ad19-e954709e7f91");
        double[] cov = this.cov(numberOfRegressors);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a741ccab-81dc-4bbc-accb-3c615cbcb282");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b58a8954-e5d4-45b3-b3f6-5da1fb0bad6a");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6aee8eb1-6ccf-492d-9a8d-29391dfbc910");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0b98efa2-40ef-486d-822d-4092ae7a68ba");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6aad2c07-3884-4959-84d9-1bf8cbe2e72f");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "25954864-05f7-4bbe-9e82-db595b3d1807");
        for (int i = 0; i < numberOfRegressors; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ce174acd-4b87-42e9-b319-1ff502b1c0a1");
            if (this.vorder[i] != i) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "48144be8-08cc-4970-9d05-d85911d05d22");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "92894d16-8838-42fc-926f-686ab3aaaabd");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6cbee6b2-e86b-4246-9867-24ccaf315153");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f66ae55b-ceea-4a3f-806f-7455d1f84411");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f07a1828-e2f5-44ed-8107-54ac31e13d4a");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5997d7e9-0ba6-478f-aebe-9a2cfc9fd255");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1c089635-7719-4487-a6d1-a847b91dce77");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "616ebd09-d6fb-4759-8f22-3b5874994881");
            for (int i = 0; i < nvars; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ee084c37-a10b-4934-8343-b5795c18802a");
                for (int j = 0; j < numberOfRegressors; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5b864418-a5b7-4757-8463-c1179bf151c3");
                    if (this.vorder[j] == i) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "72dc5850-383b-47fa-844a-26aaa37254f0");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7760295f-cf0d-424b-94d1-ee0245d8601c");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "9916c714-e777-4787-aa05-14b42c17e743");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "49a9bd08-1ba2-46be-847f-e832a742dac0");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1d451556-975e-4180-ad67-b9ce30e11f60");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "60972a68-6fa7-4a58-829b-bc1f01fc77d1");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "09b46c98-9f22-434e-a03f-5b7bc0c0fc7e");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "552e8f03-1697-437a-8e3b-2b0922091916");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "24d1f009-e70f-4ffd-8a36-b647a8e6ae01");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d4561f07-adeb-4541-863f-3c159e5b485e");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5c977681-f5a7-476f-9045-7b2bdca77bce");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "8d325e88-6d6a-447e-bd7c-c22bbd4222be");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b5e125c0-7f0a-4575-ad8f-a6a7aee812df");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "98a48988-5569-4238-b78b-82ba96c01004");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "183a644a-08f9-4127-99f2-52c4e21a6f9e");
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
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "763f41c4-57dc-4204-b9c1-223ab43e4e81");
        if (variablesToInclude.length > this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b8d15a55-4901-4fc6-9473-8ab008f91739");
            throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, variablesToInclude.length, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "610e62fe-14c3-4d5e-b152-c353f5db0917");
        if (this.nobs <= this.nvars) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "93433a42-2565-4b8c-8804-9d09232812ca");
            throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, this.nobs, this.nvars);
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a0aa3d75-1633-4e1b-9a27-e09fd103c62c");
        Arrays.sort(variablesToInclude);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "af23f781-df7c-4f03-953f-36f36c15a4ca");
        int iExclude = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "3799f605-9f60-4f0a-9bb1-0d8a4584cd7d");
        for (int i = 0; i < variablesToInclude.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "eba13b9a-2b91-4d22-99eb-8f11a57dca97");
            if (i >= this.nvars) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "ab12e50a-98df-469b-8546-549d195e1eb8");
                throw new ModelSpecificationException(LocalizedFormats.INDEX_LARGER_THAN_MAX, i, this.nvars);
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "57299b34-b5d2-4075-9d3e-2560b25b31a5");
            if (i > 0 && variablesToInclude[i] == variablesToInclude[i - 1]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "940545a4-23cc-4a44-90b1-c7a125c070a0");
                variablesToInclude[i] = -1;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7f2268b8-d928-4633-86c4-0e7a54f19075");
                ++iExclude;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7a816b05-062b-4794-b049-bec37cd28f62");
        int[] series;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f1bb1419-eefc-40f9-a5fa-cb08be41aefe");
        if (iExclude > 0) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "92cd04d5-b848-4a65-977a-610df05f0f61");
            int j = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "12453b2d-66c4-440c-a75b-b27351b3bdc3");
            series = new int[variablesToInclude.length - iExclude];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e587e3a2-e394-4226-a5ee-3d998181a94e");
            for (int i = 0; i < variablesToInclude.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e14f0ffa-5a3e-4e14-9c44-4314f2519487");
                if (variablesToInclude[i] > -1) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d8bea24d-f47d-4faf-9209-6eb299c37eb0");
                    series[j] = variablesToInclude[i];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4cc2b388-ddcc-4ebf-b454-1fa57da513c4");
                    ++j;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bde85d63-6855-4754-9a2a-ac0a1948040a");
            series = variablesToInclude;
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5c459877-7e37-4463-8ba5-3c30c0f76f50");
        reorderRegressors(series, 0);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6d7b814b-277e-4d05-8d4e-efcd6ca7c214");
        tolset();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "45dc14a9-2f2b-4570-884e-8af972ff4731");
        singcheck();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "677a7448-5930-4b92-9bbf-a4f5403c6882");
        double[] beta = this.regcf(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4760b01f-a332-401e-85f5-5be2f5f844eb");
        ss();
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "4f46a2c0-30fd-4be6-aee5-d4bf3e305c81");
        double[] cov = this.cov(series.length);
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5ae5f977-4f6d-490c-bdd2-78c60cf92f67");
        int rnk = 0;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "12de5238-c0ae-4a1c-b5a4-f60aebc65763");
        for (int i = 0; i < this.lindep.length; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "245ff156-a5bf-49aa-84a5-c59f34df0af6");
            if (!this.lindep[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "a412cf08-a76a-43dc-8b2d-86247c19a092");
                ++rnk;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1f171c71-3b4e-49a3-9c2e-a00e16093bc7");
        boolean needsReorder = false;
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "53256462-0c25-4283-a2dc-d9c8af037d85");
        for (int i = 0; i < this.nvars; i++) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "1ceaa924-85bb-45ef-a26a-cc01fb9ac821");
            if (this.vorder[i] != series[i]) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5868de63-ff74-4f97-b357-a9fe360230c4");
                needsReorder = true;
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "0e43d2a5-639c-4f20-884c-f41673925ca9");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "7225ad01-5a99-486c-a88c-d1225d3fbd78");
        if (!needsReorder) {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "6f7f11a7-021c-4176-a83e-253beccc44f7");
            return new RegressionResults(beta, new double[][] { cov }, true, this.nobs, rnk, this.sumy, this.sumsqy, this.sserr, this.hasIntercept, false);
        } else {
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b02eb1a9-6b1c-40ff-adfd-bd30a0f01085");
            double[] betaNew = new double[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "2ed9e8c6-6cc1-4922-8fd0-cc14d84ec2ba");
            int[] newIndices = new int[beta.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "61d42252-6ca4-4863-b229-1d553c70ccde");
            for (int i = 0; i < series.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "fda047fe-6dec-48bb-a69a-c18f1b490bf9");
                for (int j = 0; j < this.vorder.length; j++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b830e5f3-8972-4829-9e51-62e283dc7e4a");
                    if (this.vorder[j] == series[i]) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "59ad0cdf-6c22-493c-8731-9ed7783ff9f4");
                        betaNew[i] = beta[j];
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "79036e51-283f-4d23-b226-327eefafb93e");
                        newIndices[i] = j;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b6190e32-9bc1-479f-bd62-d2de3779b278");
            double[] covNew = new double[cov.length];
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "bfeda7d0-cab6-4765-9342-adbb78656535");
            int idx1 = 0;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e25b5280-9f44-4159-a8c7-fe27dd446fba");
            int idx2;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d38723eb-a734-47d6-a677-a02da6c31d78");
            int _i;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5f9f6845-81d0-4f38-b51a-1538f6ab179c");
            int _j;
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "d620ad26-822b-4790-9f86-a31e485097e9");
            for (int i = 0; i < beta.length; i++) {
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "5de1295e-b22b-4206-aab1-9846d64566f2");
                _i = newIndices[i];
                writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "f9e559c7-965b-4b2d-a0f0-8d6d7cba29a9");
                for (int j = 0; j <= i; j++, idx1++) {
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "86ead5ef-ce83-4717-8168-e92f1d0806b5");
                    _j = newIndices[j];
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "033f588d-c4c2-4085-b17c-20b330aff681");
                    if (_i > _j) {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "482a35da-ef6f-45ce-b3f4-9d5035cdd45e");
                        idx2 = _i * (_i + 1) / 2 + _j;
                    } else {
                        writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "e2c21739-546f-44a4-b9ab-48183a71c401");
                        idx2 = _j * (_j + 1) / 2 + _i;
                    }
                    writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "b67a92c4-c525-4660-a6bf-ed2618118f07");
                    covNew[idx1] = cov[idx2];
                }
            }
            writeline("/home/ubuntu/results/coverage/MillerUpdatingRegression/MillerUpdatingRegression_4_10.coverage", "09744ac9-59d1-4438-a7b8-e2c85d09cc4d");
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
