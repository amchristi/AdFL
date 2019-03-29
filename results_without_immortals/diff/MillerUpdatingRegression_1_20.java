MillerUpdatingRegression
MillerUpdatingRegression
~~~
hasIntercept
~~~
getN
~~~
addObservation
~
if ((!this.hasIntercept && x.length != nvars) || (this.hasIntercept && x.length + 1 != nvars)) {
    throw new ModelSpecificationException(LocalizedFormats.INVALID_REGRESSION_OBSERVATION, x.length, nvars);
}
~~~
addObservations
~
if ((x == null) || (y == null) || (x.length != y.length)) {
    throw new ModelSpecificationException(LocalizedFormats.DIMENSIONS_MISMATCH_SIMPLE, (x == null) ? 0 : x.length, (y == null) ? 0 : y.length);
}
~
if (x.length == 0) {
    throw new ModelSpecificationException(LocalizedFormats.NO_DATA);
}
~
if (x[0].length + 1 > x.length) {
    throw new ModelSpecificationException(LocalizedFormats.NOT_ENOUGH_DATA_FOR_NUMBER_OF_PREDICTORS, x.length, x[0].length);
}
~~~
include
~
this.rss_set = false;
~
for (int i = 0; i < x.length; i++) {
    if (w == 0.0) {
        return;
    }
    xi = x[i];
    if (xi == 0.0) {
        nextr += nvars - i - 1;
        continue;
    }
    di = d[i];
    wxi = w * xi;
    _w = w;
    if (di != 0.0) {
        dpi = smartAdd(di, wxi * xi);
        final double tmp = wxi * xi / di;
        if (FastMath.abs(tmp) > Precision.EPSILON) {
            w = (di * w) / dpi;
        }
    } else {
        dpi = wxi * xi;
        w = 0.0;
    }
    d[i] = dpi;
    for (int k = i + 1; k < nvars; k++) {
        xk = x[k];
        x[k] = smartAdd(xk, -xi * r[nextr]);
        if (di != 0.0) {
            r[nextr] = smartAdd(di * r[nextr], (_w * xi) * xk) / dpi;
        } else {
            r[nextr] = xk / xi;
        }
        ++nextr;
    }
    xk = y;
    y = smartAdd(xk, -xi * rhs[i]);
    if (di != 0.0) {
        rhs[i] = smartAdd(di * rhs[i], wxi * xk) / dpi;
    } else {
        rhs[i] = xk / xi;
    }
}
~~~
smartAdd
~~~
clear
~
this.vorder[i] = i;
~
Arrays.fill(this.d, 0.0);
~
Arrays.fill(this.rhs, 0.0);
~
Arrays.fill(this.r, 0.0);
~
Arrays.fill(this.tol, 0.0);
~
Arrays.fill(this.rss, 0.0);
~
Arrays.fill(this.work_tolset, 0.0);
~
Arrays.fill(this.work_sing, 0.0);
~
Arrays.fill(this.x_sing, 0.0);
~
Arrays.fill(this.lindep, false);
~
for (int i = 0; i < nvars; i++) {
    this.vorder[i] = i;
}
~
this.nobs = 0;
~
this.sserr = 0.0;
~
this.sumy = 0.0;
~
this.sumsqy = 0.0;
~
this.rss_set = false;
~
this.tol_set = false;
~~~
tolset
~
tol[0] = eps * this.work_tolset[0];
~
for (int col = 1; col < nvars; col++) {
    pos = col - 1;
    total = work_tolset[col];
    for (int row = 0; row < col; row++) {
        total += FastMath.abs(r[pos]) * work_tolset[row];
        pos += nvars - row - 2;
    }
    tol[col] = eps * total;
}
~
tol_set = true;
~~~
regcf
~
if (nreq < 1) {
    throw new ModelSpecificationException(LocalizedFormats.NO_REGRESSORS);
}
~
if (nreq > this.nvars) {
    throw new ModelSpecificationException(LocalizedFormats.TOO_MANY_REGRESSORS, nreq, this.nvars);
}
~
if (!this.tol_set) {
    tolset();
}
~
boolean rankProblem = false;
~
for (int i = nreq - 1; i > -1; i--) {
    if (FastMath.sqrt(d[i]) < tol[i]) {
        ret[i] = 0.0;
        d[i] = 0.0;
        rankProblem = true;
    } else {
        ret[i] = rhs[i];
        nextr = i * (nvars + nvars - i - 1) / 2;
        for (int j = i + 1; j < nreq; j++) {
            ret[i] = smartAdd(ret[i], -r[nextr] * ret[j]);
            ++nextr;
        }
    }
}
~
if (rankProblem) {
    for (int i = 0; i < nreq; i++) {
        if (this.lindep[i]) {
            ret[i] = Double.NaN;
        }
    }
}
~~~
singcheck
~
pos = col - 1;
~
for (int row = 0; row < col - 1; row++) {
    if (FastMath.abs(r[pos]) * work_sing[row] < temp) {
        r[pos] = 0.0;
    }
    pos += nvars - row - 2;
}
~
lindep[col] = false;
~
if (work_sing[col] < temp) {
    lindep[col] = true;
    if (col < nvars - 1) {
        Arrays.fill(x_sing, 0.0);
        int _pi = col * (nvars + nvars - col - 1) / 2;
        for (int _xi = col + 1; _xi < nvars; _xi++, _pi++) {
            x_sing[_xi] = r[_pi];
            r[_pi] = 0.0;
        }
        final double y = rhs[col];
        final double weight = d[col];
        d[col] = 0.0;
        rhs[col] = 0.0;
        this.include(x_sing, weight, y);
    } else {
        sserr += d[col] * rhs[col] * rhs[col];
    }
}
~~~
ss
~
rss_set = true;
~~~
cov
~
if (this.nobs <= nreq) {
    return null;
}
~
Arrays.fill(covmat, Double.NaN);
~
for (int row = 0; row < nreq; row++) {
    pos2 = start;
    if (!this.lindep[row]) {
        for (int col = row; col < nreq; col++) {
            if (!this.lindep[col]) {
                pos1 = start + col - row;
                if (row == col) {
                    total = 1.0 / d[col];
                } else {
                    total = rinv[pos1 - 1] / d[col];
                }
                for (int k = col + 1; k < nreq; k++) {
                    if (!this.lindep[k]) {
                        total += rinv[pos1] * rinv[pos2] / d[k];
                    }
                    ++pos1;
                    ++pos2;
                }
                covmat[(col + 1) * col / 2 + row] = total * var;
            } else {
                pos2 += nreq - col - 1;
            }
        }
    }
    start += nreq - row - 1;
}
~~~
inverse
~
Arrays.fill(rinv, Double.NaN);
~~~
getPartialCorrelations
~
int pos;
~
int pos1;
~
int pos2;
~
final int rms_off = -in;
~
final int wrk_off = -(in + 1);
~
final double[] rms = new double[nvars - in];
~
final double[] work = new double[nvars - in - 1];
~
double sumxx;
~
double sumxy;
~
double sumyy;
~
final int offXX = (nvars - in) * (nvars - in - 1) / 2;
~
if (in < -1 || in >= nvars) {
    return null;
}
~
final int nvm = nvars - 1;
~
final int base_pos = r.length - (nvm - in) * (nvm - in + 1) / 2;
~
if (d[in] > 0.0) {
    rms[in + rms_off] = 1.0 / FastMath.sqrt(d[in]);
}
~
for (int col = in + 1; col < nvars; col++) {
    pos = base_pos + col - 1 - in;
    sumxx = d[col];
    for (int row = in; row < col; row++) {
        sumxx += d[row] * r[pos] * r[pos];
        pos += nvars - row - 2;
    }
    if (sumxx > 0.0) {
        rms[col + rms_off] = 1.0 / FastMath.sqrt(sumxx);
    } else {
        rms[col + rms_off] = 0.0;
    }
}
~
sumyy = sserr;
~
for (int row = in; row < nvars; row++) {
    sumyy += d[row] * rhs[row] * rhs[row];
}
~
if (sumyy > 0.0) {
    sumyy = 1.0 / FastMath.sqrt(sumyy);
}
~
pos = 0;
~
for (int col1 = in; col1 < nvars; col1++) {
    sumxy = 0.0;
    Arrays.fill(work, 0.0);
    pos1 = base_pos + col1 - in - 1;
    for (int row = in; row < col1; row++) {
        pos2 = pos1 + 1;
        for (int col2 = col1 + 1; col2 < nvars; col2++) {
            work[col2 + wrk_off] += d[row] * r[pos1] * r[pos2];
            pos2++;
        }
        sumxy += d[row] * r[pos1] * rhs[row];
        pos1 += nvars - row - 2;
    }
    pos2 = pos1 + 1;
    for (int col2 = col1 + 1; col2 < nvars; col2++) {
        work[col2 + wrk_off] += d[col1] * r[pos2];
        ++pos2;
        output[(col2 - 1 - in) * (col2 - in) / 2 + col1 - in] = work[col2 + wrk_off] * rms[col1 + rms_off] * rms[col2 + rms_off];
        ++pos;
    }
    sumxy += d[col1] * rhs[col1];
    output[col1 + rms_off + offXX] = sumxy * rms[col1 + rms_off] * sumyy;
}
~~~
vmove
~~~
reorderRegressors
~~~
getDiagonalOfHatMatrix
~
if (row_data.length > nvars) {
    return Double.NaN;
}
~
for (int col = 0; col < xrow.length; col++) {
    if (FastMath.sqrt(d[col]) < tol[col]) {
        wk[col] = 0.0;
    } else {
        pos = col - 1;
        total = xrow[col];
        for (int row = 0; row < col; row++) {
            total = smartAdd(total, -wk[row] * r[pos]);
            pos += nvars - row - 2;
        }
        wk[col] = total;
        hii = smartAdd(hii, (total * total) / d[col]);
    }
}
~~~
getOrderOfRegressors
~~~
regress
~~~
regress
~~~
regress
