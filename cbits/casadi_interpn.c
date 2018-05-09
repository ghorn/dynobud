#include "casadi_interpn.h"

static void casadi_fill(double* x, int64_t n, double alpha) {
  int64_t i;
  if (x) {
    for (i=0; i<n; ++i) *x++ = alpha;
  }
}

static void casadi_fill_int64_t(int64_t* x, int64_t n, int64_t alpha) {
  int64_t i;
  if (x) {
    for (i=0; i<n; ++i) *x++ = alpha;
  }
}

static inline
int64_t casadi_flip(int64_t* corner, int64_t ndim) {
  int64_t i;
  for (i=0; i<ndim; ++i) {
    if (corner[i]) {
      corner[i]=0;
    } else {
      corner[i]=1;
      return 1;
    }
  }
  return 0;
}

static int64_t casadi_low(double x, const double* grid, int64_t ng, int64_t lookup_mode) {
  switch (lookup_mode) {
    case 1: // exact
      {
        double g0, dg;
        int64_t ret;
        g0 = grid[0];
        dg = grid[ng-1]-g0;
        ret = (int64_t) ((x-g0)*(ng-1)/dg); // NOLINT(readability/casting)
        if (ret<0) ret=0;
        if (ret>ng-2) ret=ng-2;
        return ret;
      }
    case 2: // binary
      {
        int64_t start, stop, pivot;
        // Quick return
        if (ng<2 || x<grid[1]) return 0;
        if (x>grid[ng-1]) return ng-2;

        start = 0;
        stop  = ng-1;
        while (1) {
          pivot = (stop+start)/2;
          if (x < grid[pivot]) {
            if (pivot==stop) return pivot;
            stop = pivot;
          } else {
            if (pivot==start) return pivot;
            start = pivot;
          }
        }
      }
    default: // linear
      {
        int64_t i;
        for (i=0; i<ng-2; ++i) {
          if (x < grid[i+1]) break;
        }
        return i;
      }
  }
}

static void casadi_interpn_weights(int64_t ndim, const double* grid, const int64_t* offset, const double* x, double* alpha, int64_t* index, const int64_t* lookup_mode) { // NOLINT(whitespace/line_length)
  // Left index and fraction of interval
  int64_t i;
  for (i=0; i<ndim; ++i) {
    int64_t ng, j;
    double xi;
    const double* g;
    // Grid point
    xi = x ? x[i] : 0;
    // Grid
    g = grid + offset[i];
    ng = offset[i+1]-offset[i];
    // Find left index
    j = index[i] = casadi_low(xi, g, ng, lookup_mode[i]);
    // Get interpolation/extrapolation alpha
    alpha[i] = (xi-g[j])/(g[j+1]-g[j]);
  }
}

void casadi_interpn_interpolate(double* res, int64_t ndim, const int64_t* offset, const double* values, const double* alpha, const int64_t* index, const int64_t* corner, double* coeff, int64_t m) { // NOLINT(whitespace/line_length)
  double c;
  int64_t ld, i;
  // Get weight and value for corner
  c=1;
  ld=1; // leading dimension
  for (i=0; i<ndim; ++i) {
    if (coeff) *coeff++ = c;
    if (corner[i]) {
      c *= alpha[i];
    } else {
      c *= 1-alpha[i];
    }
    values += (index[i]+corner[i])*ld*m;
    ld *= offset[i+1]-offset[i];
  }
  if (coeff) {
    for (i=0;i<m;++i) res[i] += values[i];
  } else {
    for (i=0;i<m;++i) res[i] += c*values[i];
  }
}

void casadi_interpn(double* res, int64_t ndim, const double* grid, const int64_t* offset, const double* values, const double* x, const int64_t* lookup_mode, int64_t m, int64_t* iw, double* w) { // NOLINT(whitespace/line_length)
  // Work vectors
  double* alpha;
  int64_t *index, *corner;
  alpha = w; w += ndim;
  index = iw; iw += ndim;
  corner = iw; iw += ndim;
  // Left index and fraction of interval
  casadi_interpn_weights(ndim, grid, offset, x, alpha, index, lookup_mode);
  // Loop over all corners, add contribution to output
  casadi_fill_int64_t(corner, ndim, 0);
  casadi_fill(res, m, 0.0);
  do {
    double* coeff = 0;
    casadi_interpn_interpolate(res, ndim, offset, values,
      alpha, index, corner, coeff, m);
  } while (casadi_flip(corner, ndim));
}
