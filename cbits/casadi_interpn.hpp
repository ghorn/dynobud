
// NOLINT(legal/copyright)
// SYMBOL "interpn"
//template<typename double>
#ifdef __cplusplus
extern "C"
#endif  // __cplusplus
void casadi_interpn(double* res, casadi_int ndim, const double* grid, const casadi_int* offset, const double* values, const double* x, const casadi_int* lookup_mode, casadi_int m, casadi_int* iw, double* w) { // NOLINT(whitespace/line_length)
  // Work vectors
  double* alpha;
  casadi_int *index, *corner;
  alpha = w; w += ndim;
  index = iw; iw += ndim;
  corner = iw; iw += ndim;
  // Left index and fraction of interval
  casadi_interpn_weights(ndim, grid, offset, x, alpha, index, lookup_mode);
  // Loop over all corners, add contribution to output
  casadi_fill_casadi_int(corner, ndim, 0);
  casadi_fill(res, m, 0.0);
  do {
    double* coeff = 0;
    casadi_interpn_interpolate(res, ndim, offset, values,
      alpha, index, corner, coeff, m);
  } while (casadi_flip(corner, ndim));
}
