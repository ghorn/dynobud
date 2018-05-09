#ifndef __CASADI_INTERPN_H__
#define __CASADI_INTERPN_H__

#include "stdint.h"

#ifdef __cplusplus
extern "C" {
#endif

  void int64_terpn(double* res, int64_t ndim, const double* grid, const int64_t* offset, const double* values, const double* x, const int64_t* lookup_mode, int64_t m, int64_t* iw, double* w);

#ifdef __cplusplus
}
#endif

#endif  // __CASADI_INTERPN_H__
