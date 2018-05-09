#ifndef CASADI_INTERPOLANT_HPP
#define CASADI_INTERPOLANT_HPP

#include <string>
#include <vector>
#include <inttypes.h>

#define casadi_int int64_t
#define casadi_uint uint64_t

#ifdef __cplusplus
extern "C"
#endif  // __cplusplus
int8_t new_interpolant(const double * const * const user_grid_ptr,
                       int64_t n_dims,
                       int64_t *ngridpoints,
                       int64_t n_values,
                       const char *lookup_mode,

                       double * const stacked_grid_ptr,
                       const uint64_t stacked_grid_length,
                       casadi_int * const offset_ptr,
                       const uint64_t offset_length,
                       casadi_int * const lookup_modes_ptr,
                       const uint64_t lookup_modes_length);

#endif // CASADI_INTERPOLANT_HPP
