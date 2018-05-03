#ifndef CASADI_INTERPOLANT_HPP
#define CASADI_INTERPOLANT_HPP

#include <string>
#include <vector>
#include <inttypes.h>

#define casadi_int int64_t
#define casadi_uint uint64_t

class Interpolant {
public:
  Interpolant(const std::vector<double>& grid,
              const std::vector<casadi_int>& offset,
              const std::vector<double>& values,
              const casadi_int m,
              const std::string lookup_mode);

  static std::vector<casadi_int> interpret_lookup_mode(
    const std::vector<std::string>& modes,
    const std::vector<double>& knots,
    const std::vector<casadi_int>& offset,
    const std::vector<casadi_int>& margin_left=std::vector<casadi_int>(),
    const std::vector<casadi_int>& margin_right=std::vector<casadi_int>());

  void eval(const double* arg, double* res);

  // Number of dimensions
  casadi_int ndim_;

  // Number of outputs
  casadi_int m_;

  // Input grid
  std::vector<double> grid_;

  // Offset for each dimension
  std::vector<casadi_int> offset_;

  // Values at gridpoints
  std::vector<double> values_;

  // Offset for each dimension
  std::vector<casadi_int> lookup_modes_;

  // work vectors
  std::vector<casadi_int> iw_;
  std::vector<double> w_;
};

Interpolant* interpolant(const std::vector<std::vector<double> >& grid,
                         const std::vector<double>& values,
                         const std::string lookup_mode);

extern "C"
Interpolant* new_interpolant(const double * const * const grid,
                             int32_t n_dims,
                             int32_t *ngridpoints,
                             const double * const values,
                             int32_t n_values,
                             const char *lookup_mode);

extern "C"
void eval_interpolant(Interpolant *obj, const double* arg, double* res);

extern "C"
void delete_interpolant(Interpolant *obj);

#endif // CASADI_INTERPOLANT_HPP
