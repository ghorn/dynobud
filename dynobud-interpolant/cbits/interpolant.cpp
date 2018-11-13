#include "interpolant.hpp"

#include "casadi_interpn.h"

#include <iostream>
#include <limits>
#include <cmath>


#define casadi_error(message) {                                   \
  std::cerr << "File " << __FILE__                       \
            << ", line " << __LINE__ << ":" << std::endl \
            << message << std::endl;                     \
  std::exit(EXIT_FAILURE); }

#define casadi_assert_dev(condition)              \
  if (!(condition)) { \
    std::cerr << "Assertion `" #condition "` failed in " << __FILE__  \
              << " line " << __LINE__ << ":" << std::endl;            \
    std::exit(EXIT_FAILURE);                                          \
  }


const double inf = std::numeric_limits<double>::infinity();

template <typename T>
int copy_vector(const std::vector<T> vec, T * const array, const uint64_t array_length) {
  if (vec.size() != array_length) {
    return 1;
  } else {
    for (uint64_t k = 0; k < array_length; k++) {
      array[k] = vec[k];
    }
    return 0;
  }
}


template<typename T>
bool is_increasing(const std::vector<T> &v) {
  if (v.size()==0) return true;
  T el = v[0];
  for (casadi_uint i = 1; i < v.size(); ++i) {
    if (!(v[i] > el)) return false;
    el = v[i];
  }
  return el==el; // nan -> false
}

bool is_equally_spaced(const std::vector<double>& v) {
  // Quick return if 2 or less entries
  if (v.size()<=2) return true;
  // Permitted error margin
  // NOTE(@jaeandersson) 1e-14 good idea?
  double margin = (v.back()-v.front())*1e-14;
  // Make sure spacing is consistent throughout
  double spacing = v[1]-v[0];
  for (size_t i=2; i<v.size(); ++i) {
    if (fabs(v[i]-v[i-1]-spacing)>margin) return false;
  }
  // Equal if reached this point
  return true;
}

static std::vector<casadi_int> interpret_lookup_mode(
  const std::vector<std::string>& modes,
  const std::vector<double>& knots,
  const std::vector<casadi_int>& offset,
  const std::vector<casadi_int>& margin_left=std::vector<casadi_int>(),
  const std::vector<casadi_int>& margin_right=std::vector<casadi_int>());


static std::vector<casadi_int> interpret_lookup_mode(
    const std::vector<std::string>& modes,
    const std::vector<double>& knots,
    const std::vector<casadi_int>& offset,
    const std::vector<casadi_int>& margin_left,
    const std::vector<casadi_int>& margin_right) {

  // Default lookup mode linear
  std::vector<casadi_int> ret(offset.size()-1, 0);

  for (casadi_uint i = 0; i < ret.size(); ++i) {
    // If more than 100 knots -> default is binary search
    if (offset[i+1]-offset[i]>100) ret[i] = 2;
  }

  if (modes.empty()) return ret;

  casadi_assert_dev(modes.size()==offset.size()-1);
  for (casadi_uint i = 0; i < offset.size() - 1; ++i) {
    if (modes[i]=="linear") {
      ret[i] = 0;
    } else if (modes[i]=="exact") {
      ret[i] = 1;
      casadi_int m_left  = margin_left.empty() ? 0 : margin_left[i];
      casadi_int m_right = margin_right.empty() ? 0 : margin_right[i];

      std::vector<double> grid(
          knots.begin()+offset[i]+m_left,
          knots.begin()+offset[i+1]-m_right);
      casadi_assert_dev(is_increasing(grid) && is_equally_spaced(grid));
    } else if (modes[i]=="binary") {
      ret[i] = 2;
    } else {
      casadi_error("Unknown lookup_mode option '" + modes[i] + ". "
                   "Allowed values: linear, binary, exact.");
    }
  }
  return ret;
}

std::vector<std::vector<double> > to_user_grid (const double * const * const user_grid_ptr,
                                                int64_t n_dims,
                                                int64_t *ngridpoints) {
  std::vector<std::vector<double> > user_grid;
  for (int k = 0; k < n_dims; k++) {
    std::vector<double> x(ngridpoints[k]);
    for (int j= 0; j < ngridpoints[k]; j++) {
      x[j] = user_grid_ptr[k][j];
    }
    user_grid.push_back(x);
  }

  return user_grid;
}



// C interface
extern "C"
int8_t new_interpolant(const double * const * const user_grid_ptr,
                       int64_t n_dims,
                       int64_t *ngridpoints,
                       int64_t n_values,
                       const char *lookup_mode_ptr,

                       double * const stacked_grid_ptr,
                       const uint64_t stacked_grid_length,
                       casadi_int * const offset_ptr,
                       const uint64_t offset_length,
                       casadi_int * const lookup_modes_ptr,
                       const uint64_t lookup_modes_length)
{
  const std::vector<std::vector<double> > user_grid = to_user_grid(user_grid_ptr, n_dims, ngridpoints);

  // Dimension at least 1
  if (user_grid.size() == 0) {
    printf("At least one input required\n");
    return 1;
  }

  // Consistency check, number of elements
  casadi_uint nel=1;
  for (auto&& g : user_grid) {
    if (g.size() < 2) {
      printf("Need at least two grid points for every input\n");
      return 1;
    }
    nel *= g.size();
  }
  if (n_values % nel != 0) {
    printf("Inconsistent number of elements\n");
    return 1;
  }
  if (n_values == 0) {
    printf("Values cannot be empty\n");
    return 1;
  }

  // Grid must be strictly increasing
  for (auto&& g : user_grid) {
    double last = -inf;
    for (auto&& e : g) {
      if (std::isinf(e) || e <= last) {
        printf("Gridpoints must be finite and strictly increasing\n");
        return 1;
      }
      last = e;
    }
  }

  // Get offset for each input dimension
  std::vector<casadi_int> offset;
  offset.reserve(user_grid.size()+1);
  offset.push_back(0);
  for (auto&& g : user_grid) offset.push_back(offset.back()+g.size());

  // Stack input grids
  std::vector<double> stacked_grid;
  stacked_grid.reserve(offset.back());
  for (auto&& g : user_grid) stacked_grid.insert(stacked_grid.end(), g.begin(), g.end());

  // lookup modes
  std::vector<std::string> lookup_modes_str(user_grid.size(), std::string(lookup_mode_ptr));
  std::vector<casadi_int> lookup_modes_int = interpret_lookup_mode(lookup_modes_str, stacked_grid, offset);

  // set outputs
  if (copy_vector<double>(stacked_grid, stacked_grid_ptr, stacked_grid_length)) {
    printf("wrong size stacked_grid, expected %zu, actual %" PRIu64 "\n", stacked_grid.size(), stacked_grid_length);
    return 1;
  }

  if (copy_vector<int64_t>(offset, offset_ptr, offset_length)) {
    printf("wrong size offset, expected %zu, actual %" PRIu64 "\n", offset.size(), offset_length);
    return 1;
  }

  if (copy_vector<int64_t>(lookup_modes_int, lookup_modes_ptr, lookup_modes_length)) {
    printf("wrong size lookup_modes, expected %zu, actual %" PRIu64 "\n", lookup_modes_int.size(), lookup_modes_length);
    return 1;
  }

  return 0;
}
