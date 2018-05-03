#include "interpolant.hpp"

#include "casadi_fill.hpp"
inline void casadi_fill_casadi_int(casadi_int* x, casadi_int n, casadi_int alpha) {
  casadi_fill(x, n, alpha);
}
#include "casadi_flip.hpp"
#include "casadi_low.hpp"
#include "casadi_interpn_weights.hpp"
#include "casadi_interpn_interpolate.hpp"
#include "casadi_interpn.hpp"

#include <iostream>
#include <limits>
#include <cmath>


#define casadi_assert(condition, message)       \
  if (!(condition)) { \
    std::cerr << message << std::endl                                 \
              << "Assertion `" #condition "` failed in " << __FILE__  \
              << " line " << __LINE__ << std::endl;                   \
    std::exit(EXIT_FAILURE);                                          \
  }

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

template<typename T>
T* get_ptr(std::vector<T> &v) {
  if (v.empty())
    return nullptr;
  else
    return &v.front();
}

template<typename T>
const T* get_ptr(const std::vector<T> &v) {
  if (v.empty())
    return nullptr;
  else
    return &v.front();
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


bool is_equally_spaced(const std::vector<double> &v) {
  if (v.size()<=1) return true;

  double margin = (v[v.size()-1]-v[0])*1e-14;

  for (casadi_uint i=2;i<v.size();++i) {
    double ref = v[0]+(static_cast<double>(i)*(v[v.size()-1]-v[0]))/
      static_cast<double>(v.size()-1);
    if (abs(ref-v[i])>margin) return false;
  }
  return true;
}


void Interpolant::eval(const double* arg, double* res) {
  casadi_interpn(res, ndim_, get_ptr(grid_), get_ptr(offset_),
                 get_ptr(values_), arg, get_ptr(lookup_modes_),
                 m_,
                 get_ptr(iw_),
                 get_ptr(w_));
}


Interpolant* interpolant(const std::vector<std::vector<double> >& grid,
                         const std::vector<double>& values,
                         std::string lookup_mode) {

  // Dimension at least 1
  casadi_assert(grid.size()>0, "At least one input required");

  // Consistency check, number of elements
  casadi_uint nel=1;
  for (auto&& g : grid) {
    casadi_assert(g.size()>=2, "Need at least two grid points for every input");
    nel *= g.size();
  }
  casadi_assert(values.size() % nel== 0, "Inconsistent number of elements");
  casadi_assert(values.size()>0, "Values cannot be empty");

  // Grid must be strictly increasing
  for (auto&& g : grid) {
    double last = -inf;
    for (auto&& e : g) {
      casadi_assert(!std::isinf(e) && e>last,
        "Gridpoints must be finite and strictly increasing");
      last = e;
    }
  }

  // Get offset for each input dimension
  std::vector<casadi_int> offset;
  offset.reserve(grid.size()+1);
  offset.push_back(0);
  for (auto&& g : grid) offset.push_back(offset.back()+g.size());

  // Stack input grids
  std::vector<double> stacked;
  stacked.reserve(offset.back());
  for (auto&& g : grid) stacked.insert(stacked.end(), g.begin(), g.end());
  casadi_int m = values.size()/nel;

  // return interpoland
  return new Interpolant(stacked, offset, values, m, lookup_mode);
}

Interpolant::
Interpolant(const std::vector<double>& grid,
            const std::vector<casadi_int>& offset,
            const std::vector<double>& values,
            const casadi_int m,
            const std::string lookup_mode)
  : m_(m), grid_(grid), offset_(offset),  values_(values) {
  // Number of grid points
  ndim_ = offset_.size()-1;

  // lookup modes
  std::vector<std::string> lookup_modes(ndim_, lookup_mode);
  lookup_modes_ = Interpolant::interpret_lookup_mode(lookup_modes, grid_, offset_);

  // work vectors
  iw_.resize(2 * ndim_);
  w_.resize(ndim_);
}


std::vector<casadi_int> Interpolant::interpret_lookup_mode(
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


// C interface
extern "C"
Interpolant* new_interpolant(const double * const * const grid,
                             int32_t n_dims,
                             int32_t *ngridpoints,
                             const double * const values,
                             int32_t n_values,
                             const char *lookup_mode) {
  std::vector<std::vector<double> > vgrid;
  for (int k = 0; k < n_dims; k++) {
    std::vector<double> x(ngridpoints[k]);
    for (int j= 0; j < ngridpoints[k]; j++) {
      x[j] = grid[k][j];
    }
    vgrid.push_back(x);
  }

  std::vector<double> vvalues(n_values);
  for (int k = 0; k < n_values; k++) {
    vvalues[k] = values[k];
  }

  return interpolant(vgrid, vvalues, std::string(lookup_mode));
}

extern "C"
void eval_interpolant(Interpolant *obj, const double* arg, double* res) {
  obj->eval(arg, res);
}

extern "C"
void delete_interpolant(Interpolant *obj) {
  delete obj;
}
