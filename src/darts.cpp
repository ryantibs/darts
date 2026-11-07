// Core computations for the darts package.
//
// Board geometry is passed in from R as two vectors:
//   R: radii (mm) of the six wires, from the center outward
//      R[0] double bullseye, R[1] single bullseye, R[2] inner triple ring,
//      R[3] outer triple ring, R[4] inner double ring, R[5] outer double ring
//   S: the 20 numbers on the board, clockwise starting at the top

#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

const double TWO_PI = 2.0 * M_PI;

// A region of the board that yields a single score: an annulus [r1, r2],
// either the full circle (slice = 0) or the slice belonging to the number
// at clockwise position `slice` (1-based).
struct Region {
  double r1, r2;
  int slice;
  double area;
};

void check_board(const NumericVector& R, const IntegerVector& S) {
  if (R.size() != 6) stop("`R` must have length 6.");
  if (S.size() != 20) stop("`S` must have length 20.");
}

int score_one(double x, double y, const NumericVector& R,
              const IntegerVector& S) {
  double r = std::sqrt(x * x + y * y);
  if (r > R[5]) return 0;
  if (r <= R[0]) return 50;
  if (r <= R[1]) return 25;

  // Angle measured clockwise from the top, shifted by half a slice
  double phi = M_PI / 2.0 - std::atan2(y, x) + M_PI / 20.0;
  phi -= TWO_PI * std::floor(phi / TWO_PI);
  int i = static_cast<int>(std::floor(phi / TWO_PI * 20.0));
  if (i > 19) i = 19;
  int n = S[i];

  if (r <= R[2]) return n;
  if (r <= R[3]) return 3 * n;
  if (r <= R[4]) return n;
  return 2 * n;
}

// All board regions that produce score x. The miss region (x = 0) is an
// unbounded annulus, signalled by r2 < 0.
std::vector<Region> regions_for_score(int x, const NumericVector& R,
                                      const IntegerVector& S) {
  std::vector<Region> out;
  auto slice_area = [](double r1, double r2) {
    return M_PI * (r2 * r2 - r1 * r1) / 20.0;
  };

  if (x == 0) {
    out.push_back({R[5], -1.0, 0, 0.0});
    return out;
  }
  if (x == 50) {
    out.push_back({0.0, R[0], 0, M_PI * R[0] * R[0]});
    return out;
  }
  if (x == 25) {
    out.push_back({R[0], R[1], 0, M_PI * (R[1] * R[1] - R[0] * R[0])});
    return out;
  }
  for (int k = 0; k < 20; k++) {
    int n = S[k];
    if (n == x) {  // inner and outer single
      out.push_back({R[1], R[2], k + 1, slice_area(R[1], R[2])});
      out.push_back({R[3], R[4], k + 1, slice_area(R[3], R[4])});
    }
    if (2 * n == x) {  // double
      out.push_back({R[4], R[5], k + 1, slice_area(R[4], R[5])});
    }
    if (3 * n == x) {  // triple
      out.push_back({R[2], R[3], k + 1, slice_area(R[2], R[3])});
    }
  }
  if (out.empty()) stop("%d is not a possible score on this board.", x);
  return out;
}

// ---------------------------------------------------------------------------
// Simple model: isotropic Gaussian with variance s in each coordinate.
// For a region [r1, r2] covering angular fraction f, with u = r^2 / (2s):
//   P(region)            = f * (exp(-u1) - exp(-u2))
//   E[r^2; region] / s   = f * ((r1^2 + 2s) exp(-u1) - (r2^2 + 2s) exp(-u2))

double tail_prob(double r, double s) {
  return std::exp(-r * r / (2.0 * s));
}

double tail_moment(double r, double s) {
  return (r * r + 2.0 * s) * std::exp(-r * r / (2.0 * s));
}

double region_prob(const Region& g, double s) {
  double f = g.slice == 0 ? 1.0 : 1.0 / 20.0;
  double hi = g.r2 < 0 ? 0.0 : tail_prob(g.r2, s);
  return f * (tail_prob(g.r1, s) - hi);
}

double region_moment(const Region& g, double s) {
  double f = g.slice == 0 ? 1.0 : 1.0 / 20.0;
  double hi = g.r2 < 0 ? 0.0 : tail_moment(g.r2, s);
  return f * (tail_moment(g.r1, s) - hi);
}

// ---------------------------------------------------------------------------
// General model: Gaussian with covariance Sig = [s1 s3; s3 s2]. The E-step
// is computed by self-normalized importance sampling, drawing points
// uniformly from the regions that produce each score.

double rand_radius(double r1, double r2) {
  return std::sqrt(r1 * r1 + (r2 * r2 - r1 * r1) * R::unif_rand());
}

void rand_point(const Region& g, double rmax, double& zx, double& zy) {
  double r2 = g.r2 < 0 ? rmax : g.r2;
  double r = rand_radius(g.r1, r2);
  double th;
  if (g.slice == 0) {
    th = TWO_PI * R::unif_rand();
  } else {
    // Clockwise angle from the top, then convert to the usual convention
    double phi = -M_PI / 20.0 + (g.slice - 1) * TWO_PI / 20.0 +
                 TWO_PI / 20.0 * R::unif_rand();
    th = M_PI / 2.0 - phi;
  }
  zx = r * std::cos(th);
  zy = r * std::sin(th);
}

}  // namespace

// [[Rcpp::export(rng = false)]]
IntegerVector score_cpp(NumericVector x, NumericVector y, NumericVector R,
                        IntegerVector S) {
  check_board(R, S);
  R_xlen_t n = x.size();
  if (y.size() != n) stop("`x` and `y` must have the same length.");
  IntegerVector out(n);
  for (R_xlen_t i = 0; i < n; i++) {
    if (NumericVector::is_na(x[i]) || NumericVector::is_na(y[i])) {
      out[i] = NA_INTEGER;
    } else {
      out[i] = score_one(x[i], y[i], R, S);
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerMatrix score_matrix_cpp(int half, NumericVector R, IntegerVector S) {
  check_board(R, S);
  int m = 2 * half + 1;
  IntegerMatrix A(m, m);
  for (int j = 0; j < m; j++) {
    for (int i = 0; i < m; i++) {
      A(i, j) = score_one(i - half, j - half, R, S);
    }
  }
  return A;
}

// [[Rcpp::export(rng = false)]]
List simple_em_cpp(IntegerVector x, double s_init, int niter, NumericVector R,
                   IntegerVector S) {
  check_board(R, S);
  int n = x.size();
  std::vector<std::vector<Region>> reg(n);
  for (int i = 0; i < n; i++) reg[i] = regions_for_score(x[i], R, S);

  NumericVector s(niter), loglik(niter);
  double cur = s_init;
  for (int t = 0; t < niter; t++) {
    // E-step and M-step: s = mean of E[r^2 | score] / 2
    double tot = 0.0;
    for (int i = 0; i < n; i++) {
      double num = 0.0, den = 0.0;
      for (const Region& g : reg[i]) {
        num += region_moment(g, cur);
        den += region_prob(g, cur);
      }
      tot += num / den;
    }
    cur = tot / (2.0 * n);
    s[t] = cur;

    // Observed-data log likelihood at the new estimate
    double ll = 0.0;
    for (int i = 0; i < n; i++) {
      double p = 0.0;
      for (const Region& g : reg[i]) p += region_prob(g, cur);
      ll += std::log(p);
    }
    loglik[t] = ll;
  }
  return List::create(_["s"] = s, _["loglik"] = loglik);
}

// [[Rcpp::export]]
NumericMatrix general_em_cpp(IntegerVector x, NumericVector Sig_init,
                             int niter, int nsamp, NumericVector R,
                             IntegerVector S) {
  check_board(R, S);
  int n = x.size();
  std::vector<std::vector<Region>> reg(n);
  for (int i = 0; i < n; i++) reg[i] = regions_for_score(x[i], R, S);

  NumericMatrix Sig(niter, 3);
  double s1 = Sig_init[0], s2 = Sig_init[1], s3 = Sig_init[2];
  std::vector<double> zx(nsamp), zy(nsamp), lw(nsamp);

  for (int t = 0; t < niter; t++) {
    double det = s1 * s2 - s3 * s3;
    if (!(det > 0)) stop("Covariance estimate is not positive definite.");

    // Outer radius for sampling the (unbounded) miss region: far enough out
    // that the Gaussian density beyond it is negligible
    double lmax = 0.5 * (s1 + s2) +
                  std::sqrt(0.25 * (s1 - s2) * (s1 - s2) + s3 * s3);
    double rmax = R[5] + 10.0 * std::sqrt(lmax);

    double c1 = 0.0, c2 = 0.0, c3 = 0.0;
    for (int i = 0; i < n; i++) {
      // Region areas, for sampling uniformly over their union
      const std::vector<Region>& g = reg[i];
      std::vector<double> cum(g.size());
      double tot = 0.0;
      for (size_t k = 0; k < g.size(); k++) {
        double a = g[k].r2 < 0 ? M_PI * (rmax * rmax - g[k].r1 * g[k].r1)
                               : g[k].area;
        tot += a;
        cum[k] = tot;
      }

      double lwmax = R_NegInf;
      for (int b = 0; b < nsamp; b++) {
        double u = R::unif_rand() * tot;
        size_t k = 0;
        while (k + 1 < g.size() && u > cum[k]) k++;
        rand_point(g[k], rmax, zx[b], zy[b]);
        lw[b] = -(s2 * zx[b] * zx[b] - 2.0 * s3 * zx[b] * zy[b] +
                  s1 * zy[b] * zy[b]) / (2.0 * det);
        if (lw[b] > lwmax) lwmax = lw[b];
      }

      // Normalize on the log scale to avoid underflow
      double W = 0.0, b1 = 0.0, b2 = 0.0, b3 = 0.0;
      for (int b = 0; b < nsamp; b++) {
        double w = std::exp(lw[b] - lwmax);
        b1 += zx[b] * zx[b] * w;
        b2 += zy[b] * zy[b] * w;
        b3 += zx[b] * zy[b] * w;
        W += w;
      }
      c1 += b1 / W;
      c2 += b2 / W;
      c3 += b3 / W;
    }

    s1 = c1 / n;
    s2 = c2 / n;
    s3 = c3 / n;
    Sig(t, 0) = s1;
    Sig(t, 1) = s2;
    Sig(t, 2) = s3;
  }
  return Sig;
}
