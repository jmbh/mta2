#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double getLength(NumericVector x, NumericVector y) {
  double Length = 0;
  double xd, yd; 
  for(int i = 0; i < x.length() - 1; i++){
    xd = x[i+1] - x[i];
    yd = y[i+1] - y[i];
    Length += sqrt(xd*xd + yd*yd);
    }
  return Length;
  }

// [[Rcpp::export]]
double getLength3d(NumericVector x, NumericVector y, NumericVector z) {
  double Length = 0;
  double xd, yd, zd;
  for(int i = 0; i < x.length() - 1; i++){
    xd = x[i+1] - x[i];;
    yd = y[i+1] - y[i];
    zd = z[i+1] - z[i];
    Length += sqrt(xd*xd + yd*yd + zd*zd);
  }
  return Length;
}  

// [[Rcpp::export]]
NumericVector getLengths(NumericMatrix xs, NumericMatrix ys) {
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericVector Lengths(xs.nrow());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    Lengths[i] = getLength(x,y);
    }
  return Lengths;
  }

// [[Rcpp::export]]
NumericVector getLengths3d(NumericMatrix xs, NumericMatrix ys, NumericMatrix zs) {
  NumericVector x(xs.ncol()), y(ys.ncol()), z(zs.ncol());
  NumericVector Lengths(xs.nrow());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    z = zs(i,_);
    Lengths[i] = getLength3d(x,y,z);
    }
  return Lengths;
  }