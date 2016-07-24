#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector getAngleP(NumericVector x, NumericVector y) {
  NumericVector angles(x.length());
  double len12, len13, len23, len12x, len12y, len13x, len13y, len23x, len23y, tmp, angle; 
  angles[0] = -100;
  for(int i = 0; i < x.length() - 2; i++){
    len12x = (x[i] -   x[i+1]);
    len12y = (y[i] -   y[i+1]);   
    len13x = (x[i] -   x[i+2]);
    len13y = (y[i] -   y[i+2]);
    len23x = (x[i+1] - x[i+2]);
    len23y = (y[i+1] - y[i+2]);
    len12 = len12x*len12x+len12y*len12y;
    len13 = len13x*len13x+len13y*len13y;
    len23 = len23x*len23x+len23y*len23y;
    if(len12 > 0 && len23 > 0){
      tmp = (len12+len23-len13)/std::pow(4*len12*len23,.5);
      if(tmp >  1) tmp = 1;
      if(tmp < -1) tmp = -1;
      angle = std::acos(tmp);
      angles[i+1] = angle;
      } else {
      angles[i+1] = -100;      
      }
    }
  angles[x.length()-1] = -100;
  return angles;
  }


// [[Rcpp::export]]
NumericVector getAngleV(NumericVector x, NumericVector y) {
  NumericVector angles(x.length());
  double len12, len13, len23, len12x, len12y, len13x, len13y, len23x, len23y, tmp, angle; 
  angles[0] = -100;
  for(int i = 0; i < x.length() - 1; i++){
    len12x = 0;
    len12y = 1;   
    len13x = (x[i]     -   x[i+1]);
    len13y = ((y[i]+1) -   y[i+1]);
    len23x = (x[i] -       x[i+1]);
    len23y = (y[i] -       y[i+1]);
    len12 = len12x*len12x+len12y*len12y;
    len13 = len13x*len13x+len13y*len13y;
    len23 = len23x*len23x+len23y*len23y;
    if(len12 > 0 && len23 > 0){
      tmp = (len12+len23-len13)/std::pow(4*len12*len23,.5);
      if(tmp >  1) tmp = 1;
      if(tmp < -1) tmp = -1;
      angle = std::acos(tmp);
      if(len23x > 0){
        angles[i+1] = angle;
        } else {
        angles[i+1] = -angle;
        }
      } else {
      angles[i+1] = -100;      
      }
    }
  return angles;
}

// [[Rcpp::export]]
NumericMatrix getAnglesP(NumericMatrix xs, NumericMatrix ys){
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericMatrix angleMat(xs.nrow(),xs.ncol());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    angleMat(i,_) = getAngleP(x,y);
    }
  return angleMat;
  }

// [[Rcpp::export]]
NumericMatrix getAnglesV(NumericMatrix xs, NumericMatrix ys){
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericMatrix angleMat(xs.nrow(),xs.ncol());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    angleMat(i,_) = getAngleV(x,y);
    }
  return angleMat;
  }
