#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
GenericVector trajAlign(NumericMatrix xs, NumericMatrix ys,
                        bool start, bool end, NumericVector coordinates) {
  double x0, y0, xn, yn, xmean0 = 0, ymean0 = 0,xmeann = 0, ymeann = 0;
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericMatrix nxs(xs.nrow(),xs.ncol()), nys(ys.nrow(),ys.ncol());
  GenericVector res(2);
  NumericVector end_is(xs.nrow());
  // find ends and means
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    xmean0 += x[0];
    ymean0 += y[0];
    for(int j = x.size() - 1; j > -1; j--){
      if(x[j] != -3.141592653589793){
        end_is[i] = j;
        break;
        }
      }
    xmeann += x[end_is[i]];
    ymeann += y[end_is[i]];
    }
  xmean0 = xmean0 / xs.nrow();
  ymean0 = ymean0 / xs.nrow();
  xmeann = xmeann / xs.nrow();
  ymeann = ymeann / xs.nrow();
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    x0 = x[0];
    y0 = y[0];
    xn = x[end_is[i]];
    yn = y[end_is[i]];
    for(int j = 0; j < end_is[i] + 1; j++){
      if(start){
        if(end){
          x[j] = (((x[j] - x0)/(xn - x0)) * (coordinates[2] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - y0)/(yn - y0)) * (coordinates[3] - coordinates[1]) + coordinates[1]);
        } else {
          x[j] = (((x[j] - x0)/(xmeann - x0)) * (coordinates[2] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - y0)/(ymeann - y0)) * (coordinates[3] - coordinates[1]) + coordinates[1]);
        }
      } else {
        if(end){
          x[j] = (((x[j] - xmean0)/(xn - xmean0)) * (coordinates[2] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - ymean0)/(yn - ymean0)) * (coordinates[3] - coordinates[1]) + coordinates[1]);
        } else {
          x[j] = (((x[j] - xmean0)/(xmeann - xmean0)) * (coordinates[2] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - ymean0)/(ymeann - ymean0)) * (coordinates[3] - coordinates[1]) + coordinates[1]);
        }
      }
    }
    nxs(i,_) = x;
    nys(i,_) = y;
  }
  res[0] = nxs;
  res[1] = nys;
  return(res);
}





// [[Rcpp::export]]
GenericVector trajAlign3d(NumericMatrix xs, NumericMatrix ys, NumericMatrix zs,
                        bool start, bool end, NumericVector coordinates) {
  double x0, y0, z0, xn, yn, zn, xmean0 = 0, ymean0 = 0, zmean0 = 0,xmeann = 0, ymeann = 0, zmeann = 0;
  NumericVector x(xs.ncol()), y(ys.ncol()), z(zs.ncol());
  NumericMatrix nxs(xs.nrow(),xs.ncol()), nys(ys.nrow(),ys.ncol()), nzs(zs.nrow(),zs.ncol());
  GenericVector res(3);
  NumericVector end_is(xs.nrow());
  // find ends and means
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    z = ys(i,_);
    xmean0 += x[0];
    ymean0 += y[0];
    zmean0 += z[0];
    for(int j = x.size() - 1; j > -1; j--){
      if(x[j] != -3.141592653589793){
        end_is[i] = j;
        break;
        }
      }
    xmeann += x[end_is[i]];
    ymeann += y[end_is[i]];
    zmeann += z[end_is[i]];
    }
  xmean0 = xmean0 / xs.nrow();
  ymean0 = ymean0 / ys.nrow();
  zmean0 = zmean0 / zs.nrow();
  xmeann = xmeann / xs.nrow();
  ymeann = ymeann / ys.nrow();
  zmeann = zmeann / zs.nrow();
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    z = zs(i,_);
    x0 = x[0];
    y0 = y[0];
    z0 = z[0];
    xn = x[x.length()-1];
    yn = y[y.length()-1];
    zn = z[z.length()-1];
    for(int j = 0; j < end_is[i] + 1; j++){
      if(start){
        if(end){
          x[j] = (((x[j] - x0)/xn) * (coordinates[3] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - y0)/yn) * (coordinates[4] - coordinates[1]) + coordinates[1]);
          z[j] = (((z[j] - z0)/zn) * (coordinates[5] - coordinates[2]) + coordinates[2]);
        } else {
          x[j] = (((x[j] - x0)/xmeann) * (coordinates[3] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - y0)/ymeann) * (coordinates[4] - coordinates[1]) + coordinates[1]);
          z[j] = (((z[j] - z0)/zmeann) * (coordinates[5] - coordinates[2]) + coordinates[2]);
        }
      } else {
        if(end){
          x[j] = (((x[j] - xmean0)/xn) * (coordinates[3] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - ymean0)/yn) * (coordinates[4] - coordinates[1]) + coordinates[1]);
          z[j] = (((z[j] - zmean0)/zn) * (coordinates[5] - coordinates[2]) + coordinates[2]);
        } else {
          x[j] = (((x[j] - xmean0)/xmeann) * (coordinates[3] - coordinates[0]) + coordinates[0]);
          y[j] = (((y[j] - ymean0)/ymeann) * (coordinates[4] - coordinates[1]) + coordinates[1]);
          z[j] = (((z[j] - zmean0)/zmeann) * (coordinates[5] - coordinates[2]) + coordinates[2]);
        }
      }
    }
    nxs(i,_) = x;
    nys(i,_) = y;
    nzs(i,_) = z;
  }
  res[0] = nxs;
  res[1] = nys;
  res[2] = nzs;
  return(res);
}

