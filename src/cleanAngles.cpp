#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cleanAngles(NumericMatrix as) {
  for(int i = 0; i < as.nrow(); i++){
    NumericVector traj = as(i,_);
    for(int j = 0; j < traj.length(); j++){
      int pt = traj[j];
      if(pt == -100){
        for(int k = 0; k < traj.length(); k++){
          double npt = traj[k];
          if(int(npt) != -100){
            traj[j] = npt;
            break;
            }
          }
        }
      }
      as(i,_) = traj;
    }
  return 0;
  }

