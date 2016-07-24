#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix spatialRescale(NumericVector x, NumericVector y, int npts) {
  int ind, n = x.size();
  NumericVector cumdiffs(n), steps(npts);
  NumericMatrix xyn(npts,2);
  double step, w1, w2, stepi, cumdiffi;
  
  
  // Calculate cumulative distances between points
  for(int i = 0; i < n; i++){
    if(i < 1){
      cumdiffs[i] = 0.0;
      } else {
      cumdiffs[i] = cumdiffs[i-1] + sqrt(pow(x[i] - x[i-1],2) + pow(y[i] - y[i-1],2));
      }
    }
  
  // Calculate vector with equidistant steps
  step = double(cumdiffs[n-1]) / double(npts-1);
  for(double i = 0; i < npts; i++){
    steps[i] = step * i;
    }
  
  // Loop over number of points for final 
  for(int i = 0; i < npts; i++){
    ind = 0;
    for(int j = 0; j < n; j++){
      stepi    = steps[i];
      cumdiffi = cumdiffs[j];
      if(stepi > cumdiffi) ind++;
      }
    if(i != (npts-1) && i != 0){
      w1 = std::abs(double(steps[i]) - double(cumdiffs[ind-1]));
      w2 = std::abs(double(steps[i]) - double(cumdiffs[ind]));
      xyn(i,0) = double(x[ind-1]) * w2/(w1+w2) + double(x[ind]) * w1/(w1+w2);
      xyn(i,1) = double(y[ind-1]) * w2/(w1+w2) + double(y[ind]) * w1/(w1+w2);        
      }      
    else if(i == 0){
      xyn(i,0) = double(x[0]);
      xyn(i,1) = double(y[0]);        
      }
    else {
      xyn(i,0) = double(x[n-1]);
      xyn(i,1) = double(y[n-1]);        
      }
    }
  return xyn;
  }

// [[Rcpp::export]]
NumericMatrix spatialRescale3d(NumericVector x, NumericVector y, NumericVector z, int npts) {
  int ind, n = x.size();
  NumericVector cumdiffs(n), steps(npts);
  NumericMatrix xyn(npts,3);
  double step, w1, w2, stepi, cumdiffi;
  
  
  // Calculate cumulative distances between points
  for(int i = 0; i < n; i++){
    if(i < 1){
      cumdiffs[i] = 0.0;
    } else {
      cumdiffs[i] = cumdiffs[i-1] + sqrt(pow(x[i] - x[i-1],2) + pow(y[i] - y[i-1],2));
    }
  }
  
  // Calculate vector with equidistant steps
  step = double(cumdiffs[n-1]) / double(npts-1);
  for(double i = 0; i < npts; i++){
    steps[i] = step * i;
  }
  
  // Loop over number of points for final 
  for(int i = 0; i < npts; i++){
    ind = 0;
    for(int j = 0; j < n; j++){
      stepi    = steps[i];
      cumdiffi = cumdiffs[j];
      if(stepi > cumdiffi) ind++;
    }
    if(i != (npts-1) && i != 0){
      w1 = std::abs(double(steps[i]) - double(cumdiffs[ind-1]));
      w2 = std::abs(double(steps[i]) - double(cumdiffs[ind]));
      xyn(i,0) = double(x[ind-1]) * w2/(w1+w2) + double(x[ind]) * w1/(w1+w2);
      xyn(i,1) = double(y[ind-1]) * w2/(w1+w2) + double(y[ind]) * w1/(w1+w2);
      xyn(i,2) = double(z[ind-1]) * w2/(w1+w2) + double(z[ind]) * w1/(w1+w2); 
    }      
    else if(i == 0){
      xyn(i,0) = double(x[0]);
      xyn(i,1) = double(y[0]);
      xyn(i,2) = double(z[0]);
    }
    else {
      xyn(i,0) = double(x[n-1]);
      xyn(i,1) = double(y[n-1]);
      xyn(i,2) = double(z[n-1]);
    }
  }
  return xyn;
}




// [[Rcpp::export]]
GenericVector spatialRescaleA(NumericMatrix xs,
                              NumericMatrix ys,
                              NumericVector n_pts){
  GenericVector xy(2);
  NumericVector n_pts_v(xs.nrow());
  int max_pts = 0;
  if(n_pts.length() != xs.nrow()){  
    for(int i = 0; i < xs.nrow(); i++){
      n_pts_v[i] = n_pts[0];
      max_pts = n_pts[0];
      }
    } else {
    n_pts_v = n_pts;
    for(int i = 0; i < xs.nrow(); i++){
      if(n_pts[i] > max_pts) max_pts = n_pts[i];
      }
    }
  NumericMatrix nxs(xs.nrow(), max_pts);
  NumericMatrix nys(ys.nrow(), max_pts);
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialRescale(xs(i,_), ys(i,_), n_pts_v[i]);
    for(int j = 0; j < max_pts; j++){
      if(j < resc_traj.nrow()){
        nxs(i, j) = resc_traj(j,0);
        nys(i, j) = resc_traj(j,1);
        } else {
        nxs(i, j) = -10000;
        nys(i, j) = -10000;
        }
      }
    }
  xy[0] = nxs;
  xy[1] = nys; 
  return xy;
}


// [[Rcpp::export]]
NumericMatrix spatialRescaleAlong(NumericMatrix xs,
                                 NumericMatrix ys,
                                 NumericVector n_pts){
  NumericVector n_pts_v(xs.nrow());
  int total_pts = 0;
  for(int i = 0; i < n_pts.size(); i++){  
      total_pts += n_pts[i];
      }
  if(n_pts.length() != xs.nrow()){  
    for(int i = 0; i < xs.nrow(); i++){
      n_pts_v[i] = n_pts[0];
      }
    } else {
    n_pts_v = n_pts;
    }
  NumericMatrix xy(total_pts, 2);
  int ind = 0;
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialRescale(xs(i,_), ys(i,_), n_pts_v[i]);
    for(int j = 0; j < resc_traj.nrow(); j++){
      xy(ind, 0) = resc_traj(j,0);
      xy(ind, 1) = resc_traj(j,1);
      ind++;
      }
    }
  return xy;
  }



// [[Rcpp::export]]
GenericVector spatialRescaleA3d(NumericMatrix xs,
                                NumericMatrix ys,
                                NumericMatrix zs,
                                NumericVector n_pts){
  GenericVector xyz(3);
  NumericVector n_pts_v(xs.nrow());
  int max_pts = 0;
  if(n_pts.length() != xs.nrow()){  
    for(int i = 0; i < xs.nrow(); i++){
      n_pts_v[i] = n_pts[0];
      max_pts = n_pts[0];
      }
    } else {
    n_pts_v = n_pts;
    for(int i = 0; i < xs.nrow(); i++){
      if(n_pts[i] > max_pts) max_pts = n_pts[i];
      }
    }
  NumericMatrix nxs(xs.nrow(), max_pts);
  NumericMatrix nys(ys.nrow(), max_pts);
  NumericMatrix nzs(zs.nrow(), max_pts);
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialRescale3d(xs(i,_), ys(i,_), zs(i,_), n_pts_v[i]);
    for(int j = 0; j < max_pts; j++){
      if(j < resc_traj.nrow()){
        nxs(i, j) = resc_traj(j,0);
        nys(i, j) = resc_traj(j,1);
        nzs(i, j) = resc_traj(j,2);
        } else {
        nxs(i, j) = -10000;
        nys(i, j) = -10000;
        nzs(i, j) = -10000;
        }
      }
    }
  xyz[0] = nxs;
  xyz[1] = nys; 
  xyz[2] = nzs; 
  return xyz;
  }


// [[Rcpp::export]]
NumericMatrix spatialRescaleAlong3d(NumericMatrix xs,
                                    NumericMatrix ys,
                                    NumericMatrix zs,
                                    NumericVector n_pts){
  NumericVector n_pts_v(xs.nrow());
  int total_pts = 0;
  for(int i = 0; i < n_pts.size(); i++){  
    total_pts += n_pts[i];
    }
  if(n_pts.length() != xs.nrow()){  
    for(int i = 0; i < xs.nrow(); i++){
      n_pts_v[i] = n_pts[0];
      }
    } else {
    n_pts_v = n_pts;
    }
  NumericMatrix xyz(total_pts, 3);
  int ind = 0;
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialRescale3d(xs(i,_), ys(i,_), zs(i,_), n_pts_v[i]);
    for(int j = 0; j < resc_traj.nrow(); j++){
      xyz(ind, 0) = resc_traj(j,0);
      xyz(ind, 1) = resc_traj(j,1);
      xyz(ind, 2) = resc_traj(j,2);
      ind++;
    }
  }
  return xyz;
}
