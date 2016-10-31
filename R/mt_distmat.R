#' Compute distance matrix
#' 
#' \code{mt_distmat} computes the point- or vector-wise dissimilarity between 
#'   each pair of trajectories. 
#'
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory 
#'   array can be provided directly (in this case \code{use} will be ignored).
#' @param use a character string specifying which trajectory data should be 
#'   used.
#' @param dimensions a character string specifying which trajectory variables 
#'   should be used. Can be of length 2 or 3 for 2-dimensional or 3-dimensional
#'   trajectories.
#' @param save_as a character string specifying where the resulting trajectory 
#'   data should be stored.
#' @param point_wise boolean specifying whether dissimilarity should be
#'   assessed point- or vector-wise. Point-wise measures the average dissimilarity 
#'   across all points, whereas vector-wise measures dissimilarity once for the 
#'   entire trajectory.  
#' @param minkowski_p an integer specifying the distance metric. \code{minkowski_p}
#'   = 1 computes the city-block distance, \code{minkowski_p} = 2 computes the
#'   Euclidian distance, and so on.  
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional
#'   object (by default called \code{distmat}) containing the distance matrix. If a 
#'   trajectory array was provided directly as \code{data}, only the distance matrix
#'   will be returned.
#'   
#' @examples
#' mt_example <- mt_distmat(data=mt_example,
#'   dimensions = c('xpos','ypos'), save_as="distmat",
#'   n_points = 20, format = 'wide')
#'   
#' @export

mt_distmat = function(data,
                      use = 'trajectories',
                      dimensions = c('xpos','ypos'),
                      save_as = 'distmat',
                      pointwise = TRUE,
                      minkowski_p = 2
                      ){
  
  # ---- tests
  if(!length(dimensions) %in% c(2,3)) stop('Dimensions must of length 2 or 3') 
  if(is.list(data)){
    if(!use %in% names(data)) stop(paste0(use, ' does not exist!'))
    trajectories = data[[use]]
    } else {
    if(is.array(data)){
      trajectories = data
      } else {
      stop('Argument data must be either list or array!')  
      }  
    }
  if(!all(dimensions %in% dimnames(trajectories)[[2]])) stop('Not all dimensions exist')
  
  
  # ---- get distances
  if(length(dimensions) == 2){
    if(pointwise == TRUE){
        dmat = distMat(trajectories[,dimensions[1],],
                       trajectories[,dimensions[2],],
                       power = minkowski_p)
      } else {
        dmat = distMatV(trajectories[,dimensions[1],],
                        trajectories[,dimensions[2],],
                        power = minkowski_p)
      } 
    } else {
      if(pointwise == TRUE){
          dmat = distMat3d(trajectories[,dimensions[1],],
                           trajectories[,dimensions[2],],
                           trajectories[,dimensions[3],],
                           power = minkowski_p)          
        } else {
          dmat = distMat3dV(data[[use]][,dimensions[1],],
                            data[[use]][,dimensions[2],],
                            data[[use]][,dimensions[3],],
                            minkowski_p = power)        
      }
    }

  # ---- save data
  if(is.list(data)){
    data[[save_as]] = dmat
    } else {
    data = dmat  
    }
  
  return(data)
  }


