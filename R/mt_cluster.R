#' Cluster trajectories
#'
#' \code{trajectory_align} aligns trajectories to common start point, end point,
#'   and coordinate system.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory
#'   array can be provided directly (in this case \code{use} will be ignored).
#' @param use a character string specifying which trajectory data should be
#'   used.
#' @param dimensions a character string specifying which trajectory variables
#'   should be used. Can be of length 2 or 3 for 2-dimensional or 3-dimensional
#'   alignment respectively.
#' @param save_as a character string specifying where the resulting trajectory
#'   data should be stored.


mt_cluster = function(data,
                      use = 'trajectories',
                      dimensions = c('xpos','ypos'),
                      save_as = 'clusters',

                      # distance arguments
                      pointwise = TRUE,
                      minkowski_p = 2,

                      # cluster arguments
                      method = 'hierarchical',
                      linkage = 'complete',
                      n_cluster = 5, # = k
                      n_km = 10
                      ){

  # ---- tests
  if(!length(dimensions) %in% 2){
    stop('Dimensions must of length 2!')
    }
  if(is.list(data)){
    trajectories = data[[use]]
    } else {
    if(is.array(data)){
      trajectories = data
      } else {
      stop('Data must be array or list!')
      }
    }
  if(!all(c(dimensions) %in% dimnames(trajectories)[[2]])) stop('Not all dimensions exist')

  # ---- cluster trajectories
  if(method == 'hierarchical') {

    distm =  mt_distmat(trajectories,
                       dimensions = dimensions,
                       pointwise = pointwise,
                       minkowski_p = minkowski_p)
    distm = stats::as.dist(distm)


    # clustering
    cl_obj = fastcluster::hclust(distm, method = linkage)
    cl_ass = cutree(cl_obj, n_cluster)

    } else {

    # rearrange data structura for clustering input
    rearranged_trajectories = t(cbind(trajectories[,dimensions[1],],trajectories[,dimensions[2],]))

    # k-means
    cl_obj <- kmeans(rearranged_trajectories, centers = n_cluster, nstart = n_km)
    cl_ass <- cl_obj$cluster

    }


  # ---- save data
  if(is.list(data)){
    if(save_as %in% names(data)){
      data[[save_as]]$clustering = cl_ass
      } else {
      result = data.frame(mt_id = dimnames(trajectories)[[1]],clustering = cl_ass)
      data[[save_as]] = result
      }
    } else {
    result = data.frame(mt_id = dimnames(trajectories)[[1]],clustering = cl_ass)
    data = result
    }

  return(data)

  }
