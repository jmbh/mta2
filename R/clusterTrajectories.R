############# cluster trajectories ################
#
# INPUT: Mousetrap data object
# OUPUT: Mousetrap data object + clusters (+ distance object)
#
# This function mainly adds a single vector to the
# foo$data data.frame indicating the number of clusters. In addition
# it will add the distance matrix by applying the add_dist function
# (see R-file addDist).
#
# Moroever the function should be given access to cluster_eval. That
# is there should be the option of selecting k using one f our methods.
#

cluster_trajectory = function(
  data,
  dimensions = c('xpos','ypos'),
  trajectory_object = 'trajectories',

  # preprocessing
  align = TRUE,
  rescale = TRUE,
  n_resc = 20,

  # cluster arguments
  method = 'hierarchical', # or 'kmeans'
  linkage = 'complete',
  n_km = 10, # number of reinitializations for k-means algorithm
  n_cluster, # = k
  n_subsample = NULL,

  # distance arguments
  point_wise = TRUE, # ???
  p = 2,

  # output options
  verbose = TRUE
  )

{

  # ---- tests

  # ---- define variable names
  id_variables = c('ptp','trial')
  clean_variables = c('box_clean','sim_clean','max_dist','mean_sim')
  CoM_variables = c('dist_CoM','angle_CoM','min_angle','max_x_dist')


  # ---- data align
  if(verbose == TRUE) cat('aligning','\n')
  if(align == TRUE) data = trajectory_align(data = data,
                                            dimensions = dimensions,
                                            trajectory_object = trajectory_object)

  # rescale trajectories
  data = spatial_rescale(data,
                         n_points = n_resc,
                         dimensions = dimensions,
                         trajectory_object = trajectory_object)

  # subsample data
  n <- nrow(data$data)
  if(is.null(n_subsample)) n_subsample <- n
  ss_ind <- sample(1:n, n_subsample, replace=F)
  data_short <- subset_data(data, condition = 1:n %in% ss_ind)


  # transform data structura for clustering input
  data_ReA <- t(apply(data_short$rescaled_trajectories, 1, function(x) c(x[1,], x[2,])))

  # ---- cluster trajectories
  if(method == 'hierarchical') {

    # distance calculation
    distm <- dist(data_ReA, method = 'minkowski', p = p)
    # clustering
    hcl_obj <- fastcluster::hclust(distm, method = linkage)
    cl <- cutree(hcl_obj, n_cluster)

  } else {
    # k-means
    km_obj <- kmeans(data_ReA, centers = n_cluster, nstart = n_km)
    cl <- km_obj$cluster

  # maybe: use fpc package to return some cluster diagnostics?
  }

  # add to input obkect
  data$data$cluster <- cl

  return(data)

  }
