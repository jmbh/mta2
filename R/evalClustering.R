############# eval clustering ################
#
# INPUT: Mousetrap data object + Considered k sequence
# OUPUT: Mousetrap data object + Numeric indicating optimal k (or ks) + evidence (e.g., curves or slopes)
#



eval_clustering = function(
  data,
  dimensions = c('xpos','ypos'),
  trajectory_object = 'trajetcories',

  # preprocessing
  align = TRUE,
  rescale = TRUE,
  n_points = 20,

  # clustering arguments
  method = 'hierarchical', # or 'kmeans'
  linkage = 'complete',
  n_km = 10, # number of reinitializations for k-means algorithm
  n_subsample = NULL,

  # distance arguments
  point_wise = TRUE,
  power = 2,

  # k-selection type
  ksel_methods = c('Dist', 'Stab'), # distance based or/ and stability based methods
  k_seq = 2:15, # considered k-sequence

  # arguments instability-based k-selection methods
  B = 10, # bootstrap samples
  model_based = FALSE,

  # arguments distance-based k-selection methods
  n_Gap = 10 # simulated datasets for Gap Statistic

  )

{

  # ---- tests
  # ifs and stops
  if(model_based == TRUE & method = 'hierarchical') stop('Model-based instability methods are only available for k-means clustering.')

  # ---- data align
  if(verbose == TRUE) cat('aligning','\n')
  if(align == TRUE){
  data = trajectory_align(data,
                          trajectory_object = trajectory_object,
                          dimensions = dimensions,
                          coordinates = 'mt')
  }


  # rescale trajectories
  data = spatial_rescale(data,
                         n_points = n_points,
                         dimensions = dimensions,
                         trajectory_object = trajectory_object)

  # subsample data
  n <- nrow(data$data)
  if(is.null(n_subsample)) n_subsample <- n
  ss_ind <- sample(1:n, n_subsample, replace=F)
  data_short <- subset_data(data, condition = 1:n %in% ss_ind)

  # transform data structura for clustering input
  data_ReA <- t(apply(data_short$rescaled_trajectories, 1, function(x) c(x[1,], x[2,])))

  # ---- Distance-based k-selection methods
  if('Dist' %in% ksel_methods) {

  if(verbose == TRUE) cat('calculating distance-based k-selection methods','\n')
  cDist_obj <- cDistance(data = data_ReA,
                         kseq = k_seq,
                         method = method,
                         linkage = linkage,
                         kmIter = n_km,
                         RunsGap = n_Gap)

  }

  # ---- Stability-based k-selection methods
  if('Stab' %in% ksel_methods) {

  tt <- proc.time()[3]
  if(verbose == TRUE) cat('calculating stability-based k-selection methods','\n')
  cStab_obj <- cStability(data = data_ReA,
                          kseq = kseq,
                          B = B,
                          norm = TRUE,
                          prediction = model_based,
                          type = method,
                          linkage = linkage,
                          kmIter = n_km,
                          pbar = TRUE)

  }
  TT <- proc.time()[3] - tt; TT

  data$optimal_k =
  data$k_selection_details =

  return(data)

    }





