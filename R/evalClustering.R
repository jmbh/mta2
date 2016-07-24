############# eval clustering ################
#
# INPUT: Mousetrap data object
# OUPUT: Mousetrap data object + Numeric indicating optimal k (or ks) + evidence (e.g., curves or slopes)
#



eval_clustering = function(
  data,
  dimensions = c('xpos','ypos'),
  trajectory_object = 'trajetcories',
  
  # preprocessing
  align = TRUE,
  rescale = TRUE,
  n_resc = 20,
  
  # distance arguments
  point_wise = TRUE,
  power = 2
  
  # cluster arguments
  method = 'hierarchical',
  linkage = '????'
  n_cluster, # = k
  
  # arguments k-selection method 1
  foo = bar
  
  # arguments k-selection method 2
  foo = bar
  
  # arguments k-selection method k
  foo = bar
  
  )


  # ---- tests
  # ifs and stops
  
  # ---- data align
  if(verbose == TRUE) cat('aligning','\n')
  if(align == TRUE){
    data = trajectory_align(data, 
                            trajectory_object = trajectory_object, 
                            dimensions = dimensions, 
                            coordinates = 'mt')
                            }


  # rescale trajectories
  data = spatial_rescale(data,n_points = n_points,
                         dimensions = dimensions,
                         trajectory_object = trajectory_object)


  # ---- cluster trajectories
  
  
  
  # ---- evaluate clustering using method 1
  
  # ---- evaluate clustering using method 2
  
  # ---- evaluate clustering using method 3
  


  data$optimal_k = 
  data$k_selection_details = 
  
  return(data)
  }
