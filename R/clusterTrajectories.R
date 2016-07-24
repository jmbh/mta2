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
#


cluster_trajectory = function(
  data,
  dimensions = c('xpos','ypos'),
  trajectory_object = 'trajetcories',
  
  # preprocessing
  align = TRUE,
  rescale = TRUE,
  n_resc = 20,
  
  # cluster arguments
  method = 'hierarchical',
  linkage = '????'
  n_cluster, # = k
  evaluate = TRUE,
  
  # distance arguments
  point_wise = TRUE,
  power = 2
  
  )


  # ---- tests
  
  # ---- define variable names
  id_variables = c('ptp','trial')
  clean_variables = c('box_clean','sim_clean','max_dist','mean_sim')
  CoM_variables = c('dist_CoM','angle_CoM','min_angle','max_x_dist')
  
  
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

  
  # ---- evaluate clustering   
  
  # ---- evaluate clustering 
  
  
  
  # ---- cluster trajectories
  
  
  
  
  return(data)
  }
