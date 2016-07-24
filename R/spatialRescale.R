############# spatial rescale ################
#
# INPUT  
# 
#
# OUTPUT
# 
#
# DESCRIPTIO
#

spatial_rescale = function(data,
                           n_points,
                           dimensions = c('xpos','ypos'),
                           trajectory_object = 'trajectories',
                           format = 'wide'
                           ){
  
  # ---- tests
  n_dim = length(dimensions)
  if(!n_dim %in% c(2,3)) stop('Dimensions must of length 2 or 3')
  if(!trajectory_object %in% names(data)) stop(paste0(trajectory_object,' does not exist!'))
  if(!all(dimensions %in% dimnames(data[[trajectory_object]])[[2]])) stop(paste0('Not all dimensions exist!'))
  if(!format %in% c('wide','long')) stop('Incorrect format.')
  
  # ---- rescale trajectories
  if(format == 'wide'){
    if(n_dim == 2) resc_list = spatialRescaleA(data[[trajectory_object]][,dimensions[1],],
                                               data[[trajectory_object]][,dimensions[2],],
                                               n_points)
    if(n_dim == 3) resc_list = spatialRescaleA3d(data[[trajectory_object]][,dimensions[1],],
                                                 data[[trajectory_object]][,dimensions[2],],
                                                 data[[trajectory_object]][,dimensions[3],],
                                                 n_points)
    
    # ---- add rescaled trajectories
    traj_resc = array(dim = c(dim(data[[trajectory_object]][,,])[1],
                              length(dimensions),
                              max(n_points)),
                      dimnames = list(dimnames(data[[trajectory_object]])[[1]],dimensions,NULL))
    for(i in 1:length(resc_list)) {
      traj_tmp = resc_list[[i]]
      traj_tmp[traj_tmp == -10000] = NA
      traj_resc[,i,] = traj_tmp
      }
    data$rescaled_trajectories = traj_resc
    }
  if(format == 'long'){
    if(n_dim == 2) resc_mat = spatialRescaleAlong(data[[trajectory_object]][,dimensions[1],],
                                                  data[[trajectory_object]][,dimensions[2],],
                                                  n_points)
    if(n_dim == 3) resc_mat = spatialRescaleAlong3d(data[[trajectory_object]][,dimensions[1],],
                                                    data[[trajectory_object]][,dimensions[2],],
                                                    data[[trajectory_object]][,dimensions[3],],
                                                    n_points)    
    data$rescaled_trajectories_long = resc_mat                                            
    }
  
  return(data)
  }



