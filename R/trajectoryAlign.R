############# trajectoryAlign ################
#
# INPUT  
# 
#
# OUTPUT
# 
#
# DESCRIPTIO
#

trajectory_align = function(data,
                            dimensions = c('xpos','ypos'),
                            trajectory_object = 'trajectories',
                            start = TRUE, end = TRUE,
                            coordinates = 'norm'
                            ){
  
  # ---- tests
  n_dim = length(dimensions)
  if(!n_dim %in% c(2,3)) stop('Dimensions must of length 2 or 3') 
  if(is.character(coordinates) & !any(coordinates == 'mt') & !any(coordinates == 'norm')) {
    stop("Use 'mt' or 'norm' or numeric vector")
    }
  if(any(coordinates == 'mt'))   if(n_dim == 2) coordinates = c(0,0,-1,1.5) else coordinates = c(0,0,0,-1,1.5,-1)
  if(any(coordinates == 'norm')) if(n_dim == 2) coordinates = c(0,0,1,1)  else coordinates = c(0,0,0,1,1,1)
  if(!trajectory_object %in% names(data)) stop(paste0(trajectory_object,' does not exist!'))
  if(!all(dimensions %in% dimnames(data[[trajectory_object]])[[2]])) stop(paste0('Not all dimensions exist!'))
  
  # ---- get aligned data   
  if(length(dimensions) == 2){
    if(length(coordinates) != 4) stop('Coordinates must be a numeric vector of length 4')
    traj_align = trajAlign(data[[trajectory_object]][,dimensions[1],],
                           data[[trajectory_object]][,dimensions[2],],
                           start = start,end = end,coordinates = coordinates)
    }
  if(length(dimensions) ==3){
    if(length(coordinates) != 6) stop('Coordinates must be a numeric vector of length 6')
    traj_align = trajAlign3d(data[[trajectory_object]][,dimensions[1],],
                             data[[trajectory_object]][,dimensions[2],],
                             data[[trajectory_object]][,dimensions[3],],
                             start = start,end = end,coordinates = coordinates)
    }
    
  # ---- add aligned data    
  for(i in 1:length(dimensions)){
    data[[trajectory_object]][,dimensions[i],] = traj_align[[i]]
    }

  return(data)
  }





