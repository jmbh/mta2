############# add velocities ################
#
# INPUT  
# 
#
# OUTPUT
# 
#
# DESCRIPTIO
#

add_velocities = function(data,
                      dimensions = c('xpos','ypos'),
                      trajectory_object = 'trajectories',
                      overwrite = FALSE
  ){
  
  # ---- Iterate over trajectory objects
  for(object in trajectory_object){
    
    # ---- tests
    n_dim = length(dimensions)
    if(!n_dim %in% c(2,3)) stop('Dimensions must of length 2') 
    if(!object %in% names(data)) stop(paste0(object,' does not exist!'))
    if(!all(dimensions %in% dimnames(data[[object]])[[2]])) stop(paste0('Not all dimensions exist!'))
    
    # ---- get angles
    velocities = getVelocities(data[[object]][,dimensions[1],],
                               data[[object]][,dimensions[2],])
    
    
    # ---- add angles
    data = add_column(data,
                      trajectory_object = object,
                      velocities,
                      name = 'velocity',
                      overwrite = overwrite)
  }
  
  return(data)
}



