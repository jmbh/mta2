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

add_lengths = function(data,
                       dimensions = c('xpos','ypos'),
                       trajectory_object = 'trajectories',
                       overwrite = TRUE
                       ){

  # ---- tests
  n_dim = length(dimensions)
  if(!n_dim %in% c(2,3)) stop('Dimensions must of length 2 or 3') 
  if(!trajectory_object %in% names(data)) stop(paste0(trajectory_object,' does not exist!'))
  if(!all(dimensions %in% dimnames(data[[trajectory_object]])[[2]])) stop(paste0('Not all dimensions exist!'))
  
  # ---- get lengths
  if(n_dim == 2){
    lengths = getLengths(data[[trajectory_object]][,dimensions[1],],data[[trajectory_object]][,dimensions[2],])      
    }
  if(n_dim == 3){
    lengths = getLengths3d(data[[trajectory_object]][,dimensions[1],],data[[trajectory_object]][,dimensions[2],],data[[trajectory_object]][,dimensions[3],])      
    }
  
  # ---- add lengths
  if('lengths' %in% names(data$data) & overwrite == FALSE){
    stop('lengths already exist')
    } else{
    data$data$lengths = lengths    
    }
  
  return(data)
  }



