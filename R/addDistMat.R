############# dist mat ################
#
# INPUT  
# 
#
# OUTPUT
# 
#
# DESCRIPTIO
#

add_dist_mat = function(data,
                    dimensions = c('xpos','ypos'),
                    trajectory_object = 'trajectories',
                    point_wise = TRUE,
                    power = 2
                    ){
  
  # ---- tests
  n_dim   = length(dimensions)
  d_names = dimnames(data[[trajectory_object]])[[2]]
  if(!all(dimensions %in% d_names)) stop('Dimensions not existend') 
  if(!n_dim %in% c(2,3)) stop('Dimensions must of length 2 or 3') 
  
  # ---- get distances
  if(n_dim == 2){
    if(point_wise == TRUE){
        dmat = distMat(data[[trajectory_object]][,dimensions[1],],
                       data[[trajectory_object]][,dimensions[2],],
                       power = power)
      } else {
        dmat = distMatV(data[[trajectory_object]][,dimensions[1],],
                        data[[trajectory_object]][,dimensions[2],],
                        power = power)
      } 
    } else {
      if(point_wise == TRUE){
          dmat = distMat3d(data[[trajectory_object]][,dimensions[1],],
                           data[[trajectory_object]][,dimensions[2],],
                           data[[trajectory_object]][,dimensions[3],])          
        } else {
          dmat = distMat3dV(data[[trajectory_object]][,dimensions[1],],
                            data[[trajectory_object]][,dimensions[2],],
                            data[[trajectory_object]][,dimensions[3],],
                            power = power)        
      }
    }

  # ---- add distances
  data$distances = dmat
 
  
  return(data)
  }


