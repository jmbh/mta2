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

add_angles = function(data,
                      dimensions = c('xpos','ypos'),
                      trajectory_object = 'trajectories',
                      overwrite = FALSE,
                      replace_NAs = FALSE
                      ){
  
  # ---- Iterate over trajectory objects
  for(object in trajectory_object){
  
  # ---- tests
  n_dim = length(dimensions)
  if(!n_dim %in% c(2,3)) stop('Dimensions must of length 2') 
  if(!object %in% names(data)) stop(paste0(object,' does not exist!'))
  if(!all(dimensions %in% dimnames(data[[object]])[[2]])) stop(paste0('Not all dimensions exist!'))
  
  # ---- get angles
  anglesP = getAnglesP(data[[object]][,dimensions[1],],
                       data[[object]][,dimensions[2],])
  anglesV = getAnglesV(data[[object]][,dimensions[1],],
                       data[[object]][,dimensions[2],])

  
  if(replace_NAs == TRUE){
    cleanAngles(anglesP) 
    cleanAngles(anglesV)
    }
  
  # ---- set NAs
  anglesP[anglesP == -100] = NA
  anglesV[anglesV == -100] = NA
  

  # ---- add angles
  data = add_column(data,
                    trajectory_object = object,
                    anglesP,
                    name = 'angleP',
                    overwrite = overwrite)
  data = add_column(data,
                 trajectory_object = object,
                 anglesV,
                 name = 'angleV',
                 overwrite = overwrite)
  
  }
  
  return(data)
  }



