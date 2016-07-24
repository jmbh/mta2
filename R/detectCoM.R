############# trajectoryViewer ################
#
# INPUT  
# data                  Mousetrap data object
# type                  Character indicating the type of change of mind detection. Either 'position' or 'direction'
# threshold_position    Numeric. Percent of distance from start to non-chosen option. Defaulst to 50.
# threshold_direction   Numeric. Angle from 0 to 180 indicating change in direction. Defaults to 160.



detect_CoM = function(data,
                     dimensions = c('xpos','ypos'),
                     trajectory_object = 'trajectories',
                     threshold_dist = 30,
                     threshold_angle = 50,
                     align = TRUE) {

  # ---- Tests
  if(!trajectory_object %in% names(data)) stop(paste0(trajectory_object,' does not exist!'))
  if(!all(dimensions %in% dimnames(data[[trajectory_object]])[[2]])) stop(paste0('Not all dimensions exist!'))
  
    
  # ---- align data
  if(align == TRUE){
    data_tmp = trajectory_align(data, 
                                trajectory_object = trajectory_object, 
                                dimensions = dimensions, 
                                coordinates = 'norm')
    } else {
    data_tmp = data  
    }
  
  # ---- distance based detection
  max_x_dists = apply(data_tmp[[trajectory_object]][,'xpos',],1,function(x) max(-x))
  dist_CoM  = max_x_dists > -threshold_dist/100
  
  # ---- direction based detection
  if(!all(c('anglesP','anglesV') %in% dimnames(data_tmp[[trajectory_object]]))){
    data_tmp = add_angles(data_tmp,
                          dimensions = dimensions,
                          trajectory_object = trajectory_object,
                          overwrite = FALSE)
    }
  min_angles = apply(data_tmp[[trajectory_object]][,'anglesP',],1,function(x) min(x*(180/pi),na.rm=T))
  angle_CoM  = min_angles < threshold_angle
  
  
  # ---- add CoM
  data$data$dist_CoM      = dist_CoM
  data$data$angle_CoM     = angle_CoM
  data$data$min_angle     = min_angles  
  data$data$max_x_dist    = max_x_dists
  
  return(data)
  }


