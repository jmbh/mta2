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

trajectory_clean = function(data,
                            dimensions = c('xpos','ypos'),
                            trajectory_object = 'trajectories',
                            box_limits = c(2,2),
                            sim_limit  = 2,
                            rescaled_sim = TRUE,
                            n_rescale = 20,
                            minkowski_p = 2,
                            clean = FALSE,
                            clean_variable = 'union',
                            align = TRUE
                            ){
  
  # ---- tests
  n_dim = length(dimensions)
  if(!trajectory_object %in% names(data)) stop(paste0(trajectory_object,' does not exist.'))
  if(!all(dimensions %in% dimnames(data[[trajectory_object]])[[2]])) stop(paste0('Not all dimensions exist!'))
  if(any(is.na(data[[trajectory_object]]))) stop('Trajectory object contains NAs.')
  
  # ---- align data
  if(align == TRUE){
    data_tmp = trajectory_align(data, 
                                trajectory_object = trajectory_object, 
                                dimensions = dimensions, 
                                coordinates = 'norm')
  } else {
    data_tmp = data  
  }
  
  # ---- box clean
  # Remove trajectories that have points beyond box.xlim and box.ylim
  max_dist = c()
  box_clean = rep(FALSE,dim(data_tmp[[trajectory_object]])[1])
  for(i in 1:n_dim){
    max_dist_tmp = apply(data_tmp[[trajectory_object]][,dimensions[i],],1,function(x) max(abs(x),na.rm=TRUE))
    max_dist = apply(cbind(max_dist,max_dist_tmp),1,function(x) x[which.max(x)])
    box_clean = box_clean | max_dist > box_limits[i]
    }

  
  # ---- sim clean
  # determine average similarity of trajectory to all other trajectories
  if(rescaled_sim == TRUE){
    data_tmp = spatial_rescale(data_tmp,n_rescale)
    data_tmp = add_dist_mat(data_tmp, dimensions = dimensions, trajectory_object = 'rescaled_trajectories', power = minkowski_p)
  } else {
    data_tmp = add_dist_mat(data_tmp, dimensions = dimensions, trajectory_object = 'trajectories', power = minkowski_p)      
    }
  mean_sim  = scale(-apply(data_tmp$distances,1,mean))[,1]
  sim_clean = mean_sim < -sim_limit
  

  # ---- add clean
  # join and add clean variables
  data$data$box_clean       = box_clean
  data$data$sim_clean       = sim_clean
  data$data$max_dist        = max_dist  
  data$data$mean_sim        = mean_sim  
  
  if(clean == TRUE){
    if(clean_variable == 'box')            cleaner = box_clean
    if(clean_variable == 'sim')            cleaner = sim_clean
    if(clean_variable == 'union')          cleaner = box_clean | sim_clean
    if(clean_variable == 'intersection')   cleaner = sim_clean & box_clean
    for(nam in names(data)){
      if(length(dim(data[[nam]])) == 2) data[[nam]] = data[[nam]][!cleaner,]  
      if(length(dim(data[[nam]])) == 3) data[[nam]] = data[[nam]][!cleaner,,]       
      }
    cat(paste0(sum(cleaner), ' trials removed!'),'\n')
    } 
  
  return(data)
  }

