############# trajectoryViewer ################
#
# INPUT  
# data            N x 3 x Kmatrix, with x, y, and z values of N trajectories with K measurements each
# name            Character. Filename.
# directory       Character. Target directory.
# ncol            Interger. Number of columns for plot layout.          
# nrow            Interger. Number of rows for plot layout.  
# margins         Numeric vector. Canvas dimensions.
# flags           Character vector indicating for what properties the data should be analyzed. Can be
#                 'CoM', 'Outlier', 
# align           Character vector. Either 'start', 'end, or both.
# order           Character vector. Variable names passed to order() to sort trajectories.


trajectory_viewer = function(data,
                             ncol = 6, 
                             nrow = 12,
                             dimensions        = c('xpos','ypos'),
                             trajectory_object = 'trajectories',
                             
                             # plot arguments
                             device     = '',
                             filename   = 'trajectories',
                             directory  = getwd(),
                             n_pages    = 5,
                             margins    = c(20,20,20,20),
                             sort_by    = c('mean_sim','max_dist'),
                             sort_order = c('increasing','decreasing'),
                             show       = c('points','time'),
                             info       = c('id','clean','CoM'),
                             cex        = .2,
                             lwd        = 1,
                             
                             # rescale arguments
                             rescale   = FALSE,
                             n_rescale = 50,
                             
                             # clean arguments
                             box_limits    = c(2,2),
                             sim_limit     = 2,
                             rescaled_sim  = FALSE,
                             clean_rescale = 20,
                             minkowski_p   = 2,
                             
                             # CoM arguements
                             limits          = c(-3,-2,-3,-2),
                             threshold_dist  = 30,
                             threshold_angle = 50,
                             
                             # Control arguments
                             rerun       = FALSE,
                             return_data = FALSE,
                             verbose     = FALSE
                             ){

  # ---- define variable names
  id_variables = c('ptp','trial')
  clean_variables = c('box_clean','sim_clean','max_dist','mean_sim')
  CoM_variables = c('dist_CoM','angle_CoM','min_angle','max_x_dist')

  
  # ---- data align
  if(verbose == TRUE) cat('aligning','\n')
  data = trajectory_align(data, 
                          trajectory_object = trajectory_object, 
                          dimensions = dimensions, 
                          coordinates = 'norm')
  
  
  # ---- trajectory clean
  if(('clean' %in% info | any(clean_variables %in% sort_by)) &
    (!all(clean_variables %in% names(data$data)) | rerun == TRUE)){
    if(verbose == TRUE) cat('clean trajectories','\n')
    data = trajectory_clean(data,
                            dimensions = dimensions,
                            trajectory_object = trajectory_object,
                            box_limits = box_limits,
                            sim_limit = sim_limit,
                            rescaled_sim = rescaled_sim,
                            n_rescale = n_rescale,
                            minkowski_p = minkowski_p,
                            clean = FALSE,
                            align = FALSE)
                            }
  
  
  # ---- detect CoM
  if(('CoM' %in% info | any(CoM_variables %in% sort_by)) & 
     (!all(CoM_variables %in% names(data$data)) | rerun == TRUE)){
    if(verbose == TRUE) cat('detect CoM','\n')
    data = detect_CoM(data,
                      dimensions = dimensions,
                      trajectory_object = trajectory_object,
                      threshold_dist = threshold_dist, 
                      threshold_angle = threshold_angle,
                      align = FALSE)
                      }

    
  # ---- add velocity
  if('velocity' %in% show & 
     (!'velocity' %in% dimnames(data[[trajectory_object]])[[2]] | rerun == TRUE)){
    if(verbose == TRUE) cat('add velocity','\n')
    data = add_velocities(data,dimensions = dimensions, trajectory_object = trajectory_object, overwrite = FALSE)
    }

        
  # ---- rescale
  if(rescale == TRUE){
    if(verbose == TRUE) cat('rescale trajectories','\n')
    data = add_lengths(data,
                       dimensions = dimensions,
                       trajectory_object = trajectory_object,
                       overwrite = TRUE)
    n_points = round(n_rescale * data$data$lengths / sqrt(2))
    if(!'velocity' %in% show) data = spatial_rescale(data,dimensions = dimensions,n_points)
    if('velocity' %in% show) data = spatial_rescale(data,dimensions = c(dimensions,'velocity'),n_points)
    trajectory_object = 'rescaled_trajectories'
    }

  
  # ---- order data
  if(all(sort_by %in% names(data$data)) & length(sort_by) > 0){
    if(verbose == TRUE) cat('order data','\n')
    for(i in length(sort_by):1){
      data[[trajectory_object]] = 
        data[[trajectory_object]][order(data$data[,sort_by[i]],decreasing = ifelse(sort_order[i]=='decreasing',T,F)),,]
      data$data = data$data[order(data$data[,sort_by[i]],decreasing = ifelse(sort_order[i]=='decreasing',T,F)),]    
      }
    } else {
    if(length(sort_by) > 0) stop('Not all sort variables exist')
    }
  
  # ---- plot
  
  # wait for plot function
  wait_for_enter = function(){cat ("Press [enter] to continue");line <- readline(); if(line == 'q') break}
  
  # layout
  par(mfrow = c(nrow,ncol))
  xlim = c(-1 - margins[1]/100,1   + margins[3]/100)
  ylim = c( 0 - margins[2]/100,1.5 + margins[4]/100 + .1)
  n_trajectories = dim(data[[trajectory_object]])[1]
  
  # device
  if(device == 'pdf') {
    pdf(paste0(directory,name,'.pdf'),width = ncol, height = nrow)
    } else {
    n_trajectories = (nrow * ncol) * 5  
    }

  # loop over trajectories  
  for(i in 1:n_trajectories){
  
    # begin plot
    par(mar = c(0,0,0,0))
    plot.new(); plot.window(xlim = xlim, ylim = ylim)

    # get x and y points
    x = -data[[trajectory_object]][i,dimensions[1],]
    y =  data[[trajectory_object]][i,dimensions[2],] * 1.5

    
    # determine colors
    ind = which(!is.na(data[[trajectory_object]][i,'xpos',]))    
    if('time' %in% show) red  = seq(0,1,length = length(ind)) else red = 0
    if('velocity' %in% show) {
      velo = data[[trajectory_object]][i,'velocity',ind]
      blue = velo / max(velo)
      } else { blue = 0 }
    col = rgb(red,0,blue)
      
  
    # plot lines
    if('line' %in% show){
      if(any(c('time','velocity') %in% show))  for(j in 1:(length(x)-1)) lines(c(x[j],x[j+1]),c(y[j],y[j+1]),lwd=2,col=col[j])
      if(!any(c('time','velocity') %in% show)) lines(x,y,lwd=lwd)
      }

    # plot points    
    if('points' %in% show){
      if('line' %in% show)  points(x,y,col=col,cex=cex,lwd=lwd,pch=21,bg='white')
      if(!'line' %in% show) points(x,y,col=col,cex=cex,lwd=lwd)    
      }
    
    # add ID  
    if('id' %in% info){
      text(xlim[1]+.5,ylim[1]+.05,cex=.6,font=1,adj=0,
           labels=paste0(data$data$ptp[i],'  ',data$data$trial[i]))
           }
  
    # add clean  
    if('clean' %in% info) {
      clean_names  = c('box_clean','sim_clean')
      clean_values = data$data[i,clean_names]
      rect(c(xlim[1],xlim[1]+.1),c(ylim[1],ylim[1]),c(xlim[1]+.1,xlim[1]+.2),c(ylim[1]+.1,ylim[1]+.1),
           border='grey90',col = rgb(1-clean_values,1-clean_values,1-clean_values))
          }
    
    # add clean  
    if('CoM' %in% info) {
      CoM_names  = c('dist_CoM','angle_CoM')
      CoM_values = data$data[i,CoM_names]
      rect(c(xlim[1]+.25,xlim[1]+.35),c(ylim[1],ylim[1]),c(xlim[1]+.35,xlim[1]+.45),c(ylim[1]+.1,ylim[1]+.1),
           border='grey90',col = rgb(1-CoM_values,1-CoM_values,1-CoM_values))
           }

    # add box
    if('box' %in% show){
      box(lwd=.5,col='black')
      }
    
    # wait for enter
    if(device != 'pdf' & i %% (nrow*ncol) == 0) wait_for_enter()
    
    }
  
  # close device
  if(device == 'pdf') dev.off()
  if(return_data) return(data)
}

