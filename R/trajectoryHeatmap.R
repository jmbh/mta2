############# trajectoryHeatmap ################
#
# INPUT  
# data            N x 3 x Kmatrix, with x, y, and z values of N trajectories with K measurements each
# margins         Numeric vector specifiying the corners (xmin,ymin,xmax,ymax) of the plot region. Note 
#                 that the trajectories start and end points are mapped to (0,0) and (-1,1.5), 
#                 respectivly. Hence, margins should at least include the rectangle c(-1,0,1,1.5).
#                 Defaults to c(-1.4,-.3,1.4,1.9).
# px_size         Integer indicating the size of the px. A pxsize of .01 implies an x-resolution of 
#                 (xmax-xmin)/pxsize and a y-resolution of (ymax-ymin)/pxsize.
# smooth          Character string indicating the choice of smoothing Use 'gauss' for optimal results, 
#                 and 'box' for optimal performance.  blue = 'NA' omits any smoothing.
# radius          Numeric. Indiciating radious for gaussian or box smoothing.
# n_box           Integer. Number of iterations for box smoothing.
# low_pass        Numeric between 0 and 1 specifying the maximum quantile for a low-pass filter.
# enhance         Numeric... 
# overlay         Character. Variable used to overlay the image. If 'none' no variable is selected. 
# n_cluster       Either integer indicating the number of cluster or numeric vector of length 
#                 N indicating the cluster assignments of each trajectory. If ncluster = 1 or
#                 sd(ncluster) == 0 a single plot is created.
# ?clspecs?       Number of data points, cluster method, etc.                 
# n_trajectories  Integer. Number of trajectories. If ntrajctories is smaller than the number of 
#                 trajectories in the dataset, ntrajectories are sampled at random from the data.
# seed            Integer. Seed for random number of generator. Used in trajectory subsampling.
# plot            Boolean. If plot = FALSE, the function plots the heatmap, if plot = FALSE,
#                 the image is returned in either long or wide (matrix) format.
# device          Characte indicating the plot device. Either 'png', 'tiff', 'pdf', or 'none'. If
#                 'none' the current device is used.
# name            Character. Filename.
# directory       Character. Target directory for image file.
#
# OUTPUT
# plot and/or image object
#
# DESCRIPTIO
# Function creates a heatmap of a set of trajectories. Prior to processing the image
# the function aligns the trajectories' start and end points, if needed. Once the 
# trajectories are aligned, the function determines the number of points...
#
#
# 
# 


trajectory_heatmap = function(
  data,
  trajectory_object = 'trajectories', 
  dimensions = c('xpos','ypos'),  
  
  # plot arguments
  name = 'Image',
  directory = getwd(),
  margins   = c(-1.4,-.3,1.4,1.9),
  px_size   = .0005,
  smooth    = 'gauss',
  radius    = 1.5,
  n_box     = 3,
  low_pass  = 100,
  mean_color_value = .2,
  device    = 'tiff',
  
  # overlay arguments
  overlay = NA,
  rev     = TRUE,
  color   = 'blue', 
  
  # subsample arguments
  n_trajectories = 10000,
  seed = 1,
  
  # align arguments
  coordinates = 'mt',
  start = TRUE,
  end   = TRUE,
  
  # clean arguments
  clean = FALSE,
  box_limits = c(2,2),
  sim_limit = 2,
  rescaled_sim = TRUE,
  n_rescale = 50,
  minkowski_p = 2,
  
  # control
  plot = T,
  verbose = TRUE,
  return_img = FALSE
  ){
  
  # ---- get time
  t = proc.time()[3]

  # ---- tests
  if(!any(c(is.na(smooth),smooth %in% c('gauss','box')))) {
    stop("Smooth must be 'gauss', 'box', or NA.")
    }
  if(!any(c(is.na(overlay),overlay %in% c('angleP','angleV','velocity')))) {
    stop("Overlay must be NA, a numeric array or one of 'angleP','angleV', 'velocity'")
    }
  
  # ---- Align data
  # Align start and end points of all trajectories and
  # warp trajectories into common space.
  if(verbose == TRUE) cat('aligning','\n')
  data. = trajectory_align(data, 
                          trajectory_object = trajectory_object, 
                          dimensions = dimensions, 
                          coordinates = coordinates,
                          start = start,
                          end = end
                          )
  
  
  
  # ---- Clean trajectories
  # Remove bad trials if desired
  if((clean == TRUE)){
    if(verbose == TRUE) cat('clean trajectories: ')
    data = trajectory_clean(data,
                            dimensions = dimensions,
                            trajectory_object = trajectory_object,
                            box_limits = box_limits,
                            sim_limit = sim_limit,
                            rescaled_sim = rescaled_sim,
                            n_rescale = n_rescale,
                            minkowski_p = minkowski_p,
                            clean = TRUE,
                            align = FALSE)
                            }
  
  
  # ------------------ Subsample trajectories
  # If ntrajectories is smaller than number of trajectories, 
  # subsample data to ntrajectories
  set.seed(seed)
  if(n_trajectories < dim(data[[trajectory_object]])[1]){
    if(verbose == TRUE) cat('subset tracjectories','\n')  
    condition  = sample(1:dim(data[[trajectory_object]])[1],n_trajectories)
    data = subset_data(data,condition)
    }
  
  # ------------------ Add overlay variables

  if(!is.na(overlay)){
    if(overlay == 'velocity'){
      if(verbose == TRUE) cat('add velocity','\n')
      data = add_velocities(data,dimensions = dimensions, trajectory_object = trajectory_object, overwrite = TRUE)
      }
   
    if(overlay %in% c('angleP','angleV')){
      if(verbose == TRUE) cat('add angles','\n')
      data = add_angles(data,dimensions = dimensions, trajectory_object = trajectory_object, overwrite = TRUE, replace_NAs = TRUE)
      } 
    }
  
  # ------------------ Rescale trajectories
  # Rescale trajectories so that there are sufficient points to fill
  # the diagonal, i.e, length of diagonal divided by px
  
  if(verbose == TRUE) cat('rescale trajectories','\n')
  
  # Determine number of resc
  xres   = round((margins[3] - margins[1]) / px_size)
  yres   = round((margins[4] - margins[2]) / px_size)
  n_resc = ceiling(sqrt(xres * xres + yres * yres) / sqrt(2))
  l_diag = sqrt((margins[3] - margins[1])**2+(margins[4] - margins[2])**2)
  
  # Determine number of resc
  data = add_lengths(data,
                     dimensions = dimensions,
                     trajectory_object = trajectory_object,
                     overwrite = TRUE)
  n_points = round(n_resc * data$data$lengths / l_diag)
  if(is.na(overlay))  data = spatial_rescale(data,dimensions = dimensions,n_points, format = 'long')
  if(!is.na(overlay)) data = spatial_rescale(data,dimensions = c(dimensions,overlay),n_points, format = 'long')
  
  
  # ------------------ Compute raw image
  # compute raw image
  
  if(verbose == TRUE) cat('calculate image','\n')
  
  # retrieve image
  pts = data$rescaled_trajectories_long
  
  # range of pixels plus white space around image
  xs = 0 : (xres + radius * 4) + 1
  ys = 0 : (yres + radius * 4) + 1
  
  # Cut trajectory points to margins
  pts = pts[pts[,1] >= margins[1] &
            pts[,1] <= margins[3] &
            pts[,2] >= margins[2] &
            pts[,2] <= margins[4],]
  
  # Determine pixel locations
  x  = round(((pts[,1] - margins[1]) / px_size) + 1) 
  y  = round(((pts[,2] - margins[2]) / px_size) + 1) 
  
  # Determine table of pixels
  img_df = data_frame('x'=x,'y'=y)
  img_tb = img_df %>% group_by(x,y) %>% tally() %>% ungroup() 
  
  
  # Map pixels into matrix of zeros
  img_mat = matrix(0,ncol = length(xs),nrow = length(ys))
  img_mat[as.matrix(img_tb[,2:1]) + radius * 2] = img_tb$n
  
  # Store raw image, pixel locations and 
  raw_img = c(t(img_mat))
  xys     = expand.grid(1:length(xs),1:length(ys))
  
  # Calculate time information and create image
  if(!is.na(overlay)){
    img_df = data_frame('x'=x,'y'=y,'a'=pts[,3])
    img_tb = img_df %>% group_by(x,y) %>% summarize(a = min(a)) %>% ungroup() 
    
    img_mat = matrix(0,ncol = length(xs),nrow = length(ys))
    img_mat[as.matrix(img_tb[,2:1]) + radius * 2] = img_tb$a
    
    add_img = c(t(img_mat))
    
  } else {
    add_img = rep(1, length(raw_img))   
  }
  

  # ------------------ Smooth image
  # smooth image

  smooth_img = raw_img
  if(!is.na(smooth)){
    if(verbose == TRUE) cat('smooth image','\n')
    # apply gaussian smoothing
    if(smooth == 'gauss'){
      smooth_img = gaussBlur(smooth_img,smooth_img,max(xs),max(ys),radius)
      if(!is.na(overlay)) add_img = gaussBlur(add_img,add_img,max(xs),max(ys),radius)
    }
    # apply repeated box smoothing
    if(smooth == 'box'){
      for(i in 1:n_box){
        smooth_img = boxBlur(smooth_img,smooth_img,max(xs),max(ys),radius)
        if(!is.na(overlay)) add_img = gaussBlur(add_img,add_img,max(xs),max(ys),radius)
      }
    }
  }
  
  
  # ------------------ create, normalize, and enhance image image
  # Low-pass: shave off max color intensities
  # Enhance contrast
  # Normalize image
  
  if(verbose == TRUE) cat('enhance image','\n')
  
  # create image object
  img = data.frame(xys,smooth_img,add_img) ; names(img) = c('x','y','img','a')
  
  # Remove all zero points
  img = img[img$img>0,]
  
  # Low - pass
  img$img[img$img > low_pass] = low_pass
  
  # Normalize image
  img$img = img$img - min(img$img) ; img$img = img$img / max(img$img)
  if(!is.na(overlay)){
    img$a = img$a - min(img$a) ; img$a = img$a / max(img$a)
    }
  
  # enhance image
  ms = c(); steps = c(.1,2,5,10,20,50,100,180,300,500)
  for(i in steps){
      ms = c(ms,mean(abs(abs(img$img-1) ** i - 1))); 
      }
  enhance = splinefun(ms,steps)(mean_color_value)
  if(enhance > max(steps)) stop('mean_color_value too high')
  img$img = abs(abs(img$img-1) ** enhance - 1)
  
#   
#   sel = sample(1:nrow(img),10000)
#   plot(img$y[sel],img$a[sel],col=rgb(0,0,0,alpha=.1),pch=16)
#   lines(splinefun(img$y[sel],img$a[sel])(1:max(img$y[sel])),col='red')
#   
#   
  # ------------------ Plot
  # Plot or return image
  
  if(verbose == TRUE) cat('creating heatmap: ', max(img$x), 'x',max(img$y), 'px','\n')
  
  if(plot == T){
    if(device == 'pdf'){
      pdf(paste0(directory,'/',name,'.pdf'),width = 10 * (max(img$x)/max(img$y)),height = 10)
      }
    if(device == 'png'){
      tiff(paste0(directory,'/',name,'.png'),width = max(img$x),height = max(img$y))
      }
    if(device == 'tiff'){
      tiff(paste0(directory,'/',name,'.tiff'),width = max(img$x),height = max(img$y))
      }
    
    xs_range = range(xs)
    ys_range = range(ys)
    plot.new();par(mar=c(0,0,0,0));plot.window(xlim=xs_range+c(-.5,.5),
                                               ylim=ys_range+c(-.5,.5))

    # determien colors
    f   = img$img
    if(rev == FALSE) a = img$a else a = 1-img$a
    if(color == 'red')   red   = (1 - f * a) else  red   = (1 - f)
    if(color == 'green') green = (1 - f * a) else  green = (1 - f)
    if(color == 'blue')  blue  = (1 - f * a) else  blue  = (1 - f)
    cols = rgb(red,green,blue)
    rect(img$x - .5,img$y - .5,
         img$x + .5,img$y + .5, col=cols,
         border=NA)
    
    if(device %in% c('pdf','png','tiff')) dev.off()
    
  } else {
    
    return(img)
  }
  
  
  # Give feedback
  t = proc.time()[3] - t
  cat('heatmap created in ',round(t),'s',sep='')
  
  if(return_img == TRUE) return(img)
  }

