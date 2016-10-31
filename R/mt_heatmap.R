 #' Creates high-resolution heatmap of trajectory data
#'
#' \code{mt_heatmap} creates a high-resolution heatmap of the trajectory
#'   data using gaussian smoothing.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory
#'   array can be provided directly (in this case \code{use} will be ignored).
#' @param use a character string specifying which trajectory object should be
#'   used.
#' @param dimensions a character vector specifying the trajectory variables
#'   used to create the heatmap. The first two are used as x and y-coordinates,
#'   the third, if provided, will be added as color information-.
#' @param device a character specifying the plotting device. Either \code{tiff},
#'   \code{png}, or \code{pdf}. Defaults to \code{tiff}.
#' @param name a character string specifying the filename
#' @param directory a character string specifying the directory for the image
#'   file. Defaults to \code{get.wd()}.
#' @param margins numeric vector specifiying the corners (xmin,ymin,xmax,ymax) of
#'   the plot region. Note that the trajectories start and end points are mapped to
#'   (0,0) and (-1,1.5), respectivly. Margins should therefore include the rectangle
#'   c(-1,0,1,1.5). Defaults to c(-1.4,-.3,1.4,1.9).
#' @param px_size an integer specifying the size of px. A px_size of .01 implies an
#'   x-resolution of (xmax-xmin)/px_size and a y-resolution of (ymax-ymin)/px_size.
#'   Defaults to .001 implying an image of 2800x2200 px.
#' @param upscale a numeric by which the output resolution of the image is increased or
#'   decreased. Only applies if \code{device} is one of \code{c("tiff","png","pdf")}.
#' @param upscale a numeric by which the number of points used to represent individual
#'   trajectories are increased or decreased. Values of smaller than one will improve speed
#'   but also introduce a certain level of granularity.
#' @param smooth_radius a numeric specifying the standard deviation of the gaussian smoothing.
#'   If zero smoothing is omitted.
#' @param low_pass an integer specifying allowed number of counts per pixel. This
#'   arguments limits the maximum pixel color intensity.
#' @param mean_intensity a numeric between 0 and 1 specifying the average color intensity
#'   across the entire image. Defaults to .2.
#' @param color a character string or numeric vector of length three specifying
#'   the color used to plot the third dimension.
#' @param color_order a character string, either "increasing" or "decreasing" specifying the
#'   whether large or small values of the third dimension are assigned high color values,
#'   respectively.
#' @param n_trajectories an integer specifying the number of trajectories used to create the
#'   image. If \code{n_trajectories} is smaller than containes in the trajectorie object
#'   specified by \code{use} the \code{n_trajectories} are randomly sampled.
#' @param seed an integer specifying the seed used for the trajectory sampling.
#' @param plot boolean specifying whether the image should be plotted. If \code{plot} is
#'   \code{FALSE}, then the image will be returned.
#' @param verbose boolean specifying whether progress updates should printed.


mt_heatmap = function(
  data,
  use = 'trajectories',
  dimensions = c('xpos','ypos','vel'),

  # plot arguments
  name = 'Image',
  directory = getwd(),
  margins   = c(-1.4,-.3,1.4,1.9),
  px_size   = .001,
  upscale   = 1,
  upsample  = 1,
  smooth_radius    = 1.5,
  low_pass  = 200,
  mean_intensity = .1,
  device    = 'tiff',

  # color arguments
  colors      = c('black','blue'),
  n_shades    = c(1000,10),

  # plot aggregate
  aggregate_lwd = 0,
  aggregate_col = 'black',

  # subsample arguments
  n_trajectories = 10000,
  seed = 1,

  # control
  plot = TRUE,
  verbose = TRUE
  ){

  # ---- get time
  t = proc.time()[3]



  # ---- tests
  if(!length(dimensions) %in% c(2,3)){
    stop('Dimensions must of length 2 or 3!')
    }
  if(is.list(data)){
    trajectories = data[[use]]
  } else {
    if(is.array(data)){
      trajectories = data
      } else {
      stop('Data must be array or list!')
      }
  }
  if(!all(dimensions %in% dimnames(trajectories)[[2]])) stop('Not all dimensions exist')


  # ------------------ Subsample trajectories
  # If ntrajectories is smaller than number of trajectories,
  # subsample data to ntrajectories
  if(n_trajectories < dim(trajectories)[1]){
    if(verbose == TRUE) cat('subset tracjectories','\n')
    trajectories = subsample(data,n = n_trajectories,seed = seed)
    }

  # ------------------ Get aggregate
  # Compute aggregate x and y
  aggregate = cbind(colMeans(trajectories[,dimensions[1],]),
                    colMeans(trajectories[,dimensions[2],]))

  # ------------------ Rescale trajectories
  # Rescale trajectories so that there are sufficient points to fill
  # the diagonal, i.e, length of diagonal divided by px

  if(verbose == TRUE) cat('spatializing trajectories','\n')

  # Determine number of resc
  xres   = round((margins[3] - margins[1]) / px_size)
  yres   = round((margins[4] - margins[2]) / px_size)
  n_resc = ceiling(sqrt(xres * xres + yres * yres))
  l_diag = sqrt((margins[3] - margins[1])**2+(margins[4] - margins[2])**2)

  # Determine number of resc
  lengths  = mta2:::getLengths(trajectories[,dimensions[1],],trajectories[,dimensions[2],])
  n_points = round(n_resc * lengths / l_diag)
  n_points = n_points * upsample
  if(length(dimensions) == 2){
    spatialized_trajectories = mt_spatialize(trajectories,
                                             dimensions = dimensions,
                                             n_points = n_points,
                                             format = 'long')}

  if(length(dimensions) == 3){
    spatialized_trajectories = mt_spatialize(trajectories,
                                             dimensions = c(dimensions),
                                             n_points = n_points,
                                             format = 'long')}
  if(aggregate_lwd > 0){
    agg_x = aggregate[,1]
    agg_y = aggregate[,2]
    agg_l = mta2:::getLength(agg_x,agg_y)
    spatialized_aggregate = mta2:::spatialize(agg_x,agg_y,round(upscale * n_resc * agg_l / l_diag))
    }


  # ------------------ Compute raw image
  # compute raw image

  if(verbose == TRUE) cat('calculate image','\n')

  # retrieve image
  pts = spatialized_trajectories

  # range of pixels plus white space around image
  xs = 0 : (xres + ceiling(smooth_radius * 4)) + 1
  ys = 0 : (yres + ceiling(smooth_radius * 4)) + 1

  # Remove points outside of margins
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
  img_mat[as.matrix(img_tb[,2:1]) + smooth_radius * 2] = img_tb$n

  # Store raw image, pixel locations and
  raw_img = c(t(img_mat))
  xys     = expand.grid(1:length(xs),1:length(ys))

  # Calculate overlay information and create ovrlay image
  if(length(dimensions) == 3){
    a = pts[,3]
    if(any(is.na(a))){
      a[is.na(a)] = 0
      message('NAs replaced by 0')
      }
    img_df = data_frame('x'=x,'y'=y,'a'=a)
    img_tb = img_df %>% group_by(x,y) %>% summarize(a = mean(a)) %>% ungroup()
    img_mat = matrix(0,ncol = length(xs),nrow = length(ys))
    img_mat[as.matrix(img_tb[,2:1]) + smooth_radius * 2] = img_tb$a
    add_img = c(t(img_mat))
    } else {
    add_img = rep(1, length(raw_img))
    }


  # get aggregate points
  if(aggregate_lwd > 0){
    agg_x = round(((spatialized_aggregate[,1] - margins[1]) / px_size) + 1) + smooth_radius * 2
    agg_y = round(((spatialized_aggregate[,2] - margins[2]) / px_size) + 1) + smooth_radius * 2
    test  = agg_x > 0 & agg_x <= max(xs) & agg_y > 0 & agg_y <= max(ys)
    agg_x = agg_x[test]
    agg_y = agg_y[test]
    agg   = data.frame('x'=agg_x,'y'=agg_y,'col'=aggregate_col,'lwd'=aggregate_lwd)
    }

  # ------------------ Smooth image
  # smooth image

  smooth_img = raw_img
  if(smooth_radius > 0){
    if(verbose == TRUE) cat('smooth image','\n')
    smooth_img = mta2:::gaussBlur(smooth_img,smooth_img,max(xs),max(ys),smooth_radius)
    if(length(dimensions) == 3) add_img = mta2:::gaussBlur(add_img,add_img,max(xs),max(ys),smooth_radius)
    }


  # ------------------ create, normalize, and enhance image image
  # Low-pass: shave off max color intensities
  # Enhance contrast
  # Normalize image

  # create image object
  img = data.frame(xys,smooth_img,add_img) ; names(img) = c('x','y','img','a')

  # Low - pass
  img$img[img$img > low_pass * upsample] = low_pass  * upsample

  # Normalize image
  img$img = img$img - min(img$img) ; img$img = img$img / max(img$img)
  if(length(dimensions) == 3){
    img$a = img$a - min(img$a) ; img$a = img$a / max(img$a)
    }

  # enhance image
  ms = c(); steps = c(.1,2,5,10,20,50,100)
  for(i in steps){
      ms = c(ms,mean(abs(abs(img$img-1) ** i - 1)));
      }
  enhance = splinefun(ms,steps)(mean_intensity)
  if(enhance > max(steps)) enhance = max(steps)
  if(verbose == TRUE) print(enhance)
  img$img = abs(abs(img$img-1) ** enhance - 1)

  if(verbose == TRUE) cat('enhance image by',round(enhance,1),'\n')

  # ------------------ determine colors

  # determine colors
  img$img   = mousetrap:::group(img$img,n_shades[1])
  bg  = par()$bg ; if(bg == 'transparent') bg = 'white'
  if(length(dimensions) == 2){
    img$col     = mousetrap:::colormixer(bg,colors[1],img$img,format = 'hex')
    }
  if(length(dimensions) == 3){
    if(length(colors) < 2 | length(n_shades) < 2) stop('Colors and n_shades must be of length 2')
    img$a   = mousetrap:::group(img$a,n_shades[2])
    color_tone  = mousetrap:::colormixer(colors[1],colors[2],img$a)
    img$col     = mousetrap:::colormixer(bg,color_tone,img$img,format = 'hex')
    }


  # ------------------ Plot
  # Plot or return image

  if(verbose == TRUE) cat('creating heatmap: ', max(img$x), 'x',max(img$y), 'px','\n')

  if(plot == T){
    if(device == 'pdf'){
      pdf(paste0(directory,'/',name,'.pdf'),
          width = 10 * (max(img$x)/max(img$y)) * upscale,
          height = 10 * upscale)
          }
    if(device == 'png'){
      tiff(paste0(directory,'/',name,'.png'),
           width = max(img$x) * upscale,
           height = max(img$y) * upscale)
           }
    if(device == 'tiff'){
      tiff(paste0(directory,'/',name,'.tiff'),
           width = max(img$x) * upscale,
           height = max(img$y) * upscale)
           }

    # remove 0s
    p_img = img
    p_img = p_img[img$img>0,]


    xs_range = range(xs)
    ys_range = range(ys)
    plot.new();par(mar=c(0,0,0,0));plot.window(xlim=xs_range+c(-.5,.5),
                                               ylim=ys_range+c(-.5,.5))

    # plot points
    rect(p_img$x - .5,p_img$y - .5,
         p_img$x + .5,p_img$y + .5, col = p_img$col,
         border=NA)

    if(aggregate_lwd > 0) lines(agg[,1:2],col=agg[1,3],lwd=agg[1,4])

    if(device %in% c('pdf','png','tiff')) dev.off()
    }


  # Give feedback
  t = proc.time()[3] - t
  if(verbose == T) cat('heatmap created in ',round(t),'s\n',sep='')

  if(plot == FALSE){
    if(aggregate_lwd > 0){
      return(list(img[,c('x','y','img','col')],agg))
      } else {
      return(img[,c('x','y','img','col')])
      }
    }
  }

