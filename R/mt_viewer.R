#' Illustrates individual trajectories
#'
#' \code{mt_viewer} plots individual trajectories in a specified order including
#'   trajectory meta-information, e.g., whether a trial is identified as a change of
#'   mind trial.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory
#'   array can be provided directly (in this case \code{use} will be ignored).
#' @param use a character string specifying which trajectory object should be
#'   used.
#' @param dimensions a character string specifying which trajectory variables
#'   should be used.
#' @param ids a list of character or numeric vectors containing (short) identifying
#'   labels for each trajectory.
#' @param labels a list of boolean vectors describing the absence or presence of
#'   a property of interest.
#' @param sort_variables a list of character or numeric vectors used to sort the
#'   trajectories, respecting the specific order of sort_variables. If \code{NULL}
#'   the \code{labels} argument will be used to sort the trajectories.
#' @param decreasing a boolean vector matching the length of either sort_variables
#'   or labels (depending on whether sort_variables is specified) which specifies
#'   for each variable the whether the trajectories should be sorted in an increasing
#'   or decreasing fashion.
#' @param overlay a character string specifying the variable used to color the
#'   trajectory.
#' @param filename a character string specifying the filename the to be created pdf image.
#'   If NULL the RStzdi device will be used. In this case \code{directory} will be ignored.
#'   If not NULL \code{n_pages} will be ignored
#' @param directory a character string specifying the directory where the file should
#'   saved.
#' @param pagewise a boolean specifying whether the program should pause after filling
#'   \code{nrow} times \code{ncol} plots and wait for input.
#' @param nrow an integer specifying the number of panels per column.
#' @param ncol an integer specifying the number of panels per row.
#' @param margins an integer vector of length four specifying the bottom, left, upper,
#'   and right margins in terms of percent of the distance between the trajectories'
#'   start and end points.
#' @param pch plotting character passed on to \code{points()}.
#' @param cex chracter size passed on to \code{points().
#' @param lwd line width passed on to \code{lines()}.
#' @param plot_box boolean specifying whether a box should be drawn to outline each
#'   panel.
#' @param plot_index a boolean specifying whether a trajectory should be
#'   colored according to the indices of the trajectory points.
#' @param spatialize a boolean specifying whether the trajectories should be
#'   spatialized (see \link{mt_spatialize}). Spatialization helps visualize
#'   the trajectory's shape, but loses the trajectory's time information.
#' @param n_points an integer specifying the number of points on the spatialized
#'   trajectory.
#' @param start boolean specifying wheter the trajectorie's start points should
#'   be aligned.
#' @param end boolean specifying wheter the trajectorie's end points should
#'   be aligned.

mt_viewer = function(data,
                     use = 'trajectories',
                     dimensions = c('xpos','ypos'),

                     # additional variables
                     ids = NULL,
                     labels = NULL,
                     sort_variables = NULL,
                     decreasing = NULL,
                     overlay = NULL,

                     # plot arguments
                     filename   = NULL,
                     directory  = getwd(),
                     pagewise   = TRUE,
                     nrow       = 12,
                     ncol       = 6,
                     margins    = c(20,20,20,20),
                     pch        = 16,
                     cex        = .2,
                     lwd        = NULL,
                     plot_box   = TRUE,
                     plot_index = TRUE,

                     # spatialize arguments
                     spatialize = TRUE,
                     n_points   = 50,

                     # align arguments
                     start = TRUE,
                     end   = TRUE

                     ){


  # ---- tests
  if(!length(dimensions) %in% 2){
    stop('Dimensions must of length 2!')
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
  if(!all(c(dimensions,overlay) %in% dimnames(trajectories)[[2]])) stop('Not all dimensions exist')


  # ---- align
  trajectories = trajectory_align(trajectories,
                                  dimensions = dimensions,
                                  coordinates = 'norm',
                                  start = start,
                                  end = end)

  # ---- spatialize
  if(rescale == TRUE){
    lengths  = getLengths(trajectories[,dimensions[1],],trajectories[,dimensions[2],])
    n_points = round(n_rescale * lengths / sqrt(2))
    if(is.null(overlay))  trajectories = spatial_rescale(trajectories,dimensions = dimensions,n_points)
    if(!is.null(overlay)) trajectories = spatial_rescale(trajectories,dimensions = c(dimensions,overlay),n_points)
    }


  # ---- sorting data
  if(!is.null(sort_variables) | !is.null(labels)){
    if(is.null(sort_variables)) sort_variables = labels
    for(i in length(sort_variables):1){
      trajectories = trajectories[order(sort_variables[[i]],decreasing = decreasing[i]),,]
      for(j in 1:length(ids)){
        ids[[j]] = ids[[j]][order(sort_variables[[i]],decreasing = decreasing[i]),]
        }
      for(j in 1:length(categories)){
        labels[[j]] = labels[[j]][order(sort_variables[[i]],decreasing = decreasing[i]),]
        }
      }
    }


  # ---- plot

  # wait for plot function
  wait_for_enter = function(){cat ("Press [enter] to continue");line <- readline(); return(line)}

  # layout
  par(mfrow = c(nrow,ncol))
  xlim = c(-1 - margins[1]/100,1   + margins[3]/100)
  ylim = c( 0 - margins[2]/100,1.5 + margins[4]/100 + .1)
  n_trajectories = dim(data[[use]])[1]

  # set device
  if(!is.null(filename)) {
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
    x = -trajectories[i,dimensions[1],]
    y =  trajectories[i,dimensions[2],] * 1.5


    # determine colors
    ind = which(!is.na(data[[use]][i,'xpos',]))
    if(plot_index == TRUE){
      time_intensity  = seq(0,1,length = length(ind))
      } else {
      time_intensity = 0
      }
    if(!is.null(overlay)) {
      overlay_values = trajectories[i,overlay,ind]
      overlay_intensity = overlay_values / max(overlay_values)
      } else {
      overlay_intensity = 0
      }
    col = rgb(time_intensity,0,overlay_intensity)


    # plot lines
    if(!is.null(lwd)){
      if(!is.null(overlay) | plot_time == TRUE){
        for(j in 1:(length(x)-1)) lines(c(x[j],x[j+1]),c(y[j],y[j+1]),lwd=lwd,col=col[j])
        } else {
        lines(x,y,lwd=lwd)
        }
      }

    # plot points
    if(!is.null(pch)){
      if(!is.null(lwd)){
        points(x,y,col=col,cex=cex,lwd=lwd,pch=pch,bg='white')
        } else {
        points(x,y,col=col,cex=cex,lwd=lwd)
        }
      }

    # add ID
    id_pos = c(.5,.05)
    if(!is.null(ids)){
      id = ''
      for(j in 1:length(ids)){
        id = paste(id,ids[[j]][i],sep=ifelse(id=='','','  '))
        }
        text(xlim[1]+id_pos[1],ylim[1]+id_pos[2],cex=.6,font=1,adj=0,
           labels=id)
           }


    # add labels
    label_pos = c(0,.1,.2,.3,.4)
    if(!is.null(labels)){
      for(j in 1:length(labels)){
        value = labels[[j]][i]
        rect(xlim[1]+label_pos[j],
             ylim[1],
             xlim[1]+label_pos[j + 1],
             ylim[1]+.1,
             border='grey90',col = rgb(1-value,1-value,1-value))
             }
           }

    # add box
    if(plot_box == TRUE){
      box(lwd=.5,col='black')
      }

    # wait for enter
    if(pagewise == TRUE & i %% (nrow*ncol) == 0) if(wait_for_enter() == 'q') break

    }

  # close device
  if(device == 'pdf') dev.off()
  if(return_data) return(data)
  }

