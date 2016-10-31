############# eval clustering ################
#
# INPUT: Mousetrap data object
# OUPUT: Mousetrap data object +
#        N (number of traj) x k (number of prototypes) similarity matrix +
#        prototypes
#





mt_map = function(
  data,
  use = 'trajectories',
  dimensions = c('xpos','ypos'),
  save_as = 'prototyping',

  # prototype arguments
  proto_list = 'default', # list object containing prototypes in a n x 2 -matrix

  # distance arguments
  pointwise = TRUE,
  minkowski_p = 2
  )

{

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
  if(is.character(proto_list)){
    if(proto_list == 'default'){
      proto_list = readRDS('~/Dropbox (2.0)/Work/Software/mta2/data/prototypes.RDS')
      }
    }

  # rescale prototypes
  n_points = dim(trajectories)[3]
  n_proto  = length(proto_list)
  joint_array = array(dim = c(dim(trajectories)[1] + n_proto,
                            length(dimensions),
                            n_points),
                            dimnames = list(c(paste0('proto_',1:n_proto),dimnames(trajectories)[[1]]),
                                            dimensions,
                                            NULL))

  for(i in 1:n_proto){
    joint_array[i,,] = mt_spatialize(proto_list[[i]],
                                       n_points = n_points,
                                       dimensions = dimnames(proto_list[[i]])[[2]])
    }

  joint_array[(n_proto + 1) : dim(joint_array)[1],dimensions,] = trajectories[,dimensions,]


  # ---- compute distance & closest prototype
  distm =  mt_distmat(joint_array,
                      dimensions = dimensions,
                      pointwise = pointwise,
                      minkowski_p = minkowski_p)
  dists = distm[1:n_proto,-c(1:n_proto)]
  prototypes = apply(dists,2,which.min)


  # ---- save data
  if(is.list(data)){
    if(save_as %in% names(data)){
      data[[save_as]]$prototypes = prototypes
      } else {
      result = data.frame(mt_id = dimnames(trajectories)[[1]],prototyping = prototypes)
      data[[save_as]] = result
      }
    } else {
    result = data.frame(mt_id = dimnames(trajectories)[[1]],prototyping = prototypes)
    data = result
    }

  return(data)
  }
