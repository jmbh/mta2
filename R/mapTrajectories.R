############# eval clustering ################
#
# INPUT: Mousetrap data object
# OUPUT: Mousetrap data object +
#        N (number of traj) x k (number of prototypes) similarity matrix +
#        prototypes
#



map_trajectories = function(
  data,
  dimensions = c('xpos','ypos'),
  trajectory_object = 'trajetcories',

  # preprocessing
  align = TRUE,
  rescale = TRUE,
  n_resc = 20,

  # prototype arguments
  proto_list = # list object containing prototypes in a n x 2 -matrix

)

{

# ---- tests
# ifs and stops

# ---- data align
if(verbose == TRUE) cat('aligning','\n')
if(align == TRUE){
  data = trajectory_align(data,
                          trajectory_object = trajectory_object,
                          dimensions = dimensions,
                          coordinates = 'mt')
}


# rescale trajectories
data = spatial_rescale(data,n_points = n_points,
                       dimensions = dimensions,
                       trajectory_object = trajectory_object)


# ---- cluster trajectories to prototype



# ---- evaluate clustering using method 1

# ---- evaluate clustering using method 2

# ---- evaluate clustering using method 3


data$prototypes =
data$prototype_similarity =


  return(data)
  }
