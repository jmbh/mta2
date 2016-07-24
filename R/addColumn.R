############# add column ################
#
# INPUT  
# 
#
# OUTPUT
# 
#
# DESCRIPTIO
#




add_column = function(data,
                      column,
                      name,
                      trajectory_object = 'trajectories',
                      overwrite = FALSE){

  # ---- tests
  col_names = dimnames(data[[trajectory_object]])[[2]]
  if(name %in% col_names & !overwrite){
    stop(paste0(name, 'already exist. Set overwrite = TRUE.'))
    }
  
  if(name %in% col_names & overwrite){
    if(length(col_names) == 1) stop('Impossible to replace the only column in array.')
    data[[trajectory_object]] = data[[trajectory_object]][,col_names != name,]
    }
  
  # ---- add column
  data[[trajectory_object]] = abind(data[[trajectory_object]],
                                    column,
                                    along = 2,
                                    new.names = list(dimnames(data[[trajectory_object]])[[1]],
                                                     c(dimnames(data[[trajectory_object]])[[2]],name),
                                                     NULL))
  return(data)
  }



