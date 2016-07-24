############# dist mat ################
#
# INPUT  
# 
#
# OUTPUT
# 
#
# DESCRIPTIO
#

subset_data = function(data,
                       condition
                       ){
  
  # ---- tests
  if(is.logical(condition)){
    if(length(conditon) != nrow(data$data)) stop('Non matching dimensions.')
    } else {
    if(is.numeric(condition)){ 
      if(max(condition) > nrow(data$data)) stop('Subset out of range.')
      } else {
      stop('Condition must be logical or numeric.')  
      }
    }
  
  # ---- subset      
  for(nam in names(data)){
    if(length(dim(data[[nam]])) == 2) data[[nam]] = data[[nam]][condition,]
    if(length(dim(data[[nam]])) == 3) data[[nam]] = data[[nam]][condition,,]
    }
    
  return(data)
}


