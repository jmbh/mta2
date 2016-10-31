### Fast color mixer


colormixer = function(col1,col2,weight,format = 'rgb'){
  
  if(ifelse(is.matrix(col1),nrow(col1),length(col1)) != length(weight) &
     length(col1) != 1){
    stop('Length of col1 must be either 1 or matching the length of weight')
    }
  if(ifelse(is.matrix(col2),nrow(col2),length(col2)) != length(weight) &
     length(col2) != 1){
    stop('Length of col1 must be either 1 or matching the length of weight')
    } 
  if(length(weight) == 1){
    if(ifelse(is.matrix(col1),nrow(col1),length(col1)) != 
       ifelse(is.matrix(col2),nrow(col2),length(col2))){
       stop('If length of weight = 1, number of colors in col1 and col2 must match')
       }
     }  

  nrows = max(c(ifelse(is.matrix(col1),nrow(col1),length(col1)),
                ifelse(is.matrix(col2),nrow(col2),length(col2)),
                length(weight)))
  
  if(is.character(col1)){
    if(length(col1) == 1){
      col1 = col2rgb(col1)
      col1 = matrix(c(col1),ncol=3,nrow=nrows,byrow=T)
      } else {
      col1 = t(sapply(col1,col2rgb))      
      }
    } else{
    col1 = matrix(c(col1),ncol=3,nrow=nrows,byrow=F)
    }
  if(is.character(col2)){
    if(length(col2) == 1){
      col2 = col2rgb(col2)
      col2 = matrix(c(col2),ncol=3,nrow=nrows,byrow=T)
      } else {
      col2 = t(sapply(col2,col2rgb))      
      }
    } else{
    col2 = matrix(c(col2),ncol=3,nrow=nrows,byrow=F)
    } 
  
  
  col = col1 * (1-weight) + col2 * weight
  
  if(format == 'rgb') return(col)
  if(format == 'hex') return(rgb(data.frame(col),maxColorValue = 255))
  if(!format %in% c('rgb','hex')) stop('Choose either "rgb" or "hex" as format')
  
  }



