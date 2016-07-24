############# transform data ################
#
# INPUT  
# data            our long format
#
# OUTPUT
# Pascal and Felix format


trnsf = function(data,
                 id.ptp = c('ptp','trial'),
                 id.xyt = c('x','y','t')){
  
  xs   = dlply(data,c('ptp','trial'),function(x) x$x)
  ys   = dlply(data,c('ptp','trial'),function(x) x$y)
  ts   = dlply(data,c('ptp','trial'),function(x) x$t)
  
  
  if(sd(sapply(xs,length))>0){
    ts = lapply(1:length(xs),function(x) 1:101)
    xs = lapply(xs,function(x) approx(x,n=101)$y)
    ys = lapply(ys,function(x) approx(x,n=101)$y)
    xn = sapply(xs,function(x) x[101])
    for(i in 1:length(xs)){
      if(xn[i] > mean(xn)){
        xs[[i]] = -xs[[i]]
        }
      }
    }
  
  ts   = do.call(rbind,ts[sapply(ts,function(x) length(x)>0)])
  xs   = do.call(rbind,xs[sapply(xs,function(x) length(x)>0)])
  ys   = do.call(rbind,ys[sapply(ys,function(x) length(x)>0)])
  
  mt_variable_labels_short <- c(timestamps="timestamps",xpos="xpos",ypos="ypos")
  
  trajectories = array(dim = c(nrow(xs),3, ncol(xs)),
                       dimnames = list(apply(unique(data[,c('ptp','trial')]),1,paste0,collapse='_'),
                                        mt_variable_labels_short,NULL))
  
  trajectories[,1,] = ts
  trajectories[,2,] = xs
  trajectories[,3,] = ys
  
  ndata   = ddply(data,c('ptp','trial'),function(x) x[1,!names(x)%in%c('x','y','t')])
  
  return(list('data'=ndata,'trajectories'=trajectories))
  }

