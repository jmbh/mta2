# #Test
# 
# 
# require(Rcpp)
# require(plyr)
# require(dplyr)
# require(abind)
# 
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/trajAlign.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/getAngles.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/getLengths.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/distMat.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/spatialRescale.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/getVelocities.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/gaussianBlurs.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/cleanAngles.cpp')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/addAngles.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/trajectoryAlign.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/transform.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/detectCoM.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/addDistMat.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/trajectoryClean.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/addColumn.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/addLengths.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/spatialRescale.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/trajectoryViewer.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/addVelocities.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/trajectoryHeatmap.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/subsetData.R')
# 
# datDIR = '~/Dropbox (2.0)/Work/Projects/Mousetracking/0 Discontinuous mind/3 CleanData/'
# 
# paths = list.files(datDIR,full.names = T)
# files = list.files(datDIR,full.names = F)
# names = substr(files,1,regexpr('[_E]',files)-1)
# uninams = unique(names)
# 
# nam = 'Koop2013'
# 
# fils = paths[which(nam == names)]
# 
# data = readRDS(fils[1])
# data = trnsf(data)
# 
# 
# d = trajectory_viewer(data,show = c('points','box'),
#                   sort_by = c('mean_sim'), sort_order = 'increasing',
#                   rescale = TRUE,rescaled_sim = T, clean_rescale = 50,
#                   info = c('id'),n_rescale = 100,cex=.2,
#                   return_data = TRUE,verbose=TRUE)
# 
# 
# source('~/Dropbox (2.0)/Work/Software/mta2/R/trajectoryHeatmap.R')
# 
# trajectoryHeatmap(data,
#   name = 'Koop2013',
#   px_size  = .0005,
#   mean_color_value = .2, 
#   overlay = 'velocity',
#   directory = '~/Dropbox (2.0)/Work/Projects/Mousetracking/0 Discontinuous mind/5 Results & Figures/Heatmaps/')
# 
# 
# compileAttributes('~/Dropbox (2.0)/Work/Software/mta2/')
# install.packages('~/Dropbox (2.0)/Work/Software/mta2/',repos=NULL,type='source')
# 
# require()
# 
# mean(a$img)
# 
# 
# col = 'steelblue'
# 
# HSV = rgb2hsv(col2rgb(col))
# test(hsv(HSV[1],HSV[2]*0,1))
# 
# test(rgb(RGB[1],RGB[2],RGB[3],maxColorValue=255))
# 
# 
# 
# 
# 
# 
# test = function(x) {plot.new();plot.window(c(0,1),c(0,1));rect(0,0,1,1,col=x,border=NA)}
# 
# 
# 
# 
# 
