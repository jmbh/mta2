# # #Test
# #
# #
# require(Rcpp)
# require(plyr)
# require(dplyr)
# require(abind)
# #
# 
# sourceCpp('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/src/getAngles.cpp')
# sourceCpp('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/src/getLengths.cpp')
# 
# ^sourceCpp('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/src/getVelocities.cpp')
# sourceCpp('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/src/gaussianBlurs.cpp')
# sourceCpp('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/src/cleanAngles.cpp')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/addAngles.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/trajectoryAlign.R')
# 
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/detectCoM.R')
# 
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/trajectoryClean.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/addColumn.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/addLengths.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/spatialRescale.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/trajectoryViewer.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/addVelocities.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/trajectoryHeatmap.R')
# source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/subsetData.R')
# #
# 
# source('~/Dropbox (2.0)/Work/Software/mta2/R/ut_transform.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/mt_distmat.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/mt_align.R')
# source('~/Dropbox (2.0)/Work/Software/mta2/R/mt_spatialize.R')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/distMat.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/spatialize.cpp')
# sourceCpp('~/Dropbox (2.0)/Work/Software/mta2/src/trajAlign.cpp')
# 
# datDIR = 'Dropbox (2.0)/Work/Projects/Mousetracking/0 Discontinuous mind/3 CleanData/'
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
# 
# 
# # example data
# str(data)
# plot(data$trajectories[1,2,], data$trajectories[1,3,])
# data$trajectories[1,'ypos',]
# dim(data$trajectories)
# 
# 
# #
# #
# # d = trajectory_viewer(data,show = c('points','box'),
# #                   sort_by = c('mean_sim'), sort_order = 'increasing',
# #                   rescale = TRUE,rescaled_sim = T, clean_rescale = 50,
# #                   info = c('id'),n_rescale = 100,cex=.2,
# #                   return_data = TRUE,verbose=TRUE)
# #
# #
# # source('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/R/trajectoryHeatmap.R')
# #
# # trajectoryHeatmap(data,
# #   name = 'Koop2013',
# #   px_size  = .0005,
# #   mean_color_value = .2,
# #   overlay = 'velocity',
# #   directory = '~/Dropbox (2.0)/Work/Projects/Mousetracking/0 Discontinuous mind/5 Results & Figures/Heatmaps/')
# #
# #
# # compileAttributes('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/')
# # install.packages('/Users/jmb/Dropbox/MyData/_PhD/__software/mta2/',repos=NULL,type='source')
# #
# # require()
# #
# # mean(a$img)
# #
# #
# # col = 'steelblue'
# #
# # HSV = rgb2hsv(col2rgb(col))
# # test(hsv(HSV[1],HSV[2]*0,1))
# #
# # test(rgb(RGB[1],RGB[2],RGB[3],maxColorValue=255))
# #
# #
# #
# #
# #
# #
# # test = function(x) {plot.new();plot.window(c(0,1),c(0,1));rect(0,0,1,1,col=x,border=NA)}
# #
# #
# #
# #
# #
