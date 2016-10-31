# ############# eval clustering ################
# #
# # INPUT: Mousetrap data object + Considered k sequence
# # OUPUT: Mousetrap data object + Numeric indicating optimal k (or ks) + evidence (e.g., curves or slopes)
# #
# 
# # prompt(eval_clustering)
# 
# eval_clustering = function(
#   data,
#   dimensions = c('xpos','ypos'),
#   trajectory_object = 'trajetcories',
# 
#   # preprocessing
#   align = TRUE,
#   rescale = TRUE,
#   n_resc = 20,
# 
#   # clustering arguments
#   method = 'hierarchical', # or 'kmeans'
#   linkage = 'complete',
#   n_km = 10, # number of reinitializations for k-means algorithm
#   n_subsample = NULL,
# 
#   # distance arguments
#   point_wise = TRUE, # ???
#   power = 2,
# 
#   # k-selection type
#   ksel_methods = c('Dist', 'Stab'), # distance based or/ and stability based methods
#   k_seq = 2:15, # considered k-sequence
# 
#   # arguments instability-based k-selection methods
#   B = 10, # bootstrap samples
#   model_based = FALSE,
#   normalize = TRUE,
# 
#   # arguments distance-based k-selection methods
#   n_Gap = 10 # simulated datasets for Gap Statistic
# 
# )
# 
# {
# 
#   # ---- tests
#   # ifs and stops
#   if(model_based == TRUE & method == 'hierarchical') stop('Model-based instability methods are only available for k-means clustering.')
# 
#   # ---- data align
#   if(verbose == TRUE) cat('aligning','\n')
#   if(align == TRUE){
#     data = trajectory_align(data,
#                             trajectory_object = trajectory_object,
#                             dimensions = dimensions,
#                             coordinates = 'mt')
#   }
# 
# 
#   # rescale trajectories
#   data = spatial_rescale(data,
#                          n_points = n_resc,
#                          dimensions = dimensions,
#                          trajectory_object = trajectory_object)
# 
#   # subsample data
#   n <- nrow(data$data)
#   if(is.null(n_subsample)) n_subsample <- n
#   ss_ind <- sample(1:n, n_subsample, replace=F)
#   data_short <- subset_data(data, condition = 1:n %in% ss_ind)
# 
#   # transform data structura for clustering input
#   data_ReA <- t(apply(data_short$rescaled_trajectories, 1, function(x) c(x[1,], x[2,])))
# 
#   # ---- Distance-based k-selection methods
#   if('Dist' %in% ksel_methods) {
# 
#     if(verbose == TRUE) cat('calculating distance-based k-selection methods','\n')
#     cDist_obj <- cDistance(data = data_ReA,
#                            kseq = k_seq,
#                            method = method,
#                            linkage = linkage,
#                            kmIter = n_km,
#                            RunsGap = n_Gap)
# 
#     l_kopt_dist <- list('kopt_Gap'=cDist_obj$kOpt_Gap,
#                         'kopt_Slope'=cDist_obj$kOpt_Slope,
#                         'kopt_Jump'=cDist_obj$kOpt_Jump)
# 
#     l_details_dist <- list('sequence_Gap'=cDist_obj$Gaps,
#                            'sequence_Slope'=cDist_obj$slopes,
#                            'sequence_Jumps'=cDist_obj$jumps)
# 
# 
#   } else {
#     l_kopt_dist <- NULL
#     l_details_dist <-  NULL
#   }
# 
#   # ---- Stability-based k-selection methods
#   if('Stab' %in% ksel_methods) {
# 
#     if(verbose == TRUE) cat('calculating stability-based k-selection methods','\n')
#     cStab_obj <- cStability(data = data_ReA,
#                             kseq = kseq,
#                             B = B,
#                             norm = TRUE,
#                             prediction = model_based,
#                             type = method,
#                             linkage = linkage,
#                             kmIter = n_km,
#                             pbar = TRUE)
# 
#     if(normalize) {
#       kopt_stab <- cStab_obj$kopt_instab_norm
#       kopt_details <- cStab_obj$Instab_path_norm
#     } else {
#       kopt_stab <- cStab_obj$kopt_instab
#       kopt_details <- cStab_obj$Instab_path
#     }
# 
#     l_kopt_stab <- list('kopt_Stability'=kopt_stab)
#     l_details_stab <- list('sequence_Stability'=kopt_details)
# 
#   } else {
#     l_kopt_stab <- NULL
#     l_details_stab <-  NULL
#   }
# 
#   # character vector with k_optimal from different functions
#   data$optimal_k = unlist(c(l_kopt_dist, l_kopt_stab))
#   # list with k-paths of different distance/stability measures
#   data$k_selection_details = c(l_details_dist, l_details_stab)
# 
#   return(data)
# 
# }
# 
# 
# 
# 
# 
