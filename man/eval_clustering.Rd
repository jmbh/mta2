\name{eval_clustering}
\alias{eval_clustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eval_clustering(data, dimensions = c("xpos", "ypos"),
                trajectory_object = "trajetcories",
                align = TRUE, rescale = TRUE, n_points = 20,
                method = "hierarchical", linkage = "complete",
                n_km = 10, n_subsample = NULL, point_wise = TRUE,
                power = 2, ksel_methods = c("Dist", "Stab"),
                k_seq = 2:15, B = 10, model_based = FALSE,
                normalize = TRUE, n_Gap = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A mouse trap data object
}
  \item{dimensions}{
The names of the dimensions for x and y. Defailts to \code{dimensions = c("xpos", "ypos")}.
}
  \item{trajectory_object}{
Name of the trajectories that should be used for clustering. Defaults to \code{trajectory_object = "trajectories"}.
}
  \item{align}{
If \code{TRUE} trajectories are aligned to a norm display. For details see \code{?trajectory_align}.
}
  \item{rescale}{
If \code{TRUE} the trajectories are transformed such that they consist of \code{n_resc} equally spaced points in the trajectory. This ensures that each part of the trajectory has equal weight in the clustering algorithms.
}
  \item{n_resc}{
The number of interpolated time points if \code{rescale = TRUE}
}
  \item{method}{
The type of clustering algoritm used for clustering. \code{method = 'hierarchical'} for hierarchical clustering,  \code{method = 'kmeans'} for k-means clustering
}
  \item{linkage}{
The linkage criterion when \code{method = 'hierarchical'}. For available options are "single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid" or "median".
}
  \item{n_km}{
The number of restarts of the k-means algorithm if \code{method = 'kmeans'} in order to avoid local minima
}
  \item{n_subsample}{
Number of cases that should be subsampled before clustering. This may be useful for large datasets.
}
  \item{point_wise}{
???
}
  \item{power}{
The power of the minkowski distance for the calculation of the distance matrix, used for \code{method = 'hierarchical'}}
  \item{ksel_methods}{
Character vector that indicates which k-selection methods that should be used. If the character vector contains 'Dist', distance-based k-selection methods are used. If the character vector contains 'Stab', stability-based k-selection methods are used. For details read the help files in the 'cstab' package. Defaults to using both methods by setting \code{ksel_methods = c("Dist", "Stab")}.
}
  \item{k_seq}{
A integer vector indicating the number of clusters that should be considered for k-selection. E.g. \code{k_seq = 2:25} considers all clusterings with 2, 3, ... 25 clusters.
}
  \item{B}{
Number of bootstrap comparisons used in the stability based-methods. For details see Haslbeck & Wulff (2016)
}
  \item{model_based}{
If \code{TRUE}, the model-based variant of the instability-based method is used. If \code{FALSE}, the model-free variant of the instability-based method is used. Note that if \code{method = "hierarchical"}, only the model-free variant can be used. For details see Haslbeck & Wulff (2016)
}
  \item{normalize}{
If \code{TRUE}, the instability in the instability-based method is used. This is always recommended. For details see Haslbeck & Wulff (2016)
}
  \item{n_Gap}{
The number of simulated datasets to compute the Gap Statistic. For details see Tibshirani et al. (2001)
}
}
\value{
The function returns an augmented A mouse trap data object. The optimal ks determined by different methods can be found in \code{data$optimal_k}. More detailed output of the k-selection methods can be found in \code{data$k_selection_details}.
}
\references{
Fujita, A., Takahashi, D. Y., & Patriota, A. G. (2014). A non-parametric method to estimate the number of clusters. Computational Statistics & Data Analysis, 73, 27-39.

Sugar, C. A., & James, G. M. (2011). Finding the number of clusters in a dataset. Journal of the American Statistical Association.

Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 63(2), 411-423.

}
\author{
Jonas Haslbeck, Dirk Wulff
}

\examples{

\dontrun{


# Here we should have an example

}


}
