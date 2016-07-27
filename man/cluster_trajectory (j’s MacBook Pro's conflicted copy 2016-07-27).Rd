\name{cluster_trajectory}
\alias{cluster_trajectory}
\title{
Clustering Mouse Trajetories
}
\description{
Clusters Mouse Trajectories using hierarchical or k-means clustering
}
\usage{
cluster_trajectory(data, dimensions = c("xpos", "ypos"),
                   trajectory_object = "trajectories",
                   align = TRUE, rescale = TRUE, n_resc = 20,
                   method = "hierarchical", linkage = "complete",
                   n_km = 10, n_cluster, n_subsample = NULL,
                   point_wise = TRUE, power = 2, verbose = TRUE)
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
  \item{n_cluster}{
The number of desired clusters.
}
  \item{n_subsample}{
Number of cases that should be subsampled before clustering. This may be useful for large datasets.
}
  \item{point_wise}{
???
}
  \item{power}{
The power of the minkowski distance for the calculation of the distance matrix, used for \code{method = 'hierarchical'}.
}
  \item{verbose}{
If \code{TRUE} the function prints information about the current computations in the console.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns an augmented A mouse trap data object. The cluster vector can be found in \code{data$data$clusters}.
}
\references{
}
\author{
Jonas Haslbeck, Dirk Wulff
}
cts to See Also as \code{\link{help}}, ~~~
}
\examples{

\dontrun{


# Here we should add an example ...

}

}
