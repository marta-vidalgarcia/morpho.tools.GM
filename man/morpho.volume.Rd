\name{morpho.volume}
\title{morpho.volume}
\alias{morpho.volume}

\description{
This function returns the overall volume (and volumes by groups) occupied in the morphospace.
}
\usage{
morpho.volume(x, groups = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Dataset with three columns (3 dimensions in the morphospace)
  \item{groups}{
     An optional factor defining groups}
}

\value{
This function returns a list with two objects. The first object is the volume for the whole dataset. If groups have been selected, then it also returns a data.frame with the volumes by group and the percentage of the total volume occupied in the morphospace.
}

\details{
This function returns the overall volume (and volumes by groups) occupied in the morphospace.}


\examples{
x <- pc.scores[,1:3] # first three PC scores
groups <- c("sp_A", "sp_A", "sp_B", "sp_A", "sp_A", "sp_B", "sp_B", "sp_A", "sp_A", "sp_A", "sp_A")
names(groups) <- row.names(x)
morpho.volume(x, groups) 
}


#' @description
#'   This function returns the volume (and volumes by groups) of a morphospace.
#'   
#' @usage
#'   morpho.volume(x, groups)
#' @param item{x} Dataset with three columns (3 dimensions in the morphospace)
#'   }
#'   \item{groups}{
#'     An optional factor defining groups
#'   }
#'   
#' @value
#'   This function returns a list with two objects. The first object is the volume for the whole dataset. If groups have been selected, then it also returns a data.frame with the volumes by group and the percentage of the total volume occupied in the morphospace.
#' 
#' @details
#'   This function returns the overall volume (and volumes by groups) occupied in the morphospace.
#'
#'   
#' @examples
#'     x <- pc.scores[,1:3] # first three PC scores
#'     groups <- c("sp_A", "sp_A", "sp_B", "sp_A", "sp_A", "sp_B", "sp_B", "sp_A", "sp_A", "sp_A", "sp_A")
#'     names(groups) <- row.names(x)
#'     morpho.volume(x, groups) 
#'   
#' @author Marta Vidal-Garcia



}
\author{
Marta Vidal-Garcia (marta.vidalga@gmail.com)
}

