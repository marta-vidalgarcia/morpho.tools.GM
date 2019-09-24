#' @name morpho.volume
#' @title morpho.volume
#' @alias morpho.volume
#' 
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
#'   This function returns the volume (and volumes by groups) occupied in the morphospace.
#'
#'   
#' @examples
#'     x <- pc.scores[,1:3] # first three PC scores
#'     groups <- c("sp_A", "sp_A", "sp_B", "sp_A", "sp_A", "sp_B", "sp_B", "sp_A", "sp_A", "sp_A", "sp_A")
#'     names(groups) <- row.names(x)
#'     morpho.volume(x, groups) 
#'   
#' @author Marta Vidal-Garcia

morpho.volume <- function (x, groups){
  vol_all <- volume(ellipsoidhull(x))
  group <- lapply(split.data.frame(x, groups), ellipsoidhull)
  vol_groups <- lapply(group, volume)
  prop <- function(x){prop <- (x*100/vol_all)}
  percent <- lapply(vol_groups, prop)
  volume_groups <- rbind(data.frame(vol_groups), data.frame(percent))
  row.names(volume_groups) <- c("vol", "%")
  return(list("Total_volume" = vol_all, "Volume_groups" = volume_groups))
}