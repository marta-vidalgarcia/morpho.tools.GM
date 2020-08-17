#' @name tag2array
#' @title tag2array
#' 
#' @description
#'   This function inputs *tag files into an array of dimensions p, k, n
#'   
#' @usage
#'   tag2array(dir = NULL, ID = NULL, string_del = NULL, propagated = FALSE, save.txt = FALSE)
#'   
#' @param dir Optional argument. Directory where the *.ascii files are. Default (NULL) is the current working directory.
#' 
#' @param ID Optional argument for specimens IDs. The default (NULL) is the file names.
#' 
#' @param string_del Optional argument. Indicates pattern in the specimens names to be deleted. Use only if ID = NULL.
#' 
#' @param propagated Optional argument indicating if the landmarks have been propagated (extra line). The default is FALSE.
#' 
#' @param save.txt Optional argument. Indicates whether to also save *.txt files with the landmark coordinates.
#' 
#' @return This function returns an array of dimensions (p, k, n), in which p is the number of landmarks, k = 3 (number of dimensions), and n is the number of specimens.
#'
#' @examples
#' # array <- tag2array()
#' # If we have other *.tag files in another directory and with the suffix "skull"
#' # For example: "spec1_skull.tag", "spec2_skull.tag", "spec3_skull.tag", etc.
#' # dir <- "~/Documents/skull_LMs")
#' # skull_array <- tag2array(dir = skull_dir, string_del="skull")
#'   
#' @author Marta Vidal-Garcia
#' @export
#' 

tag2array <- function(dir = NULL, ID = NULL, string_del = NULL, propagated = FALSE, save.txt = FALSE){
  if (is.null(dir) == TRUE){
    path <- getwd()
  }
  else{
    path <- dir
  }
  tag_list <- dir(path = path, pattern="*.tag")
  n_land <- vector("numeric", length=length(tag_list))
  for (i in 1:length(tag_list)){
    if (isTRUE(propagated) == TRUE){
      n_land[i] <- length(count.fields(tag_list[[i]])) -4
    }
    else {
      n_land[i] <- length(count.fields(tag_list[[i]])) -3 # doublecheck if this should be 3
    }
  }
  if (is.null(ID) == TRUE){
    if (is.null(string_del) == TRUE){
      dimnames_tag <- gsub(".tag", "", tag_list)
    }
    else{
      dimnames_tag <- gsub(string_del, "", gsub(".tag", "", tag_list))
    }
  }
  else{
    dimnames_tag <- ID
  }
  if (length(unique(n_land))!=1){
    stop("Specimens have different number of landmarks.")
  }
  LM_array <- array(data = NA, dim = c(n_land[1], 3, length(tag_list)))
  dimnames(LM_array)[[3]] <- dimnames_tag
  if (isTRUE(propagated) == TRUE){
    for (i in 1:length(tag_list)){
      LM_array[,1,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 5, sep = " ", header=F))[, 2]
      LM_array[,2,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 5, sep = " ", header=F))[, 3]
      LM_array[,3,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 5, sep = " ", header=F))[, 4]
    }
  }
  else{
    for (i in 1:length(tag_list)){
      LM_array[,1,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 4, sep = " ", header=F))[, 2]
      LM_array[,2,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 4, sep = " ", header=F))[, 3]
      LM_array[,3,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 4, sep = " ", header=F))[, 4]
    }
  }
  if (isTRUE(save.txt) == TRUE){
    for (i in 1:dim(LM_array)[3]){
      write.table(LM_array[,,i], paste0(dimnames(LM_array)[[3]][i], ".txt"), col.names = FALSE, row.names = FALSE)
    }
  }
  return(LM_array)
}
