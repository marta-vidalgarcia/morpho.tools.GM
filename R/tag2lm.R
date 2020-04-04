#' @name tag2lm
#' @title tag2lm
#' 
#' @description
#'   This function inputs *tag files into an array of dimensions p, k, n
#'   
#' @usage
#'   tag2lm(dir = NULL, ID = NULL, string_del = NULL)
#' @param 
#' \item{dir}{
#' Optional argument. Directory where the *tag files are. Default (NULL) is the current working directory.
#' } 
#' \item{ID}{
#' Optional argument for specimens IDs. The default (NULL) is the file names.
#' }
#' \item{string_del}{
#' Optional argument. Indicates pattern in the specimens names to be deleted. Use only if ID = NULL.
#' }
#' 
#' @value
#'   This function returns an array of dimensions (p, k, n), in which p is the number of landmarks, k = 3 (number of dimensions), and n is the number of specimens.
#' @details
#'   This function is highly based on the plotOutliers function in geomorph (Adams et al., 2018), and it has been modified to select for percentiles and save plots (including a histogram). It will check for outliers at the selected percentile (e.g. 95).
#'
#' @examples
#' # array <- tag2lm()
#' # If we have *.tag file in another directory and with the suffix "skull"
#' # For example: "spec1_skull.tag", "spec2_skull.tag", "spec3_skull.tag", etc.
#' dir <- "~/Documents/skull_LMs")
#' # skull_array <- tag2lm(dir = skull_dir, string_del="skull")
#'   
#' @author Marta Vidal-Garcia
#' 

tag2lm <- function(dir = NULL, ID = NULL, string_del = NULL){
  if (is.null(dir) == TRUE){
    path <- getwd()
  }
  else{
    path <- dir
  }
  tag_list <- dir(path = path, pattern="*.tag")
  n_land <- vector("numeric", length=length(tag_list))
  for (i in 1:length(tag_list)){
    n_land[i] <- length(count.fields(tag_list[[i]])) -3
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
  for (i in 1:length(tag_list)){
    LM_array[,1,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 4, sep = " ", header=F))[, 2]
    LM_array[,2,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 4, sep = " ", header=F))[, 3]
    LM_array[,3,i] <- suppressWarnings(read.table(file = tag_list[[i]], skip = 4, sep = " ", header=F))[, 4]
  }
  return(LM_array)
}