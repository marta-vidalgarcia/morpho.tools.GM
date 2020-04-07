#' @name ascii2lm
#' @title ascii2lm
#' 
#' @description
#'   This function inputs landmark *.ascii files (from Amira or Avizo) into an array of dimensions p, k, n
#'   
#' @usage
#'   ascii2lm(dir = NULL, ID = NULL, string_del = NULL)
#'   
#' @param dir Optional argument. Directory where the *.ascii files are. Default (NULL) is the current working directory.
#' 
#' @param ID Optional argument for specimens IDs. The default (NULL) is the file names.
#' 
#' @param string_del Optional argument. Indicates pattern in the specimens names to be deleted. Use only if ID = NULL.
#' 
#' @return This function returns an array of dimensions (p, k, n), in which p is the number of landmarks, k = 3 (number of dimensions), and n is the number of specimens.
#'   
#' @examples
#' # array <- ascii2lm()
#' # If we have *.tag file in another directory and with the suffix "skull"
#' # For example: "spec1_skull.ascii", "spec2_skull.ascii", "spec3_skull.ascii", etc.
#' # dir <- "~/Documents/skull_LMs")
#' # skull_array <- ascii2lm(dir = skull_dir, string_del="skull")
#'   
#' @author Marta Vidal-Garcia
#' 
#' @export
#' 

ascii2lm <- function(dir = NULL, ID = NULL, string_del = NULL){
  if (is.null(dir) == TRUE){
    path <- getwd()
  }
  else{
    path <- dir
  }
  ascii_list <- dir(path = path, pattern="*.ascii")
  n_land <- vector("numeric", length=length(ascii_list))
  for (i in 1:length(ascii_list)){
    n_land[i] <- length(count.fields(ascii_list[[i]])) -7
  }
  if (is.null(ID) == TRUE){
    if (is.null(string_del) == TRUE){
      dimnames_ascii <- gsub(".ascii", "", ascii_list)
    }
    else{
      dimnames_ascii <- gsub(string_del, "", gsub(".ascii", "", ascii_list))
    }
  }
  else{
    dimnames_ascii <- ID
  }
  if (length(unique(n_land))!=1){
    stop("Error. Specimens have different number of landmarks.")
  }
  LM_array <- array(data = NA, dim = c(n_land[1], 3, length(ascii_list)))
  dimnames(LM_array)[[3]] <- dimnames_ascii
  for (i in 1:length(ascii_list)){
    LM_array[,1,i] <- suppressWarnings(read.table(file = ascii_list[[i]], skip = 14, sep = " ", header=F))[, 1]
    LM_array[,2,i] <- suppressWarnings(read.table(file = ascii_list[[i]], skip = 14, sep = " ", header=F))[, 2]
    LM_array[,3,i] <- suppressWarnings(read.table(file = ascii_list[[i]], skip = 14, sep = " ", header=F))[, 3]
  }
  return(LM_array)
}