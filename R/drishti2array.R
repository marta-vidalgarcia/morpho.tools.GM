#' @name drishti2array
#' @title drishti2array
#'
#' @description
#'   This function inputs landmark files from Drishti (text file with row #1 containing number of landmarks) into an array of dimensions p, k, n
#'   If the files do not have a pattern, (e.g. *.txt) they need to be the only files in that directory
#'
#' @usage
#'   drishti2array(dir = NULL, ID = NULL, string_del = NULL, save.txt = FALSE)
#'
#' @param dir Optional argument. Directory where the Drishti landmark files are. Default (NULL) is the current working directory.
#'
#' @param ID Optional argument for specimens IDs. The default (NULL) is the file names.
#'
#' @param string_del Optional argument. Indicates pattern in the specimens names to be deleted. Use only if ID = NULL.
#'
#' @param save.txt Optional argument. Indicates whether to also save *.txt files with the landmark coordinates.
#'
#' @return This function returns an array of dimensions (p, k, n), in which p is the number of landmarks, k = 3 (number of dimensions), and n is the number of specimens.
#'
#' @examples
#' # array <- drishti2array()
#' # If we have files with the suffix "_skull"
#' # For example: "spec1_skull", "spec2_skull", "spec3_skull", etc.
#' # Make sure that the files
#' # dir <- "~/Documents/skull_LMs")
#' # skull_array <- drishti2array(dir = skull_dir, string_del="_skull", save.tag = TRUE) # will save *.tag landmark files
#'
#' @author Marta Vidal-Garcia
#' @export
#'

drishti2array <- function(dir = NULL, ID = NULL, pattern = NULL, string_del = NULL, save.txt = FALSE, save.tag = FALSE){
  if (is.null(dir) == TRUE) {
    path <- getwd()
  } else {
    path <- getwd()
    setwd(dir)
  }
  if (is.null(pattern) == TRUE) {
    lm_list <- dir(path = path)
  } else {
    lm_list <- dir(path = path, pattern = pattern)
  }
  n_land <- vector("numeric", length=length(lm_list))
  for (i in 1:length(lm_list)){
    n_land[i] <- length(count.fields(lm_list[[i]])) -1 # 1 line before the LMs
  }
  if (is.null(ID) == TRUE){
    if (is.null(string_del) == TRUE){
      dimnames_lm <- gsub(paste0(".", pattern), "", lm_list)
    } else{
      dimnames_lm <- gsub(string_del, "", gsub(paste0(".", pattern), "", lm_list))
    }
  } else{
    dimnames_lm <- ID
  }
  if (length(unique(n_land))!=1){
    stop("Specimens have different number of landmarks.")
  }
  LM_array <- array(data = NA, dim = c(n_land[1], 3, length(lm_list)))
  dimnames(LM_array)[[3]] <- dimnames_lm
  for (i in 1:length(lm_list)){
    LM_array[,1,i] <- suppressWarnings(read.table(file = lm_list[[i]], skip = 1, sep = " ", header=F))[, 1]
    LM_array[,2,i] <- suppressWarnings(read.table(file = lm_list[[i]], skip = 1, sep = " ", header=F))[, 2]
    LM_array[,3,i] <- suppressWarnings(read.table(file = lm_list[[i]], skip = 1, sep = " ", header=F))[, 3]
  }
  
  if (isTRUE(save.txt) == TRUE){
    for (i in 1:dim(LM_array)[3]){
      write.table(LM_array[,,i], paste0(dimnames(LM_array)[[3]][i], ".txt"), col.names = FALSE, row.names = FALSE)
    }
  }
  if (isTRUE(save.tag) == TRUE){
    for (i in 1:dim(LM_array)[3]){
      write.tag(LM_array[,,i], paste0(dimnames(LM_array)[[3]][i], ".tag"))
    }
  }
  setwd(path)
  return(LM_array)
}
  