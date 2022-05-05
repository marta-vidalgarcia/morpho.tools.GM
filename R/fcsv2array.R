#' @name fcsv2array
#' @title fcsv2array
#' 
#' @description
#'   This function inputs *fcsv files (3DSlicer) into an array of dimensions p, k, n
#'   
#' @usage
#'   fcsv2array(dir = NULL, ID = NULL, string_del = NULL, propagated = FALSE, save.txt = FALSE)
#'   
#' @param dir Optional argument. Directory where the *.ascii files are. Default (NULL) is the current working directory.
#' 
#' @param ID Optional argument for specimens IDs. The default (NULL) is the file names.
#' 
#' @param pattern Optional argument for specific files with a pattern. E.g. "_semilandmarks.fcsv". The default (NULL) is all the ".fcsv" files in a directory.
#' 
#' @param string_del Optional argument. Indicates pattern in the specimens names to be deleted. Use only if ID = NULL.
#' 
#' @param save.txt Optional argument. Indicates whether to also save *.txt files with the landmark coordinates.
#' 
#' @return This function returns an array of dimensions (p, k, n), in which p is the number of landmarks, k = 3 (number of dimensions), and n is the number of specimens.
#'
#' @examples
#' # array <- fcsv2array()
#' # If we have other *.fcsv files in another directory and with the suffix "skull"
#' # For example: "spec1_skull.fcsv", "spec2_skull.fcsv", "spec3_skull.fcsv", etc.
#' # dir <- "~/Documents/LMs")
#' # skull_array <- fcsv2array(dir = dir, pattern="_skull", string_del="_skull")
#'   
#' @author Marta Vidal-Garcia
#' @export
#' 
#' 

fcsv2array <- function(dir = NULL, pattern = NULL, ID = NULL, string_del = NULL, save.txt = FALSE){
  if (is.null(dir) == TRUE){
    path <- getwd()
  }
  else{
    path <- dir
  }
  if (is.null(pattern) == TRUE){
    fcsv_list <- dir(path = path, pattern = "*.fcsv")
  }
  else{
    fcsv_list <- dir(path = path, pattern = pattern)
  }
  n_land <- vector("numeric", length=length(fcsv_list))
  for (i in 1:length(fcsv_list)){
    n_land[i] <- length(count.fields(fcsv_list[i]))
  }
  if (is.null(ID) == TRUE){
    if (is.null(string_del) == TRUE){
      dimnames_fcsv <- gsub(".fcsv", "", fcsv_list)
    }
    else{
      dimnames_fcsv <- gsub(string_del, "", gsub(".fcsv", "", fcsv_list))
    }
  }
  else{
    dimnames_fcsv <- ID
  }
  if (length(unique(n_land))!=1){
    stop("Specimens have different number of landmarks.")
  }
  LM_array <- array(data = NA, dim = c(n_land[1], 3, length(fcsv_list)))
  dimnames(LM_array)[[3]] <- dimnames_fcsv
  for (i in 1:length(fcsv_list)){
    LM_array[,,i] <- as.matrix(read.csv(file = fcsv_list[i], sep = ",", skip = 3, header = FALSE)[,2:4])
  }
  if (isTRUE(save.txt) == TRUE){
    for (i in 1:dim(LM_array)[3]){
      write.table(LM_array[,,i], paste0(dimnames(LM_array)[[3]][i], ".txt"), col.names = FALSE, row.names = FALSE)
    }
  }
  return(LM_array)
}