#' @name pp2array
#' @title pp2array
#'
#' @description
#'   This function inputs *pp files into an array of dimensions p, k, n
#'
#' @usage
#'   pp2array(dir = NULL, ID = NULL, string_del = NULL, save.txt = FALSE, write.tag = FALSE)
#'
#' @param dir Optional argument. Directory where the *.pp files are. Default (NULL) is the current working directory.
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
#' # array <- pp2array()
#' # If we have other *.pp files in another directory and with the suffix "skull"
#' # For example: "spec1_skull.pp", "spec2_skull.pp", "spec3_skull.pp", etc.
#' # dir <- "~/Documents/skull_LMs")
#' # skull_array <- pp2array(dir = skull_dir, string_del="_skull", save.tag = TRUE) # will save *.tag landmark files
#'
#' @author Marta Vidal-Garcia
#' @export
#'

pp2array <- function(dir = NULL, ID = NULL, string_del = NULL, save.txt = FALSE, save.tag = FALSE){
  if (is.null(dir) == TRUE) {
    path <- getwd()
  } else {
    path <- getwd()
    setwd(dir)
  }
  pp_list <- dir(path = path, pattern="*.pp")
  n_land <- vector("numeric", length=length(pp_list))
  for (i in 1:length(pp_list)){
    n_land[i] <- length(count.fields(pp_list[[i]])) -9 # 8 lines before the LMs and one at the bottom
  }
  if (is.null(ID) == TRUE){
    if (is.null(string_del) == TRUE){
      dimnames_pp <- gsub(".pp", "", pp_list)
    } else{
      dimnames_pp <- gsub(string_del, "", gsub(".pp", "", pp_list))
    }
  } else{
    dimnames_pp <- ID
  }
  if (length(unique(n_land))!=1){
    stop("Specimens have different number of landmarks.")
  }
  LM_array <- array(data = NA, dim = c(n_land[1], 3, length(pp_list)))
  dimnames(LM_array)[[3]] <- dimnames_pp
  for (i in 1:length(pp_list)){
    raw_LM_file <- readLines(pp_list[i])[9:(8 + n_land[1])]
    for (j in 1:length(raw_LM_file)){
      LM_array[j,1,i] <- as.numeric(strsplit(strsplit(raw_LM_file[j], "x=\"")[[1]][2], "\"")[[1]][1])
      LM_array[j,2,i] <- as.numeric(strsplit(strsplit(raw_LM_file[j], "y=\"")[[1]][2], "\"")[[1]][1])
      LM_array[j,3,i] <- as.numeric(strsplit(strsplit(raw_LM_file[j], "z=\"")[[1]][2], "\"")[[1]][1])
    }
    rm(raw_LM_file)
  }
  if (isTRUE(save.txt) == TRUE){
    for (i in 1:dim(LM_array)[3]){
      write.table(LM_array[,,i], paste0(dimnames(LM_array)[[3]][i], ".txt"), col.names = FALSE, row.names = FALSE)
    }
  }
  if (isTRUE(save.tag) == TRUE){
    write.tag(LM_array, paste0(dimnames(LM_array)[[3]], ".tag"))
  }
  setwd(path)
  return(LM_array)
}
  
