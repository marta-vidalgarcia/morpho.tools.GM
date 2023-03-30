#' @name pp2lm
#' @title pp2lm
#'
#' @description
#'   This function inputs a single *pp file into a matrix of n rows and x, y, z columns
#'
#' @usage
#'   pp2lm(file = file)
#'
#' 
#' @param file
#'#'
#' @return This function returns  matrix of n rows (number of landmarks) and x, y, z columns.
#'
#' @examples
#' # LMs <- pp2lm("./test_LMs.pp")

#' @author Marta Vidal-Garcia
#' @export
#'

pp2lm <- function(file = file){
  raw_LM_file <- readLines(file)
  LM_matrix <- matrix(data = NA, nrow = length(raw_LM_file)-9, ncol = 3)
  for (i in 9:(length(raw_LM_file)-1)){
    LM_matrix[i-8,1] <- as.numeric(strsplit(strsplit(raw_LM_file, "x=\"")[[i]][2], "\"")[[1]][1])
    LM_matrix[i-8,2] <- as.numeric(strsplit(strsplit(raw_LM_file, "y=\"")[[i]][2], "\"")[[1]][1])
    LM_matrix[i-8,3] <- as.numeric(strsplit(strsplit(raw_LM_file, "z=\"")[[i]][2], "\"")[[1]][1])
  }
  rm(raw_LM_file)
  return(LM_matrix)
}
  