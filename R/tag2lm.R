#' @name tag2lm
#' @title tag2lm
#' 
#' @description
#'   This function inputs a single *tag file into an matrix with the landmark coordinates (x, y, z)
#'   
#' @usage
#'   tag2lm(file)
#'   
#' @param file MINC *.tag file to import.
#' 
#' @return This function returns matrix with the landmark coordinates (x, y, z)
#' 
#' @examples
#' # specimen1 <- tag2lm("specimen1.tag")
#'   
#' @author Marta Vidal-Garcia
#' @export
#' 

tag2lm <- function(file){
  lm_matrix <- suppressWarnings(read.table(file = file, skip = 4, sep = " ", header=F))[, 2:4]
  colnames(lm_matrix) <- c("x", "y", "z")
  return(lm_matrix)
}