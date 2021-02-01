#' @name write.tag
#' @title write.tag
#' 
#' @description
#'   This function inputs *tag files into an array of dimensions p, k, n
#'   
#' @usage
#'   write.tag(mat, file)
#'   
#' @param mat xy matrix with three columns (x, y, z) and the landmarks ordered by row
#' 
#' @param file name of the tag file to save
#' 
#' @return This function saves a *.tag file from a matrix
#'
#' @examples
#' # mat <- matrix(c(1,2,3, 4,5,6, 7,8,9, 10,11,12), nrow = 4, ncol = 3)
#' # write.tag(mat, "my_matrix.tag")
#'   
#' @author Marta Vidal-Garcia
#' @export
#' 

write.tag <- function(mat, file){
  mat_tag <- c()
  for (i in 1:nrow(mat)){
    if (i == nrow(mat)){
      point <- paste0(" ", mat[i,1], " ", mat[i,2], " ", mat[i,3], " ", 1, " ",
                      1, " ", 1, " ", '"Marker";')
      mat_tag <- append(mat_tag, point)
    }
    else {
      point <- paste0(" ", mat[i,1], " ", mat[i,2], " ", mat[i,3], " ", 1, " ",
                      1, " ", 1, " ", '"Marker"')
      mat_tag <- append(mat_tag, point)  
    }
  }
  file_tag <- c("MNI Tag Point File",
                "Volumes = 1;",
                paste0("% Volume 1 points converted with morpho.tools.GM::write.tag() ", "/", file),
                "",
                "Points =",
                mat_tag)
  writeLines(file_tag, file)
}