#' @name write.fcsv
#' @title write.fcsv
#' 
#' @description
#'   This function exports matrix to a *.fcsv file
#'   
#' @usage
#'   write.fcsv(mat, file)
#'   
#' @param mat xy matrix with three columns (x, y, z) and the landmarks ordered by row
#' 
#' @param file name of the fcsv file to save
#' 
#' @return This function saves a *.fcsv file from a matrix
#'
#' @examples
#' # mat <- matrix(1:12), nrow = 4, ncol = 3)
#' # write.fcsv(mat, "my_matrix.fcsv")
#'   
#' @author Marta Vidal-Garcia
#' @export
#' 

write.fcsv <- function(mat, file){
  mat_fcsv <- c()
  for (i in 1:nrow(mat)){
    point <- paste(i, mat[i,1], mat[i,2], mat[i,3], 0, 0, 0, 1, 1, 1, 0, i, "", "", sep = ",")
    mat_fcsv <- append(mat_fcsv, point)
  }
  file_fcsv <- c("# Markups fiducial file version = 4.11",
                "# CoordinateSystem = LPS",
                "# columns = id,x,y,z,ow,ox,oy,oz,vis,sel,lock,label,desc,associatedNodeID",
                mat_fcsv)
  writeLines(file_fcsv, file)
}