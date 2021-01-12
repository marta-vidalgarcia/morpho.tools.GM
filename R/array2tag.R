#' @name array2tag
#' @title array2tag
#' 
#' @description
#'   This function inputs an array of dimensions p, k, n and saves *tag files
#'   
#' @usage
#'   array2tag(dir = NULL, ID = NULL, string_del = NULL, propagated = FALSE, save.txt = FALSE)
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
#' # my_array <- array(1:10, c(5,3,4))
#' # dimnames(my_array)[[3]] <- c("spec1", "spec2", "spec3", "spec4")
#' # array2tag(my_array)
#'   
#' @author Marta Vidal-Garcia
#' @export
#' 

array2tag <- function(array, ID = NULL){
  for (i in 1:dim(array)[3]){
    mat <- array[,,i]
    if (is.null(ID) == TRUE){
      file <- paste0(dimnames(array)[[3]][i], ".tag")
    }
    else{
      file <- paste0(ID[i], ".tag")
    }
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
                  paste0("% Volume 1 points converted with write.tag() ", getwd(), "/", file),
                  "",
                  "Points =",
                  mat_tag)
    writeLines(file_tag, file)
    rm(mat)
    rm(mat_tag)
    rm(point)
    rm(file_tag)
    rm(file)
  }
}    
    
    


