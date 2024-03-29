% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.fcsv.R
\name{write.fcsv}
\alias{write.fcsv}
\title{write.fcsv}
\usage{
write.fcsv(mat, file)
}
\arguments{
\item{mat}{xy matrix with three columns (x, y, z) and the landmarks ordered by row}

\item{file}{name of the fcsv file to save}
}
\value{
This function saves a *.fcsv file from a matrix
}
\description{
This function exports matrix to a *.fcsv file
}
\examples{
# mat <- matrix(1:12), nrow = 4, ncol = 3)
# write.fcsv(mat, "my_matrix.fcsv")
  
}
\author{
Marta Vidal-Garcia
}
