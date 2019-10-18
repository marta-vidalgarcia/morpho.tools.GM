\name{plotOutliers_percentile}
\title{plotOutliers_percentile}
\alias{plotOutliers_percentile}

\description{
This function has been adapted from the plotOutliers function in geomorph, and it has been modified to select for percentiles and save plots
}
\usage{
plotOutliers_percentile(A, groups = NULL, inspect.outliers = FALSE, percentile, save.plot = FALSE)
}

\arguments{
  \item{A}{
Same as in the plotOutliers function in geomorph. An object of the class "array" that contains Procrustes shape variables for 'n' specimens
}
  \item{groups}{
Same as in the plotOutliers function in geomorph. An optional factor defining groups
}
  \item{inspect.outliers}{
Same as in the plotOutliers function in geomorph. A logical value indicating whether to plot outlier shape configurations as compared to the consensus
}
  \item{percentile}{
A numerical value indicating the prefered percentile (e.g. 95%)
}
  \item{save.plot}{
A logical value indicating whether to save the plots in the current working directory.
}
}

\value{
This function returns a list with two objects and plots two different plots.
The first object in the list ($All_Proc_d) contains all the Procrustes distances to the mean shape. The second object on the list ($Proc_d_percentile) only contains Procrustes distances from the potential outliers.
The first plot is exactly the same as in the plotOutliers function in geomorph (Adams et al., 2018): all specimens are plotted in order by their Procrustes distance from the mean shape. Both the median distance (unbroken blue line) and the percentile (dashed blue line) summarize the distances from the mean shape. Specimens falling above the selected percentile are plotted in red, like in the plotOutliers function in geomorph (Adams et al., 2018). 
The second plot is a histogram, that shows the frequency of different prouped values of Procrustes distance from the mean shape. The abline separates the values based on the selected percentile (dashed blue line).

}

\details{
This function is highly based on the plotOutliers function in geomorph (Adams et al., 2018), and it has been modified to select for percentiles and save plots (including a histogram). It will check for outliers at the selected percentile (e.g. 95%, save the plots - if chosen), and return a list with: (1) all the Procrustes distances to the mean shape, (2) only Procustes distances from the potential outliers, (3) a plot of like in plotOutliers function in geomorph (Adams et al., 2018) and an histogram of the Procrustes distances to mean shape.
}

\examples{
species <- c("sp_A", "sp_A", "sp_B", "sp_A", "sp_A", "sp_B", "sp_B", "sp_A", "sp_A", "sp_A", "sp_A")
# Only visualise the 
plotOutliers_percentile(GPA$coords, percentile = 0.95) 
plotOutliers_percentile(GPA$coords, percentile = 0.99, groups = species, save.plot = TRUE)
}

\references{
Adams, D. C., M. L. Collyer, and A. Kaliontzopoulou. 2018. Geomorph: Software for geometric morphometric analysis. R package version 3.0.6.
Available at http://CRAN.R-project.org/package=geomorph.


}
\author{
Marta Vidal-Garcia (marta.vidalga@gmail.com)
}

