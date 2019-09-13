
### plotOutliers_percentile FUNCTION ###

# This function has been adapted from the plotOutliers function in geomorph, and it has been modified to select for percentiles and save plots
# It will check for outliers at the selected percentile (e.g. 95%, save the plots - if chosen -, and return a list with:
    # 1. all the Procrustes distances to the mean shape
    # 2. only Proc dist. from the potential outliers)
    # 3. a plot of like in plotOutliers and an histogram of the Procrustes distances to mean shape

plotOutliers_percentile <- function (A, groups = NULL, inspect.outliers = FALSE, percentile, save.plot = FALSE) {
  if (length(dim(A)) != 3) {
    stop("Data matrix not a 3D array (see 'arrayspecs').")
  }
  if (is.null(groups)) {
    groups = factor(rep("All Specimens", dim(A)[3]))
  }
  mean_sym <- mshape(A) # estimate mean shape from the rotated configurations
  dist <- ShapeDist(A, mean_sym) # Calculate Procrustes distance to the mean
  names(dist) <- dimnames(A)[[3]]
  N_percentile <- quantile(dist, as.numeric(percentile))
  res <- lapply(levels(groups), function(j) {
    mn <- matrix(t(mshape(A[, , which(groups == j)])), nrow = 1)
    A.d <- two.d.array(A[, , which(groups == j)])
    d <- NULL
    for (i in 1:nrow(A.d)) {
      d <- c(d, as.vector(dist(rbind(mn, A.d[i, ]))))
    }
    if (is.null(dimnames(A.d)[[1]])) {
      dimnames(A.d)[[1]] <- as.character(seq(1:nrow(A.d)))
    }
    names(d) <- dimnames(A.d)[[1]]
    D <- d[order(d, decreasing = TRUE)]
    Q <- summary(D)
    P_percentile <- quantile(D, as.numeric(percentile))
    Med <- as.numeric(summary(D)[3])
    if (isTRUE(save.plot)) {
      dir.create("outliers_figs") # decide whether to put it in a separate folder
      pdf(paste("outliers_figs/group_", j, ".pdf", sep = ""))
      # pdf(paste("group_", j, ".pdf", sep = "")) # save it in the same directory
      par(mfrow=c(2,1))
      plot(D, type = "p", ylab = "Procrustes Distance from Mean", 
           pch = 19, xlab = "", xaxt = "n", main = j)
      abline(a = Med, b = 0, col = "blue")
      abline(a = P_percentile, b = 0, lty = 2, col = "blue")
      text(x = nrow(A.d), y = Med, labels = "median", col = "blue", 
           cex = 0.5, adj = c(0.5, -0.5))
      text(x = nrow(A.d), y = P_percentile, labels = paste(percentile*100, "th percentile", sep = ""), 
           col = "blue", cex = 0.5, adj = c(0.5, -0.5))
      if (any(D >= P_percentile)) {
        points(D[which(D >= P_percentile)], pch = 19, col = "red")
        text(D[which(D >= P_percentile)], labels = names(D)[which(D >= 
                                                                    P_percentile)], col = "red", adj = 0.8, pos = 4, cex = 0.5)
      }
      else {
        text(D, labels = names(D), adj = c(0.5, 0.1), pos = 4, cex = 0.5)
      }
      hist(D)
      abline(v = mean(P_percentile), col="blue", lwd=2, lty=2)
      dev.off()
    }
    par(mfrow=c(2,1))
    plot(D, type = "p", ylab = "Procrustes Distance from Mean", 
         pch = 19, xlab = "", xaxt = "n", main = j)
    abline(a = Med, b = 0, col = "blue")
    abline(a = P_percentile, b = 0, lty = 2, col = "blue")
    text(x = nrow(A.d), y = Med, labels = "median", col = "blue", 
         cex = 0.5, adj = c(0.5, -0.5))
    text(x = nrow(A.d), y = P_percentile, labels = paste(percentile*100, "th percentile", sep = ""), 
         col = "blue", cex = 0.5, adj = c(0.5, -0.5))
    if (any(D >= P_percentile)) {
      points(D[which(D >= P_percentile)], pch = 19, col = "red")
      text(D[which(D >= P_percentile)], labels = names(D)[which(D >= 
                                                                  P_percentile)], col = "red", adj = 0.8, pos = 4, cex = 0.5)
      if (inspect.outliers == TRUE) {
        out.config <- names(D)[which(D >= P_percentile)]
        for (oc in out.config) {
          plotRefToTarget(mshape(A), matrix(A.d[oc, ], 
                                            ncol = dim(A)[2], byrow = T), method = "vector", 
                          label = TRUE)
          title(main = paste("group: ", j, ", specimen: ", 
                             oc, sep = ""))
        }
      }
    }
    else {
      text(D, labels = names(D), adj = c(0.5, 0.1), pos = 4, 
           cex = 0.5)
    }
    hist(D)
    abline(v = mean(P_percentile), col="blue", lwd=2, lty=2)
    ordered <- match(D, d)
    names(ordered) <- names(D)
    return(ordered)
  })
  names(res) <- levels(groups)
  if (length(levels(groups)) == 1) {
    res <- res$`All Specimens`
  }
    percentile_points <- as.data.frame(cbind(as.character(groups)[which(dist >= N_percentile)], dist[which(dist >= N_percentile)]))
    values <- as.data.frame(cbind(as.character(groups), dist))
    colnames(values) <- c("group", "Proc. d. from mean")
    colnames(percentile_points) <- c("group", "Proc. d. from mean")
    par(mfrow=c(1,1))
  return(list("All_Proc_d" = values, "Proc_d_percentile" = percentile_points))
}


