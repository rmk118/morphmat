infl_pt_fun <- function(x,y, plot=FALSE) {
  ratio <- y/x # find the ratio between the two morphometric variables

  # compute a kernel density estimate (essentially a smoothed histogram) for the ratio
  densityTest <- density(ratio)

  # convert into a data frame
  densityTest <- data.frame(x=densityTest$x, y=densityTest$y)

  # find the local minimum between the two peaks
  densityTest$is_min <- ggpmisc:::find_peaks(-densityTest$y, ignore_threshold = -0.01)
  min <- densityTest %>% dplyr::filter(is_min==TRUE) %>% dplyr::pull(x)
  min <- stats::median(min)

  if(is.na(min)){
    densityTest$is_min <- ggpmisc:::find_peaks(-densityTest$y)
    min <- densityTest %>% dplyr::filter(is_min==TRUE) %>% dplyr::pull(x)
    min <- stats::median(min)
  }

  # optionally visualize the density plot with minimum
  if(plot==TRUE) {
    print(ggplot2::ggplot()+
            ggplot2::geom_line(aes(x=densityTest$x, y=densityTest$y))+
            ggplot2::geom_vline(xintercept = min, lty="dashed")+
            labs(x="Ratio", y=NULL)+
            ggplot2::theme_light())
  }

  return(min) #return the minimum ratio, equivalent to the slope of a line separating the two clouds of points
}
