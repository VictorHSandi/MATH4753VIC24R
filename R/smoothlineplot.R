#' Title
#'
#' @param data data frame to work with
#' @param x_input the x value for the data
#' @param y_input the y value for the data
#' @param col_input what the color should be for the plot
#'
#' @return Plot with smooth lines connecting the data points
#' @export
#'
#' @examples smoothlineplot(spruce.df, BHDiameter, Height, BHDiameter)
smoothlineplot <- function(data,x_input,y_input, col_input){
  library(ggplot2)
  g=ggplot(data, aes(x=x_input,y=y_input,col=col_input))
  g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
  g
}
