#' Simple regression model
#'
#' @param x an independent variable
#' @param y a dependent or response variable
#'
#' @return regression coeficient
#' @export
#'
#' @examples
#' x <- c(1,3,4,5,8,10)
#' y <- c(4,6,8,7,10,16)
#' model1(x,y)
model1 <- function(x,y){
  lm(y~x)
}
