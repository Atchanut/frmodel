#' Classical linear regression  model
#'
#' @description
#' To fit simple and multiple linear regression  model
#'
#' @param X a matrix of independent variables with dimension \code{nxp}, where \code{p} is the number of independent variable (\code{k}) + 1
#' @param y a vector of dependent variable (or response variable) with dimension \code{nx1}
#'
#' @return \code{B} is a matrix of regression coefficients with dimension \code{px1}, where
#' \code{the first element} is intercept (\code{b0})
#' \code{the second element}, \code{the third element},...,\code{the k element} be regression coefficient \code{b1, b2,...,bk} corresponding to \code{x1, x2,...,xk}, respectively.
#'
#' @export
#'
#' @references Myers, Raymond H.; Milton, Janet S.(1991) A First Course in the Theory of Linear Statistical Models. PWS-KENT Pub. Co, Boston.
#'
#' @examples
#' x1 <- c(25,40,50,45,35,28,34,52,38,41,27,50)
#' x2 <- c(4,1,0.5,0.5,2,4.5,2,0.5,2.6,1,4.5,0.5)
#' y <- c(3,4.2,4.8,4.5,4.5,3.5,4,5.5,4.6,5,4,4.8)
#' X <- as.matrix(data.frame(x0=1,x1,x2))
#' regmodel(X,y)
regmodel <- function(X,y){
   XTX <- t(X)%*%X
   XTXINV <- solve(XTX)
   XTY  <- t(X)%*%y
   B <-  XTXINV%*%XTY
  return(B)
}
