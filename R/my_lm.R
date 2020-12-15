#' Linear Model function
#'
#' This function creates and fits a linear model
#'
#' @param formula Formula class object being used
#' @param data Data frame containing data used within \code{formula}
#'
#' @keywords inference
#'
#' @return Table containing columns with Estimate, Std. Error, t value and Pr(>|t|)
#'
#' @examples
#' my_lm(dist ~ speed, data = cars)
#'
#' @export
#Creates my Linear Model Function with inputs of the formula and the data were using
my_lm <- function(formula, data) {
  #Creates a model frame from inputs
  modFrame <- stats::model.frame(formula, data = data)
  #Creates a matrix using inputs
  xMat <- stats::model.matrix(formula, data = data)
  #Creates a matrix from created model frame
  yMat <- stats::model.response(modFrame)
  #Finds the Linear Regression Coefficients
  linRegCoef <- solve((t(xMat) %*% xMat)) %*% (t(xMat) %*% yMat)
  #Finds the degree of freedom
  degFree <- nrow(data) - nrow(linRegCoef)
  #Finds the variance
  lmVar <- sum((yMat - (xMat %*% linRegCoef)) ^ 2 / degFree)
  #Finds the standard error
  lmStdErr <- sqrt(diag(lmVar * solve(t(xMat) %*% xMat)))
  #Finds the test statistic
  lmTestStat <- linRegCoef / lmStdErr
  #Finds the p-value
  lmPVal <- stats::pt(abs(lmTestStat), degFree, lower.tail = FALSE) * 2

  #Creates an empty matrix
  lmTab <- matrix(NA, nrow = nrow(linRegCoef), ncol = 4)
  #Fills in the four columns of the matrix with information calculated above
  lmTab[, 1] <- round(linRegCoef, 4)
  lmTab[, 2] <- round(lmStdErr, 4)
  lmTab[, 3] <- round(lmTestStat ,4)
  lmTab[, 4] <- lmPVal
  #Gives the created matrix column and row names
  colnames(lmTab) <- c("Estimate", "Std. Error", "t Value", "Pr(>|t|)")
  rownames(lmTab) <- colnames(xMat)
  #Returns the filled out matrix
  return(lmTab)
}
