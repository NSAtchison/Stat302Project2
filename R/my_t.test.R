#' T-Test function
#'
#' This function runs a t-test
#'
#' @param x A numeric vector of data
#' @param alternative A character string specifying alternate hypothesis. Must
#'   take on the value of "two.sided", "less", or "greater"
#' @param mu A numeric indicating the null hypothesis value of the mean
#' @keywords inference
#'
#' @return A list with elements \code{test_stat}, a numeric representing the test statistic,
#'   \code{df}, a numeric representing the degree of freedom,
#'   \code{alternative}, a character string indicating the alternative
#'   hypothesis, and \code{p_val}, a numeric containing the p-value
#'
#' @examples
#' set.seed(203)
#' sample.size <- 100
#' test_data <- rbinom(sample.size, size = 1, prob = 0.59)
#' my_t.test(test_data, "two.sided", 0.5)
#' my_t.test(test_data, "less", mu = 0.5)
#'
#' @export
#Creates the my_t.test function with inputs of data x, alternative hypothesis, and value of mu)
my_t.test <- function(x, alternative = c("two.sided", "less", "greater"), mu) {
  #Finds degree of freedom for data
  degFree <- length(x) - 1
  #Finds the test Statistic for the data
  testStat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  #Finds the p value for either a two sided t-test or a one sided t-test
  if(alternative == "two.sided") {
    pval <- 2 * pt(abs(testStat), df = degFree, lower.tail = FALSE)
  } else if (alternative == "greater") {
    pval <- pt(testStat, df = degFree, lower.tail = TRUE)
  } else {
    pval <- pt(testStat, df = degFree, lower.tail = FALSE)
  }
  #returns the test statistic, degree of freedom, alternative hypothesis, and p value for this t-test
  return(list(test_stat = testStat, df = degFree, alternative = alternative, p_val = pval))
}
