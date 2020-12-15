#' Random Forest cross-validation function
#'
#' This function does a Random Forest cross validation
#'
#' @param k Numeric input for number of folds desired
#'
#' @keywords prediction
#'
#' @return Numeric representing the cross-validation error
#'
#' @import randomForest
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(8)
#'
#' @export
#Creates the function my_rf_cv with 1 input: k
my_rf_cv <- function (k) {
  #Stores the my_penguins data into a variable we can change
  penguins_df <- my_penguins
  #Removes all data that contains NA Values
  penguins_df <- stats::na.omit(penguins_df)
  #Seperates the penguins_df dataframe into folds
  fold <- sample(rep(1:k, length = nrow(penguins_df)))
  penguins_df$fold <- fold

  #Creates a matrix to contain the mse values for each fold
  mse_mat <- matrix(NA, k)

  #Creates a for loop to run each fold through the inside
  for (i in 1:k) {
    #Creates the training and test data
    data_train <- penguins_df %>% dplyr::filter(fold != i)
    data_test <- penguins_df %>% dplyr::filter(fold == i)
    #Creates and trains a random forest model with 100 trees
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm, data = data_train, ntree = 100)
    #Predicts the body_mass_g for the current fold
    prediction <- stats::predict(model, data_test[, -1])
    #Finds the MSE for this fold and inputs it into our mse matrix
    mse_mat[i,] <- mean((prediction - data_test$body_mass_g)^2)
  }
  #Finds the average MSE across all folds
  cv_err <- mean(mse_mat)
  #Returns the CV MSE
  return(cv_err)
}
