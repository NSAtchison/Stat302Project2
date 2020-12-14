#' KNN function
#'
#' This function runs a K-Nearest-Neighbor cross validation
#'
#' @param train Dataframe input containing information used to predict class
#'   of each observation.
#' @param cl Vector input containing the true class of each data point within
#'   the dataframe \code{train}.
#' @param k_nn Numeric input indicating number of nearest neighbors to use
#'   within function.
#' @param k_cv Numeric input indicating number of folds used to seperate data.
#' @keywords prediction
#'
#' @return A list with elements \code{class} which contains predicted values for
#'   all observations and a numeric \code{cv_err} representing the
#'   cross-validation misclassification error.
#'
#' @import magrittr
#' @import class
#'
#' @examples
#' my_knn_cv(my_penguins[,3:6], my_penguins$species, 1, 5)
#'
#' @export
#Creates the function my_knn_cv with 4 inputs: train, cl, k_nn, and k_cv
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  #puts the data set, train, into a variable that I can change
  data <- train
  #adds the class vector to the dataframe data
  data$cl <- cl
  #Gets rid of any data that has NA Answers
  data <- stats::na.omit(data)
  #Creates folds to apply to the data frame
  fold <- sample(rep(1:k_cv, length = nrow(data)))
  data$fold <- fold

  #Creates 2 matrices, one for the predictions and one for the misclarification error
  pred_mat <- matrix(NA, nrow(data), 2)
  missclar_mat <- matrix(NA, k_cv)

  #Creates a for loop
  for(i in 1:k_cv) {
    #sets number of incorrect predictions to zero
    incorrect <- 0
    #records which fold that is currently the test data
    fold_num <- i

    #Creates the training and test data
    data_train <- data %>% dplyr::filter(fold != i)
    data_test <- data %>% dplyr::filter(fold == i)
    #Creates the class vectors for the training and test data
    cl_train <- as.vector(data_train$cl)
    cl_test <- as.vector(data_test$cl)

    #Predicts the class of the test data
    knn_pred <- as.vector(knn(train = data_train[,1:ncol(train)],
                              test = data_test[,1:ncol(train)],
                              cl = cl_train, k = k_nn, prob = TRUE))

    #stores the predictions and true class of the test data
    pred_mat[fold == i, 1] <- knn_pred
    pred_mat[fold == i, 2] <- cl_test

    #Creates a for loop to test how many incorrect predictions
    for(i in 1:length(cl_test)) {
      if(knn_pred[i] != cl_test[i]) {
        incorrect = incorrect + 1
      }
    }
    #Finds the misclarification error for this fold
    missclar_mat[fold_num,] <- incorrect / length(cl_test)
  }
  #Finds the average misclarifcation error for all folds
  cv_err <- mean(missclar_mat)
  #Stores all of the predictions into the class variable
  class <- as.vector(knn(train = data[,1:ncol(train)], test = data[,1:ncol(train)], cl = data$cl, k = k_nn, prob = TRUE))
  #Creates a list containing the class vector and the cv_error
  x <- list("Class" = class, "CV_error" <- cv_err)
  #returns the list created
  return(x)
}
