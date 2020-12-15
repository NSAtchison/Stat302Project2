sample_data <- my_penguins[,3:6]
sample_cl <- as.character(my_penguins$species)
k_nn <- 5
k_cv <- 5
output <- my_knn_cv(sample_data, sample_cl, k_nn, k_cv)



test_that("my_knn_cv has correct output types", {
  expect_is(output, "list")
  expect_is(output$Class, "character")
  expect_is(output$CV_error, "numeric")
})

test_that("my_knn_cv has correct input types", {
  expect_is(sample_data, "data.frame")
  expect_is(sample_cl, "character")
  expect_is(k_nn, "numeric")
  expect_is(k_cv, "numeric")
})
