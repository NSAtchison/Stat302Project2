k <- 5

output <- my_rf_cv(k)

test_that("my_rf_cv has correct input type", {
  expect_is(k, "numeric")
})

test_that("my_rf_cv has correct output type", {
  expect_is(output, "numeric")
})
