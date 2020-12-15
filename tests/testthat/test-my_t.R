sample_data <- my_gapminder$lifeExp
sample_alt_hypoth <- "two.sided"
sample_mu <- 0.5

output <- my_t.test(sample_data, sample_alt_hypoth, sample_mu)

test_that("my_t.test has correct output types", {
  expect_is(output, "list")
  expect_is(output$test_stat, "numeric")
  expect_is(output$df, "numeric")
  expect_is(output$alternative, "character")
  expect_is(output$p_val, "numeric")
})

test_that("my_t.test has correct input types", {
  expect_is(sample_data, "numeric")
  expect_is(sample_alt_hypoth, "character")
  expect_is(sample_mu, "numeric")
})
