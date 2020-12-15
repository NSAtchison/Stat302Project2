sample_formula <- lifeExp ~ gdpPercap + continent
sample_data <- my_gapminder
output <- my_lm(sample_formula, data = sample_data)

test_that("my_lm has correct output", {
  expect_is(output, "matrix")
})

test_that("my_lm has correct input", {
  expect_is(sample_formula, "formula")
  expect_is(sample_data, "data.frame")
})
