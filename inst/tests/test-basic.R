context("basic tests")

test_that("logistic regression works on arcene data", {
  data(arcene, package="buckshot")
  model <- buckshot(A, y, 'logistic', lambda=0.166)
  accuracy <- sum(predict(model, A) == y) / length(y)
  expect_equal(accuracy, 1)
})
