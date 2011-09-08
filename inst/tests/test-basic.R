context("basic tests")

test_that("logistic regression works on arcene data", {
  data(arcene, package="buckshot")
  model <- buckshot(A.arcene, y.arcene, 'logistic', lambda=0.166)
  accuracy <- sum(predict(model, A.arcene) == y.arcene) / length(y.arcene)
  expect_equal(accuracy, 1)
})
