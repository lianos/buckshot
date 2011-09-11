context("basic tests")
data(arcene, package="buckshot")

test_that("logistic regression on dense data", {
  suppressWarnings({
    model <- buckshot(A.arcene, y.arcene, 'logistic', lambda=0.166)
    bingo <- all(predict(model, A.arcene) == y.arcene)
  })
  expect_true(bingo, info="Dense model")
})

test_that("logistic regression on sparse data", {
  ms <- Matrix(A.arcene, sparse=TRUE)
  suppressWarnings({
    model <- buckshot(ms, y.arcene, 'logistic', lambda=0.166)
    bingo <- all(predict(model, ms) == y.arcene)
  })
  expect_true(bingo, info="Sparse model")
})  

test_that("warnings emmited when design matrices have all-zero columns", {
  expect_warning({
    bd <- BuckshotData(A.arcene, y.arcene)
  }, "Removing.*?69", info="Warn on building data object")
  
  model <- buckshot(bd, lambda=0.166)
  expect_warning(predict(model, A.arcene), "remove", info="warn on predict")
})
