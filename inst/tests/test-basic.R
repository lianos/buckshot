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

test_that("design matrix reconstruction works from BuckshotData", {
  m <- matrix(sample(100), 10)
  colnames(m) <- sample(letters[1:10], )
  m[sample(100, 30)] <- 0
  y <- sample(c(-1, 1), nrow(m), replace=TRUE)
  M <- Matrix(m, sparse=TRUE)
  
  xy <- buckshot:::preprocess.xy(M, y)
  
  bd <- BuckshotData(xy$x, xy$y)
  re.x <- designMatrix(bd)
  
  expect_identical(as.matrix(re.x), as.matrix(xy$x))
})
