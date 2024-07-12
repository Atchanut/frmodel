test_that("test-regmodel", {
  x1 <- c(1,3,4,5,8,10)
  y <- c(4,6,8,7,10,16)
  X <- as.matrix(data.frame(x0=1,x1))
  X2 <- as.matrix(data.frame(x1))
  expect_equal(regmodel(X,y), regmodel(X=X,y=y))
  expect_equal(regmodel(X2,y), regmodel(X=X2,y=y))
})
