test_that("test-model1", {
  x <- c(1,3,4,5,8,10)
  y <- c(4,6,8,7,10,16)
  expect_equal(model1(x,y), model1(x=x,y=y))
})
