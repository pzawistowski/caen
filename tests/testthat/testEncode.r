
test_that("encoding a perfectly dependent column returns 1s", {
  nrow <- 100
  x <- rep("A", nrow)
  y <- rep("1", nrow)
  enc <- CaEn(x,y)
  
  expect_equal(enc$encode(x), rep(1,nrow))
})

test_that("encoding an constant column returns 0.5", {
  nrow <- 1000
  x <- rep("A", 2*nrow)
  y <- rep(c("-1","1"), nrow)
  enc <- CaEn(x,y)
  
  expect_equal(enc$encode(x), rep(0.5,2*nrow))
})

test_that("encoding can handle previously unseen values", {
  x <- c("A","A","A","B","B","B")
  y <- c("1","1","1","1","2","2")
  enc <- CaEn(x,y)
  
  
  expect_equal(enc$encode(c('C')), c(4/6))
})