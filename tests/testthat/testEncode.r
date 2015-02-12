library(testthat)
library(caen)

test_that("encoding a perfectly dependent column returns 1s", {
  nrow <- 100
  x <- rep("A", nrow)
  y <- rep("1", nrow)
  
  expect_equal(encode(x,y), rep(1,nrow))
})

test_that("encoding an constant column returns 0.5", {
  nrow <- 1000
  x <- rep("A", 2*nrow)
  y <- rep(c("-1","1"), nrow)
  
  expect_equal(encode(x,y), rep(0.5,2*nrow))
})

test_that("encoding can utilise a given mapping object", {
  map <- list(A=0.2, B=0.3)
  expect_equal(encode(c('B','B','A'),map=map), c(0.3,0.3,0.2))
})

test_that("encoding can utilise return the mapping object", {
  nrow <- 5
  x <- rep(c("A","B"), nrow)
  y <- rep(c("1","2"), nrow)
  
  res <- encode(x, target=y, map.return = T)
  
  expect_equal(res$encoded, rep(c(1.0,0),nrow))
  expect_false(is.null(res$map))
})

test_that("encoding can handle values not present in the mapping object", {
  x <- c("A","A","A","B","B","B")
  y <- c("1","1","1","1","2","2")
  
  res <- encode(x,target=y,map.return = TRUE)
  expect_equal(encode(c('C'),map=res$map), c(4/6))
})