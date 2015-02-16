
test_that("encoding a perfectly dependent column returns 1s", {
  nrow <- 100
  x <- rep("A", nrow)
  y <- rep("1", nrow)
  enc <- CaEn(x,y)
  
  expect_equal(enc$encode(x), rep(1,nrow))
})

test_that("encoding a constant column returns 0.5", {
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

test_that("encoding rare values is influenced by global means", {
  nrow <- 1000
  x <- c("A","A", rep("B", nrow-2))
  y <- c("1","1",rep(c("-1","1"), nrow/2-1))
  enc <- CaEn(x,y, k=2, f = 2)
  
  expect_equal(enc$encode(x)[1:2], c(0.75, 0.75), tolerance=0.01)
})


test_that("encoding skewed values works properly", {
  x <- c(rep("A", 500), rep("B",500))
  y <- c(rep("1", 200), rep("-1",300), rep("1", 300), rep("-1",200))
  enc <- CaEn(x,y)
  
  expect_equal(enc$encode(x), c(rep(0.4, 500), rep(0.6, 500)), tolerance=0.01)
})


test_that("encoding allows choosing the target value", {
  x <- c("A","A","A","B","B","B")
  y <- c("1","1","2","2","2","2")
  enc <- CaEn(x,y, target.value = "2", k=0, f=0.1)
  
  
  expect_equal(enc$encode(c('A','B')), c(0.33, 1.0), tolerance=0.01)
})


test_that("encoding multiple column data.frames is possible", {
  nrow <- 100
  x <- data.frame(x1 = rep("A", nrow), x2 = rep("B", nrow))
  y <- rep("1", nrow)
  enc <- CaEn(x,y)
  
  expect_equal(enc$encode(x), array(1,c(nrow,2)))
})
