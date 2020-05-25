test_that("multiplication works", {
  set.seed(123) # centers 10, 2, 6

  # Este es donde no converge
  set.seed(123)
  rkmeans(datasets[[1]][sample(1:300,100), -3])
})
