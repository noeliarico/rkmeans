test_that("Convergence with single distances", {
  test_distances <- c("can","che","cos","euc","jac","man","mat","ney","pea","trd")
  #test_errors <- vector(mode = "list", length = length(test_distances))
  for(da in datasets[1:5]) {
    for (di in seq_along(test_distances)) {
      #test_errors[[di]] <- rkmeans(toy2[,-3], distanceFunction = test_distances[d])$errors
      test_errors <- rkmeans(da[,-ncol(da)], distanceFunction = test_distances[di])$errors
      for(e in seq_along(test_errors)[-1]) {
        if(test_errors[e] == 0) {
          break
        }
        expect_lte(test_errors[e-1], test_errors[e])
      }
    }
  }
})
