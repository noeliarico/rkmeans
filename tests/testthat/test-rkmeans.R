test_that("Arguments are valid", {


# Invalid assignation -----------------------------------------------------

expect_error(rkmeans(iris, assignation = "random"),
             'Invalid value for assignation argument. Try "kmeans" or "rkmeans"')

# Invalid update ----------------------------------------------------------

expect_error(rkmeans(iris, update = "random"),
             'Invalid value for update argument. Try "kmeans" or "rkmeans"')


})
