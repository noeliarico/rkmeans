# The aim of this file is looking for a small dataset to use as an example for
# explaining the method rkmeans. An example which illustrates good results of Borda
#for(the_seed in 1000:2500) { # Try 500 different seeds
for(the_seed in 100:200) { # Try 500 different seeds
# 98
  #no se si 139 o 140 da error

  # Create a dataset of 2 varibles with 5 clustes of three points each
  data <- clusterlab(centers = 5, # the number of clusters to simulate
                     # the number of units of the radius of the circle on which the clusters are generated
                     r = 2,
                     # the number of samples in each cluster
                     numbervec = c(3, 3, 3, 3, 3),
                     # standard deviation of each cluster
                     sdvec = c(1, 3, 2, 1, 1),
                     alphas = c(3, 5, 1, 4, 2),
                     # the number of features for the data
                     features = 2,
                     seed = the_seed, showplots = FALSE)
  (points <- as_tibble(t(data$synthetic_data)) %>% rename(x = 1, y = 2))

  normalize <- function(x){((x-min(x))/(max(x)-min(x)))} # ya se hace en el train_rkmeans
  points <- as_tibble(apply(points, 2, normalize))
  points <- points %>% mutate(cluster = data$identity_matrix$cluster)
  #ggplot(points, aes(x, y)) + geom_point(aes(color = cluster), size = 6) + theme_light()

  #dyn.load("02.method/distances/distances.so")
  #dyn.load("02.method/rkmeans/rkmeans.so")

  # Clustering with rkmeans
  set.seed(the_seed)
  example <- train_borda(points[,-3])

  the_best <- names(example)[example == 1]
  if(length(the_best) == 1) {
    if(the_best == "borda") {
      sink("search.txt", append = TRUE)
      cat("Seed ", the_seed, " -- The best!!\n")
      cat(the_best, "\n")
      sink()
      print(paste0(the_seed,";"))
      #break() # El primero que encuentra es 1007
    }
  } else {
    sink("search.txt", append = TRUE)
    cat("Seed", the_seed, "-- Ties\n")
    cat(the_best, "\n")
    sink()
  }



}
