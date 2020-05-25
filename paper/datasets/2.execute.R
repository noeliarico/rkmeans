results <- vector(mode = "list", length = length(datasets))
#for (i in 1:length(datasets)) {
for (i in 1:20) {
  dataset <- copy_datasets[[i]]
  j <- 0
  r <- vector(mode = "list", length = length(dists::availableDistances())+1)
  for (distance in dists::availableDistances()) {
    j <- j + 1
    print(paste0("Dataset ",i," distance ", distance))
    set.seed(123)
    r[[j]] <- rkmeans(dataset, distanceFunction = distance, assignation = "kmeans")
  }
  print(paste0("Dataset ",i," Borda"))
  set.seed(123)
  r[[j+1]] <- rkmeans(dataset, assignation = "rkmeans")
  results[[i]] <- r
}
names(results) <- names(datasets)
#save(results, file = "results.RData")
