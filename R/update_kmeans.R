update_kmeans <- function(data, assignations,
                          developer = FALSE) {
  k <- length(unique(assignations))
  clusters <- lapply(1:k, function(x) { data[which(assignations == x), , drop = FALSE] })
  t(sapply(clusters, colMeans))
}
