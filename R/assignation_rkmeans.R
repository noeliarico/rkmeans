# Aquí tiene que llegar una lista de matrices de todos los objetos solo a los centros
#' @return A vector of length equal to the number of objects in the dataset
#' where each element contains the cluster to which belong the object in the
#' position i
assignation_rkmeans <- function(matrices_of_distances, ranking_rule) {
  # Matrices of distances contains the distance from all the points of the dataset
  # to all the centers.
  #matrices_of_distances <- consensus::as.por(matrices_of_distances)
  # For each object (row) take all the rankings (that row from all the matrices)

  assignations <- sapply(1:nrow(matrices_of_distances[[1]]), function(row) {
    ranking <- t(sapply(matrices_of_distances,"[",row,,drop=FALSE))
    ranking[is.nan(ranking)] <- 0
    ranking <- borda_count(consensus::as.por(ranking))
    which.min(as.numeric(ranking))
  }
  )

  # assignations <- sapply(1:nrow(matrices_of_distances[[1]]), function(row)
  #   consensus::borda_winner(consensus::as.por(t(sapply(matrices_of_distances,"[",row,,drop=FALSE))))
  # )
  #assignations <- str_remove(assignations, "C") %>% as.numeric()
  return(assignations)
  # Take the distance to all the centers and create a ranking
  #
}
