#' @export
update_rkmeans <- function(data, assignations, matrices_of_distances,
                           developer = FALSE) {

  # Calculate the matrice of distances
  #matrices_of_distances <- create_distance_matrices(data)

  # The vector clusters contains the cluster to which the object in the position
  # i belongs
  set.seed(1234)
  assignations <- sample(1:3, nrow(data), TRUE)
  k <- length(unique(assignations))

  # We already have a list of distance matrices, which contains one matrix
  # for each considered distance function that stores the distance between
  # each pair of points. Our aim is to create a ranking of objects in each
  # cluster so we need to filter the matrices in order to keep only the
  # points that belong to each cluster so we can rank them later.
  matrices_k <- lapply(1:k, filter_objects_in_cluster,
                       assignations, matrices_of_distances)
  names(matrices_k) <- paste0("cluster", 1:k)
  #print(matrices_k)

  # With the filtered rankings, we need to translate each row to a ranking.
  # By doing this, we get to know which are the nearest objects to each
  # object in a concrete cluster
  rankings_of_objects <- determine_center(matrices_k)

  # All the rankings are aggregated and map into a profile of rankings
  #rankings_of_objects <- lapply(rankings_of_objects, function(x) Reduce(bind_rows, x))

  #rankings_of_objects <- lapply(rankings_of_objects, profile_of_rankings)

  #print(rankings_of_objects)

  # Borda count is applied to the resultant profile of rankings
  #rankings_of_objects <- lapply(rankings_of_objects, borda_count)

}

# Aux functions -----------------------------------------------------------

#' @param the_cluster The index of the cluster for which the objects will be
#' filtered
#' @param assignations A vector containing the cluster to which each object belongs
#' @param distance_matrices A matrix containing the distance between the n
#' objects in the dataset pair by pair
filter_objects_in_cluster <- function(the_cluster, assignations, distance_matrices) {
  indexes_objects_in_cluster <- which(assignations == the_cluster)
  map(distance_matrices, function(x) { x[indexes_objects_in_cluster,indexes_objects_in_cluster, drop = FALSE] })
}

determine_center <- function(matrices_k) {
  # For each distance function, this is each matrix:
  # -> For each object: we sum the distances to every other objects in
  #     the cluster. This is, for each row, we sum all the values
  # rankings <- lapply(matrices_k, function(x) {
  #   lapply(x, function(y) {
  #     y %>% mutate(sum = rowSums(.[-1])) %>%
  #       select(rowname, sum) %>% deframe()
  #   })
  # })
  print("Inside ranking of objects")
  rankings <- lapply(matrices_k, function(x) {
    #matrix(lapply(x, FUN = function(y) { rowSums(y) }) %>% unlist(use.names = TRUE), nrow = rows, byrow = TRUE)
    t(as.matrix(as.data.frame(lapply(x, function(y) { rowSums(y)}))))
  })
  #print(rankings)

  # Now we have a numeric value for each pair (cluster, distance function)
  # We want to rank the distance functions according to these values:
  rankings <- lapply(rankings, consensus::as.por)
  print(rankings)

  # The center will be the winners from the rankings
  rankings <- lapply(rankings, consensus::borda_winner, verbose = TRUE)
  print(rankings)

  return(rankings)
}
