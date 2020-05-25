#' TDWC: For each cluster, the squeared distance from the center
#' to all the objects that belong to the cluster is computed. Then,
#' all the obtained distances are sum again.
#'
#' @param matrix_of_distances
#' @param assignations
#' @param k
#'
#' @return
#' @export
#'
#' @examples
tdwc <- function(matrix_of_distances, assignations, k) {

  #print("----> TDWC.....................................")
  #print(matrix_of_distances)

  # For each matrix of distances
  l <- sapply(matrix_of_distances, function(m) {
    d <- lapply(1:length(unique(assignations)), function(x) { # For each cluster
      sum(m[which(assignations == x), x]^2)
    }) %>% unlist() %>% sum()
    d

  })
  #print(l)
  l
}
