#' Title
#'
#' @param data
#' @param assignation
#' @param update
#' @param distanceFunction
#' @param rankingRule
#'
#' @return
#' @export
#'
#' @examples
rkmeans <- function(data, assignation = "kmeans", update = "kmeans",
                    distanceFunction = "euc", ranking_rule = "bordaCount") {

  if(!assignation %in% c("kmeans", "rkmeans")) {
    stop('Invalid value for assignation argument. Try "kmeans" or "rkmeans"')
  }

  if(!update %in% c("kmeans", "rkmeans")) {
    stop('Invalid value for update argument. Try "kmeans" or "rkmeans"')
  }

  #TODO hacer un método en el paquete dists que devuelva un vector con los nombres de distancias disponibles
  #TODO validar que la distancia está donde debe

  #' If the update step is done using rkmeans, the new centers of the clusters
  #' will be always within the original dataset.
  #' Therefore, the matrix (for kmeans) or matrices (for rkmeans) of distances
  #' between the points only need to be calculated once on the method starts.
  #' If the update step is done using classic kmeans, then the centers will be
  #' new objects because the will be computed as the medium points of the
  #' objects that belong to the cluster, and thus a new matrix/matrices need
  #' to be calculated.

  if(update == "rkmeans") {
    matrix_of_distances <- distance(data, distance)
  }


# Assignation step --------------------------------------------------------

  if(assignation == "kmeans") {
    #
  }
  else { # rkmeans
    # apply on matrix of distances a function
  }


# Update step -------------------------------------------------------------

  if(update == "kmeans") {
    # Matrix of distances must contain the distance from the points to the centers
    # For each row get the min value of column
  }
  else { # rkmeans
    # apply on matrix of distances a function
  }


# Break or continue? ------------------------------------------------------

  # lastAssignation == currentAsgination break
  # if iter == max.iter break
}


# Aux functions -----------------------------------------------------------

create_distance_matrices <- function(data, distanceFunction) {
  matrices <- lapply(distanceFunction, function(x) dists::distance(data, x))
  names(matrices) <- distanceFunction
  return(matrices)
}


