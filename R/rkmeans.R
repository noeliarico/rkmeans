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
rkmeans <- function(data, k = 3, iter.max = 10,
                    assignation = "kmeans", update = "kmeans",
                    distanceFunction = "euc", ranking_rule = "bordaCount",
                    developer = FALSE) {

  if (developer && (ncol(data) > 15 || nrow(data) > 15)) {
    developer <- FALSE
    warning("Developer mode available only for small datasets")
  }

  nobjects <- nrow(data)

  # To store the errors in each iter
  #error_per_iter <- double(iter.max)
  error_per_iter <- vector(mode = "list", iter.max)

  if (!assignation %in% c("kmeans", "rkmeans")) {
    stop('Invalid value for assignation argument. Try "kmeans" or "rkmeans"')
  }

  if (!update %in% c("kmeans", "rkmeans")) {
    stop('Invalid value for update argument. Try "kmeans" or "rkmeans"')
  }

  if (is.data.frame(data)) { # also works for tibbles
    data <- as.matrix(data)
    if(developer) print(data)
  }

  #TODO validar que la distancia está donde debe
  if(!distanceFunction %in% dists::availableDistances()) {
    stop('Invalid distance function')
  }

  iter <- 0
  old_assignations <- integer(nobjects) -1
  assignations <- integer(nobjects)

  # Track the assignations
  all_assignations <- matrix(rep(0, nrow(data)*iter.max), nrow = iter.max)

  # Initialize centers ------------------------------------------------------

  centers <- sample(1:nobjects, k)
  # Track the centers in each iteration
  all_centers <- vector(mode = "list", length = iter.max)
  all_centers[[1]] <- data[centers, ]

  if(developer) {
    print("The points selected as centers are:")
    print(paste0("x", centers))
    print(all_centers[[1]])
  }

  #' If the update step is done using rkmeans, the new centers of the clusters
  #' will be always within the original dataset.
  #' Therefore, the matrix (for kmeans) or matrices (for rkmeans) of distances
  #' between the points only need to be calculated once on the method starts.
  #' If the update step is done using classic kmeans, then the centers will be
  #' new objects because the will be computed as the medium points of the
  #' objects that belong to the cluster, and thus a new matrix/matrices need
  #' to be calculated.
  if(assignation == "kmeans") {
    matrix_of_distances <- create_distance_matrices(data,
                                                    data[centers,],
                                                    distanceFunction)
  }
  else { # because the centers will alwaya be inside the original dataset
    matrix_of_distances <- create_distance_matrices(data,
                                                    data[centers,],
                                                    dists::availableDistances())
  }
  if(developer) print(matrix_of_distances)

  while(iter < iter.max && any(old_assignations != assignations)) {
    iter <- iter + 1
    if(developer) print(paste0("Iter ", iter, " ------------------------------------------------"))

    old_assignations <- assignations

  # Assignation step --------------------------------------------------------

    if(assignation == "kmeans") {
      #assignations <- assignation_kmeans(matrix_of_distances[[1]][,centers])
      # matrix of distances contains the distance from all the objects of
      # the dataset (rows) to the centers (cols)
      assignations <- assignation_kmeans(matrix_of_distances[[1]])
    }
    else { # rkmeans
      # apply on matrix of distances a function
      assignations <- assignation_rkmeans(matrix_of_distances)

    }

    all_assignations[iter,] <- assignations
    if(developer) print(assignations)

  #' assignations: vector with the index of the cluster which is object is assing
  #' center:

  # Update step -------------------------------------------------------------

    if(update == "kmeans") {
      # Matrix of distances must contain the distance from the points to the centers
      # For each row get the min value of column
      centers <- update_kmeans(data, assignations)
      all_centers[[iter+1]] <- centers
      if(developer) print(centers)
    }
    else { # rkmeans
      # apply on matrix of distances a function
    }

  # Error -------------------------------------------------------------------

    #if(assignation == "kmeans" && update == "kmeans") {
    # esto funciona si matrix of distances es una, pero para los tests las
    # necesito todas y por lo tanto matrix of distances la tengo que hacer con todas
    # las avilable distances
    if(assignation == "kmeans") {
      matrix_of_distances <- create_distance_matrices(data,
                                                      centers,
                                                      dists::availableDistances())
    }
    error <- tdwc(matrix_of_distances, assignations, k)
    error_per_iter[[iter]] <- error


  # Break or continue? ------------------------------------------------------

    # lastAssignation == currentAsgination break
    # if iter == max.iter break

    # if we continue and the update is rkmeans we don't need to upload
    # the matrix of distances because the centers will be points inside the
    # the current matrix
    if(update == "kmeans") {
      if(assignation == "kmeans") {
        matrix_of_distances <- create_distance_matrices(data,
                                                        centers,
                                                        distanceFunction)
      }
      else { # because the centers will alwaya be inside the original dataset
        matrix_of_distances <- create_distance_matrices(data,
                                                        centers,
                                                        dists::availableDistances())
      }
    }
    #if(developer) print(matrix_of_distances)




    #Esto para cuando haga rkmeans
    #error <- lapply(matrix_of_distances, function(x) tdwc(x[,centers], assignations, k))
    #if(length(error) == 1) error_per_iter[[iter]] <- unlist(error)

  }

  #TODO Arreglar esto porque si sale en la ultima iter porque alcanzo un min sale el
  # mensaje de que no converge
  if(iter == iter.max) {
    message("Maximum number of iters reached")
  }
  else {
    message("The method converge in iter = ", iter)
  }

  if (length(error_per_iter[[1]]) == 1)
    errors <- unlist(error_per_iter)
  else
    errors <- Reduce(bind_rows, error_per_iter)

  out <- list(cluster = assignations,
              all_centers = all_centers,
              all_assignations = all_assignations,
              iter = iter,
              errors = errors)
  return(out)
}

# Aux functions -----------------------------------------------------------

create_distance_matrices <- function(data, centers = NULL, distanceFunction) {

  # When using kmeans to update the centers are recalculated everytime because
  # they not belong to the original dataset. Thus it is necessary to calculate
  # the new distance only to those centers.
  # For instance, when using rkmeans for update the new centeres will always
  # be in the original dataset and thus it is necessary to compute the distance
  # between all the points only once.

  if(!is.null(centers)) {
    matrices <- lapply(distanceFunction,
                       function(x) dists::distanceFromTo(data, centers, x))
  }
  else {
    matrices <- lapply(distanceFunction,
                       function(x) dists::distance(data, x))
  }

  names(matrices) <- distanceFunction

  return(matrices)
}


