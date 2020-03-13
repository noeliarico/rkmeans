# Aquí tiene que llegar una matriz de todos los objetos solo a los centros
#' Title
#'
#' @param matrix_of_distances
#'
#' @return
#' @export
#'
#' @examples
assignation_kmeans <- function(matrix_of_distances) {
  print(matrix_of_distances)
  apply(matrix_of_distances, 1, which.min)
}
