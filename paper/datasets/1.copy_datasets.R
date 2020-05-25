# Copy and normalize

copy_datasets <- datasets
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
copy_datasets <- lapply(datasets, function(x) x[,-ncol(x)])
for (i in 1:length(copy_datasets)) {
  copy_datasets[[i]] <- as.data.frame(sapply(copy_datasets[[i]], normalize))
}
