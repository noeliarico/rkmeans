# Toy dataset of the paper ------------------------------------------------

x <- c(0.03, 0.01, 0.00, 1.00, 0.68, 0.58, 0.44, 0.69, 0.27, 0.23, 0.31, 0.46, 0.09, 0.00, 0.02)
y <- c(0.90, 0.96, 1.00, 0.25, 0.66, 0.80, 0.38, 0.10, 0.54, 0.22, 0.07, 0.00, 0.52, 0.64, 0.62)
example <- tibble(x, y)

# Get the results ---------------------------------------------------------

results_example <- vector(mode = "list", length = length(dists::availableDistances())+1)

# Calculate kmeans with the single distance functions
i <- 0
for(d in dists::availableDistances()) {
  i <- i + 1
  set.seed(2480)
  results_example[[i]] <- rkmeans(example, distanceFunction = d)
}

# Calculate rkmeans
set.seed(2480)
i <- i + 1
results_example[[i]] <- rkmeans(example, assignation = "rkmeans")

# All the errors to check if converges ------------------------------------

# It will store all the errors of every iter and every method
errors_example <- vector(mode = "list", length = length(dists::availableDistances())+1)
names(errors_example) <- c(dists::availableDistances(), "borda")

# Keep the errors and mix them in a single tibble
j <- 0
for(result_dataset in results_example) {
  j <- j + 1
  error <- result_dataset$errors %>%
    rename_(.dots = setNames(names(.), paste0('e', names(.)))) %>%
    mutate(training_dist = names(errors_example)[j]) %>%
    rownames_to_column("iter") %>%
    pivot_longer(starts_with("e"), names_to = "error_dist", values_to = "error") %>%
    mutate(error_dist = sub(".", "", error_dist),
           iter = as.numeric(iter))
  if(is.null(errors_example[[j]])) {
    errors_example[[j]] <- error
  }
  else {
    errors_example[[j]] <- errors_example[[j]] %>% bind_rows(error)
  }

}
errors_example <- bind_rows(errors_example)
errors_example <- errors_example %>% mutate(training_dist = as.factor(training_dist),
                                            error_dist = as.factor(error_dist))

# Table with the final errors --------------------------------------------

# Keep only the last iter
final_errors_example <- lapply(results_example, function(x) {
  x$errors[nrow(x$errors),]
})
# Identify the method used for training rkmeans
names(final_errors_example) <- c(dists::availableDistances(), "borda")
final_errors_example <- Map(cbind, final_errors_example, training = names(final_errors_example))
# Create one single dataset
final_errors_example <- bind_rows(final_errors_example)
# Transpose the matrix as we want a ranking for each error
final_errors_example <- final_errors_example %>%
  pivot_longer(-training, names_to = "error", values_to = "value") %>%
  pivot_wider(names_from = training, values_from = value) %>%
  mutate(error = paste0("error_", error))

# To profile of rankings --------------------------------------------------

por <- consensus::as.por(final_errors_example %>% mutate(error = NULL))

# Borda -------------------------------------------------------------------

example_borda <- consensus::borda_count(por)



# Export to latex for the paper -------------------------------------------

# Export final_errors_example (table 4)

# Plot for each error how the method converge
ggplot(errors_example %>% mutate(tsize = as.factor(ifelse(training_dist=="borda", "rr", "d"))), aes(iter, error, color = training_dist)) +
  geom_line(aes(size = tsize, alpha = tsize)) +
  scale_size_manual(values = c(0.3, 1)) +
  scale_alpha_manual(values = c(0.8, 1)) +
  #scale_color_brewer(palette = "Spectral") +
  scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
  ggtitle(paste("Dataset", name, "- metrics")) +
  facet_wrap(error_dist ~ ., scales = "free_y", ncol = 5)


# Plot borda
ggplot(errors_example %>% filter(training_dist == "borda"), aes(iter, error, color = error_dist)) +
  geom_line() +
  scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
  ggtitle(paste("Dataset", name, "- borda"))


# Plot borda
ggplot(errors_example %>% filter(training_dist == "borda"), aes(iter, error, color = error_dist)) +
  geom_line() +
  scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
  ggtitle(paste("Dataset", name, "- borda"))


# Plot converge
ggplot(errors_example %>% filter(as.character(training_dist) == as.character(error_dist)), aes(iter, error, color = error_dist)) +
  geom_line() +
  scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
  ggtitle(paste("Dataset", name, "- distance"))
#theme(legend.direction = "horizontal")
#plots[[i]] <- grid.draw(shift_legend(p))
