distance_name <- append(dists::availableDistances(), "borda")
errors <- vector(mode = "list", length = length(datasets))
j <- 0
for(result_dataset in results[1:20]) {
  j <- j + 1
  for(i in 1:length(result_dataset)) {
    model <- result_dataset[[i]]
    error <- model$errors %>%
      rename_(.dots = setNames(names(.), paste0('e', names(.)))) %>%
      mutate(training_dist = distance_name[i]) %>%
      rownames_to_column("iter") %>%
      pivot_longer(starts_with("e"), names_to = "error_dist", values_to = "error") %>%
      mutate(error_dist = sub(".", "", error_dist),
             #error_dist = as.factor(error_dist),
             #training_dist = as.factor(training_dist),
             iter = as.numeric(iter))
    if(is.null(errors[[j]])) {
      errors[[j]] <- error
    }
    else {
      errors[[j]] <- errors[[j]] %>% bind_rows(error)
    }
  }
  errors[[j]] <- errors[[j]] %>% mutate(training_dist = as.factor(training_dist),
                                        error_dist = as.factor(error_dist))
}

# Create a tibble for each error measure
analisis <- errors[1:20]
names(analisis) <- names(datasets)[1:20]
# Keep only the results of the last iter
analisis <- lapply(analisis, function(a) {
  a %>%
    group_by(error_dist, training_dist) %>%
    filter(iter == max(iter)) %>%
    ungroup()
})

analisis <- Map(cbind, analisis, dataset = names(analisis))
analisis <- bind_rows(analisis)

# We are not interested in the last iter anymore
analisis <- analisis %>%
  mutate(iter = NULL,
         training_dist = paste0("training_", training_dist),
         error_dist = paste0("error_", error_dist))
analisis <- analisis %>% pivot_wider(names_from = "training_dist", values_from = "error")
#



