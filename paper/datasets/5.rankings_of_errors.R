
dnames <- analisis %>% pull(dataset) %>% unique()
rankingsOfErrors <- vector(mode="list",length=length(datasets))
rankingsOfErrors_borda <- vector(mode="list",length=length(datasets))
i <- 0
for (ds in dnames) {
  i <- i +1
  por <- analisis %>% filter(dataset == ds) %>%
    mutate(dataset = NULL, error_dist = NULL)
  colnames(por) <- str_remove(colnames(por), "training_")
  por <- consensus::as.por(por)
  rankingsOfErrors[[i]] <- por
  rankingsOfErrors_borda[[i]] <- consensus::borda_count(por)
  print(por)
}
rm(i)




