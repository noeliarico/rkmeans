# Comparison matrix -------------------------------------------------------

# mcomp will contain the comparison matrix
# initially, it is a tibble of the format
# dist1 ---- dist2 ---- better
# better is 0 for all the rows in the initialitation but it will be
# filled with the number of times that dist1 is better than dist2
mcomp <- tibble(m1 = c(dists::availableDistances(), "borda"),
                m2 = c(dists::availableDistances(), "borda")) %>%
  complete(m1, m2) %>% filter(m1 != m2) %>% mutate(better = 0)

# Given a ranking and two distance function (m1 and m2) this function
# returns 1 if m1 is in a better position than m2 and 0 otherwise
comparar_par <- function(m1, m2, ranking) {
  return(as.numeric(ranking[m1] < ranking[m2]))
}

for(r in 1:length(rankingsOfErrors_borda)) {
  for(i in 1:nrow(mcomp)) {
    row <- mcomp %>% slice(i)
    the_m1 <- row %>% pull(m1)
    the_m2 <- row %>% pull(m2)
    print(the_m1)
    print(the_m2)
    mcomp <- mcomp %>% mutate(better = ifelse((m1 == the_m1 & m2 == the_m2),
                                              better+comparar_par(the_m1,the_m2,rankingsOfErrors_borda[[r]]), better))

    # print(comparar_par(the_m1,the_m2,rankingsOfErrors_borda[[r]]))
    # mcomp[mcomp$m1==the_m1&mcomp$m2==the_m2, ]$better <-
    #   mcomp[mcomp$m1==the_m1&mcomp$m2==the_m2, ]$better +
    #   comparar_par(the_m1,the_m2,rankingsOfErrors_borda[[r]])
  }
}

tol <- mcomp %>% pivot_wider(names_from = m2, values_from = better) %>% replace(is.na(.), 0)

# Just to check that everything adds up to the number of datasets
for(i in 1:nrow(tol)) {
  for(j in 1:ncol(tol)) {
    print(as.numeric(tol[i, j])+as.numeric(tol[j, i]))
  }
}
