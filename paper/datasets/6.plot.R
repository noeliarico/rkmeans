# plot --------------------------------------------------------------------

library(RColorBrewer)

plots <- vector(mode = "list", length = length(datasets))

# Plot by training distance
i <- 0
for(errors_dataset in errors) {
  i <- i + 1
  name <- names(datasets)[i]
  plots[[i]] <- ggplot(errors_dataset %>% mutate(tsize = as.factor(ifelse(training_dist=="borda", "rr", "d"))), aes(iter, error, color = training_dist)) +
    geom_line(aes(size = tsize, alpha = tsize)) +
    scale_size_manual(values = c(0.3, 1)) +
    scale_alpha_manual(values = c(0.8, 1)) +
    #scale_color_brewer(palette = "Spectral") +
    scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
    ggtitle(paste("Dataset", name, "- metrics")) +
    facet_wrap(error_dist ~ ., scales = "free_y")
  #theme(legend.direction = "horizontal")
  #plots[[i]] <- grid.draw(shift_legend(p))
}

# plotborda ---------------------------------------------------------------

plotsb <- vector(mode = "list", length = length(datasets))
i <- 0
for(errors_dataset in errors) {
  i <- i + 1
  name <- names(datasets)[i]
  plotsb[[i]] <- ggplot(errors_dataset %>% filter(training_dist == "borda"), aes(iter, error, color = error_dist)) +
    geom_line() +
    scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
    ggtitle(paste("Dataset", name, "- borda"))
  #theme(legend.direction = "horizontal")
  #plots[[i]] <- grid.draw(shift_legend(p))
}

# plotconverge ---------------------------------------------------------------

plotsc <- vector(mode = "list", length = length(datasets))
i <- 0
for(errors_dataset in errors) {
  i <- i + 1
  name <- names(datasets)[i]
  plotsc[[i]] <- ggplot(errors_dataset %>% filter(as.character(training_dist) == as.character(error_dist)), aes(iter, error, color = error_dist)) +
    geom_line() +
    scale_x_continuous(breaks = 1:max(errors_dataset$iter)) +
    ggtitle(paste("Dataset", name, "- distance"))
  #theme(legend.direction = "horizontal")
  #plots[[i]] <- grid.draw(shift_legend(p))
}

# Save pdfs ---------------------------------------------------------------

pdf("metrics.pdf", paper = "a4")
plots
dev.off()


library(gridExtra)
p <- do.call(marrangeGrob, list(grobs = plots,ncol=1,nrow=1))
ggsave("metrics.pdf", p, width=11, height=8.5)
p <- do.call(marrangeGrob, list(grobs = plotsb,ncol=1,nrow=1))
ggsave("borda.pdf", p, width=11/2, height=8.5/2)
p <- do.call(marrangeGrob, list(grobs = plotsc,ncol=1,nrow=1))
ggsave("distance.pdf", p, width=11/2, height=8.5/2)

