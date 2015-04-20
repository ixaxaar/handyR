

# Multiple histograms
library(psych)
multi.hist(dat)

# Plot a decision tree

plot(model)
text(model, use.n=TRUE)

plot_dendogram = function(model) {
  model = dendro_data(model)
  require(ggdendro)
  plt = ggplot() +
    geom_segment(data = model$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = model$labels,
              aes(x = x, y = y, label = label), size = 3, vjust = 0) +
    geom_text(data = model$leaf_labels,
              aes(x = x, y = y, label = label), size = 3, vjust = 1) +
    theme_dendro()
  return(plt)
}
