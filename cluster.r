

model = kmeans(scale(dat[, sapply(dat, is.numeric)]), 10)

# Radial plot vizualization
radial = function(model) {
  clusters = length(model$size)
  params = ncol(model$centers)
  melted = melt(model$centers)

  names(melted) = c("Cluster", "Variable", "Value")
  melted$Cluster = as.factor(melted$Cluster)

  plt = ggplot(melted, aes(x=Variable, y=Value, group=Cluster, color=Cluster))
  plt = plt + coord_polar() + geom_point(size=3) + geom_path()
  plt
}

# Evaluate cluster robustness
robustness = function(dat, runs=10, krange=10) {
  library(fpc)

  perf = clusterboot(scale(dat[, sapply(dat, is.numeric)]), clusterMethod=kmeansCBI, runs=runs, krange=krange)
  return(perf)
}

# Evaluate criterias for selecting optimal number of clusters
criterias = function(dat, model) {
  library(clusterCrit)

  ic = intCriteria(as.matrix(dat), model$cluster, "all")
  crit = data.frame()
  for (k in 2:20) {
    m = kmeans(scale(dat), k)
    crit = rbind(crit, as.numeric(intCriteria(as.matrix(dat), m$cluster, "all")))
  }
  crit = data.frame(sapply(crit, function(x) {x[is.nan(x)] = 0; x}))

  dsc = cbind(k=2:20, data.frame(sapply(crit, scale)))
  dscm = melt(dsc, id.vars="k", variable.name="Measure")
  dscm$value[is.nan(dscm$value)] = 0
  ms = as.character(unique(dscm$Measure))

  plt = ggplot(subset(dscm, Measure %in% ms), aes(x=k, y=value, colour=Measure))
  plt = plt + geom_point(aes(shape=Measure)) + geom_line(aes(linetype=Measure))
  plt
}

selfOrganizingMap = function(dat) {
  library(kohonen)
  model <- som(scale(dat), grid = somgrid(5, 4, "hexagonal"))
  plot(model, main="Clusters")

  return(model)
}
