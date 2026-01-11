zeroOrder = function(data, lambda, AND = TRUE){
  # browser()
  nItems = ncol(data)
  df_matrix = makeX(data)

  # Weights
  weights.opt = cor(df_matrix)
  diag(weights.opt) = 0

  if (AND == TRUE) {
    adj = weights.opt
    adj = (adj != 0) * 1
    EN.weights = adj * t(adj)
    EN.weights = EN.weights * weights.opt
    meanweights.opt = (EN.weights + t(EN.weights)) / 2
    meanweights.opt[meanweights.opt < 0] = 0
  } else {
    meanweights.opt = (weights.opt + t(weights.opt)) / 2
    meanweights.opt[meanweights.opt < 0] = 0
  }

  # Graph
  graph = graph_from_adjacency_matrix(
    meanweights.opt,
    weighted = TRUE,
    mode = "undirected",
    diag = FALSE
  )

  # Clustering in igraph
  communities = cluster_walktrap(graph, steps = 1)
  labels = membership(communities)

  return(list(
    graph = graph,
    communities = communities,
    labels = labels,
    # intercepts.opt = intercepts.opt,
    weights.opt = meanweights.opt
  ))

}
