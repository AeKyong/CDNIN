nominalIsing = function(data, lambda, gamma = 0.25, AND = TRUE){

  nItems = ncol(data)
  df = as.data.frame(data)
  df[] <- lapply(df, as.factor)

  # Estimation in glmnet
  df_list = levels_list = lambdas_list = intercepts_list = betas_list =  N_list = P_list = convg = list()
  maxNlambdas = 100
  logL = J = EBIC = penalty = matrix(0, maxNlambdas, nItems)
  nlambdas_vec = N = c()
  for (i in 1:nItems) {

    # remove levels where frequency is 0 or 1
    tab = table( df[,i])
    valid_levels = which(tab >1)
    levels = sort(names(tab)[valid_levels])

    df[,i] = factor(df[,i], levels = levels)
    df.com = df[complete.cases(df), ]  # list-wise deletion if the frequency of the category is one.

    y = df.com[, i]
    x_matrix = makeX(train = df.com[,-i])
    # estimation
    if(lambda == TRUE) { # regularized (LASSO)
      fit = glmnet(x = x_matrix, y = y, family = "multinomial", alpha = 1, nlambda = 100)
    } else if(lambda == FALSE){ # non-regularized
      fit = bigGlm(x = x_matrix, y = y, family = "multinomial", path = TRUE)
    }

    betas = fit$beta
    lambdas = fit$lambda
    nlambdas = length(lambdas)
    intercepts = fit$a0
    N = nrow(x_matrix)

    # J: the number of non-zero betas
    for(l in 1:nlambdas){
      J_level = 0
      for(level in levels){
        J_level = sum(J_level, sum(betas[[level]][,l] != 0))
      }
      J[l,i] = J_level
    }

    # P: the number of predictors
    P = ncol(x_matrix)

    # log-likelihood
    for(l in 1:nlambdas){
      int = intercepts[,l]
      names(int) = levels
      beta = list()
      for(level in levels){
        beta[[level]] = betas[[level]][,l]
      }
      logL_N = 0
      for(e in 1:N){
        x = x_matrix[e, ]
        nu = list()
        deno = 0
        for(level in levels){
          nu[[level]] = int[level] + x %*% beta[[level]]
          deno = deno + exp(nu[[level]])
        }
        log_deno = max(log(deno), .Machine$double.xmin)
        logL_e = nu[[y[e]]] - log_deno
        logL_N = logL_N + logL_e
      } #examinee
      logL[l,i] = logL_N
    } #lambda

    # EBIC
    penalty[,i]  = J[,i] * log(N) + 2 * gamma * log(P-1)
    EBIC[, i] = -2 * logL[, i] +  penalty[,i]
    penalty[logL[,i]==0, i] = NA
    EBIC[logL[,i]==0, i] = NA

    df_list[[paste0("item", i)]] = df.com
    levels_list[[paste0("item", i)]] = levels
    intercepts_list[[paste0("item",i)]] = intercepts
    betas_list[[paste0("item",i)]] = betas
    lambdas_list[[paste0("item",i)]] = lambdas
    nlambdas_vec[paste0("item",i)] = nlambdas
    N_list[paste0("item",i)] = N
    P_list[[paste0("item",i)]] = P
    convg[[paste0("item",i)]] = ifelse(fit$npasses <= 1e5, TRUE, FALSE)
  }
browser()
  # Optimal lambda
  lambda.opt = apply(EBIC, 2, which.min)
  lambda_vec = c()
  for(i in 1:nItems){
    lambda_vec[i] = lambdas_list[[i]][lambda.opt[i]]
  }

  # Optimal thresholds
  intercepts.opt = list()
  for(i in 1:nItems){
    intercepts.opt[[i]] = intercepts_list[[i]][,lambda.opt[i]]
  }

  # Weights
  nLevels = sapply(levels_list, length)
  maxLevel = max(nLevels)
  levels = levels_list[[which(nLevels==maxLevel)[1]]]
  items = paste0("item", 1:nItems)
  nNodes = nItems * maxLevel
  weights.opt =  matrix(0, nNodes, nNodes)
  rownames(weights.opt) = colnames(weights.opt) =
    as.vector(t(outer(items, levels, paste0)))
  for(i in 1:nItems){
    for(level in levels_list[[i]]){
      beta.opt = betas_list[[i]][[level]][,lambda.opt[i]]
      weights.opt[paste0("item",i,level), names(beta.opt)] = beta.opt
    }
  }

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

  # remove non-convergence items's weight
  ncItems = names(which(convg == FALSE))
  if(length(ncItems) > 0){
    pattern = paste0("^(", paste(ncItems, collapse = "|"), ")[^0-9]")
    meanweights.opt = meanweights.opt[!grepl(pattern, rownames(meanweights.opt)), !grepl(pattern, colnames(meanweights.opt)), drop=FALSE]
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
    EBIC = EBIC,
    lambda_vec = lambda_vec,
    convg = convg,
    graph = graph,
    communities = communities,
    labels = labels,
    intercepts = intercepts.opt,
    weights = meanweights.opt
  ))
}





