rm(list=ls())
devtools::load_all(".")

if (!require(mvtnorm)) install.packages("mvtnorm")
if (!require(psych)) install.packages("psych")
if (!require(glmnet)) install.packages("glmnet")
if (!require(igraph)) install.packages("igraph")
if (!require(mclust)) install.packages("mclust")
if (!require(gtools)) install.packages("gtools")
if (!require(mclust)) install.packages("mclust")
if (!require(foreach)) install.packages("foreach")
if (!require(doParallel)) install.packages("doParallel")
if (!require(pacman)) install.packages("pacman")

library(mvtnorm)
library(psych)
library(glmnet)
library(igraph)
library(mclust)
library(gtools)
library(foreach)
library(doParallel)

# arrayNumber = as.numeric(commandArgs(trailingOnly = TRUE)[1])
arrayNumber = 1
nReplicationsPerCondition = 1
initialInfo = conditionInformation(arrayNumber = 1, nReplicationsPerCondition = nReplicationsPerCondition)
nConditions = initialInfo$nConditions
totalIterations = nConditions * nReplicationsPerCondition

# # prepare for parallel
# current_wd = getwd()
# result_dir = file.path(current_wd, "results_foreach")
# if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)
#
# # try(stopCluster(cl), silent = TRUE)
# # closeAllConnections()
#
# n_cores = detectCores() - 1
# cl = makeCluster(n_cores, type = "PSOCK")
# registerDoParallel(cl)
#
# clusterEvalQ(cl, {
#   library(devtools)
#   devtools::load_all(".")
#   library(gtools)
#   library(mclust)
#   library(igraph)
# })
#
# clusterExport(cl, varlist = c("nReplicationsPerCondition", "result_dir"))
#
# # Run simualtion
# results = foreach(arrayNumber = 1:totalIterations,
#                    .packages = c("gtools", "mclust", "igraph","glmnet"),
#                    .errorhandling = 'pass',
#                    .verbose = FALSE) %dopar% {

  print(paste("arrayNumber:", arrayNumber))

  simulationSpecs = conditionInformation(arrayNumber = arrayNumber,
    nReplicationsPerCondition = nReplicationsPerCondition)

  simDataList = simulateSICMdata(
    lStructure = simulationSpecs$lStructure,
    int.min = simulationSpecs$int.min,
    int.max = simulationSpecs$int.max,
    two.min = simulationSpecs$two.min,
    two.max = simulationSpecs$two.max,
    mainA.min = simulationSpecs$mainA.min,
    mainA.max = simulationSpecs$mainA.max,
    mainT.min = simulationSpecs$mainT.min,
    mainT.max = simulationSpecs$mainT.max,
    nAttributes = simulationSpecs$nAttributes,
    tetra = simulationSpecs$tetra,
    nSamples = simulationSpecs$nSamples,
    nItems = simulationSpecs$nItems,
    qMatrix = simulationSpecs$qMatrix,
    seed = arrayNumber
  )

  test = simulationSpecs$estimation(
    data = simDataList$trueParametersExaminee$Y,
    lambda = simulationSpecs$lambda)

  # ARI
  true_labels = apply(simulationSpecs$qMatrix, 1, paste, collapse = "")
  item_names = names(true_labels)
  sorted_names = mixedsort(item_names)
  true_labels_sorted = true_labels[sorted_names]
  common_items = intersect(names(true_labels_sorted), names(test$labels))
  ARI = adjustedRandIndex(true_labels_sorted[common_items], test$labels[common_items])

  # PC, MAE, MBE
  if(simulationSpecs$lStructure == "simple"){
    nMis.t = simulationSpecs$nAttributes + 1
  } else if(simulationSpecs$lStructure == "complex"){
    nMis.t = choose(simulationSpecs$nAttributes, 1) + choose(simulationSpecs$nAttributes, 2) + 1
  }
  nMis.e = length(unique(test$labels))
  C = ifelse(nMis.e == nMis.t, 1, 0) # for PC
  A = abs(nMis.e - nMis.t) # for MAE
  B = nMis.e - nMis.t  # for MBE

  # save
  out_path = file.path(result_dir, paste0("rep_", arrayNumber, ".RData"))
  save(ARI, A, B, C, simDataList, test, file = out_path)

  return(paste0("ARI: ", ARI))
# }
#
#
# stopCluster(cl)
