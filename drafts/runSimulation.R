rm(list=ls())
devtools::load_all(".")

if (!require(mvtnorm)) install.packages("mvtnorm")
if (!require(psych)) install.packages("psych")
if (!require(glmnet)) install.packages("glmnet")
if (!require(igraph)) install.packages("igraph")
if (!require(mclust)) install.packages("mclust")
if (!require(gtools)) install.packages("gtools")
if (!require(mclust)) install.packages("mclust")

library(mvtnorm)
library(psych)
library(glmnet)
library(igraph)
library(mclust)
library(gtools)
#
# arrayNumber = as.numeric(commandArgs(trailingOnly = TRUE)[1])
arrayNumber = 200633
nReplicationsPerCondition = 100

# print(paste("arrayNumber:", arrayNumber))

# get conditions for the arrayNumber
simulationSpecs = conditionInformation(arrayNumber = arrayNumber,
  nReplicationsPerCondition = nReplicationsPerCondition)

# generate data
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

# # check generated data
# library(ggplot2)
# categories  =  c("a", "b", "c", "d")
# for(cat in categories){
#   check_df = data.frame(theta = simDataList$trueParametersExaminee$theta,
#                           category = as.numeric(simDataList$trueParametersExaminee$Y[,1] == cat))
#
#   p = ggplot(check_df, aes(x=theta, y=category)) +
#     geom_smooth(method="glm", method.args=list(family="binomial")) +
#     labs(
#       title = paste0("Category '", cat, "' Response Curve"),
#       x = "True Theta",
#       y = "Probability of Correctness"
#     )
#   print(p)
#   glm.fit = glm(category~theta , data = as.data.frame(check_df))
#   print(glm.fit)
# }


# estimation
test = simulationSpecs$estimation(
  data = simDataList$trueParametersExaminee$Y,
  lambda = simulationSpecs$lambda,
  undirectedRule = simulationSpecs$undirectedRule)

# results
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
out_path = file.path(paste0("results/rep_", arrayNumber, ".RData"))
save(ARI, A, B, C, simDataList, test, file = out_path)

# print(paste0("ARI: ", ARI))
