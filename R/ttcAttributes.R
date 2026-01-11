ttcAttributes = function(correlation, nAttributes, nSamples, probMaster=0.5) {

  # correlation=0.7
  # nAttributes=4
  # nSamples =10000
  # probMaster=0.5


  probMasters=rep(probMaster, nAttributes)
  cutpoint = qnorm(probMasters)
  ttcMat = matrix(1, ncol = nAttributes, nrow = nAttributes)

  # correlation matrix for multivariate normal distirbution
  for(i in 1:(nAttributes-1)){
    for(j in (i+1):nAttributes){
      ttcMat[i, j] = correlation
      ttcMat[j, i] = correlation
    }
  }

  # underlying MVN of attributes
  underlying = mvtnorm::rmvnorm(nSamples, mean = rep(0, nAttributes), sigma = ttcMat)

  # categorize the underlying distribution
  attributes = underlying
  att =1
  for(att in 1:nAttributes){
    attributes[which(underlying[,att] >= cutpoint[att]), att] = 1
    attributes[which(underlying[,att] <  cutpoint[att]), att] = 0
  }
  colnames(attributes) = paste0("a", 1:nAttributes)


  class = NULL
  person = 1
  for (person in 1:nrow(attributes)){
    class = c(class, bin2dec(attributes[person,], nAttributes, rep(2, nAttributes))+1)
  }


  return(list(
    attributes = attributes,
    class = class))

}
