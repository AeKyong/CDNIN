conditionInformation = function(arrayNumber, nReplicationsPerCondition, nCores = 4){
  # convert array number to condition number
  conditionNumber = floor((arrayNumber - 1)/nReplicationsPerCondition) + 1

  #-------------------------
  # Constant Conditions
  #-------------------------
  nOptionsPerItem = 4
  int.min = -1
  int.max = 1
  two.min = 0.5
  two.max = 1

  #-------------------------
  # Manipulated Conditions
  #-------------------------
  conditions = list(
    lStructure = c("simple", "complex"),
    mainA = c("low", "high"),  # main effects for attributes
    mainT = c("low", "high"),  # main effects for theta
    tetra = c(0.00, 0.35, 0.70),
    nSamples = c(100, 500, 1000),
    misconcept = c("low", "medium", "high"),
    nItems = c(10, 20),
    estimationName = c("zeroOrder" , "nonRegularized" , "regularized")
  )

  nConditions = prod(sapply(conditions, FUN = length))
  conditionsMatrix = matrix(NA, nrow = nConditions, ncol = length(conditions))
  colnames(conditionsMatrix) = names(conditions)
  cond = 1
  for(cond in 1:nConditions){
    conditionsMatrix[cond, ] = dec2bin(
      decimal_number = cond - 1,
      nattributes = length(conditions),
      basevector = sapply(X = conditions, FUN = length)
    ) + 1
  }

  lStructure = conditions$lStructure[conditionsMatrix[conditionNumber,1]]
  mainA = conditions$mainA[conditionsMatrix[conditionNumber,2]]
  mainT = conditions$mainT[conditionsMatrix[conditionNumber,3]]
  tetra = conditions$tetra[conditionsMatrix[conditionNumber,4]]
  nSamples = conditions$nSamples[conditionsMatrix[conditionNumber,5]]
  misconcept = conditions$misconcept[conditionsMatrix[conditionNumber,6]]
  nItems = conditions$nItems[conditionsMatrix[conditionNumber,7]]
  estimationName = conditions$estimationName[conditionsMatrix[conditionNumber,8]]


  nOptions = nOptionsPerItem * nItems
  nDistractors = (nOptionsPerItem-1) * nItems


  if(mainA == "low"){mainA.min = 0.75; mainA.max = 1.25}
  # else if(mainA =="medium"){mainA.min = 1.25; mainA.max = 1.75}
  else if(mainA =="high"){mainA.min = 1.75; mainA.max = 2.25}

  if(mainT == "low"){mainT.min = 0.3; mainT.max = 0.6}
  else if(mainT =="high"){mainT.min = 0.6; mainT.max = 0.9}

  #-----------------------------------------------------------------------------
  # set the number of attributes and Qmatrix by size of misconcept & loading structure
  #-----------------------------------------------------------------------------
  if(lStructure == "simple"){

    # number of attributes
    if(misconcept == "low") {nAttributes = 2}
    else if(misconcept == "medium"){nAttributes = 5}
    else if(misconcept == "high"){nAttributes = 10}

    nMisconcepts = choose(n=nAttributes, k=1)
    nDistractorsPerMisconcept = nDistractors / nMisconcepts

    # q-matrix
    qMatrix = matrix(0, ncol = nAttributes, nrow = nOptions)
    for (att in 1:nAttributes){
      rows = c((1+(att-1)*(nDistractorsPerMisconcept)):(att*nDistractorsPerMisconcept))
      qMatrix[rows,att] = 1
    }
  }
  else if (lStructure == "complex"){

    # number of attributes
    if(misconcept == "low") {nAttributes = 2}
    else if(misconcept == "medium"){nAttributes = 3}
    else if(misconcept == "high"){nAttributes = 4}

    nMisconcepts = choose(n=nAttributes, k=1) + choose(n=nAttributes, k=2)
    nDistractorsPerMisconcept = nDistractors / nMisconcepts

    # q-matrix
    combCols = c(combn(1:nAttributes, 1, simplify = FALSE),
                 combn(1:nAttributes, 2, simplify = FALSE))

    qMatrix = matrix(0, ncol = nAttributes, nrow = nOptions)
    for (mis in 1:nMisconcepts){
      rows = c((1+(mis-1)*(nDistractorsPerMisconcept)):(mis*nDistractorsPerMisconcept))
      qMatrix[rows,combCols[[mis]]] = 1
    }

  }
  # item/option name on q-matrix
  rownames(qMatrix) = c(paste0("item", 1:nItems, c("b")), paste0("item", 1:nItems, c("c")),
                        paste0("item", 1:nItems, c("d")), paste0("item", 1:nItems, c("a")))


 # estimation
  if(estimationName == "zeroOrder"){
    estimation = eval(quote(zeroOrder))
    lambda = NULL
  } else if(estimationName == "nonRegularized"){
    estimation = eval(quote(nominalIsing))
    lambda = FALSE
  } else if(estimationName == "regularized"){
    estimation = eval(quote(nominalIsing))
    lambda = TRUE
  }



  return(list(
    nConditions = nConditions, conditionNumber= conditionNumber, conditions = conditions,  lStructure = lStructure,
    int.min = int.min, int.max = int.max, two.min = two.min, two.max = two.max,
    mainA.min = mainA.min, mainA.max = mainA.max, mainT.min = mainT.min, mainT.max= mainT.max,
    misconcept = misconcept, nAttributes= nAttributes, tetra = tetra, nSamples = nSamples,
    nItems = nItems, qMatrix= qMatrix, estimationName = estimationName, estimation = estimation, lambda = lambda
  ))

}
