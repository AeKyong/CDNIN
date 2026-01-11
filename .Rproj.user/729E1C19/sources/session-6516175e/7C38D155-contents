simulateSICMdata = function(lStructure, int.min, int.max, two.min, two.max, mainA.min, mainA.max, mainT.min, mainT.max,
                            nAttributes, tetra, nSamples, nItems, qMatrix, seed){

  set.seed(seed)
  #-----------------------------------------------------------------------------
  # generate true person parameters
  #-----------------------------------------------------------------------------
  # theta
  theta = rnorm(nSamples, 0, 1)

  # profile
  examineeProfiles = ttcAttributes(correlation=tetra, nAttributes=nAttributes, nSamples=nSamples)

  #-----------------------------------------------------------------------------
  # generate true item parameters
  #-----------------------------------------------------------------------------
  # test parameter matrix
  if(lStructure =="simple"){
    truePara = matrix(NA, nrow = nrow(qMatrix), ncol = (2 + nAttributes))
    colnames(truePara) = c("l_0", "l_t", paste0("l_1_", 1:nAttributes))
    rownames(truePara) = rownames(qMatrix)
  } else if(lStructure =="complex"){
    two = combn(nAttributes, 2)
    twoName = apply(two, 2, function(x) paste0("l_2_", x[1], ",", x[2]))
    truePara = matrix(NA, nrow = nrow(qMatrix), ncol = (2 + nAttributes + ncol(two)))
    colnames(truePara) = c("l_0", "l_t", paste0("l_1_", 1:nAttributes), twoName)
    rownames(truePara) = rownames(qMatrix)
  }

  # sample item parameters
  nOptions = 4
  item = 1
  Y = NULL
  for (item in 1:nItems){

    itemrow = grep(paste0("item", item, "[a-z]"), rownames(qMatrix))
    itemQ = qMatrix[itemrow, ]
    itemPara = truePara[itemrow,]

    # sample para
    l0 = runif((nOptions-1), int.min, int.max)
    lt = runif(1, mainT.min, mainT.max)
    nl1 = length(which(itemQ>0))
    l1 = runif(nl1, mainA.min, mainA.max)
    nl2 = length(which(rowSums(itemQ)==2))
    l2 = runif(nl2, two.min, two.max)

    # assign sampled item parameters into the itemPara matrix where the itemQ has attributes
    # l_0 and l_t
    aRow = grep("a", rownames(itemPara))
    itemPara[-aRow,"l_0"] = l0
    itemPara[aRow,"l_0"] = 0
    itemPara[,"l_t"] = lt


    #l_1 and l_2
    l_pools = list("1" = l1, "2" = l2)
    max_order = 2
    for (o in 1:max_order) {
      current_pool = l_pools[[as.character(o)]]
      for (r in 1:nrow(itemPara)) {
        att = which(itemQ[r, ] == 1)
        if (length(att) < o) next
        if (o == 1) {
          comb_idx = matrix(att, nrow = 1)
        } else {
          comb_idx = combn(att, o)
        }
        col_names = apply(comb_idx, 2, function(x) {
          paste0("l_", o, "_", paste(x, collapse = ","))
        })
        n_needed = length(col_names)
        if (length(current_pool) >= n_needed) {
          l_values = current_pool[1:n_needed]
          itemPara[r, col_names] = l_values
          current_pool = current_pool[-(1:n_needed)]
        }
      }
      l_pools[[as.character(o)]] = current_pool
    }
    common_rows = intersect(rownames(truePara), rownames(itemPara))
    common_cols = intersect(colnames(truePara), colnames(itemPara))
    truePara[common_rows, common_cols] = itemPara[common_rows, common_cols]

    #-----------------------------------------------------------------------------
    # generate item response
    #-----------------------------------------------------------------------------

    Y_i = NULL
    for(e in 1:nSamples){
      a_e = examineeProfiles$attributes[e,]
      theta_e = theta[e]
      logit = itemPara
      logit[,grep("l_1_", colnames(logit))[1]:ncol(logit)] = NA

      # logit: reflect examinee profile
      max_order = 2
      for (o in 1:max_order) {
        combIdx = combn(1:length(a_e), o)
        products = apply(combIdx, 2, function(x) prod(a_e[x]))
        valid_cols = which(products == 1)

        if (length(valid_cols) > 0) {
          final_comb = combIdx[, valid_cols, drop = FALSE]
          col_names = apply(final_comb, 2, function(x) {
            paste0("l_", o, "_", paste(x, collapse = ","))
          })
          existing_cols = intersect(col_names, colnames(logit))
          if (length(existing_cols) > 0) {
            logit[, existing_cols] = itemPara[, existing_cols, drop = FALSE]
          }
        }
      }

      # logit_e: reflect theta_e value into the logit matrix
      logit_e = logit
      logit_e[,"l_t"] = -1*exp(logit_e[,"l_t"]) * theta_e
      logit_e[aRow, "l_t"] = 0  # Q. Is it correct for identification?

      # logit_k: generate an examinee's response
      logit_k = rowSums(logit_e, na.rm=T)
      exp_k = exp(logit_k)
      exp_deno = sum(exp_k)
      p_k = exp_k/exp_deno
      y_idx = rmultinom(1, size = 1, p_k)
      y_ei = sub(".*(.$)", "\\1", rownames(y_idx)[which(y_idx==1)])
      Y_i = rbind(Y_i, y_ei)

    } #//examinee
    colnames(Y_i) = paste0("item", item)
    Y = cbind(Y, Y_i)

  } #//item
  rownames(Y) = NULL
  trueParametersExaminee = list(theta = theta,
                                attribute = examineeProfiles$attributes,
                                Y = Y)

  return(list(
    trueParametersExaminee = trueParametersExaminee,
    trueParametersItem = truePara
  ))


}





