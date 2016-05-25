# with a financial time series


######## 1) prepare the data.frame of features (we should have a function for that?) ##########


ROCs = function(x = getBB("CL"), endROC = 100, numROC = 25, hlROC = floor(numROC/4), plotROC = FALSE) {
  
  nROCs = unique(floor(seqExp(start=1, end = endROC, n = numROC, hl=hlROC, plot=plotROC)))
  
  x_xts = NULL
  for (nROC in nROCs) {
    x_xts = merge.xts(x_xts, ROC(x, n = nROC, type = "discrete"))
  }
  x_xts = first(x_xts,-max(nROCs))
  colnames(x_xts) = nROCs
  
  return(x_xts)
}


df = as.data.frame(ROCs(x = getBB("CL"), endROC = 100, numROC = 25, plotROC = TRUE))
tail(df)

