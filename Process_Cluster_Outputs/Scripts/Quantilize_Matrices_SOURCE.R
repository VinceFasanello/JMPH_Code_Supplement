CutCostMatrixToQuantile <- function(cost.matrix, lower.quantile, upper.quantile){
  for (i in 1:nrow(cost.matrix)){
    cost.matrix[i,which(cost.matrix[i,] < quantile(x = cost.matrix[i,], probs = lower.quantile, na.rm = T))] <- NA
    cost.matrix[i,which(cost.matrix[i,] > quantile(x = cost.matrix[i,], probs = upper.quantile, na.rm = T))] <- NA
  }
  return(cost.matrix)
}

CutMatrixtoCostMatrix <- function(cost.matrix, tocut.matrix) {
  tocut.matrix[is.na(cost.matrix)] <- NA
  return(tocut.matrix)
}