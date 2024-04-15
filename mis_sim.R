mis.sim <- function(data, mis.matrix) {
  
  mis.temp <- data
  mis.temp[mis.matrix == 1] <- NA
  
  mis.vars <- colnames(data)[colSums(mis.matrix) > 0]
  
  original.means <- colMeans(data[, mis.vars], na.rm = TRUE)
  original.sds <- apply(data[, mis.vars], 2, sd, na.rm = TRUE)
  original.cov <- cov(data[, mis.vars], use = "pairwise.complete.obs")
  
  listwise.data <- na.omit(mis.temp)
  listwise.means <- colMeans(listwise.data[, mis.vars])
  listwise.sds <- apply(listwise.data[, mis.vars], 2, sd)
  listwise.cov <- cov(listwise.data[, mis.vars])
  
  pairwise.means <- colMeans(mis.temp[, mis.vars], na.rm = TRUE)
  pairwise.sds <- apply(mis.temp[, mis.vars], 2, sd, na.rm = TRUE)
  pairwise.cov <- cov(mis.temp[, mis.vars], use = "pairwise.complete.obs")
  
  mean.imputed.data <- mis.temp
  for (var in mis.vars) {
    mean.imputed.data[is.na(mean.imputed.data[, var]), var] <- mean(mean.imputed.data[, var], na.rm = TRUE)
  }
  mean.imputed.means <- colMeans(mean.imputed.data[, mis.vars])
  mean.imputed.sds <- apply(mean.imputed.data[, mis.vars], 2, sd)
  mean.imputed.cov <- cov(mean.imputed.data[, mis.vars])
  
  library(mice)
  regression.imputed.data <- mice(mis.temp, method = "norm.predict", m = 1, maxit = 1)
  regression.imputed.data <- complete(regression.imputed.data)
  regression.imputed.means <- colMeans(regression.imputed.data[, mis.vars])
  regression.imputed.sds <- apply(regression.imputed.data[, mis.vars], 2, sd)
  regression.imputed.cov <- cov(regression.imputed.data[, mis.vars])
  
  library(mvnmle)
  ml.estimates <- mlest(mis.temp[, mis.vars], method = "mvn")
  ml.means <- ml.estimates$mu
  ml.sds <- sqrt(diag(ml.estimates$sigma))
  ml.cov <- ml.estimates$sigma
  
  library(mice)
  imputed.data <- mice(mis.temp, method = "pmm", m = 5, maxit = 5)
  imputed.means <- apply(imputed.data$imp, 2, function(x) mean(colMeans(x[, mis.vars])))
  imputed.sds <- apply(imputed.data$imp, 2, function(x) mean(apply(x[, mis.vars], 2, sd)))
  imputed.cov <- apply(imputed.data$imp, 1, function(x) cov(x[, mis.vars]))
  imputed.cov <- Reduce("+", imputed.cov) / length(imputed.cov)
  
  frobenius.norm <- function(cov1, cov2) {
    sqrt(sum((cov1 - cov2)^2))
  }
  
  listwise.frobenius <- frobenius.norm(original.cov, listwise.cov)
  pairwise.frobenius <- frobenius.norm(original.cov, pairwise.cov)
  mean.imputed.frobenius <- frobenius.norm(original.cov, mean.imputed.cov)
  regression.imputed.frobenius <- frobenius.norm(original.cov, regression.imputed.cov)
  ml.frobenius <- frobenius.norm(original.cov, ml.cov)
  imputed.frobenius <- frobenius.norm(original.cov, imputed.cov)
  
  summary.table <- data.frame(
    Method = c("Original", "Listwise Deletion", "Pairwise Deletion", "Mean Imputation",
               "Regression Imputation", "Maximum Likelihood", "Multiple Imputation"),
    matrix(c(original.means, original.sds,
             listwise.means, listwise.sds,
             pairwise.means, pairwise.sds,
             mean.imputed.means, mean.imputed.sds,
             regression.imputed.means, regression.imputed.sds,
             ml.means, ml.sds,
             imputed.means, imputed.sds), ncol = 2 * length(mis.vars), byrow = TRUE),
    Frobenius.Norm = c(NA, listwise.frobenius, pairwise.frobenius, mean.imputed.frobenius,
                       regression.imputed.frobenius, ml.frobenius, imputed.frobenius)
  )
  
  colnames(summary.table)[-c(1, ncol(summary.table))] <- rep(mis.vars, each = 2)
  colnames(summary.table)[seq(2, 2 * length(mis.vars), 2)] <- 
    paste(colnames(summary.table)[seq(2, 2 * length(mis.vars), 2)], ".Mean")
  colnames(summary.table)[seq(3, 2 * length(mis.vars) + 1, 2)] <- 
    paste(colnames(summary.table)[seq(3, 2 * length(mis.vars) + 1, 2)], ".SD")
  
  return(summary.table)
}
