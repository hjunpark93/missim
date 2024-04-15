from_cont <- function(data, model, input = "cov") {

  parsed.model <- parse_mis(data = data, model = model)
  parsed.vars <- parse_var(data = data, model = model)
  
  if (input == "cor") {
    mis.list <- mis_list(parsed.model = parsed.model, data = data, 
                         input = "cor")
    mis.vectors <- matrix(nrow = nrow(data), ncol = length(mis.list))
    colnames(mis.vectors) <- sapply(mis.list, function(x) colnames(x)[1])
    for (i in 1:length(mis.list)){
      mis.function <- mis.list[[i]][1,-1]
      mis.predictor.name <- c()
      mis.predictor <- matrix()
      mis.indicator.name <- colnames(mis.list[[i]])[1]
      mis.indicator <- c()
      mis.var.name <- parsed.vars[[i]][1]
      mis.temp <- c()
      
      mis.predictor.name <- colnames(mis.list[[i]])[-1]
      mis.predictor <- scale(as.matrix(data[, mis.predictor.name]), 
                             center = TRUE, scale = FALSE)
      mis.predictor.s <- scale(as.matrix(data[, mis.predictor.name]), 
                               center = TRUE, scale = TRUE)
      mis.temp <- matrix(data[,mis.var.name])
      
      res <- residuals(lm(mis.temp ~ mis.predictor))
      temp <- svd(mis.predictor.s)
      
      if (length(mis.function) == 0){
        stop("Missingness model not recognized; please check whether the model is correctly parsed.")
      } else if (length(mis.function) == 1) {
        dual <- (nrow(mis.predictor)-1)*temp$u %*% as.matrix(ifelse(temp$d > 0, 1/temp$d, 0)) %*% t(temp$v)
        sigma.2 <- c((1 - mis.function %*% cov(dual) %*% mis.function) / var(res))
      } else {
        dual <- (nrow(mis.predictor)-1)*temp$u %*% diag(ifelse(temp$d > 0, 1/temp$d, 0)) %*% t(temp$v)
        sigma.2 <- c((1 - mis.function %*% cov(dual) %*% mis.function) / var(res))
      }
      mis.indicator <- dual %*% mis.function + sqrt(sigma.2)*res
      mis.vectors[,i] <- mis.indicator
    }
  } else if (input == "cov") {
    mis.list <- mis_list(parsed.model = parsed.model, data = data, 
                         input = "cov")
    mis.vectors <- matrix(nrow = nrow(data), ncol = length(mis.list))
    colnames(mis.vectors) <- sapply(mis.list, function(x) colnames(x)[1])
    for (i in 1:length(mis.list)){
      mis.function <- mis.list[[i]][1,-1]
      mis.predictor.name <- c()
      mis.predictor <- matrix()
      mis.indicator.name <- colnames(mis.list[[i]])[1]
      mis.indicator <- c()
      mis.var.name <- parsed.vars[[i]][1]
      mis.temp <- c()
      
      mis.predictor.name <- colnames(mis.list[[i]])[-1]
      mis.predictor <- scale(as.matrix(data[, mis.predictor.name]), 
                             center = TRUE, scale = FALSE)
      mis.temp <- matrix(data[,mis.var.name])
      
      res <- residuals(lm(mis.temp ~ mis.predictor))
      temp <- svd(mis.predictor)
      
      if (length(mis.function) == 0){
        stop("Missingness model not recognized; please check whether the model is correctly parsed.")
      } else if (length(mis.function) == 1) {
        dual <- (nrow(mis.predictor)-1)*temp$u %*% as.matrix(ifelse(temp$d > 0, 1/temp$d, 0)) %*% t(temp$v)
        sigma.2 <- c((1 - mis.function %*% cov(dual) %*% mis.function) / var(res))
      } else {
        dual <- (nrow(mis.predictor)-1)*temp$u %*% diag(ifelse(temp$d > 0, 1/temp$d, 0)) %*% t(temp$v)
        sigma.2 <- c((1 - mis.function %*% cov(dual) %*% mis.function) / var(res))
      }
      mis.indicator <- dual %*% mis.function + sqrt(sigma.2)*res
      mis.vectors[,i] <- mis.indicator
    }
  } else {
    stop("Invalid input matrix; please specify 'cor' or 'cov'.")
  }
  return(mis.vectors)
}
