mis_logit <- function(data, model, input = "cov", prop) {
  if (missing(prop)) {
    stop("No specified proportion of missing data.")
  }
  
  if (!is.numeric(prop) || any(prop < 0) || any(prop > 1)) {
    stop("prop must be a numeric vector with values between 0 and 1.")
  }
  
  mis.matrix <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  colnames(mis.matrix) <- colnames(data)
  
  mis.vectors <- from_cont(data = data, model = model, input = input)

  if (length(prop) != ncol(mis.vectors)) {
    stop("The number of specified proportions must be equal to the number of missing variables.")
  }
  
  for (i in 1:ncol(mis.vectors)) {
    target.prop <- prop[i]
    threshold <- quantile(mis.vectors[,i], 1 - target.prop)
    logit.temp <- ifelse(mis.vectors[,i] >= threshold, 1, 0)
    
    logit.model <- glm(logit.temp ~ mis.vectors[,i],
                       family = binomial(link = "logit"))
    
    mis.prob <- predict(logit.model,
                        newdata = data.frame(mis.vectors[,i]),
                        type = "response")
    
    missing <- mis.vectors[runif(length(mis.vectors[,i])) < mis.prob]
  }
  return(mis.matrix)
}