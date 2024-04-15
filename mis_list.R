mis_list <- function(data, parsed.model, input = "cov"){
  if (missing(parsed.model)) {
    stop("Empty model; please specify a missingness model.")
  }
  if (!(is.list(parsed.model))) {
    stop("mis_matrix() only accepts models in a list of matrices.")
  }
  if (missing(data)) {
    stop("No data input; please provide reference data.")
  }
  
  mis.reference <- list()
  
  if (input == "cor") {
    submatrix <- cor(data)
  } else if (input == "cov") {
    submatrix <- cov(data)
  } else {
    stop("Invalid input matrix; please specify 'cor' or 'cov'.")
  }
  
  for (i in 1:length(parsed.model)){
    temp <- parsed.model[[i]]
    vec.temp <- colnames(parsed.model[[i]])[2:ncol(parsed.model[[i]])]
    mat.temp <- submatrix[vec.temp, vec.temp]
    
    temp1 <- cbind(temp[2:length(temp)], mat.temp)
    temp2 <- rbind(temp, temp1)
    rownames(temp2) <- colnames(temp2)
    
    if (!(isSymmetric(temp2))){
      print(temp2)
      stop("The matrix above is not symmetrical. Please check if a variable name or coefficient is not parsed correctly.")
    }
    ev.check <- eigen(temp2, symmetric = TRUE)$values
    
    if (!all(ev.check >= 0)) {
      print(temp2)
      stop("Your model generates the above matrix that is not positive-definite.")
    }
    mis.reference[[i]] <- temp2
  }
  return(mis.reference)
}
