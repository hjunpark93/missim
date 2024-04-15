parse_mis <- function(data, model){
  if (missing(model)) {
    stop("Empty model; please specify a missingness model.")
  }
  if (!(is.character(model))) {
    stop("parse_mis() only accepts models in a form of character string.")
  }
  if (any(is.na(data))) {
    stop("Input data is incomplete; only complete data is acceptable.")
  }
  if (missing(data)) {
    stop("No data input; please provide reference data.")
  }
  
  parsed.model <- list()
  
  model.clean <- gsub(" ", "", model, perl = TRUE)
  model.clean <- gsub("\\*", "", model.clean, perl = TRUE)
  model.clean <- gsub("\t", "", model.clean, perl = TRUE)
  model.clean <- gsub(";", "\n", model.clean, fixed = TRUE)
  model.clean <- gsub("~", "\t", model.clean, fixed = TRUE)
  model.clean <- gsub("=", "\t", model.clean, fixed = TRUE)
  model.clean <- gsub("\t{2,}", "\t", model.clean, perl = TRUE)
  model.clean <- gsub("\n{2,}", "\n", model.clean, perl = TRUE)
  
  model.inter <- unlist(strsplit(model.clean, "\n"))
  if (length(model.inter) == 0) {
    stop("Empty model; please specify a missingness model.")
  }
  
  for (i in 1:length(model.inter)){
    mat.temp <- matrix(nrow = 1)
    rownames(mat.temp) <- "coef"
    
    lhs <- c()
    rhs <- c()
    var.temp <- c()
    rhs.temp <- c()
    
    lhs <- unlist(strsplit(model.inter[i], "\t"))[1]
    if (!(lhs %in% colnames(data))){
      stop("A variable in LHS cannot be identified; please remove coefficient or operator if there is any.")
    } else {
      mat.temp[1,1] <- as.numeric(1)
      colnames(mat.temp)[1] <- paste0("mis_", lhs)
    }
    
    rhs <- unlist(strsplit(model.inter[i], "\t"))[2]
    var.temp <- unlist(
      regmatches(rhs,
                 gregexpr("[[:alpha:]]+", rhs, perl = TRUE)))
    if (!(all(var.temp %in% colnames(data) == TRUE))){
      print(paste("Requires attention: ", var.temp))
      stop("A variable in RHS cannot be identified; please check if there is any typo in variable names, or an operator other than + or -.")
    }
    
    rhs.temp <- unlist(strsplit(rhs,
                                "(?<!^|[\\s+-])(?=[+-])", perl = TRUE))
    for (j in 1:length(rhs.temp)){
      temp1 <- c()
      temp2 <- c()
      temp1 <- regexpr("[[:alpha:]]", rhs.temp[j])
      temp2 <- substring(rhs.temp[j],c(0,temp1),c(temp1-1,nchar(rhs.temp[j])))
      
      if (nchar(temp2[1]) == 0 | is.na(temp2[1]) == TRUE){
        mat.temp <- cbind(mat.temp, as.numeric(1))
      } else if (temp2[1] == "-") {
        mat.temp <- cbind(mat.temp, as.numeric(-1))
      } else {
        mat.temp <- cbind(mat.temp, as.numeric(temp2[1]))
      }
      colnames(mat.temp)[j+1] <- temp2[2]
    }
    parsed.model[[i]] <- mat.temp
  }
  return(parsed.model)
}
