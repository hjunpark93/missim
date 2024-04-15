parse_var <- function(data, model){
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
  
  parsed.vars <- list()
  
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
  
  for (i in 1:length(model.inter)) {
    lhs <- unlist(strsplit(model.inter[i], "\t"))[1]
    if (!(lhs %in% colnames(data))) {
      stop("A variable in LHS cannot be identified; please remove coefficient or operator if there is any.")
    }
    
    rhs <- unlist(strsplit(model.inter[i], "\t"))[2]
    var.temp <- unlist(
      regmatches(rhs,
                 gregexpr("[[:alpha:]]+", rhs, perl = TRUE)))
    if (!(all(var.temp %in% colnames(data) == TRUE))) {
      print(paste("Requires attention: ", var.temp))
      stop("A variable in RHS cannot be identified; please check if there is any typo in variable names, or an operator other than + or -.")
    }
    parsed.vars[[i]] <- c(lhs, var.temp)
  }
  return(parsed.vars)
}
