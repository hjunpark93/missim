mis_prop <- function(data, model, prop, input = "cov"){
  if (missing(prop)) {
    stop("No specified proportion of missing data.")
  }
  if (!(is.character(prop))) {
    stop("parse_prop() only accepts models in a form of character string.")
  }
  
  parsed.model <- parse_mis(data = data, model = model)
  parsed.prop <- parse_prop(prop)
  parsed.vars <- parse_var(data = data, model = model)

  mis.matrix <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  colnames(mis.matrix) <- colnames(data)
  
  mis.vectors <- from_cont(data = data, model = model, input = input)
  
  if (!(ncol(mis.vectors) == length(parsed.prop))){
    stop("The number of specified proportions must be equal to the number of missing variables.")
  }
  
  missing <- select_prop(mis.vectors, parsed.prop)
  mis.match <- match(sub("^mis_", "", colnames(missing)), colnames(mis.matrix))
  mis.matrix[, mis.match] <- missing

  return(mis.matrix)
}
