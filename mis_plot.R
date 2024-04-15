mis_plot <- function(data, var.vector, mis.matrix) {
  if (missing(data)) {
    stop("No data input; please provide reference data.")
  }
  
  if (!identical(dim(data), dim(mis.matrix))) {
    stop("The dimensions of data and missingness matrix are not identical.")
  }
  
  if (!all(mis.matrix %in% c(0, 1))) {
    stop("Missingness matrix must be composed of only 0s and 1s.")
  }
  
  if (!is.character(var.vector) || length(var.vector) < 2) {
    stop("var.vector must be a character vector with at least two elements(variables).")
  }
  
  mis.var <- var.vector[1]
  prd.vars <- var.vector[-1]
  
  # Determine the range of x-axis values across all predictor variables
  x_range <- range(data[, prd.vars])
  
  par(mfrow = c(2, length(prd.vars)))
  
  for (i in seq_along(prd.vars)) {
    prd.var <- prd.vars[i]
    
    plot(data[, mis.var], data[, prd.var],
         main = paste(mis.var, "vs", prd.var), xlab = prd.var,
         type = "h", xlim = x_range)
    
    plot(data[mis.matrix[, mis.var] == 1, mis.var], data[mis.matrix[, mis.var] == 1, prd.var],
         main = paste(mis.var, "vs", prd.var, "(Missing Values)"), xlab = prd.var,
         type = "h", xlim = x_range)
  }
}
