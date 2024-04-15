parse_prop <- function(prop){
  if (missing(prop)) {
    stop("No specified proportion of missing data.")
  }
  if (!(is.character(prop))) {
    stop("parse_prop() only accepts models in a form of character string.")
  }
  
  parsed.prop <- list()
  
  prop.clean <- gsub(" ", "", prop, perl = TRUE)
  prop.clean <- gsub("\t", "", prop.clean, perl = TRUE)
  prop.clean <- gsub("\n{2,}", "\n", prop.clean, perl = TRUE)
  
  lines <- strsplit(prop.clean, "\n")[[1]]
  if (length(lines) == 0) {
    stop("Empty proportion specification; please provide valid proportions.")
  }

  for (i in 1:length(lines)) {
    intv.prop <- unlist(strsplit(lines[i], ";"))
    if (!(length(intv.prop)==2)){
      stop("Intervals and proportions not correctly parsed.")
    }
    
    intv <- c()
    prp <- c()
    intvs <- list()
    intvs.mat <- matrix()
    
    intv <- strsplit(intv.prop[1], ",")[[1]]
    intvs <- strsplit(trimws(intv), "~")
    intvs <- lapply(intvs, as.numeric)
    if (any(unlist(intvs) < 0) || any(unlist(intvs) > 1)) {
      print(intvs)
      stop("Interval values should be between 0 and 1.")
    }
    
    if (length(intvs) == 1){
      intvs.mat <- matrix(unlist(intvs), ncol = 2, byrow = TRUE)
    } else {
      for (j in 1:(length(intvs) - 1)) {
        for (k in (j + 1):length(intvs)) {
          min_j <- min(intvs[[j]])
          max_j <- max(intvs[[j]])
          min_k <- min(intvs[[k]])
          max_k <- max(intvs[[k]])
          if (max_j >= min_k && max_k >= min_j) {
            print(intvs)
            stop("There is an overlap between the intervals.")
          }
        }
      }
      intvs.mat <- matrix(unlist(intvs), ncol = 2, byrow = TRUE)
    }

    prp <- as.numeric(intv.prop[2])
    if (prp < 0 || prp > 1) {
      print(paste("Proportion value: ", prp))
      stop("Proportion values should be between 0 and 1.")
    }
    
    intvs.agg <- sum(rowSums(diff(t(intvs.mat))))
    tol <- 1e-06
    if (prp > intvs.agg + tol) {
      print(paste("Proportion value: ", prp))
      print(paste("Aggregate interval: ", intvs.agg))
      stop("Proportion should not be larger than the aggregate interval.")
    }
    
    parsed.prop[[i]] <- list(interval = intvs.mat, proportion = prp)
  }
  return(parsed.prop)
}
