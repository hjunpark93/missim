select_prop <- function(mis.vectors, parsed.prop){
  missing <- matrix(0, nrow = nrow(mis.vectors), ncol = ncol(mis.vectors))
  colnames(missing) <- colnames(mis.vectors)
  
  for (i in 1:ncol(mis.vectors)) {
    intvs <- parsed.prop[[i]]$interval
    prp <- parsed.prop[[i]]$proportion
    
    intvs.sort <- intvs[order(intvs[, 1]), ]
    intvs.agg <- sum(abs(diff(intvs.sort)))
    
    if (intvs.agg > prp) {
      select <- logical(nrow(mis.vectors))
      if (is.null(nrow(intvs.sort))) {
        lower <- intvs.sort[1]
        upper <- intvs.sort[2]
        percent <- quantile(mis.vectors[, i], c(lower, upper))
        indices <- which(mis.vectors[, i] > percent[1] & mis.vectors[, i] < percent[2])
        select[indices] <- TRUE
      } else {
        for (j in 1:nrow(intvs.sort)) {
          lower <- intvs.sort[j, 1]
          upper <- intvs.sort[j, 2]
          percent <- quantile(mis.vectors[, i], c(lower, upper))
          index <- which(mis.vectors[, i] > percent[1] & mis.vectors[, i] < percent[2])
          select[index] <- TRUE
        }
      }
      target <- round(prp * nrow(mis.vectors))
      if (sum(select) > target) {
        select[sample(which(select), sum(select) - target)] <- FALSE
      }
      missing[select, i] <- 1
    } else {
      if (is.null(nrow(intvs.sort))) {
        lower <- intvs.sort[1]
        upper <- intvs.sort[2]
        percent <- quantile(mis.vectors[, i], c(lower, upper))
        indices <- which(mis.vectors[, i] > percent[1] & mis.vectors[, i] < percent[2])
        missing[indices, i] <- 1
      } else {
        for (j in 1:nrow(intvs.sort)) {
          lower <- intvs.sort[j, 1]
          upper <- intvs.sort[j, 2]
          percent <- quantile(mis.vectors[, i], c(lower, upper))
          indices <- which(mis.vectors[, i] >= percent[1] & mis.vectors[, i] <= percent[2])
          missing[indices, i] <- 1
        }
      }
    }
  }
  return(missing)
}
