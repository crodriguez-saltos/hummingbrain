merge_specdf <- function(x){
  # For this function to work, all power spectra must be in the same
  # frequency scale

  merged <- do.call("cbind", lapply(x, function(el) el[,-1]))

  # Add frequency scale
  merged <- cbind(x[[1]][,1], merged)
  colnames(merged)[1] <- colnames(x[[1]])[1]

  # Add info on each column
  segmentinfo <- lapply(x, function(el) attr(el, "segment_info"))
  attributes(merged) <- c(attributes(merged),
                          list(segment_info= do.call("rbind", segmentinfo)))

  # Output
  return(merged)
}
