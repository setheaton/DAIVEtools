check_colnames <- function(frame, cols) {
  check <- TRUE
  for(i in 1:length(cols)) {
    if(!(cols[i] %in% colnames(frame))) {
      check <- FALSE
    }
  }
  return(check)
}
