# the input is a n dimensional data set
modify_n_dim_data <- function(nDimStates, d, id) {
  if(length(nDimStates) == 0) return(d)
  len <- length(nDimStates)
  stateNames <- names(nDimStates)
  for(i in seq(len)) {
    stateName <- stateNames[[i]]
    nDimState <- nDimStates[[i]]

    if(length(nDimState) == 0) next
    if(length(nDimState) == 1) {
      d[[stateName]] <- nDimState
    } else {
      d[[stateName]] <- nDimState[id]
    }
  }

  return(d)
}
