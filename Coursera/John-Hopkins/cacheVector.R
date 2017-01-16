makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  k <- x$getmean()
  if(!is.null(k)) {
    message("getting cached data")
    return(paste("cached value",k))
  }
  data <- x$get()
  k <- mean(data, ...)
  x$setmean(k)
  return(paste("non-cached value",k))
}