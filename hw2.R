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
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix<-function(m=matrix()) {
  m_inv <- NULL
  set_m <- function(a){
    m<<-a
    m_inv<<-NULL
  }
  
  get_m <- function() m
  setInv <- function() inv_m <<- solve(m)
  getInv <- function() inv_m
  
  list(set_m = set_m,
       get_m = get_m,
       setInv= setInv,
       getInv = getInv)
}

cacheSolve <- function(m, ...) {
  m_inv <- m$getInv()
  if (!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  m_data <- m$get_m()
  m_inv <- solve(m_data, ...)
  m$setInv(m_inv)
  m_inv
}