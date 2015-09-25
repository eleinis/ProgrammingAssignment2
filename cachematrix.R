## The functions return a inverse matrix of a input matrix.
##If the inverse matrix was already computed, the cache function retrieves that info
##instead of computing the inverse matrix again

#Function that computes and stores the inverse of a matrix x (s) in the cache memory

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function that get the cached data based on the function above

cacheSolve <- function(x, ...) {
  s <- solve(x)
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s   ## Return a matrix that is the inverse of 'x'
}
