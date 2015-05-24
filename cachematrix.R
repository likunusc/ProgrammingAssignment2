## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function contains the basic functions of get set setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setinv <- function(inv) r <<- inv
  getinv <- function() r 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## this is the function to cache the inverse of matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getinv()
  if(!is.null(r)) {
    message("getting cached data.")
    return(r)
  }
  data <- x$get()
  r <- solve(data)
  x$setinv(r)
  r
}
