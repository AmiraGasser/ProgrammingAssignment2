
# makeCacheMatrix function creates a special "matrix"
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}
