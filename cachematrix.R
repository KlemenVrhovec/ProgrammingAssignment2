## Caching the Inverse of a Matrix


##The first function, makeCacheMatrix creates a special "Matrix"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    mx <<- y
    inv <<- NULL
  }
  get <- function() mx
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function calculate inverse of special Matrix with the abowe function. 
##First it checks if inverse was already calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- mx$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mx$get()
  inv <- solve(data)
  mx$setinverse(inv)
  inv
}
