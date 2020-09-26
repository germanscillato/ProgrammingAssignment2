
##  Assumptions: the matrix supplied is always invertible.
## Objetive: Calculate the inverse of a invertible matrix, and save compunting time with a cache method storage of previus calculations.


# Create list with a function that define a matrix data, take matrix data, generate inv matrix, take inv matrix 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) 
    {m <<- solve}
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## solve matrix inverse, with a previus verification of inv matrix stored in cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
          ## Return a matrix that is the inverse of 'x'
}
