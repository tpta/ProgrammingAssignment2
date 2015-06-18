## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix as input. The result lets the 
## user to store and retrieve the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setInv <- function(invs) inv <<- invs
   getInv <- function() inv
   list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)  
}


## Write a short comment describing this function
## This function takes a cacheMatrix, the output of
## makeCacheMatrix, as input. The output is the inverse 
## matrix. The output is retrieved if it exists or calculated
## and stored if it doesn't.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInv(inv)
  inv
}
