## Put comments here that give an overall description of what your
## functions do

##Below are two functions makeCacheMatrix, makeCacheMatrix. 
##It contains of get, set, setInv, and getInv.
##The function can create a matrix and caches its inverse. 

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
##This function is used to get cache data. 

cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv    ##Return a matrix that is the inverse of 'x'
}
