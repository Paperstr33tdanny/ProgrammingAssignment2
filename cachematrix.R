## Assignment objectives 
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.

##Using the examples for reference
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }

## Similar to makeVector but instead of vector it returns a matrix
# you must create the maatrix with makeCacheMatrix and then use cacheSolve to process the inverse
makeCacheMatrix <- function(x = matrix()) {
  #nothing different from exercise example
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #function to setting the inverse of m
  setinverse <- function(inverse) m <<- inverse
  
  #return the inverse of the passed matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
# similar to cachemean it  returns the matrix inverse of the matrix passed as parameter
# if its not cached in makeCacheMatrix$setinverse it will fetch the matrix and use solve to inverse and return
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    #use solve to return the inverse of data into m
    m <- solve(data, ...)
    #
    x$setinverse(m)
    #return m
    m
}
