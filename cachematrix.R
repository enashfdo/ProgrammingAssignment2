## Put comments here that give an overall description of what your
## functions do
## My aim in this experiment is to write the two functions, which are, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)
## makeCacheMatrix consits of set ,get ,setinv ,getinv

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(z)
  j
}