## Two functions are created in this. Both of these perform different task
## Inverse of Matrix is obtained at the end of execution

##Creating a function to CacheMatrix

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)i <<- inverse
  getInverse <- function()i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Creating a function to get matrix inverse

cacheSolve <- function(x, ...){
  ##Return inverse of matrix which will be a matrix
  i <- x$getInverse()
  if(!is.null(i)){
    message("Data is getting cached")
    return(i)
  }
  matrix_new <- x$get()
  i <- solve(matrix_new, ...)
  x$setInverse(i)
  i
}