## These two functions are designed to 1- Make a Matrix Object (makeCacheMatrix(x)) and to
## 2 - compute (cacheSolve(<matrixobject>)) and store the inverse of the matrix in the 
## cached matrix object x as x$getInverse()

## makeCacheMatrix() takes a matrix as input and stores it in an R object, say mC, so that code can be
## used to 1 retrieve the object as mC$get() (sort of silly since you have to have it in the first place)
## or 2 change it as mC$set(), or 3 compute and store the cached matrix's inverse as mC$setInverse() and
## finally 4, retrieve the cached inverse as mC$getInverse()

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix) m <<- solve(matrix)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve() will take a matrix object created by makeCacheMatrix as input and compute and store the 
## inverse of the matrix in the matrix object so that it can be retrieved without recomputing the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

## the following is my test code for the above functions:
wM <- matrix(rnorm(16),4,4)
wMCache <- makeCacheMatrix(wM)
wM == wMCache$get()
wMI <- cacheSolve(wMCache)
wMI %*% wM