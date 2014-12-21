
## Archibald Gillett - RProgramming - 12/20/14
## Programming assignment2

## The two functions below, create a special Matrix and cache its inverse; so that when it is needed, 
## it can be looked up in the cache, thus saving coding and recomputation time

## --------------------------------------------------------------------------------------------------


## The function "makeCacheMatrix" creates a special Matrix that will cache its inverse

makeCacheMatrix <- function (a = matrix()) {
  m <- NULL
  set <- function(s) {
    a <<- s
    m <<- NULL
  }
  get <- function() a
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list( set = set, get = get,
        setMatrix = setMatrix,
        getMatrix = getMatrix )   
}


## The function "cachesolve" returns a matrix that is the inverse of 'a'

CacheSolve <- function ( a = matrix(), ...) {
  m <- a$getMatrix()
  if ( !NULL(m)) {
    message("Get cached data")
    return (m)
  }
  matrix <- a$get()
  m <- solve(matrix, ...)
  a$setMatrix(m)
  m
}
