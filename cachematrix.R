## cacheMatrix functions by David Tomashek
## package built to meet requirements of Programming Assignment 2
## Coursera Data Science program, R Programming Course, July 2014
## Roger D. Peng, Instructor

## mt

## The makeVector function from the example, as written,
## can actually work to cache any function result, not just mean.
## I pasted it here and changed the variable names to reflect
## the more generalized purpose of the function.
## The assignment wants us to name it makeCacheMatrix, but
## this could as easily be named makeCache and be used for caching
## any function results that work on a numeric vector
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setfctn <- function(fctn) m <<- fctn
  getfctn <- function() m
  list(set = set, get = get,
       setfctn = setfctn,
       getfctn = getfctn)
}

## cacheSolve interacts with makeCacheMatrix and makes use of
## the functions in its list.
## cacheSolve uses getfctn() to test if the cache variable has a result stored 
## if so, it returns the cached value
## otherwise it calculates the inverse matrix then
## loads the results in the cache variable and returns the variable value
cacheSolve <- function(x = matrix()) {
      m <- x$getfctn()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setfctn(m)
      m
}



