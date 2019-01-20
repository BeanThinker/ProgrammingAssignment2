## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: Returns a list with functions to:
# 1. Get Data Matrix
# 2. Set Data Matrix
# 3. Get Inverse of Matrix
# 4. Set Inverse of Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setData <- function(data) {
    x <<- data
    inv <<- NULL
  }
  getData <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setData = setData, getData = getData,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve:Return a matrix that is the inverse of 'x'
# Uses either cache to fetch already calculated inverse
# or calculates inverse (and returns after storing in cache)
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
      message("Getting Cached Data...")
      return(inv)
  }
  data <- x$getData()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


# Testing commands - To test out both functions
# mat <- matrix(rnorm(625), c(25, 25)) # Big enough matrix to notice change
# invVec <- makeCacheMatrix(mat)
# invVec$getInverse() # No inverse found
# cacheSolve(invVec)  # Calculates and stores inverse in cache
# cacheSolve(invVec)  # "Getting cached Data..." message is printed. Proof that inverse isn't calculated again
# invVec$getInverse() # Retrieves inverse from cache

