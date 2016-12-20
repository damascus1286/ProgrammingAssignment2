## Put comments here that give an overall description of what your
## functions do



makeCacheMatrix <- function(x = matrix()) {
  ## this function makes a cache matrix 
  
  # This should 1) set the value of the matrix
  # 2) get the value of the matrix
  # 3) set the value of the inverse
  # 4) get the value of the inverse
  # the cacheSolve() function takes this thing as input
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) sol <<- inverse 
  getinverse <- function() sol
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  

}

cacheSolve <- function(x, ...) {
  # given the output of makeCacheMatrix(),
  # this should return the inverse of the original matrix using caching
  sol <- x$getinverse()
  #in the event that sol (the solution) is not null, retrieve the value from the cache (m)
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <-x$get()
  sol <- solve(data, ...)
  x$setinverse(sol)
  sol
}
