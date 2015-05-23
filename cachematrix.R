## Since matrix-inversion might take much computation-time, we create 2 functions
## which allow to cache the computed results
## This soultion is based on https://github.com/rdpeng/ProgrammingAssignment2
## and therefor part of the 'R Programming' course on Coursera.org


## The function, `makeCacheMatrix` creates a special "vector",
## which is really a list containing a functions to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The following function 'cacheSolve' calculates the inverse of the special "vector"
## created with the above function. It first checks if the inverse is in cache -
## if so, it skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse` function.


cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
