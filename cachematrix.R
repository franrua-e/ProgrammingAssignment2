## This function will recycle and saved the inverse of a matrix 
#in case it is needed afterwards. This will help us to avoid having to
# recalculate if required.

# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}  


## This function will determine whether it is required to calculate the inverse
#of the given matrix or if it just has to pull its value from cache.

cacheinverse <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
