#makeCacheMatrix creates a matrix that can cache its inverse.  



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  

#cacheSolve is function that returns the inverse of the matrix, by first 
#checking if the inverse has already been computed. If the inverse has been computed, 
#it skips the computation and return the cached result. If not, 
#it computes the inverse and sets the value in the cache by using the 
#setinverse function. 


cacheSolve <- function(x, ...) {
      

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  }
