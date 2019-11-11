## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function

#The first function, makeCacheMatrix creates a list containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  A <- NULL #Matrix
  I <- NULL #Inverse
  
  # set the value of the matrix and clear inverse cache
  set <- function(M) {
    A <<- M
    I <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inverse
  setinverse <- function(J) I <<- J
  
  # get the inverse
  getinverse <- function() I
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# The following function calculates the inverse of the special list created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
