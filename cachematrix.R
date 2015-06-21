## Put comments here that give an overall description of what your
## functions do:
# While makeCacheMatrix is a getter and setter 'wrapper' function, cacheSolve checks if 
# an inverse matrix is already included in makeCacheMatrix 'wrapper' function. If it doesn't
# an inverse matrix will be calculated based on the matrix stored in makeCacheMatrix object. Else 
# it will just return the inversed matrix stored. 

## Write a short comment describing this function:
# The nested 'set function' takes the matrix and assigns it to variable i.
# The nested 'get function' displays the matrix captured through the get function
# The nested 'setinverse function' takes a 'solve' function and assigns it to variable i.
# The nested 'getinverse function' displays the 'solve' function captured through the setinverse function
makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(somevar) i <<- somevar
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function:
# cacheSolve takes a makeCacheMatrix object as argument
# It then tests if the makeCacheMatrix object has a inverse matrix value or null.
# If makeCacheMatrixOBJ$getinverse() has value, it just returns value. Otherwise function i takes 
#   the matrix stored in the makeCacheMatrixOBJ$get() object to calculate an inverse matrix and returns it. 

cacheSolve <- function(x,...){
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- -solve(data, ...)
  message("calculating inverse of matrix")
  x$setinverse(i)
  i     # Return a matrix that is the inverse of 'x'
}
