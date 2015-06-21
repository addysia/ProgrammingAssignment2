makeCacheMatrix <- function(x = matrix()){
  
  # The nested set function takes the matrix and assigns it to variable i.
  # The nested get function displays the matrix captured through the get function
  # The nested setinverse function takes a 'solve' function and assigns it to variable i.
  # The nested getinverse function displays the 'solve' function captured through the setinverse function
  
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


cacheSolve <- function(x,...){
  
  # cacheSolve takes a makeCacheMatrix object as argument
  # It then tests if the setinverse() function has a captured inverse matrix value or null.
  # If setinverse() has value, it just returns value. Otherwise function i takes 
  #   the matrix stored in the makeCacheMatrix object to calculate and inverse matrix and returns it. 
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- -solve(data, ...)
  message("calculating inverse of matrix")
  x$setinverse(i)
  i
}