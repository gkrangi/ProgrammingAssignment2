## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The following function creates a special "matrix" object to cache its inverse.
## It is assumed that the matrix supplied is always invertible (square matrix).

makeCacheMatrix <- function(x = matrix()) { # define the argument with default mode 'matrix'
  inv<-NULL                                 # initialize inv as NULL to store the value of inverse matrix
  set<-function(y){                         #  define the set function to assign new value of matrix in parent environment
  x<<-y
  inv<<-NULL                                #  reset inv to NULL if there's a new matrix
  }
  get<-function()x                          # define the get function to return value of the matrix argument
  
  setinverse<-function(inverse) inv<<-inverse #assigning value of inv in parent environment
  getinverse<-function() inv                  # gets the value of inv when called 
  list(set = set, get = get,                  # assigns each function names to access them using $
       setinverse = setinverse,
       getinverse = getinverse)  

}

## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then cacheSolve will get the inverse from cache given the matrix remains unchanged.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {                       # checking if inverse is NULL
    message("getting cached data")
    return(inv)                             # returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)                   # calculates inverse value
  x$setinverse(inv)
  inv                                       # returns matrix inverse of 'x'
}

