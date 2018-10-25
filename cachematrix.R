## "makeCacheMatrix" and "cacheSolve" to cache the inverse of matrix
## these are functions

## creates a special matrix object that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinve <- function(inverse)inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinve=setinve, getinv=getinv)
  

}


## calculates the inverse for the matrix object which is returned from the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinve(inv)
  inv
}
