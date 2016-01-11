## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invi <- NULL
  set <- function(y){
    x <<- y
    invi <<- NULL
  }
  get <- function() x
  calinvi <- function(z) invi <<- z
  getinvi <- function() invi
  list(set = set, get = get,                         #use to call the four functions
       calinvi = calinvi,
       getinvi = getinvi)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invi <- x$getinvi()
  if(!is.null(invi)){
    message("getting cached data")  
    return(invi) 
  }
  data<-x$get()
  invi <- solve(data,...)
  x$calinvi(invi)
  invi
}