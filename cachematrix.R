## The purpose of makeCacheMatrix() is to be able to  
## produce a matrix which is able to cache it's own  
## inverse and to define functions which will allow   
## cacheSolve() to either reproduce the inverses, or to
## simply calculate them if the inverse is defined as NULL

## makeCacheMatrix() will create a matrix which will cache 
## the inverse of it's values. It will define functions
## to set and get the matrix and the inverse which can
## be used by the cacheSolve() function 

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinv<- function(inverse) inv <<- inverse 
  getinv<- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve() will either retrieve the inverse cached by
## makeCacheMatrix utilising lexical scoping, or it will
## calculate the inverse and return it

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data<- x$get()
  inv<- solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
