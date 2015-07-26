## These functions create a 'class' of sorts that can retrieve and define 
## both a matrix and its inverse

## This defines a matrix 'class' that stores its inverse, as well as several
## call functions

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL  ## i is the inverse of the set matrix
  ## the set function is the typical mutator, when redefining x, the inverse is no longer valid
  set <- function(y) { 
    x<<-y
    i<<- NULL
  }
  get<-function() x ## the typical accessor function
  setInverse<- function(inverse) i<<-inverse ## defines the inverse matrix
  getInverse<- function() i ## accessor for the inverse matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This both sets and returns the inverse of the matrix x

cacheSolve <- function(x, ...) {
  m<- x$getInverse() ## checks if the inverse matrix is already cached
  if(is.null(m)) {  ## if not already cached, the inverse is calculated and cached
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
  }
  m
}
