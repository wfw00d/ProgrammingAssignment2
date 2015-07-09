## These functions create a 'class' of sorts that can retrieve and define 
## both a matrix and its inverse

## This defines a matrix 'class' that stores its inverse, as well as several
## call functions

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL  ## m is the inverse of the set matrix
  set <- function(y) { 
    x<<-y
    m<<- NULL
  }
  ## x is the set matrix and the set function redefines it and sets m to NULL
  get<-function() x ## calls set matrix
  setInverse<- function(inverse) m<<-inverse ## defines the inverse matrix
  getInverse<- function() m ## call the inverse matrix
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
