## A pair of functions that help maintain a structure for caching a matrix and its inverse (makeCacheMatrix(x)), 
## R Programming: Week3
## Caching the Inverse of a Matrix
##
##Assumption: the matrix supplied is always invertible
##Creation: the followed functions
##    makeCacheMatrix(X): this fuction creates a special matrix structure that cache its inverse.
##          INPUT: a matrix X or setting the matrix's value using the "set function"
##          OUTPUT: a  list of 4 functions [set the matrix value, get the matrix value, set its inverse, get its inverse]
##    cacheSolve() : this fuction computes of the matrix structure returned by makeCacheMatrix
##          INPUT:a special matrix structure made by makeCacheMatrix(X) 
##          OUTPUT: the inverse of the matrix X.(NOTE: If nothing has changed, then the cached inverse is returned.)

makeCacheMatrix <- function(x = matrix()) { 
  # Variables of makeCacheMatrix(x): x (formal), inverse (local)
  
  # Initialization:variable "inverse"
  inverse<-NULL 
  
  # function makeCacheMatrix$set(y): caches matrix y to x, where "inverse" is a free variable
  set<-function(y){ 
    x<<-y  
    inverse<<-NULL 
  }
  
  # function makeCacheMatrix$get(): get the matrix X
  get <- function() x
  
  # function makeVacheMatrix$setinverse(inv): caches the value of inv as the inverse of the matrix x.
  setinverse <- function(inv) inverse <<- inv  
  
  # function makeCacheMatrix$getinverse(): get the inverse of x
  getinverse <- function() inverse
  
  # the output of function makeCacheMatrix(x): a list containing the 4 above functions 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##    cacheSolve() : this fuction computes of the matrix structure returned by makeCacheMatrix
##          INPUT:a special matrix structure made by makeCacheMatrix(X) 
##          OUTPUT: the inverse of the matrix X.(NOTE: If nothing has changed, then the cached inverse is returned.)

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of matrix "x"
  inverse<-x$getinverse()
  
  # Unless new data "appear", return cached inverse(which has already been computed) and exit
  ## Otherwise, get new data
  if (!is.null(inverse)){
    message("getting cached inverse")
    return(inverse) 
  }
  
  data<- x$get() 
  
  #get the inverse and cache it to the matrix
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  # return the inverse
  inverse
  
}
