## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ##Set up the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ##Get the matrix
  get<-function()x
  
  ##Set the inverse of the matrix
  setInverse<-function(inverse) m<<-inverse
  
  ##Get the inverse of the matrix
  getInverse<-function()m
  
  ##Returns a list of the functions above
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##Return inverse matrix of 'x'
  m<-x$getInverse()
  
  ##If the inverse has already been calculated, then return value with message
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ##Otherwise, calculate the inverse
  data<-x$get()
  m<-solve(data, ...)
  
  ##Set the value of the inverse in the cache
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}