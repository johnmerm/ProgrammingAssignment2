## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  getInverse <- function() {
    if (!is.null(i)){
      message("using cached inverse")
    }else{
      i <- solve(x)
    }
    return(i)
  }
  list(set=set,get=get,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, b1,b2) {
  ## Return a matrix that is the inverse of 'x'
  mm <-makeCacheMatrix(x)
  s1 = mm$getInverse()*b1
  s2 = mm$getInverse()*b2
  
  return (c(s1,s2))
  
}
