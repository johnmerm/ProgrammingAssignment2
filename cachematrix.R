## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
  
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- m$getInverse()
        if (!is.null(i)){
                message("using cached inverse")
                return(i)
        }
        data <- x$get()
        i <-solve(data)
        x$setInverse(i)
        return(i*b)
  
}
