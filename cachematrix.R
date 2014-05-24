## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#initialize i (the inverse matrix to null)
  	i <- NULL
  	set <- function(y){
		#set shall put y into x and (re)set i to NULL
    	x <<- y
    	i <<- NULL
  	}
  	
	#getter for Matrix :retun x
  	get <- function() x
  	getInverse <- function() {
		#Lazy init inverse: If i exists return else calculate
		#must use  the <<- operator since i is in upper scope
		
	   	if (is.null(i)){
			message("inverting matrix")
	    	i <<- solve(x)
	    }
	    return(i)
  }
  return(list(set=set,get=get,getInverse=getInverse))
}


## Write a short comment describing this function

cacheSolve <- function(A,b) {
  ## create cached matrix
  Ac <-makeCacheMatrix(A)
  
  #solving for x means multiplying A inverse with b
  # matrix - vector mult is done by the %*% operator	
  x <- Ac$getInverse() %*% b
  return (x)
  
}

#some test code to check that everything works
# A is a rotation matrix (rotate axes by pi/8)
A = rbind(c(cos(pi/8),-sin(pi/8)),c(sin(pi/8),cos(pi/8)))
b = c(4,0)

x <- cacheSolve(A,b)
