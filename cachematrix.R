## This code is used to compute the inverse of a matrix.
## makeCacheMatrix creates a matrix object which is cached
## cacheSolve stores the inverse of the matrix object into cache
## if the inverse of that matrix object was previously computed
## it simply retrieves the inverse from cache


## makeCacheMatrix defines 3 function for a matrix object
## getMat - returns the current matrix object
## setInv - caches a new inverse value
## getInv - returns the inverse of the current matrix object
## returns a list of all the 3 functions
makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      ## return matrix
      getMat <- function() x       
      
      ## set inverse
      setInv <- function(inverse) inv <<- inverse
        
      ## return inverse
      getInv <- function() inv     
      
      ## return list of functions
      list(getMat = getMat, getInv = getInv, setInv = setInv)
}


## cacheSolve checks if the inverse of the matrix object x is already cached
## if so, then it retrieves the value from cache
## if not, it computes the inverse and stores it in cache for future use
## This function returns the inverse of x
cacheSolve <- function(x, ...) {
  
      ## obtain the inverse for the input matrix object
      invMat <- x$getInv()
      
      ## check if the inverse exists
      if(!is.null(invMat)){
            ## returns cached inverse
            message("getting cached data")
            return(invMat)
      }
      
      ## if inverse does not exist, get the matrix 
      mat <- x$getMat()
      
      ## compute inverse
      invMat <- solve(mat)
      
      ## store inverse in cache
      x$setInv(invMat)
      
      ## return inverse
      invMat
      
}




