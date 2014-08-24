## makeCacheMatrix and cacheSolve functions work together to calculate the inverse
## of a input matrix and cache its inverse matrix for subsequent accesss if the 
## input matrix has not changed.


makeCacheMatrix <- function(x = matrix()) {
      ## makeCacheMatrix function creates a matrix object that stores a numeric matrix 
      ## and caches its inverse.   
      ##
      ## Args:
      ##   x: An invertible matrix object.
      ##
      ## Returns:
      ##   A list object containing four functions to manipulate the matrix.
      
      
      inv <- NULL       # Defines a local variable to store the inverse of the 
      # matrix and sets is to NULL.   
      
      set <- function(y) {    # Changes the stored matrix and sets the value of
            x <<- y           # inverse variable to NULL to force recalculation  
            inv <<- NULL      # of inverse matrix if the stored matrix has changed.
      }
      
      get <- function() x     # Returns the value of input matrix stored in the 
      # matrix object
      
      setinverse <- function(inverse) inv <<- inverse # Receives the calculated 
      # inverse matrix and stores it in a variable in the 
      # parent environment. 
      
      getinverse <- function() inv  # Retreives the inverse matrix from the parent
      # environments and returns it.
      
      list(set = set, get = get,       # Returns a list containing four functions 
           setinverse = setinverse,    # defined above to access and manipulate  
           getinverse = getinverse)    # the input and inverse matrices stored. 
}




cacheSolve <- function(x, ...) {
      ## cacheSolve function calculates the inverse of the matrix returned by 
      ## makeCacheMatrix function or retreives the inverse from the cache.
      ##
      ## Args:
      ##   x: Special matrix object returned by makeCacheMatrix function. 
      ##
      ## Returns:
      ##   The inverse of the input matrix to the makeCacheMatrix function.
      
      
      inv <- x$getinverse()   # Uses object method to retreive the stored inverse matrix.
      
      if(!is.null(inv)) {     # If a previously calculated inverse matrix exists
            message("getting cached data")      # returns it and exits the function.
            return(inv)
      }
      
      data <- x$get()         # Accesses the input matrix stored in the matrix object.
      
      inv <- solve(data, ...) # Calculates the inverse of the input matrix. 
      
      x$setinverse(inv)       # Saves the calculated inverse matrix in the matrix object.
      
      inv         # Returns the inverse matrix to the calling expression. 
}
