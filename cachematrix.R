## Matrix inversion can be costly computation wise so these functions
## allow the inverse to be cached so that it does not need to be 
## computed over again if the matrix has not changed

## makeCacheMatrix takes a matrix as its argument and creates a list
## of functions that initialize the matrix and the cached inverse,
## get the matrix, set the inverse in the cached matrix, and get
## the cached inverse. The list of functions is then returned to a
## parent environment.

makeCacheMatrix <- function(x = matrix()) {
    # define & initialize local variable used for cache to NULL
    cached_inverse <- NULL
    
    # Create function to cache new matrix and reset inverse to NULL
    set <- function(y) {
        # Cache new matrix in parent environment
        x <<- y
        
        # Reset cached inverse in parent environment to NULL
        cached_inverse <<- NULL
    }
    
    # Create function that returns cached matrix x
    get <- function() x
    
    # Create function that that takes matrix inverse as argument
    # and saves it to as cached version in parent environment
    setinverse <- function(inverse) cached_inverse <<- inverse
    
    # Create function that returns cached inverse
    getinverse <- function() cached_inverse
    
    # Return named list of functions created above to parent environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## cacheSolve takes a variable x describing the parent 
## environment and uses functions created in makeCacheMatrix to
## determine if inverse of the matrix has been created. If it
## has been created it returns the inverse already computed.
## If it hasn't been created it computes the inverse, saves
## a cached version, and returns the computed inverse

cacheSolve <- function(x, ...) {
    # Get copy of cached inverse from parent environment
    cached_inverse <- x$getinverse()
    
    # Check if inverse already computed
    if(!is.null(cached_inverse)) {
        # Let user know function is retrieving cached inverse
        message("getting cached data")
        
        # Return already computed inverse and exit function
        return(cached_inverse)
    }
    
    # Get copy of new matrix from parent environment
    data <- x$get()
    
    # Compute inverse of new matrix
    inv_matrix <- solve(data)
    
    # Cache copy of new inverse in parent environment
    x$setinverse(inv_matrix)
    
    # Return new inverse
    inv_matrix

}
