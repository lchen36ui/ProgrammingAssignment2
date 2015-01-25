## This function creates a special "matrix" object that can cache its inverse ##

	makeCacheMatrix <- function(x = matrix()) {
		## defines where to store inverse null ##
    	inverse <- NULL
		
		## matrix y as input to the variable x##
    		## it is used to alter the matrix ##
    	set <- function(y) {
        x <<- y
		## initialize inverse in the parent environment to null ##
        inverse <<- NULL 
    }

    		## returns the raw matrix ##
    	get <- function() x

    		## setinverse sets the inverse variable ##
    	setinverse <- function(i) 
        inverse <<- i
    

    		## getinverse gets the cached inverse ##
    getinverse <- function() 
        inverse
    

   	 	## return the special matrix ##
   	 list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)    


}


## The second function "cacheSolve" calculates the inverse of what returned by the above makeCacheMatrix. ##
## cacheSolve will use the inverse directly from the cache if it was calculated. ##

	cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' ##

	 inverse <- x$getinverse()

	    	if(!is.null(inverse)) {
        
        message("getting inverse from the cache")
        ## return the already-cached inverse ##
		
	return(inverse)
    }

    ## if the inverse has not been cached, do the following ## 
    	
	 	mtrx <- x$get()
    	 	inverse <- solve(mtrx, ...)
    	 	x$setinverse(inverse)
    ## then compute the inverse and cache it ##

	return(inverse)

}
