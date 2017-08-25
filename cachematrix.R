##--------------------------------------------------------------------------##
## The following fuction Create Matrix that sets and retrieve its			##
## values from an object that is stored in an environment different from    ##
## the current environment.													##
## The function performs the following tasks:								## 														
## Set and Retrieve the value of matrix									    ##
## Get and Sets the value of Inverse of the matrix							##
##--------------------------------------------------------------------------##

makeCacheMatrix <- function(x = matrix()) {
	i   <-  NULL
	set <-  function(y){
		x <<- y
		i <<-  NULL
	}
	## function returns the value assigned to x
	get <- function() x
	
	## function assigns a value and stores it as 
	## an object in the environment
	setInverse <- function(inv) i <<- inv
	
	## returns the Inverse Matrix
	getInverse <- function() i    
	matrix(set = set , get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##---------------------------------------------------------##
## The function calculates the Inverse of the matrix   	   ##
## However, it check if the inverse has already been       ##
## calculated. If it is already calculated then it skips   ##	
## the calculation.										   ##	
## Otherwise, it calculates the inverse of the matrix and  ##
## sets the value in the cache via setInverse function.    ##
##---------------------------------------------------------##
cacheSolve <- function(x, ...) {
	## Gets the inverse value and 
	## returns it if it already has a value
	i    <- x$getInverse()
	if(!is.null(i)) {
	    message("Retrieving Cached Data")
        return(i)
	}	    
		   
	## Otherwise, it calculates the inverse of the matrix and 
	## sets the value in the cache via setInverse function.
	data <- x$get()
	i 	 <- solve(data, ...)
	x$setInverse(i)
	i	
    ## Return a matrix that is the inverse of 'x'
}
