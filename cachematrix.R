## The following two functions "makeCacheMatrix" create a special "matrix" object to cache its inverse and "cacheSolve" computes the inverse of the special "matrix" 
## object returned by makeCacheMatrix function.

## makeCacheMatrix function creates a special "Matrix" object which  will cache its own inverse using the following methods - 

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

	## Initializing the inverse variable
	
        i <- NULL
	
        ## Method to set the value of the matrix
        
        set <- function(y) {
        x <<- y
        i <<- NULL
}


	## Method to get the value of the matrix
	get <- function() x

	## Method to set the inverse of the matrix
	setinv <- function(inv) i <<- inv

	## Method to return cached inverse of the matrix
	getinv <- function() i

	## Returns list of supported methods
  	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve function computes the inverse of the matrix which is returned by the first function "makeCacheMatrix".
## If the inverse of the matrix has already been calculated, then this function will get the inverse from the cache else it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	 i <- x$getinv()
        
	## Verify if the inverse has already been cached
	 if(!is.null(i)) {
	        message("getting cached inverse.")

	## Return the cached inverse
        return(i)
    }

	## Get the matrix data
	matdata <- x$get()

	## Compute matrix inverse
        i <- solve(matdata)

	## Cache the inverse for use in future
        x$setinv(i)

	## Return the inverse of matrix
        i
}
