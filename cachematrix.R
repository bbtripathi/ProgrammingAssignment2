## FUNCTION: makeCacheMatrix
## DESCRIPTION: Creates a special "matrix" object that can cache its inverse.
## INPUT: A matrix
## OUTPUT: list of functions
makeCacheMatrix <- function(x = matrix()) {
        
		## initialize m
        m <- NULL
		## define function set
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## define function get
        get <- function() x
		
		## set the solve function that returns the inverse
        setsolve <- function(solve) m <<- solve
		## get the inverse if it is there
        getsolve <- function() m
		## return the list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## FUNCTION: cacheSolve
## DESCRIPTION: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.
## INPUT: A matrix
## OUTPUT: Inverse of the matrix

cacheSolve <- function(x, ...) {
        ## get the function
        m <- x$getsolve()
		## check if the matrix is cached 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		## if not, then calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
