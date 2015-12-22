    ## The makeCacheMatrix function creates a list of functions
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the inverse of the matrix
    ## 4. get the inverse of the matrix
    ## Takes an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

    ## The cacheSolve function will compute the inverse of the matrix returned by makeCacheMatrix
    ## If the inverse has already been calculated and the matrix hasn't changed, get the inverse from cache

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	        
    m <- x$getinv()
    
    ## Check if inverse has been calculated, if it has then get it from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    ## Calculate the inverse 
    m <- solve(data, ...)
    x$setinv(m)
    m
}
