## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(Z){
                x <<- matrix(Z)
                I < NULL        
        }
        get <- function() x
        setInv <- function(inverse) I <<- inverse
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        I <- x$getInv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInv(I)
        I
}

