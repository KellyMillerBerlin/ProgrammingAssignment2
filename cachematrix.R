## makeCacheMatrix creates a matrix object to cache its inventory
## Part 1: creates matrix object, initializes inverse, sets matrix, gets matrix, returns matrix
## sets inverse of matrix, gets inverse and returns inverse property and methodology


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    get <- function() {
        m
    }
    
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    getInverse <- function() {
        i
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve finds the inverse of the matrix object as determined by makeCacheMatrix. 
## If the inverse has already been returned above, then this function retrieves inverse from the cache. 
## Part II: finds inverse of the matrix, returns matrix that is inverse of variable,
## returns inverse if it has been set, gets matrix, calculates inverse by multiplication, 
## sets inverse to the object, returns the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
    
}
