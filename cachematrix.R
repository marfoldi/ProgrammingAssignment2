## Matrix cache creation for calculating inverse operations. 
## Inversion of a matrix is pretty expensive operation, it has some benefit to
## store the inverted matrix in memory cache, instead of recalculating it again and again.
## The following two functions are used to cache the inverse of a matrix. 


## This function creates and caches matrix and allows accessing of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        #Safety check: quadratic matrix was determined
        if(dim(x)[1] != dim(x)[2]) stop("Matrix isn't quadratic.")
        
        det <- determinant( x )$modulus[1]
    
        #Safety check: the determinant of the matrix is not 0
        if( det == 0 ) stop("Matrix is singular (determinant=0).")
    
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function caches the inverse of matrix so that it can be retrieved by the previous function 
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        #Cache check
        if(!is.null(inv)){
                message("Providing matrix inverse from cache.")
                return(inv)
        }
    
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
