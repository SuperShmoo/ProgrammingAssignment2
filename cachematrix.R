## These functions work together to cache the inverse of a matrix.
## Step 1: Feed your matrix into makeCacheMatrix:
##         If "mat" is your matrix then: matcache <- makeCacheMatrix(mat)
## Step 2: mat_inverse <- cacheSolve(matcache)
##         This will return the inverse of "mat"


##This function generates a list of functions to cache information about your matrix. 
##Both the matrix itelf and its inverse are cached
makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ix <<- inverse
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function searches to see if the inverse of a matrix has been cached. If it has, the
##cached inverse is retrieved. If it has not been cached yet, the inverse is calculated and then
##cached, and the inverse is then returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverse()
        if (!is.null(ix)) {
                message("getting cached inverse")
                return(ix)
        } 
        
        mat <- x$get()
        ix <- solve(mat)
        x$setinverse(ix)
        ix
}
