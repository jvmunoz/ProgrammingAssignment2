## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly
## Here we can find a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## m_1 will be the inverse matrix
        m_1<-NULL
        
        ## Set the matrix
        set <- function(y) {
                x <<- y
                m_1 <<- NULL
        }
        
        ## Get the matrix
        get <- function() x
        
        ## Set the inverse
        setm_1 <- function(inverse) m_1 <<- inverse
        ## Get the inverse
        getm_1 <- function() m_1
        
        list(set = set, get = get, setm_1 = setm_1, getm_1 = getm_1)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        m_1 <- x$getm_1()
        
        ## Have we got inverse matrix computed?
        
        if (!is.null(m_1)) {
                message("The matrix is already in the cache")
                return(m_1)
        }
        
        ## else inverse matrix is not computed yet. We solve it
        
        m <- x$get()
        m_1 <- solve(m, ...)
        
        ## Set the cache for inverse matrix
        
        x$setm_1(m_1)
        
        m_1
        
}
