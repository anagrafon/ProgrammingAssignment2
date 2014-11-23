## 
## Matrix inversion is usually a costly computation. If the contents of the matrix
## are not changing, it may make sense to cache the value of its inverse so that 
## when we need it again, it can be looked up in the cache rather than recomputed. 
## 
## The following two functions allows us to cache the inverse of a matrix rather 
## than compute it repeatedly.
## 


## 
## This function creates a special "matrix" object that can cache its inverse. Ex:
##      matrix = makeCacheMatrix()
## 
## After creating the object you can set the contents of the matrix using 
## funtion set (it gets a matrix as argument). Ex: 
##      matrix$set(matrix(1:4, 2, 2))
## 
## Then you can compute 
## 
makeCacheMatrix <- function(x = matrix()) {
    ## local variable 'inverse'
    inverse <- NULL
    
    ## function set: sets the contents of the matrix and assigns NULL to 'inverse' so that 
    ## the inverse will be recomputed if the contents of the matrix change
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    
    ## function get: gets the contents of the matrix
    get <- function() m
    
    ## function setinverse: sets the inverse
    setinverse <- function(im) inverse <<- im
    
    ## function getinverse: gets the inverse
    getinverse <- function() inverse
    
    ## makeCacheMatrix returns a list of for elements (functions set, get, setinverse and getinverse)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 
## This function computes the inverse of the special matrix 'x' created using the function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve 
## retrieves the inverse from the cache.
## 
cacheSolve <- function(x, ...) {
    ## 'inv' contains the inverse of 'x' returned by the function 'getinverse'      
    inv <- x$getinverse()
    
    ## if 'inv' is not empty, it is returned without computing the inverse
    if(!is.null(inv)) {
        message("getting cached data")
        
        ## cacheSolve returns the inverse of the matrix (cached, not recomputed)
        return(inv)
    }
    
    ## if 'inv' is empty, then the inverse has to be computed insted of cached:
    ##
    ## the matrix contained in 'x' is obtained using the function 'get', and it is
    ## saved into 'data'
    data <- x$get()
    ##
    ## the inverse of 'data' is computed using the function 'solve'
    result <- solve(data, ...)
    ##    
    ## the inverse is saved in 'x' using the function 'setinverse'
    x$setinverse(result)

    ## cacheSolve returns the inverse of the matrix which has just been computed
    result
}
