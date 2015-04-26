## The following two functions resolve the on hands problem of caching and computing the inverse of a given matrix.

## This function cache the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      
        inv = NULL
        setmtx = function(y) {
                # use `<<-` to assign a value to an object in an environment which is different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        getmtx = function() x # get the matrix


        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(setmtx=setmtx, getmtx=getmtx, setinv=setinv, getinv=getinv)

}


## The following function computes the cached matrix
## If the inverse has already been calculated (and the matrix has not changed), then
## function should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv = x$getinv()
        
        # if the inverse is calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting from the cached data")
                return(inv)
        }
        
        # else the inverse calculation 
        mat.data = x$getmtx()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}