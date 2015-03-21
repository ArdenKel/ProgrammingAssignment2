## This thing possibly solves the Programming Assignment, making a matrix able to cache its inverse



##This function makes a matrix capable of storing its inverse, together with methods 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <-function(y) {
                x <<- y
                inv <<- NULL
        }
        get <-function() x 
        setinv <-function (inver) inv<<-inver
        getinv <- function() inv
        list(set = set, get=get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks whether the argument x created by makeCacheMatrix has already been inverted. If so, it retrieves the cached inverse matrix. Otherwise it applies solve to invert the matrix. Then it stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
