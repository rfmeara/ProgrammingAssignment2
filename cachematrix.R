# The first function creates a cache of the matrix argument in question.
# Comprised of set/get and inverse components to facilitate inversion.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
        x <<- y
        inver <<- NULL
  }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This component returns the inverse of the matrix output above
# Except if already computed, in which case it returns a cached version
# The two functions together can reduce "costly computation"
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
        message("getting cached data.")
        return(inver)
  }
        data <- x$get()
        inver <- solve(data)
        x$setinverse(inver)
        inver
}
