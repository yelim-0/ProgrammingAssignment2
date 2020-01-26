#The assignment requires make two functions. First function is 'makeCacheMatrics.'
#According to the description of Coursera, 'makeCacheMatrix' is a fuction which can cache its inverse. 
#So, I made a function used to cache a newly inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set,get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#The other function is 'cacheSolve' function. Through this, can find out the solution from cached data or invert the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    mat <- x$get()
    inver <- solve(mat, ...)
    x$setinverse(inver)
    inver
}