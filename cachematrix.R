#The assignment requires make two functions. First function is 'makeCacheMatrics.'
#According to the description of Coursera, 'makeCacheMatrix' is a fuction which can cache its inverse. 
#So, I made a function used to cache the result, then made the other function for computing the inverse through the solve function. 

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