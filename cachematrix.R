
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    #get is a function that returns the matrix x stored in the makeCacheMatrix function
    get <- function() x
    #set is a function that changes the matrix x stored in the makeCacheMatrix function
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    #setinvm simply store the value of the input in a variable invm. It doesn't calculate the inverse of x
    setinvm <- function(newinvm) invm <<- newinvm
    #getinvm simply return the value of the input in a variable invm. It doesn't calculate the inverse of x
    getinvm <- function() invm
    #To store the 4 functions in the function makeCacheMatrix, we need the function list(), 
    #so that when we assign makeVector to an object, the object has all the 4 functions.
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    #The first thing cacheSolve does is to verify the value invm, stored previously with getmean, exists and is not NULL.
    #If it exists in memory, it simply returns a message and the value invm
    invm <- x$getinvm()
        if(!is.null(invm)) {
            message("getting cached data")
            return(invm)
        }
        #If not following code executed
        ## Return a matrix that is the inverse of 'x'
        #data gets the vector stored with makeVector
        data <- x$get()
        #invm calculates the inverse matrix of the x 
        invm <- solve(data, ...)
        #x$setinvm(invm) stores it in the object generated assigned with makeCacheMatrix
        x$setinvm(invm)
        invm

}