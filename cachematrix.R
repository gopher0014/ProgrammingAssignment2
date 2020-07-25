## This is a set of functions, that when used in tandem with each other will use cacheSolve to calculate the inverse of a matrix,
        ## after checking to see if a cached solution is available from a previous run of cacheSolve

## This creates a list of named functions that cacheSolve will use to check for cached results or calculate a new result for the inverse of "inv"
        ## First inv is set to null in the global environment
        ## set section assigns inv as NULL in the global environment / clears cache
        ## get will go get x, which would be a matrix to be assigned 
        ## setinverse will be the setter for inv, so cached result can be saved
        ## getinverse will be the getter for inv, so inv can be returned from the global invironment, and checked "if not null" later on in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}


## After creating makeCacheMatrix, entering something like a1 <- makeCacheMatrix(some_matrix) will allow cacheSolve to work as follows:
    ## cacheSolve(a1) on the first run will execute solve function and calculate + return the inverse of some_matrix, because when getinverse runs, inv was null
                ## calculation first uses get to grab the some_matrix data, then solves for the inverse
                ## it will also set inv in the global environment to the result 
    ## cacheSolve(a2) on the second run will recognize inv in global as not null, and instead return inv from the chache along with the message "getting that inverse"
                ## no calculation of setinverse is needed this run

cacheSolve <- function(x,...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting that cached inverse")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
