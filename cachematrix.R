## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix that can cache(store) its inverse. 1st created a function which takes a matrix, "m" is a variable
## that is assigned "NULL". Then setting them into the parent environment ussing set function. Defining other functions as setinverse(assigns the value into 
## the parent environment), get and getinverse and storing them in a list so that they can be accessed later on.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y){ 
                x <<- y 
                m <<- NULL 
        }
        
        get <- function () x 
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function () m 
        list (set = set, get = get, setinverse= setinverse, getinverse = getinverse) 
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the matrix using solve function. We use !is.null to check if the inverse 
## has already been calculated,if yes then the cachesolve will retrieve the inverse from the cache where it is stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

