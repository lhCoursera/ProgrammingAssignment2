## Functions to cache a matrix and its inverse and calculate the inverse
## of a matrix, using the cached version if available

## makeCacheMatrix creates a list containing functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## no inverse when list first created
    set <- function(y){ ## function to set matrix
        x <<- y ## matrix cached
        i <<- NULL ## matrix changed so any ev. former cache value for inverse is wrong
    }
    get <- function() x ## function to get matrix
    setinverse <- function(inverse) i <<- inverse ## inverse of matrix cached
    getinverse <- function() i ## function to get inverse
    
    ## put functions in list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse matrix of the matrix object in x by first looking 
## to see if a cached version of the inverse (!=null) exists in x and otherwise uses 
## solve to calculate it and caches the result

cacheSolve <- function(x, ...) {
    ## get value of cached inverse in x
    inv <- x$getinverse()
    ## check if null
    if(is.null(inv)){
        ## calculate inverse of matrix and cache it in x
        inv <- solve(x$get())
        x$setinverse(inv)
        ## print message about caching inverse
        print("No cached inverse was found - inverse calculated and cached now.")
    }
    inv
}

