## The functions provide the capability to cache time-consuming matrix inversion computations


## This function creates a special matrix which is a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the value of the inverse

makeCacheMatrix <- function(x=matrix()){
    
    inv <- NULL   # inverse placeholder
    
    # methods
    set <- function(y){
        
        x <<- y
        inv <<- NULL        
    }
    
    get <- function() x
    
    setinv <- function(inv_arg) inv <<- inv_arg
    
    getinv <- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function calculate the inverse of the special "matrix" created with the function above.
## It does this by either calling "Solve" or simply returning the cached inverse, depending on
## whether or not the value of the matrix has changed since the inverse was last cached

cacheSolve <- function(x, ...){
    
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    inv
}

