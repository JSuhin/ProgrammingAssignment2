## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a=matrix()){
        inv <- NULL
        set <- function(b){
                a <<- b
                inv <<- NULL
        }
        get <- function() a
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list( get = get,
              setInv = setInv,
              getInv = getInv)
}

## Write a short comment describing this function

cacheSolve <- function(a, ...){
        inv = a$getInv()
        if(!is.null(inv)){
                message('getting cached inverse')
                return(inv)
        }
        data <- a$get()
        inv <- solve(data)
        a$setInv(inv)
        inv
}
