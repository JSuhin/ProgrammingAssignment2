## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a=matrix()){
        inv <- NULL
        
        ## "set" is a function which can be used later to
        ##  make a new matrix without calling "makeCacheMatrix" again
        set <- function(b){
                a <<- b
                inv <<- NULL
        }
        
        ## Function "get" saves our matrix A
        get <- function() a 
        
        ## "setInv" calculates invers of the matrix A
        setInv <- function(solve) inv <<- solve
        
        ## "getInv" checks is invers od the matrix A already exists
        getInv <- function() inv
        
        ## Output is list "a" that contains
        ## functions get, set, getInv, setInv
        list( get = get,
              setInv = setInv,
              getInv = getInv)
}

## Write a short comment describing this function
## Function "cacheSolve" takes output from function "makeCacheMatrix"
## as an input
cacheSolve <- function(a, ...){

        inv <- a$getInv() ## gets invers matrix if she exists
                          ## if invers matrix does not exist than inv = NULL
        
        ## Checks if invers matrix already exists
        if(!is.null(inv)){
                ## If invers matrix already exists the condition is.null(inv) will be FALSE.
                ## Because of the negation "!" the test for "if" function will be TRUE
                ## and the "if" loop will be exicuted and the message will be writen in
                ## the console and invers matrix will be returned
                message('getting cached inverse')
                return(inv)
        }
        
        data <- a$get()         ## our matrix is assign to variable "data"
        inv <- solve(data)      ## calculates invers matrix
        a$setInv(inv)
        inv
}
