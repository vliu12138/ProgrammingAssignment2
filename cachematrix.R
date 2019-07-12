## Peer-graded assignment: Programming Assignment 
## 2: Lexical Scoping.
## The two functions below are to cache the inverse 
## of a matrix. As in the caching-the-mean-of-a-vector 
## example, if a matrix has already been inversed in the 
## previous calculations, the inversed matrix is found
## from the cache and directly returned back.
## Caching function allows to skip the computation. 
## Otherwise, it does calculations to get a result. 
## The functions aim to save time to avoid recalculations.

## The first (makeCacheMatrix) function sets a list 
## containing four functions: to set the value of a 
## matrix, to get the value of a matrix, to set the
## inverse of a matrix, and to get the inverse of a 
## matrix. Two variables "invrs" and "x" are in parent
## environment along with functions "set", "get", 
## "setinv", and "getinv".

 ## x is a matrix and the input for the function to run.
makeCacheMatrix <- function(x = matrix()) {

 ## "inv" is defined to be NULL first because in the initial
 ## conditions of the parent function (makeCacheMatrix), 
 ## an inverse matrix does not exist.
        inv <- NULL

 ## In function "set", a new matrix y substitutes x. The "<<-" 
 ## operator means that even within the parent function
 ## (makeCacheMatrix), y is assigned to x, and there is no 
 ## inverse matrix yet.
set <- function(y){
         x <<- y
         inv <<- NULL
}

 ## In function "get", matrix x is defined in the parent
 ## function. In this way, get() can retrieve the value of x. 
 get <- function() x

 ## In function "setinv", solve(x) is to solve for the inverse
 ## of matrix x. Even within the parent function, inv is defined to 
 ## be the inverse matrix, rather than NULL.
 setinv <- function() inv <<- solve(x)

 ## In function "getinv", inv is defined in the parent function to 
 ## retrieve the inverse matrix.
 getinv <- function() inv

 ## Eventually, a list of four basic functions are returned.
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second (cacheSolve) function is to skip the first time-consuming
## operation if the inverse matrix is discovered to be calculated
## before.

cacheSolve <- function(x, ...) {
        ## Make inv equal to the search value of getinv().      
        inv <- x$getinv()

        ## Check if that value is NULL. If it is not, it means that the 
        ## inverse matrix exists already. A message and that inverse matrix
        ## will be returned.
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }

        ## Set data equal to the input matrix.
        data <- x$get()

        ## Solve for the inverse of that matrix.
        inv <- solve(data, ...)

        ## Input the inverse matrix to setinv function.
        x$setinv(inv)

        ## Return that inverse matrix.
        inv
}

## Additional steps
## To test whether the functions work or not.
## To create a 2*2 matrix. Only square matrix can have
## inverse matrix.
test_matrix <- matrix(1:4, nrow = 2, ncol = 2)

## Set sleep as the parent function. Set test_matrix
## as the 'x' matrix.
sleep <- makeCacheMatrix()
sleep$set(test_matrix)

## Get the matrix displayed.
sleep$get()

## Calculate the inverse matrix of "x", and print it.
sleep$setinv()
sleep$getinv()
