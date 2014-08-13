## two functions that return inverse of a matrix from a cache or calculate it.


## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x=matrix())
{
    ## inverse property initialization
    inv <- NULL

    ## set method to set the matrix
    set <- function(matrix)
    {
        mtrix <<- matrix
        inv <<- NULL
    }

    ## get method to get the matrix
    get <- function()
    {
        ## return the matrix
        mtrix
    }

    ## set inverse method to set inverse of the matrix
    setInverse <- function(inverse)
    {
        inv <<- inverse
    }

    ## get inverse method to get inverse of the matrix
    getInverse <- function()
    {
        ## return the inverse property
        inv
    }

    ## return a list of the methods
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function compute the inverse of the matrix returned by the function "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not changed)
## then the inverse retrieved from the cache other wise it calculate it.
cacheSolve <- function(x, ...)
{
    ## return a matrix that is the inverse of 'x'
    mtrix <- x$getInverse()

    ## return the inverse if its already in cache
    if(!is.null(mtrix))
    {
        message("getting data from cached memory")
        return(mtrix)
    }

    ## get the matrix from our object
    data <- x$get()

    ## calculate inverse using matrix
    mtrix <- solve(data) %*% data

    ## set the inverse to the object
    x$setInverse(mtrix)

    ## return the matrix
    mtrix
}
