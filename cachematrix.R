## Inverting a large matrix is a time-consuming computation.
## The caching in the 2 functions below is done to overcome that.
## If the contents of a matrix is not changing, it makes sense to cache the 
## value of the inverted matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 


## makeCacheMatrix creates a special "matrix" object (actually a list) that 
## can cache its inverse. As input it receives the original real squared matrix 
## for which the inverse has to be calculated.

## example (test with large matrix): 
##  > matrixInput <- matrix(runif(4000000, min = 10, max =50), nrow =2000)
##  > matrixCache <- makeCacheMatrix(matrixInput)

makeCacheMatrix <- function(x = matrix()) {
    # whilst x is the orginal matrix, m is the inversed
    m <- NULL
    
    # set and get the original matrix
    # set is optional, you can pass the matrix directly as argument to 
    # the main makeCacheMatrix() function
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    # set and get the inversed matrix (from cacheSolve())
    setInversed <- function(solved_matrix) m <<- solved_matrix
    getInversed <- function() m
    
    # return the list
    list( set = set, get = get,
         setInversed = setInversed,
         getInversed = getInversed )
    
}


## cacheSolve calculates the inverse (standard solve) of the special "matrix" 
## created with makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets that value 
## in the cache via the setInversed function.

## example (continued)
##  > invertedMatrix <- cacheSolve(matrixCache)
## note above takes several seconds to compute, running again is immediate
##  > invertedMatrix2 <- cacheSolve(matrixCache)
##  getting cached data
##  > identical(invertedMatrix, invertedMatrix2)
##  [1] TRUE

cacheSolve <- function(x, ...) {
    # check if cached result is available
    m <- x$getInversed()
    if( !is.null(m) ){
        message("getting cached data")
        # immediately returned cached result and quit
        return(m)
    }
    matrix <- x$get()
    # first time, calculate inverse
    m <- solve(matrix, ...)
    # store inverse in makeCacheMatrix ojbect for next time
    x$setInversed(m)
    
    m
    
}
