### This is a project to understand and make use of the lexical scoping feature in R. This
### project involves caching of matrix information to cut down the repetitive workload handled
### by R. Cached information can be used to perform various operations without the need of
### recomputations.

## Function to create special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i = NULL ## Let i = inverse, set as Null initially, to hold inverse matrix later
        set = function(y){ ## define function to assign new matrix value in parent environment
            x <<- y
            i <<- NULL ## reset i to NULL when there is a new matrix
        }
        
        get = function() x  ## define function to return value of matrix argument
        
        setinverse = function(inverse) i <<- inverse ## assign value of i in parent environment
        getinverse = function() i  ## get value of i when called
        list(set = set, get = get, ## constructing a list to be called in later functions using $
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function to compute inverse matrix return by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i = x$getinverse() ## retrieve inverse matrix stored in makeCachematrix function
        if(!is.null(i)){ ## if inverse matrix is not NULL, returning cached matrix and message
            message("getting cached inverse matrix")
            return(i)
        }
        
        data = x$get() ## if inverse matrix is NULL, compute inverse matrix
        i = solve(data,...)
        x$setinverse(i) ## store computed inverse matrix in setinverse()
        i ## return computed inverse matrix
}

### Commands to verify 
Matrix1 = matrix(c(9,8,2,6),2,2) ## Construct matrices of any value and dimension here
Matrix1

cacheMatrix = makeCacheMatrix(Matrix1)
cacheMatrix$get() ## return Matrix1
cacheMatrix$getinverse() ## if Matrix1 is a new matrix, NULL will be returned, else cache results returned 

cacheSolve(cacheMatrix) ## solve for Matrix1
cacheSolve(cacheMatrix) ## to verify Matrix1 has been solved, returning message and cache results
