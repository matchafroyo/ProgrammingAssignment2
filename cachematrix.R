## This program caches results for time consuming matrix inversion calculation

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix=function(x = matrix()) {
    
    ##resets inverse matrix to null everytime makeCacheMatrix is called
    i=NULL
    
    ##create set function to set or take the value of the input matrix
    set=function(y){
        
        ##superasggiment value store, saves input matrix
        x <<- y
        ##resets inverse matrix to NULL
        i <<- NULL
    }
    
    ##get value of matrix,returns original matrix
    get=function() {x}
    
    ##set value of inverse of matrix, first call of cacheSolve()
    setinverse=function(solve) {i<<-solve}
    
    ##get value of inverse of matrix, subsequent calls of cacheSolve()
    getinverse=function() i
    
    ##return list, for external access
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix from makeCacheMatrix.  If
## the inverse has been calculated and matrix is the same, return cached value.
## Return a matrix that is the inverse of 'x'
cacheSolve=function(x, ...) {
    
    ##assign inverse matrix of x as i
    i=x$getinverse()
    
    ##check if inverse matrix already has cached value, if yes, return cache
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    ##if not cached value, calculate inverse matrix
    ##store matrix to be inverted in data
    data=x$get()
    ##invert matrix, store in i
    i=solve(data)
    ##set i as cached matrix
    x$setinverse(i)
    ##return caculated inverse
    i
    
}
