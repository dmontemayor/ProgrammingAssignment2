## A special matrix that can cache the value of its inverse
## for future use, provided the matrix has not been changed.

## creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## init inverse as null
    inv <- NULL
    ## set will check if input is good and new before reinit inverse
    set <- function(y){
      ## check input is a matrix
      if(!is.matrix(y)){
      message("input is not a matrix, try again")
      return(NA)
      }
      ## check input is square
      if(nrow(y)!=ncol(y)){
      message("input is not square, try again")
      return(NA)
      }
      ## set only if y is a new value of x
      result <- tryCatch(
        {
          ## y is a new if any value is neq to x
          if(any(y!=x)){
            message("edited value: nullifying inverse")
            inv<<-NULL
          }else{
          message("equivalent value: preserving inverse")
          }
        },error = function(e){
          ## error would mean x is NA or y not same size as x
          message("new value: nullifying inverse")
          inv<<-NULL
        }
      )
    x <<- y
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Calculates the inverse of the special matrix created by
## makeCacheMatrix(). Will first check if the matrix is new
## and that the inverse hasn't already been calculated. If so
## calculation is avoided and the cached inverse is returned.
## Otherwise, the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get current value of inverse
        inv <- x$getinv()
        ## recalculate inverse if it is NULL
        if(!is.null(inv)){
          message("getting cached inverse")
          return(inv)
        }
        message("calculating inverse")
        inv <- solve(x$get(), ...)
        x$setinv(inv)
        inv
}
