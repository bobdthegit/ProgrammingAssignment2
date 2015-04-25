## These two functions invert a matrix and cache the result.  
##  If cacheSolve is called a second time on the same matrix, 
##instead of rerunning solve it retrieves the cached inverse.

## makeCacheMatrix creates four functions that will become available
## along with the original matrix  in the calling environment (which will be cacheSolve).  



makeCacheMatrix <-function(x = matrix()) {
  invMatClone <- NULL
    set <- function(y){   #not used
      x <<- y
      invMatClone <<- NULL
    }
    get <- function() x
    setInv <- function(foo) invMatClone <<- foo 
    ##passes its argument (in cacheSolve)to invMatClone in this environment
    
    getInv<-function() invMatClone
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}
cacheSolve<-function(x, ...) {
  ## Return a matrix that is the inverse of the matrix in "x"'
  invMat<-x$getInv()  #refers to the getInv fn that is in the list x and gets invMat from there
  if(!is.null(invMat)){
    message("getting cached data")
    return(invMat)    
  }
  data <- x$get()  #No inverse yet, so get  the original matrix.
  
  invMat <- solve(data,...)
  x$setInv(invMat)
  invMat
}