## This program will create the inverse of a matrix. It assumes that the matrix is invertible. 
## The result is store in a make cache Matrix
## functions set and get the matrix and its inverse. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  Minverse<-NULL
  set<-function(y)
    {
        x<<-y
        Minverse<<-NULL
    }
   
  get<-function()
  {
    x
  }
  setinverse<-function(inverse)
  {
    Minverse<<-inverse
  }
  getinverse<-function()
  {
    Minverse
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function will check if the inverse of the matrix aready exists 
## If the inverse doesn't exist, it calculates the inverse. Assumes that the matrix is invertible 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Minverse<-x$getinverse()
  if(!is.null(Minverse))
  {
    message("getting cached data")
    return(Minverse)
  }
  data<-x$get()
  Minverse<-solve(data, ...)
  x$setinverse(Minverse)
  Minverse
}

