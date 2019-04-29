## Github/caroljmcilwain/ProgrammingAssignment2
## Assignment 2 for Week 3 R Programming - Compute the inverse of a square matrix using solve R. 
## If x is a square invertible matrix, then solve (x) returns its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {       ## define the argument
  inv<-NULL                                       ## initialize inverse as NULL
  set<-function(y){                               ## define the set function
    x<<-y                                         ## value of matrix in parent environment
    inv<<-NULL                                    ## if there is new matrix, reset inv to NULL
}
  get<-function()x                                ## define the get function
  setinverse<-function(inverse)inv,,-inverse      ## assigns value if inv in parent environment
  get inverse<-function ()inv                     ## gets the value of inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    ## need this for function with $ operator
}

## This function computes the inverse of special matrix above
## If the inverse has been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       inv<-x$getinverse()                        
       if(!is.null(inv)){                         
        message("get cache data"
       return(inv)}
       data<-x$get()
       inv<-solve(data,...)
       x$setinverse(inv)
       inv
}





