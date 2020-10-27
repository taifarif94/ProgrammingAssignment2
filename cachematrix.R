## Put comments here that give an overall description of what your
## functions do:

## These functions takes advantage of R scoping rules (and the <<- operator) to save on computing the inverse of a 
# matrix if its inverse has already been calculated.



## Write a short comment describing this function

## The makeCacheMatrix function creates a 'matrix' object that is a list of four functions. Set function sets the 
## value of the matrix in the cache, the get function returns the value of the matrix which
## was set using the get function. The setinverse function save the value of the calculated inverse function in cache
## and the get function returns the value of the inverse of the calculated matrix.

#This function takes a matrix as an input and sets the value of the object to it. 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    k<<-y
    i<<- NULL
  }
  get <- function() k
  setinverse<-function(inverse) i<<-inverse
  getinverse <-function() i
  list(set = set(x), get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## This function takes the value of the matrix the inverse of which is to be calculated. If its inverse already exists, 
## the inverse is not calculated, instead its value is returned from the cache.

cacheSolve <- function(x, ...) {
  
  i<- x$getinverse()
  data1<-x$get()
  
  if((identical(data1,k)) & !is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  j<-solve(data1)
  x$setinverse(j)
  j
        ## Return a matrix that is the inverse of 'x'
}
