## Function to make a cache matrix
## introduces the <<- operator
### more information avaiable in
### https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html
###

### Below are two functions that are used to create a special object that
### stores a numeric matrix and cache's its inverse.
### the first function makeCacheMatrix creates a special "matrix", which is
### reallly a list containning a funftion to
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the inverse of the matrix
### 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function (y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<- function(inverse) m<<- inverse
  getinv<- function() m
  list(set=set,get=get,setinv=setinv, getinv=getinv)
  # names given so that subsetting by names will be possible in next function.
}


## The following function calculates the inverse of the special "matrix" created above.
## Checking first to se if the inverse has already be computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise the inverse of the matrix is computed via setinv function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  return(m)
}

k=rnorm(10000)

mat_teste<-matrix(k, nrow=100,ncol=100)
tested_inv<-makeCacheMatrix(mat_teste)
