## First function create matrix and allow it to be put in cache
## Second function use first function to calculate inverse or get it from cache 
## for testing :
##  mtr<-makeCacheMatrix(matrix(rnorm(4000000),2000,2000))
##  mt1<-cacheSolve(mtr) -- execute this 2 times to see difference in time

## Function create a matrix and allow its value to be stored in cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
    {
    x<<-y
    m<<-NULL
    }
  get<-function() x
  setinv<-function(inverse) m<<- inverse
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)

}


## Accept matrix created from previous function and calculate inverse or take from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data") #if matrix is in cache take from it
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinv(m)
  m
}
