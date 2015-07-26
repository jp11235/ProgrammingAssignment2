## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#run prog assign #2 at the console
#> ls()
#[1] "cacheSolve"      "makeCacheMatrix"
#> a <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3))
#> a$get()
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    0    1    4
#[3,]    5    6    0
#> cacheSolve(a)
#     [,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1
#> cacheSolve(a)
#getting cached inverse
#     [,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1
