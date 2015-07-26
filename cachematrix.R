## Learning more about lexical scoping and closures.
## Good reference: https://en.wikipedia.org/wiki/Closure_(computer_programming)

#### "makeCacheMatrix" is a higher order function that retruns a closure: "function(s) + environment"
##The returned closure (e.g. "a") is a list of 4 functions and their environment:
#   set = set the value of the matrix
#   get = get the value of the matrix
#   setinverse = set the value of the inverse
#   getinverse = get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {     #x is a formal parameter
  m <- NULL                                     #m is a local variable
  #browser()
  set <- function(y) {
    x <<- y              #y is a formal parameter
    m <<- NULL           
  }
  get <- function() {
    x                    #x is a free variable, what is 'x' in the enviroment where 'get' was defined? => x = matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
  }
  setinverse <- function(inverse) {
    m <<- inverse        #this allows us to change the value of "m" in the closure's environment
  }
  getinverse <- function() {
    m                     #m is a free variable, what is 'm' in the enviroment where 'getinverse' was defined? => m = NULL
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#### "cacheSolve" accepts a clousre (e.g. "a") and returns the inverse of the matrix stored in the closure
## return one of two values "m"
# first time it is invoked: calculate the inverse and return the calculated inverse
# second time it is invoked: get the cached inverse from the closure (e.g. "a") and return the cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()            #if this is the first time, then getinverse will return NULL (see above), else the cached inverse
  browser()
  if(!is.null(m)) {                    #execute this block if inverse has already been found
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()                      #get the matrix from the closure
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

####EXAMPLE of runing prog assign #2 at the RStudio console
# > rm(list=ls())
# > source("cachematrix.R")
# > ls()
# [1] "cacheSolve"      "makeCacheMatrix"
# > a <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3))
# > a$get()
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# a$getinverse()
#     NULL                 #it is NULL the first time since we have not calculated the inverse yet
# > cacheSolve(a)
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# a$getinverse()        
#      [,1] [,2] [,3]    #now we can get the inverse
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > cacheSolve(a)
# getting cached inverse
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

## Examine the enviroment of the closure at the end of the example
# ls(environment(a$set))
# [1] "get"        "getinverse" "m"          "set"        "setinverse" "x"         
# > get("m", environment(a$set))
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > get("x", environment(a$set))
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# > get("get", environment(a$set))
# function() {
#   x                    #x is a free variab...
# }
# <environment: 0x0000000016f9bb18>
# > get("set", environment(a$set))
# function(y) {
#   x <<- y              #y is a formal parameter
#   m <<- NULL           
# }
# <environment: 0x0000000016f9bb18>
# > get("setinverse", environment(a$set))
# function(inverse) {
#   m <<- inverse
# }
# <environment: 0x0000000016f9bb18>
# > get("getinverse", environment(a$set))
# function() {
#   m                     #m is a free variabl...
# }
# <environment: 0x0000000016f9bb18>