## Put comments here that give an overall description of what your
## functions do

## the makecacheMatrix recieves a numeric vector and caches the vector 
## and inverse of the vector in a list of functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) m <<- Inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## isInversible checks whether the matrix is an inversable matrix
## since this is doing the solve 
isInversible <- function(m) {
  d<-try(solve(m),silent=T)
  if (class(d) == "matrix") {
    return (d)
  } else {
    return (NULL)
  }
}


## cacheSolve is a function that recieves a cahcMatrix (produced from the make chace matrix function)
## and returns the inverted matrix if it is inversible otherwise it will print a warning 
## and return NULL
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if (!is.null(x$getInverse())) {
        m <- x$getInverse()
        print("returning cached value")
        return (m)
    } else{
        m <- isInversible(x$get())
        if (!is.null(m)) {
          x$setInverse(m)
          print("returning new value")
          return(m)
        } else {
          warning("The matrix is not Inversable")
          return(NULL)
        }
    }
    invisible(NULL)
  }


# ma <- matrix(c(-1,-1,-1,1),2,2)
# mc <- makeCacheMatrix(ma)
# cacheSolve(mc)
# cacheSolve(mc)
#  
#  ma <- matrix(c(-1,-1,1,1),2,2)
#  mc <- makeCacheMatrix(ma)
#  cacheSolve(mc)
#  cacheSolve(mc)