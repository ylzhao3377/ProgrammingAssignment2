## makeCacheMatrix is a function to build a special vector which actually is a list of functions
## set is a function to set value of the matrix
## get is a function to get value of the matrix
## setsolve is a function to give the value of inverse to 'm'.
## getsolve is a function to get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) m <<- inverse
        getsolve <- function() m
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.
## if we want to modify the matrix, we only need to call $set() rather than run makeCacheMatrix again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m        
}

## The way to test 2 different matrices
B=matrix(1:4,nrow=2)
vec1<-makeCacheMatrix(B)
cachesolve(vec1)

D=matrix(4:7,nrow=2)
vec1$set(D) # change matrix by calling vec1$set()
cachesolve(vec1)
