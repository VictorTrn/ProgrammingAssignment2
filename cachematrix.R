##This function creates a special "matrix" object that can cache its inverse.
## Provide the matrix x to monitor and cache
## Please refer to the example below for usage and output

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

cacheinv <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

## Example:
## z=matrix(c(1,0,3,4,5,0,0,8,9),nrow=3,ncol=3)
## solve(z)                                   ##for validation
## [,1]        [,2]        [,3]
## [1,]  0.3191489 -0.25531915  0.22695035
## [2,]  0.1702128  0.06382979 -0.05673759
## [3,] -0.1063830  0.08510638  0.03546099
## x=makeCacheMatrix(z)
## cacheinv(x)                                ## First Run
## [,1]        [,2]        [,3]
## [1,]  0.3191489 -0.25531915  0.22695035
## [2,]  0.1702128  0.06382979 -0.05673759
## [3,] -0.1063830  0.08510638  0.03546099
## cacheinv(x)
## getting cached data                        ## Second Run
## [,1]        [,2]        [,3]
## [1,]  0.3191489 -0.25531915  0.22695035
## [2,]  0.1702128  0.06382979 -0.05673759
## [3,] -0.1063830  0.08510638  0.03546099
