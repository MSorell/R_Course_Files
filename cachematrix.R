
## Coursera R Course Week 3 Programming Assingment
## M Sorell, Rev 1   030620

## Make Cache Matrix Portion, swap out mean to inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,getinverse = getinverse)
}

## Make Cache Solve Portion, use solve matrix function to get invesrse
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test code portion, 4 examples
## Example 1, 2 by 2
B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
B
cacheSolve(B1)

## Example 2, 2 by 2
C <- matrix(c(4,2,7,6),2,2)
C

C1 <- makeCacheMatrix(C)
cacheSolve(C1)

## Example 3, no inverse, throws Error message of singular matrix
D <- matrix(c(3,6,4,8),2,2)
D

D1 <- makeCacheMatrix(D)
cacheSolve(D1)

## Example 4, 3 by 3
E <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
E

E1 <- makeCacheMatrix(E)
cacheSolve(E1)

