## makeCacheMatrix returns a list containing functions to do the following
## 1. set the value of a vector
## 2. get the value of a vector
## 3. set the calue of the inverse
## 4. get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}





## returns the cache inverse matrix if it has already been calculated,
## otherwise calculates the inverse of the matrix

cacheSolve <- function(x) {

    m <- x$getinv()
    if(!is.null(m)) {
      print("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    print(m)
  
}

a <- matrix(c(1,2,3,4),nrow=2,ncol=2)



cacheSolve(makeCacheMatrix(a))
