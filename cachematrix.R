## The first function, makeVector creates a special "vector", 
  # which is really a list containing a function to
  # 1.set the value of the vector
  # 2.get the value of the vector
  # 3.set the value of the mean
  # 4.get the value of the mean.
## x and m are two variables.
## x is an invertible matrix which is need to be operated.
## m is the stored matrix.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x<<-y
                m<<-NULL
        }
        get <- function () x
        setinverse <- function (inverse) m <<-inverse
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## x is a makeCacheMatrix.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'.
## x$getinverse is for checking if the inverse has already been calculated. 
  # If so, it gets the inverse from the cache and skips the computation. 
  # Otherwise, it calculates the inverse of the matrix x and sets the value of
  # the inverse matrix in the cache via the setinverse function.
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
