## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL #Create empty matrix
  set <- function(y) {  
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinvmat <- function(inverse) invmat <<- inverse #Create inverted matrix
  getinvmat <- function() invmat
  list(set = set,
       get = get,
       setinverse = setinvmat,
       getinverse = getinvmat)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #Load the inverted matrix in cache 
  if (!is.null(i)) {
    message("getting cached data") #Show this message and inverted matrix when the function is called for the second time
    print(i)
  }
  data <- x$get() #Show the inverted matrix when the function is called for the first time and save the result in cache
  i <- solve(data, ...)
  x$setinverse(i)
  print(i)
}

#Check the function
m<-matrix(rnorm(16),4,4)
m1<-makeCacheMatrix(m)
cacheSolve(m1)

#Result
#Check the function
# m<-matrix(rnorm(16),4,4)
# m1<-makeCacheMatrix(m)
# cacheSolve(m1)
#          [,1]        [,2]       [,3]       [,4]
#[1,] -0.3202257  0.03544294 -1.1696060 -0.1656117
#[2,] -0.1234031 -0.40878727  0.2698694  0.5011316
#[3,]  0.9953018 -0.31156293  0.2963927 -0.2294162
#[4,] -0.5489850 -0.01907553 -0.2567108 -0.4099103
# cacheSolve(m1)
#getting cached data
#          [,1]        [,2]       [,3]       [,4]
#[1,] -0.3202257  0.03544294 -1.1696060 -0.1656117
#[2,] -0.1234031 -0.40878727  0.2698694  0.5011316
#[3,]  0.9953018 -0.31156293  0.2963927 -0.2294162
#[4,] -0.5489850 -0.01907553 -0.2567108 -0.4099103
#          [,1]        [,2]       [,3]       [,4]
#[1,] -0.3202257  0.03544294 -1.1696060 -0.1656117
#[2,] -0.1234031 -0.40878727  0.2698694  0.5011316
#[3,]  0.9953018 -0.31156293  0.2963927 -0.2294162
#[4,] -0.5489850 -0.01907553 -0.2567108 -0.4099103


