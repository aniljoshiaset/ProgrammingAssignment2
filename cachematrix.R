##makeCacheMatrix - This function takes the input as an inertible matrix and returns a list that has components as functions that can be used to get or set input matrix or the inverse of the matrix after computation
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 
  set <- function(y) #Values from the input i.e matrix is set in this function and the inv is set as null
    {
    x <<- y
    inv <<- NULL #Default value of inv is set to Null so that it could be checked if it is present in the cache or not
    }
get <- function() x #Values are fetched using this function
  setinverse <- function(inverse) inv <<- inverse #Function used to set the inverse of the matrix obtained after computation
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #All the functions are populated as the elements of list

}

##cacheSolve - This function takes the input a list which is the output of makeCacheMatrix function and returns the inverse of the matrix(which is the input of makeCacheMatrix)provided it is invertible 
cacheSolve <- function(x, ...) {
        inv<-x$getinverse() #Value of inv is fetched from the cache in order to check if it is null or not
  if(!is.null(inv)) #For any new new matrix inv will always be null
    {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get() #Using the get function which is an element of list input matrix is returned
  inv <- solve(data) #Solve is used to compute inverse
  x$setinverse(inv) #Computed Inverse of the given matrix is set to inv
  inv #inv is returned from the function cacheSolve
}