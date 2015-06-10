#The functions set out below will allow us to cache the result of the 'solve' function on a matrix, which calculates
#the inverse of it. The makeCacheMatrix function creates several functions which allow the inverse to be stored and retrieved for later use.
#The cacheSolve function tries to retrieve a cached version of the inverse. If there isn't one already created then it will
#calculate it manually and store it. This ensures that the calculation is only ever done once and therefore saves time and computational
#power.

#A closure that returns a list of functions which can cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
	#This variable will store the inverse of the matrix.
	i <- NULL
	
	#This function can be used to set the matrix. Each time this is called, the inverse
	#(stored in the 'i' variable) will be reset to NULL.
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	#This function simply returns the matrix we are using.
	get <- function(){ return(x) }
	
	#This function will set the 'i' variable to the 'inverse' argument provided.
	setinverse <- function(inverse){ return(i <<- inverse) }
	
	#This function simply returns the stored inverse of the matrix. This could be NULL if it has not been pre-calculated.
	getinverse <- function(){ return(i) }
	
	#return a list of the functions created above.
	return(
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	)
}

#A function to get the inverse of a matrix. If it has already been calculated it will return the cached value.
#If not, then it will calculate the inverse and cache it for later use. Therefore subsequent calls to this function
#will return the cached value and not have to manually recalculate it.
cacheSolve <- function(x, ...){
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

#Create a 2x2 matrix containing the values 1,2,3,4.
my.matrix = matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
#Create a makeCacheMatrix list and pass in our matrix
my.make = makeCacheMatrix(my.matrix)
#Get the inverse of the matrix - this will actually calculate it 'from scratch'.
cacheSolve(my.make)
#Get the inverse of the matrix again - this time the function will retrieve it from the cache so it will
#not be calculated manually again.
cacheSolve(my.make)
