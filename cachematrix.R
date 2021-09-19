## Put comments here that give an overall description of what your
## functions do
## These functions are used to take a matrix and calculate the inverse of that matrix if that is not available. 
## And store that value into solvematrix. 
## If inverse is at ready to take, can just get from cache data. 

## Write a short comment describing this function
## Create special object matrix to store cache inverse
makeCacheMatrix <- function(x = matrix()) {
	
	## initialized inverse i
	i <- NULL
	
	##Set the matrix and reset inverse to NULL 
	set <- function(y) {

		x<<-y
		i <<- NULL
	}
	
	##Get the matrix if available
	get<-function()x

	## set the inverse after calculated
	setinverse<-function(solvematrix) i <<-solvematrix

	## get the inverse if it is here 
	getinverse<- function() i 

	##list of function to use
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Write a short comment describing this function

## function to calculate inverse of a matrix if it is not in cache data
## otherwise get inverse from cache 
cacheSolve <- function(x, ...) {
       
	## Return a matrix that is the inverse of 'x'
	i <-x$getinverse()
	
	## return inv without calculating since it is already set
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}

	## get the matrix from the object matrix 
	data <- x$get()
	
	## calc inverse 
	i<-solve(data,...)
	
	## store new inverse to cache
	x$setinverse(i)

	## return inv
	i	
}
