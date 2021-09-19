## Put comments here that give an overall description of what your
## functions do
## function to take a matrix and calculate the inverse of that matrix if that is not available. And store that value into solvematrix. If inverse is at ready to take, can just get from cache data. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {

		x<<-y
		i <<- NULL
	}

	get<-function()x
	setinverse<-function(solvematrix) i <<-solvematrix
	getinverse<- function() i 
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <-x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i<-solve(data,...)
	x$setinverse(i)
	i	
}
