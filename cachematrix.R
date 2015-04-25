## makeCacheMatrix takes in a square invertible matrix as a parameter,
## and returns a list containing the functions set(), get(), setInverse(),
## and getInverse().

## set() is a function that takes in a matrix and can override the
## the matrix x is set to. 

## get() is a function that returns x.

## setInverse() takes in an inverse matrix and stores it in m. 

## getInverse() returns m.
## Then a list is returned that stores the four functions. 

makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
	set <- function(y = matrix())
	{
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseMatrix) m <<- inverseMatrix
	getInverse <- function() m
	list(set=set, get=get, setInverse=setInverse,
		getInverse=getInverse)
}

## cacheSolve takes in x which is the list returned from makeCacheMatrix().

## m calls the getInverse(), then an if statement checks to see if 
## m is null, if m is not null then the matrix stored in m  
## it is returned.

## The matix variable is equal to what is returned by get().
## Then the m is set to the inverse of the matrix variable. 

## Then the inverse matrix m is set, and returned at the end of 
## the function.  

cacheSolve <- function(x, ...) 
{
	m <- x$getInverse()
	if(!is.null(m))
	{
		message("getting cached matrix")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setInverse(m)
	m
}
