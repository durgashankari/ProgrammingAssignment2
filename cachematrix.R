### Function Descriptions ###
## makeCacheMatrix - function creates a matrix and sets inverse calculation in cache
## cacheSolve - function computes inverse of matrix
### Example Usage ###
# source("cachematrix.R")   #Source the file
# X1 <- makeCacheMatrix(matrix(1:4, 2))   #set a matrix   
# class(matrix1)    #check and confirm the class of X1 as list
# X1$get()    #get the created matrix details
# X1$getInverse()   #checked the Inverse vaule is NULL
# X1$set(matrix(5:8,2))   #set a new matrix using set function
# X1$get()    #check if new matrix was set through set function
# cacheSolve(X1)    #get the inverse of matrix and set the cache
# cacheSolve(X1)    #check the inverse of matrix and inverse is being used from cache
# X1$getInverse()   #check the inverse has been set in cache


## function makeCacheMatrix creates a special matrix object and can cache its inverse
## and returns a list of functions

makeCacheMatrix <- function(x = matrix()) 
{
  # assigning inverseMatrix as NULL value
  inverseMatrix <- NULL
  # get function to get Matrix
  get <- function() 
	{
    		x
  	}
  # set function to set a new Matrix
  set <- function(newMatrix) 
	{
    		x <<- newMatrix
    		inverseMatrix <<- NULL
  	}
  # getInverse to get InverseMatrix
  getInverse <- function() 
	{
    		inverseMatrix
  	}
  # setInverse with Inverse Matrix solution
  setInverse <- function(solvedMatrix) 
	{
    		inverseMatrix <<- solvedMatrix
  	}
  #return a list of functions
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## function cacheSolve computes the inverse of the special "matrix" created by 
## function makeCacheMatrix above

cacheSolve <- function(x, ...) 
	{
  		# querying and asssigning inverseMatrix cached value
  		inverseMatrix <- x$getInverse()
  
  		# checking value of cache and using it if not NULL
  		if(!is.null(inverseMatrix)) 
		{
    			message("getting cached Inverse Matrix data ....")
    			return(inverseMatrix)
  		}
  	# gettting matrix and calculating inverse
  	data <- x$get()
  	inverseMatrix <- solve(data, ...)
  	# set value on inverse matrix in cache
  	x$setInverse(inverseMatrix)
  	## Return a matrix that is the inverse of 'x'
  	return(inverseMatrix)
	}
