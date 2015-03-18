## The function makeCacheMatrix creates a special "matrix", which is really 
##a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	a <-NULL 
## this is where the result of inversion is stored
	set <-function(y){ 
##set a matrix to object created by makeCacheMatrix function
	x <<-y
	a <<-NULL }		
	get <-function()x   
## return the input matrix		
	setinverse <-function(solve)  
## set the inversed matrix
	a <<-solve		
	getinverse <-function()a 
## return the inversed matrix
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
# return a list that contains these functions
	}

## The following function calculates the inverse of the special "matrix" 
##created with the above function. If the inverse has already been calculated
##(and the matrix has not changed),then cacheSolve should retrieve the inverse 
##from the cache.


cacheSolve <- function(x, ...) {
	a <-x$getinverse() 
##get the inversed matrix from object x,it will be null if uncalculated
	if(!is.null(a)){ 
	message("getting cached data") 
##if the inversion result is there return the calculated inversion       
	return(a)}
##if not, we do x$get to get the matrix object, solve it, we then set it to the object
## a and return the solved result  
	data <-x$get()
	a <-solve(data,...)
	x$setinverse(a)
	a         
}
