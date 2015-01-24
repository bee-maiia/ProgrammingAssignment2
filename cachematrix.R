# In general, provided functions carry out calculations of the matrix, which is inverse to the one given as an argument to the 'makeCacheMatrix' function or 'set' function. 
# Inversed matrix is stored (cached) as a value of the 'inverse' variable. 
# In case the inversed matrix has already been calculated, it can be retrieved from the 'inverse' variable in order to avoid costly computations. 
# 'makeCacheMatrix' function returns a list with four functions, which are called from 'cacheSolve' function. 
# The list contains the following functions: 
#1. 'set' - can be used to change the original matrix (it can be called from the console using >  $set(new_matrix_here), but not from the 'cacheSolve' function)
#2. 'get'- to get the original matrix 
#3. 'set_inverse' - to set (cache) the value of the inversed matrix, which was calculated in the 'cacheSolve' function 
#4. 'get_inverse' - to take the value of the inversed matrix, whether it is NULL or has been calculated already
 makeCacheMatrix <- function(x = matrix()) {  # defines the function that takes matrix, the inverse of which should be found, as an argument
	inverse<-NULL	# no binding associated with the 'inverse' variable until it is not calculated in the 'cacheSolve' function and assigned via the 'set_inverse' function
	set<-function(y){  # function to set a new original matrix ('y') 
		x<<-y    #  variable 'x' is assigned with a new matrix
		inverse<<-NULL   # zeros the inverse value as 'inverse' should be recalculated for the new matrix 
	}
	get<-function() x  # returns matrix, the inverse of which should be found 
	set_inverse<-function(solved) inverse<<-solved # assigns inversed matrix, calculated in the 'cacheSolve' function, to the variable 'inverse'
	get_inverse<-function() inverse # returns the value of the inversed matrix, whether it is NULL or has been calculated already,
	list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)  # returns a list of functions
}
# This function returns the inversed matrix, which was either previously calculated (and cached) or has been just calculated. 
# It takes list of functions, defined in 'makeCacheMatrix' as an argument. 
# Which functions are to be later used by 'cacheSolve' will depend on whether the inversed matrix has already been calculated (and cached) or not.   
cacheSolve <- function(x, ...) {
	inverse<-x$get_inverse()  # gets the value of the inversed matrix, whether it is NULL or has been calculated already, and assigns to the 'inverse' variable defined in the 'cacheSolve' function
	if(!is.null(inverse)) {   #checks whether inversed matrix was already calculated or not
		message("getting cashed data")  # so we know that the inversed matrix has already been calculated 
		return(inverse) # returns the inversed matrix, which was previously calculated
	}
	data<-x$get()  # gets the matrix, which was earlier passed to the function 'makeCacheMatrix' as x. So that in the 'cacheSolve' function variable 'data' stores the original matrix 
	inverse<-solve(data, ...)  # calculation of the matrix, which is the inverse of 'x'
	x$set_inverse(inverse)  # calls the function 'set_inverse', defined in the function "makeCacheMatrix", and assigns a new value to the "inverse" variable
	return(inverse)  # returns the inversed matrix, which has been just calculated
	  
}