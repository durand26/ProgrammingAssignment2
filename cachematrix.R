## Put comments here that give an overall description of what your
## functions do

#With these functions, you can input a matrix, and it will return its inverse.
#makeCacheMatrix returns a set of functions you can use to store your matrix and its inverse.
#cacheSolve calculates the inverse of the matrix, and stores both the original matrix and its inverse
# in a constructed copy of makeCacheMatrix.


## Write a short comment describing this function

#This function returns a list of four functions needed to store matricies:
# 1. SET - Allows you to set the value of your original matrix x
# 2. GET - Tells you what the stored value of the original matrix x currently is.
# 3. SETINVERSE - Allows you to store the value of the inverted matrix.
# 4. GETINVERSE - Tells you what the stored value of the inverted matrix currently is.
makeCacheMatrix <- function(x = matrix()) {
    #DECLARE VARIABLES
    # Initialise the "invertedMatrix" variable, used to store the inverse matrix.
    # Set its value to null at first.
    invertedMatrix <- NULL
    
    #1: SET MYMATRIX
    # This function stores the value of the original matrix variable x, found in the
    # parent makeCacheMatrix function. It replaces it with "inputtedMatrix",
    # given to us by the calling function.
    set <- function(inputtedMatrix) {
        x <<- inputtedMatrix  #Changes the value of x in the parent
        # makeCacheMatrix function to whatever inputtedMatrix is here.
        
        invertedMatrix <<- NULL  #Sets the invertedMatrix variable
        # in the parent makeCacheMatrix function to NULL
    }
    
    #2. GET THE VALUE OF THE MAIN MATRIX
    #This function returns the value of x without changing it.
    get <- function() {
        x
    }
    
    #3. SET THE VALUE OF THE INVERTED MATRIX
    #This function changes the invertedMatrix variable in the makeCacheMatrix
    # function to the value of inputtedInvertedMatrix.
    setinverse <- function(inputtedInvertedMatrix) {
        invertedMatrix <<- inputtedInvertedMatrix
    }
    
    #4. GET THE VALUE OF THE INVERSION
    #This function returns the value of invertedMatrx found in the parent function
    # called makeCacheMatrix.
    getinverse <- function() {
        invertedMatrix
    }
    
    #RETURN THE WHOLE LIST OF FUNCTIONS
    #This last step returns the all four functions in a list, so they can be
    # called in other environments.
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}



## Write a short comment describing this function

#This function returns the inverse of a matrix.
cacheSolve <- function(x, ...) {
    
    #Get the invertedMatrix value from the makeCacheMatrix function
    invertedMatrix <- x$getinverse()
    
    #If the invertedMatrix is Null, let the user know, return a NULL value,
    # and exit this function.
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }
    
    #If the invertedMatrix is not null, then
    #  a) Get the value of the original matrix, and store it in data
    data <- x$get()
    
    #  b) Calculate the inverted matrix, and store it in the makeCacheMatrix function
    invertedMatrix <- solve(data, ...)
    x$setinverse(invertedMatrix)
    
    #  c) Let the user know what the value of the invertedMatrix is
    invertedMatrix
}
