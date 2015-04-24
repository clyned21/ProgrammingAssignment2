## makeCacheMatrix sets up and returns a list of 4 functions (getMatrix,setMatrix,getInverse,setInverse)
## setMatrix sets matx = the matrix passed in to setMatrix and sets inv = Null in the 
## calling environment. The other functions just set or return values.

makeCacheMatrix <- function(x = matrix()) {
        setMatrix<-function(y) {
                matx<<-y
                inv<<-NULL
        }
        getMatrix <- function() matx
        setInverse <- function(inverse) {inv <<- inverse }          
        getInverse <- function() inv
        
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)                        

}


## cacheSolve checks to see if there is a matrix inverse stored with getInverse. If so, the function
## returns the inverse and completes. If not,the function will get the input matrix, calculate the inverse 
## and store it in setInverse. You must call this function to set the matrix inverse for the first time
## and pass in the output of makeCacheMatrix as the input parameterto the cacheSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached matrix inverse")
                return(inverse)
        }
        data<-x$getMatrix()
        inverse<-solve(data,...)
        x$setInverse(inverse)
        inverse       
}
