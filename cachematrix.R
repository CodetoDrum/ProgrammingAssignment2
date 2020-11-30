## The main role of these functions is to cache the inverse of a matrix. Both functions share
## considerable parallels to the "makeVector()"/"cachemean()" examples but instead subbing in
## a matrix and its inverse instead of a vector and its mean. The main reference that helped
## me grasp these concepts was:
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## This function simply sets up a "parent" environment consisting of objects for the matrix and
## its inverse, and objects for functions which are to be consolidated into a list at the end of
## the function. Each one of these functions has access to the data stored in the "x" and 
## "inverseMatrix" objects, due to them being "siblings" in the parent environment of 
## "makeCacheMatrix()". Make sure that any matrix you create is provided in the arguments of
## form: "makeCacheMatrix(matrix(data, row, col))
## Plugging in "makeCacheMatrix(data, row, col)", for example, will not automatically feed those
## values into a "matrix()" call. 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(solve) {
                inverseMatrix <<- solve
        }
        
        getInverse <- function() {
                inverseMatrix
        }
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
                
}

## This function takes in an "instance" of the "makeCacheMatrix()" function as its argument. 
## This just means plugging in the object you created during the "makeCacheMatrix()" call; for
## example: "object <- makeCacheMatrix(matrix(data, row, col)". Or, you could set up the 
## following form on the command line: "cacheSolve(makeCacheMatrix(matrix(data, row, col)))".
## "cachesolve()" can then query the functions nested in "makeCacheMatrix()" in order
## to calculate and cache the results of inverting the matrix object that was previously set
## in "makeCacheMatrix()". It is essentially the same as "cachemean()" from the example code,
## but using a different operation on the provided object. 

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        
        if(!is.null(inverseMatrix)) {
                message("Retrieving matrix")
                return(inverseMatrix)
        }
        
        matrix <- x$get()
        
        inverseMatrix <- solve(matrix)
        
        x$setInverse(inverseMatrix)
        
        inverseMatrix
}

## The following code shows the default nature of running both functions. For this to "work"
## the default result should be a 1 x 1 matrix consisting of one "NA" value. 

defaultMatrix <- makeCacheMatrix()
defaultSolve <- cacheSolve(defaultMatrix)

## This test matrix was created using the example from:
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

myMatrix <- makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
mySolve <- cacheSolve(myMatrix)

## As is indicated in the above post, not all matrices will have an inverse. The following test
## case shows an instance where you will receive an error message for the function call. Copying
## and pasting that error code into google will render several posts relating to why the error
## comes up, which will be much more detailed than what I can comment here. The more general
## explanation for "system is exactly singular" means that the "determinant" is equal to 0.
## The following resource is what I used to understand the "under-the-hood" process of inverting
## a matrix:
## https://www.mathsisfun.com/algebra/matrix-inverse.html

errorMatrix1 <- makeCacheMatrix(matrix(1:16, 4, 4))
errorSolve1 <- cacheSolve(errorMatrix1)

## It is mentioned in the first post I linked to at the start of this script, but you can "reset"
## the matrix object of your results using the "set()" function. Using the following code, you
## do not need to create new objects for the updated matrix and its inverse. However, you will
## notice that you need to store the result of "cacheSolve" back into the "mySolve" object
## so that it will be updated. Not doing so, as is done in "cacheSolve(myMatrix)" will render
## the update in the console but not in your environment.

myMatrix$set(matrix(c(1, 3, 27, 56), 2, 2))
cacheSolve(myMatrix)
## Look at the result in the console and compare to the environment before running the next
## line of code.

mySolve <- cacheSolve(myMatrix)
## Notice now that the object in the environment is updated 
