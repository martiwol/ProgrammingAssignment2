## The functions 'makeCacheMatrix()' and 'cacheSolve()' allow to cache the 
##      result of previous matrix-inverse operations (exploiting lexical 
##      scoping), and therefore to avoid unnecessary inverse-matrix calculations 
##      in case the matrix has not changed since the first time its inverse has
##      been calculated.

################################################################################
## Function 'makeCacheMatrix(x, ...)' defines get/set procedures for  
##      manipulating a matrix and its inverse, both stored in the function's 
##      environment. It exploits lexical scoping in order to cache the inverse 
##      of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        
        vDim <- dim(x)
        # Ensure the matrix is square...
        if ( (length(vDim)!=2) || (length(unique(vDim))!=1) ) {
                warning("Input matrix must be square.")
                return(NULL)
        }
        # Set-function for the matrix:
        set <- function(mNew) {
                vInputDim <- dim(mNew)
                # Ensure the matrix is square...
                if ( (length(vInputDim)!=2) || (length(unique(vInputDim))!=1) ) {
                        warning("Input matrix must be square.")
                        return(NULL)
                }
                x <<- mNew
                # Reset the matrix-inverse whenever the matrix data changes:
                mInv <<- NULL 
        }
        # Get-function for the matrix:
        get <- function() x
        # Set-function for the matrix inverse:
        setInv <- function(mNewInv) mInv <<- mNewInv
        # Get-function for the matrix inverse:
        getInv <- function() mInv

        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

################################################################################
## Function 'cacheSolve(x, ...)' returns the inverse of a "matrix" object 'x'
##      created using the function 'makeCacheMatrix()'. In case the inverse has
##      previously been cached the function simply returns the cached value. 
##      If not, additional arguments are forwared to the matrix-inverse 
##      calculation using 'solve()'.

cacheSolve <- function(x, ...) {
        # Check if the inverse has previously been cached...
        mInv <- x$getInv()
        if(!is.null(mInv)) {
                return(mInv)
        }
        # If the inverse has not been cached, calculate it here...
        mData <- x$get()
        mInv <- solve(mData, ...)
        x$setInv(mInv) # Cache the matrix inverse
        mInv
}
