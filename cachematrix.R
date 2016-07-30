## Please only use an inversible square matrix that can be solved by Solve()


## makeCacheMatrix creates a matrix and set inverse matrix
##
## How to test:
## 1. Create a matrix
##      x <- matrix (1:4,nrow = 2,ncol = 2)
##      matx <- makeCacheMatrix(x)
## 2. get the matrix using get()
##      matx$get()
## 3. Set the inverse of the matrix using setinmatrix()
##      matx$setinmatrix(solve(matx$get()))
## 4. get the inverse matrix using getinmatrix()
##      matx$getinmatrix()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinmatrix <- function(inmatrix) m <<- inmatrix
        getinmatrix <- function() m
        list(set = set, get = get,
             setinmatrix = setinmatrix,
             getinmatrix = getinmatrix)
}


## cacheSolve function check if the inverser function has been calculated using makeCachematrix()
## How to Test:
##      cacheSolve(mmat)
## If the inverse matrix has been calculated, this function will output the calculated inverse matrix 
## and the message "getting cached data". If not, this function will calcultate and output the inverse matrix
## 

cacheSolve <- function(x, ...) {

        m <- x$getinmatrix()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinmatrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
        
}
