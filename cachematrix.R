## makeCacheMatrix is a function that takes one argument x[Matrix] and instantiate an empty object to store the inverse of a matrix
## contains four functions  set Matrix, Get Matrix, Set inverse, get inverse


# The following code:
#create a function that takes an argument x[matrix] and instantiate an empty object called inverse to store info.

makeCacheMatrix <- function(x = matrix()) { 
        inverse <- NULL 
        
        # The following code
        # assign the input ar(y) to the (x) in the parent env, and assign NULL to the inverse object
        #in the parent env to clear the value of inverse cached in prior execution of parent function  
        
        setxm <- function(y) { 
                x <<- y
                inverse <<- NULL
                
        }
        
        
        #getx retrive x value from the parent env. as x is not defined within get function
        
        getx <- function() x  # note x is outside the get function 
        
        # setinverse function set inverse , then assign the value to inverse in previous env. using <<- operator 
        
        setInverse <- function(inverse) inverse <<- inverse # note; inverse is defined within the function
        
        # getinverse, retrive inverse from parent env. as inverse is not defined in the getinverse function 
        
        getInverse <- function() inverse 
        
        
        # list, create a list of the 4 functions [setxm, getx, setinverse, getinvers], and assign a name to each function of the 4 functions  
        # The reason to name the functions is to use the extract operator $ using the function name.
        
        list(set = setxm,
             get = getx,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve is a function that takes a singe argument X, and ... ellipsis to allow for passing additional argumnets
## cacheSolve checks the value of inverse and return it if it isn't Null, recalculate it and set it if it is Null

cacheSolve <- function(x, ...) {
        
        ## the function retrive the value of inverse (from parent env)for the object x passed to the function
        
        inverse <- x$getInverse()
        
        ## Function check if the value of inverse is NUll, if inverse is not Null (cached)-
        ## then it will return the cached inverse to the parent env.
        
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## if the value of inverse in the parent env is NULL:
        # new_mat gets the value of x, then
        new_mat <- x$get()
        # calculate the inverse
        inverse <- solve(new_mat, ...)
        # set the inverse of x in the parent env
        x$setInverse(inverse)
        # print the new inverse value
        inverse
}