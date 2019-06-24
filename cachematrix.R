## cacheSolve takes matrix called x and sees if the inverse of matrix x has already been computed. 
## If so, it returns the inverse of x without re-calculating the inverse. If not, it calculates the inverse
## matrix and then caches those values.

## 2 lists are created and caches, matrix_list is the list of x entered by user, inverse_list is the list of 
## inverse values that have already been calculated. The two lists correspond with each other.




## Takes matrix x and caches it as y
makeCacheMatrix <- function(x = matrix()) {
  y <<- x
  }  


## Takes matrix x, which has been cached through makeCacheMatrix,
## then compares to list called matrix_list to see if that matrix 
## has been calculated before. If so, it finds the corresponding
## inverse value in the list inverse_list. If not, then it calculates
## new inverse matrix and adds it to cached list called inverse_list

cacheSolve <- function(x, ...) {
    d <- makeCacheMatrix(x)
    if (exists("matrix_list") == FALSE){    ##check if matrix_list already exists, if not creates it  
      matrix_list <<- list(d)
      print("not yet calculated because nothing calculated yet")
      inverse_list <<- list(solve(d))
      return(solve(d))
      } else {                             ##if list DOES exist
        for (i in 1:length(matrix_list)){   ##compares new matrix x to each of the values in matrix_list to see if identical
          if (identical(d,matrix_list[[i]]) == TRUE){
            print("already calculated")     ##response if already calculated before
            return(solve(d))
            }}
        print("not yet calculated")        ##after loop, if identical matrix is NOT found, computes new inverse value and adds to list
        l <- length(matrix_list)
        matrix_list[[l+1]] <<- d
        inverse_list[[l+1]] <<- solve(d)
        return(solve(d))    ## Return a matrix that is the inverse of 'x'
        }
    }









