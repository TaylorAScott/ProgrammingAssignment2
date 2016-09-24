################################################################################
## 	$Author:		Taylor A. Scott		
##	$Title:			CacheMatrix.R
##	$Description:	        Cache Matrix for JHU R-Programming Week3
##	$Date: 			24 Sep 2016
################################################################################



##	<Overview>
##		makeCacheMatrix is a function that takes a 
## 		parameter x [a matrx], and holds it; defines a cachedInverse variable,
##		and functions to set the value of x to a new value, get the value of 
##		x, set the value of cachedInverse to a new value and get the value of
## 		cachedInverse;
##		Returns an list that holds the functions set, get, setinverse 
##		and getinverse;
##		
##		cacheSolve is a function that when passed an list as returned from 
##		makeCacheMatrix function, solves for the inverse of said list's 
##		x matrix, stores the value to said list's cachedInverse variable;
##		Returns the inverse of the passed list's get matrix;
##
##		Testcases is a function that tests the makeCacheMatrix and cacheSolve 
## 		functions - ensuring that they can handle the default case, a 2x2
##		Matrix, a 3x3 matrix, setting the inverse of a 2x2 matrix to NA and
##		returning a proper value after setting such
##		Prints debug info to the console as it runs;
##		Returns TRUE if all tests passed;
##	</Overview>



##	<Summary>
##		Stores a [square, inversible] matrix variable (x) and defines 	
##		functions to get or set the matrix x, and functions to get or set the 
## 		inverse of matrix x (cachedInverse)
##		Returns a list that holds functions set, get, setinverse and
##		getinverse
##	</Summary>
makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(newMatrix) {
                x <<- newMatrix
                cachedInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(newInverse) cachedInverse <<- newInverse
        getinverse <- function() cachedInverse
        list(set = set, 
             get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

##	<Summary>
##		Function to solve for the inverse of a matrix as defined 
##		by the get function from a passed list (x) [defined by 
##		makeCacheMatrix] 
##		Returns the inverse of the matrix as defined by the passed list (x)'s 
## 		get function
##	</Summary>
cacheSolve <- function(x, ...) {
        cachedInverse <- x$getinverse()
        if(!is.null(cachedInverse) &&
           !is.na(cachedInverse)) return(cachedInverse)
        cachedInverse <- solve(x$get(), ...)
        x$setinverse(cachedInverse)
        cachedInverse
}

##	<Summary>
##		Tests the above methods with known inputs and outputs;
##		Returns TRUE if all is functioning as intended;
##	</Summary>
Testcases <- function(){
	
        ##      Variables totell if tests passed, list of failed tests
        TestCasesPassed <- T
        FailedTestCases <- list("Failed Testcases ::")



        ##      TESTCASE 0
        ##
        ##      Tests the default case, get and set

        IdentityMatrix <- NULL
        StartingMatrix <- NULL
        ExpectedOutput <- NULL
        cmObj <-makeCacheMatrix()
        cmOut<- cacheSolve(cmObj)

        ## 	PART A
        ##
        ##      Tests that the default cases have identical
        ##	numbers of rows and columns
        if (ncol(cmObj$get()) != ncol(cmOut) || 
            nrow(cmObj$get()) != nrow(cmOut)) {
                cat("\nTestcase 0A :: FAILED - Default Matrix and inverse do not have same dimensions")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    0A"))
        } else cat("\nTestcase 0A :: PASSED")

        ## 	PART B 
        ##
        ##	Tests Setting to and getting a value
        cmObj$set(rbind(c(1, 0), c(-2, -1)))
        if (!identical(cmObj$get(),rbind(c(1, 0), c(-2, -1)))) {
                cat("\nTestcase 0B :: FAILED - Get Value != Set Value")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    0B"))
        } else cat("\nTestcase 0A :: PASSED")



        ## 	TESTCASE 1
        ##
        ##	Tests using a 2 dimensional matrix
        IdentityMatrix <- rbind(c(1, 0), c(0, 1))
        StartingMatrix <- rbind(c(1, 0), c(-2, -1))
        ExpectedOutput <- rbind(c(1, 0), c(-2, -1))
        cmObj <- makeCacheMatrix(StartingMatrix)
        cmOut <- cacheSolve(cmObj)

        ## 	PART A
        ## 
        ## 	Test that the  output is as expected
        if(!identical(cmOut, ExpectedOutput)) {
                cat("\nTestcase 1A :: FAILED - Output not as expected")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    1A"))
        } else cat("\nTestcase 1A :: PASSED")

        ## 	PART B
        ##
        ## 	Test that StartingMatrix multiplied by the 
        ##	output is the IdentityMatrix
        MatrixProduct <- StartingMatrix %*% cmOut
        if(!identical(IdentityMatrix, MatrixProduct)) {
                cat("\nTestcase 1B :: FAILED - Starting Matrix * Output != Identity Matrix")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    1B"))
        } else cat("\nTestcase 1B :: PASSED")

        ## 	PART C
        ## 
        ##      Test that chached matrix is returned and as expected
        cmOut2 <- cacheSolve(cmObj)
        if(!identical(cmOut2, cmOut)) {
                cat("\nTestcase 1C :: FAILED - Cached Matrix not returned as expected")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    1C"))
        } else cat("\nTestcase 1C :: PASSED")



        ##      TESTCASE 2
        ##
        ##      Same as TESTCASE 1, But with a 3 dimensional Matrix
        IdentityMatrix <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
        StartingMatrix <- rbind(c(1, -1, 0), c(0, 1, 1), c(1, 0, -1))
        ExpectedOutput <- rbind(c(.5, .5, .5), c(-.5, .5, .5), c(.5, .5, -.5))
        cmObj <- makeCacheMatrix(StartingMatrix)
        cmOut <- cacheSolve(cmObj)
    
        ## 	PART A
        ## 
        ## 	Test that the  output is as expected
        if(!identical(cmOut, ExpectedOutput)) {
                cat("\nTestcase 2A :: FAILED - Output not as expected")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    2A"))
        } else cat("\nTestcase 2A :: PASSED")

        ## 	PART B
        ##
        ## 	Test that StartingMatrix multiplied by the 
        ##	output is the IdentityMatrix
        MatrixProduct <- StartingMatrix %*% cmOut
        if(!identical(IdentityMatrix, MatrixProduct)) {
                cat("\nTestcase 2B :: FAILED - Starting Matrix * Output != Identity Matrix")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    2B"))
        } else cat("\nTestcase 2B :: PASSED")

        ## 	PART C
        ## 
        ##      Test that chached matrix is returned and as expected
        cmOut2 <- cacheSolve(cmObj)
        if(!identical(cmOut2, cmOut)) {
                cat("\nTestcase 2C :: FAILED - Cached Matrix not returned as expected")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    2C"))
        } else cat("\nTestcase 2C :: PASSED")



        ##      TESTCASE 3
        ##
        ##	Tests NA Cases
        IdentityMatrix <- rbind(c(1, 0), c(0, 1))
        StartingMatrix <- rbind(c(1, 0), c(-2, -1))
        ExpectedOutput <- rbind(c(1, 0), c(-2, -1))
        cmObj <- makeCacheMatrix(StartingMatrix)
        cmOut <- cacheSolve(cmObj)

        ##      PART A
        ##
        ## 	Tests setting inverse to na; getting inverse
        cmObj$setinverse(NA)
        if(!is.na(cmObj$getinverse())){
                cat("\nTestcase 3A :: FAILED - Cached Matrix not returned as expected")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    3A"))
        } else cat("\nTestcase 3A :: PASSED")

        ##      PART B
        ##
        ##      Tests recalculating when inverse is set to na (would also work for null)
        if(!identical(cacheSolve(cmObj), ExpectedOutput)){
                cat("\nTestcase 3B :: FAILED - Cached Matrix not returned as expected")
                TestCasesPassed <- F
                FailedTestCases <- list(c(c(FailedTestCases[[1]]), "    3B"))
        } else cat("\nTestcase 3B :: PASSED")



        ## 	REPORTING SECTION
        if (TestCasesPassed) {
                cat("\n\nAll Testcases Passed\n\n")
                return(T)
        } else {
                cat(paste("\n\nNumber of Testcases Failed :: ", 
                        length(FailedTestCases) - 1,"\n\n"))
                cat(paste(FailedTestCases[[1]]), sep="\n")
                cat("\n")
                return(F)
        }



        ##      ERROR - TestCasesPassed is not defined
        cat("\n\nAn Error Occured - TestCasesPassed is Missing, Undefined or NA\n\n")
        F
}
            
