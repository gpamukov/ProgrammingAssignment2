## =========================================================================================================
## Author: Georgi Pamukov
## Date: 2016/06/18
## Description: Packs methods to hold, process (inverse) and cache matrix data
## Sample invocation:
##
## Create test matrix
#mx <- matrix(rnorm(1e4), nrow = 1e2, ncol = 1e2)

## Create instance of the Make Cache Matrix object
#mcm_obj <- makeCacheMatrix(mx)

## Compute inverted matrix - will be calculated first time
#inv_mx <- cacheSolve(mcm_obj)

## See it
#inv_mx

## Compute inverted matrix - will be taken from cache
#inv_mx <- cacheSolve(mcm_obj)

## Create diff test matrix
#mx1 <- matrix(rnorm(1e4), nrow = 1e2, ncol = 1e2)

## Load it in the cache object - cache is reset
#mcm_obj$load_mx_data(mx1)

## Compute inverted matrix - will be calculated again - no cache is available
#inv_mx <- cacheSolve(mcm_obj)

## See it
#inv_mx
## =========================================================================================================


## Packs methods to hold and cache matrix data
## Every time the object is reinitialized (with new matrix) prev cache is destroyed.
## Every time new raw matrix is set/loaded - cache will be reset.
makeCacheMatrix <- function(mx_data = matrix()) {
    # System methods =============================================
    # Validate data object - verify it is populated numeric matrix
    val_mx <- function(tst_mx) {
        if (all(is.na(tst_mx))) {
            message("Datatype valiadtion error: data object is not populated")
            stop()
        }
        else if (!is.matrix(tst_mx)) {
            message("Datatype valiadtion error: data object is not matrix")
            stop()
        }
        else if (!is.numeric(tst_mx)) {
            message("Datatype valiadtion error: data object is not numeric")
            stop()
        }
    }
    
    # Compare cached raw data matrix with matrix passed as argument (validate before)
    identical_mx_data <- function(cmp_mx) {
        if (identical(mx_data, cmp_mx)) {
            TRUE
        } else {
            FALSE
        }
    }
    
    # Init =======================================================
    # Validate raw matrix data upon initialization of new instance. Exit if not meeting conditions...
    val_mx(mx_data)
    
    # Reset upon initialization of new instance
    inv_mx <- NULL
    
    # Interface methods ==========================================
    # Validate and initialize raw data - if new raw matrix is different - reset the cache (that was associated with the old raw matrix)
    load_mx_data <- function(mx) {
        val_mx(mx)
        if (!identical_mx_data(mx)) {
            print("New matrix data is loaded. Reseting cache.")
            mx_data <<- mx
            inv_mx <<- NULL
        }
    }
    
    # Return cached raw data
    get_mx_data <- function()
        mx_data
    
    # Validate and load cache with newly processed inverted matrix data
    load_cache_mx_data <- function(np_inv_mx) {
        val_mx(np_inv_mx)
        inv_mx <<- np_inv_mx
    }
    
    # Return cached inverted matrix
    get_cache_mx_data <- function()
        inv_mx
    
    # Expose interface
    list(
        load_mx_data = load_mx_data,
        get_mx_data = get_mx_data,
        load_cache_mx_data = load_cache_mx_data,
        get_cache_mx_data = get_cache_mx_data
    )
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# Retrieves from the cache if alredy computed for this same matrix.
cacheSolve <- function(fn_mcm_obj, ...) {
    # Gets if there is something cached already
    inv_mx <- fn_mcm_obj$get_cache_mx_data()
    
    # If not null (which means matrix is not changed and already calculated) - use the cache
    inv_mx <- if (!is.null(inv_mx)) {
        print("Getting inverted matrix from the cache")
        
        # Return inverted matrix that was retrieved from the cache
        inv_mx
    }
    else {
        print("Computing inverted matrix - not available in cache")
        
        # Gets raw data from the makeMatrix object - already validated in the function above
        mx_data <- fn_mcm_obj$get_mx_data()
        
        # Calculate the inverted matrix
        inv_mx <- solve(mx_data)
        
        # Load the inverted matrix in cache for future use
        fn_mcm_obj$load_cache_mx_data(inv_mx)
        
        # Return inverted matrix
        inv_mx
    }
    inv_mx
}