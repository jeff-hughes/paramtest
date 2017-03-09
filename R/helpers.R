#' Return results of a parameter test.
#'
#' \code{results} is a generic function that extracts the raw data from a
#' parameter test.
#'
#' @param test A paramtest object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{results} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{results.paramtest}}
#' @export
results <- function(test, ...) UseMethod('results')


#' Return results of a parameter test.
#'
#' \code{results.paramtest} returns the raw data from a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @return Returns a data frame with all the data returned from each test.
#' @export
results.paramtest <- function(test) {
    return(test$results)
}


#' Return the parameter values that were tested by paramtest.
#'
#' \code{tests} is a generic function that extracts information about the set of
#' specific tests (parameter values) for a parameter test.
#'
#' @param test A paramtest object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{tests} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{tests.paramtest}}
#' @export
tests <- function(sim, ...) UseMethod('tests')


#' Return the parameter values that were tested by paramtest.
#'
#' \code{tests.paramtest} extracts information about the set of specific tests
#' (parameter values) for a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @return Returns a data frame with one row for each set of tests that
#'   was performed.
#' @export
tests.paramtest <- function(test) {
    return(test$tests)
}


#' Return the number of iterations performed by a parameter test.
#'
#' \code{n.iter} is a generic function that extracts information about the
#' number of iterations (per specific test) performed by a parameter test.
#'
#' @param test A paramtest object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{n.iter} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{n.iter.paramtest}}
#' @export
n.iter <- function(test, ...) UseMethod('n.iter')


#' Return the number of iterations performed by a parameter test.
#'
#' \code{n.iter.paramtest} extracts information about the number of iterations
#' (per specific test) performed by a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @return Returns the number of iterations done in each test.
#' @export
n.iter.paramtest <- function(test) {
    return(test$n.iter)
}


#' Return the timing information of a parameter test.
#'
#' \code{timing} is a generic function that extracts the raw data from a
#' parameter test.
#'
#' @param test A paramtest object.
#' @param ... Additional arguments to be passed to the particular method for the
#'   given object tye.
#' @return The form of the value returned by \code{timing} depends on the class
#'   of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @seealso \code{\link{timing.paramtest}}
#' @export
timing <- function(test, ...) UseMethod('timing')


#' Return the timing information of a parameter test.
#'
#' \code{timing.paramtest} returns the raw data from a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @return Returns an object of class "proc_time" with information about how
#'   long the parameter test process took.
#' @export
timing.paramtest <- function(test) {
    return(test$timing)
}


#' Calculate error variance given model coefficients.
#'
#' \code{lm_error_var} will calculate the required error variance for a linear
#' model, given specified model coefficients, to create variance for your
#' dependent variable of approximately 'var'.
#'
#' \strong{Note:} This function assumes that \emph{all predictors are
#' independent} (i.e., uncorrelated).
#'
#' @param var The variance you wish your dependent variable to be.
#' @param ... Pass along all model coefficients, excluding the intercept. These
#'   can be named or unnamed.
#' @return Returns the required error variance so that the variance of your
#'   dependent variable is approximately 'var'.
#' @examples
#' lm_error_var(var=1, .15, .3)  # returns error variance of 0.8875
#' @export
lm_error_var <- function(var=1, ...) {
    dots <- list(...)
    exp_var <- 0
    for (i in 1:length(dots)) {
        exp_var <- exp_var + dots[[i]]^2
    }
    return(var - exp_var)
}





