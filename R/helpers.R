#' Return results of a parameter test.
#'
#' \code{results} returns the raw data from a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns a data frame with all the data returned from each test.
#' @export
results <- function(test, ...) UseMethod('results')


#' @describeIn results Results for a parameter test.
#' @export
results.paramtest <- function(test, ...) {
    return(test$results)
}


#' Return the parameter values that were tested by paramtest.
#'
#' \code{tests} extracts information about the set of specific tests (parameter
#' values) for a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns a data frame with one row for each set of tests that
#'   was performed.
#' @export
tests <- function(test, ...) UseMethod('tests')


#' @describeIn tests Parameter values for a parameter test.
#' @export
tests.paramtest <- function(test, ...) {
    return(test$tests)
}


#' Return the number of iterations performed by a parameter test.
#'
#' \code{n.iter} extracts information about the number of iterations (per
#' specific test) performed by a parameter test.
#'
#' @param test An object of type 'paramtest'.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns the number of iterations done in each test.
#' @export
n.iter <- function(test, ...) UseMethod('n.iter')


#' @describeIn n.iter Number of iterations for a parameter test.
#' @export
n.iter.paramtest <- function(test, ...) {
    return(test$n.iter)
}


#' Return the timing information of a parameter test.
#'
#' \code{timing} returns the information about how long a parameter test took.
#'
#' @param test An object of type 'paramtest'.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns an object of class "proc_time" with information about how
#'   long the parameter test process took.
#' @export
timing <- function(test, ...) UseMethod('timing')


#' @describeIn timing Timing information for a parameter test.
#' @export
timing.paramtest <- function(test, ...) {
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





