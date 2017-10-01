#' Run a function iteratively using a grid search approach for parameter values,
#' with options for parallel processing.
#'
#' \code{grid_search} runs a user-defined function iteratively. Parameter values
#' can be given to \code{grid_search}, which will fully cross all parameters so
#' that each parameter value is tested at all other values of all parameters.
#'
#' @param func A user-defined function. The first argument to this function will
#'   be the iteration number.
#' @param params A list of parameters to be passed to \code{func}. The
#'   parameters are fully crossed so that each parameter value is tested at all
#'   other values of all parameters. (For example, list(N=c(5, 10), x=c(1, 2))
#'   will test four sets of parameters: N=5 and x=1, N=5 and x=2, N=10 and x=1,
#'   and N=10 and x=2.) Each set of parameters will then be passed to
#'   \code{func} in turn.
#' @param n.iter Number of iterations (per set of params).
#' @param output Specifies how \code{grid_search} provides the ultimate output
#'   from func: can return a "list", a "data.frame", or a "vector". Note that
#'   the output from the supplied function must be able to be coerced into this
#'   output type.
#' @param boot Whether or not to use bootstrapped data to pass along to
#'   \code{func}. Using this option instead of bootstrapping within \code{func}
#'   is preferable to take advantage of parallelization.
#' @param bootParams If \code{boot=TRUE}, then use \code{bootParams} to pass
#'   along a named list of arguments to the \code{\link{boot}} function. The
#'   statistic and R parameters will be filled automatically, but at minimum you
#'   will need to pass along data. Information about parallel processing will
#'   also be passed along automatically.
#' @param parallel The type of parallel operation to be used (if any).
#' @param ncpus Integer: the number of processes to be used in parallel
#'   operation.
#' @param cl An optional \code{parallel} or \code{snow} cluster for use if
#'   \code{parallel = 'snow'}. If not supplied, a cluster on the local machine
#'   is created for the duration of the iterations.
#' @param beep Include a numeric value or character vector indicating the sound
#'   you wish to play once the tests are done running. Requires the "beepr"
#'   package, and information about supported values is available in the
#'   documentation for that package.
#' @param ... Additional arguments to be passed to \code{func}. If you do not
#'   need to vary certain parameters in your model, you can pass them to
#'   \code{func} here.
#' @return Returns a list (by default) with one element per iteration. If
#'   \code{output} is specified as "dataframe", then \code{func} must
#'   return a (named) vector with the results you wish to capture; if
#'   \code{output} is specified as "vector", then \code{func} must return a
#'   one-element vector.
#' @seealso \code{\link{boot}}
#' @examples
#' lm_test <- function(iter, N, b0, b1) {
#'     x <- rnorm(N, 0, 1)
#'     y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))
#'     data <- data.frame(y, x)
#'     model <- lm(y ~ x, data)
#'
#'     # capture output from model summary
#'     est <- coef(summary(model))['x', 'Estimate']
#'     se <- coef(summary(model))['x', 'Std. Error']
#'     p <- coef(summary(model))['x', 'Pr(>|t|)']
#'
#'     return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
#'         sig=est > 0 & p <= .05))
#' }
#'
#' # test power for sample size N=200 and N=300, with 5000 iterations for each
#' power_sim <- grid_search(lm_test, params=list(N=c(200, 300)), n.iter=5000, b0=0, b1=.15)
#' @export
grid_search <- function(func, params=NULL, n.iter=1,
    output=c('list', 'data.frame', 'vector'), boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, beep=NULL, ...) {

    # cross each param value with every other one, to create all combinations
    if (!is.null(params)) {
        grid <- expand.grid(params, KEEP.OUT.ATTRS=FALSE)
    } else {
        grid <- data.frame()  # empty
    }

    output <- run_test(func=func, params=grid, n.iter=n.iter, output=output,
        boot=boot, bootParams=bootParams, parallel=parallel, ncpus=ncpus, cl=cl,
        beep=beep, ...)

    return(output)
}


#' Run a function iteratively using a random search approach for parameter
#' values, with options for parallel processing.
#'
#' \code{random_search} runs a user-defined function iteratively. Lower and
#' upper bounds for parameter values can be given to \code{random_search}, which
#' will then (uniformly) randomly select values within those bounds on each
#' iteration.
#'
#' @param func A user-defined function. The first argument to this function will
#'   be the iteration number.
#' @param params A named list of parameters to be passed to \code{func}. For
#'   continuous numeric values, a parameter must provide a two-element named
#'   vector with names "lower" and "upper" to specify the lower and upper bounds
#'   within which to sample. For parameters with integer values, provide a
#'   sequence, e.g., \code{seq(5, 10)}. For parameters with non-numeric values,
#'   provide a vector with the values from which to sample. On each iteration,
#'   the \code{random_search} function will select a uniformly random value
#'   for each parameter and pass this set of parameter values to \code{func}.
#' @param n.sample Number of times to sample from the parameter values.
#' @param n.iter Number of iterations (per set of params).
#' @param output Specifies how \code{random_search} provides the ultimate output
#'   from func: can return a "list", a "data.frame", or a "vector". Note that
#'   the output from the supplied function must be able to be coerced into this
#'   output type.
#' @param boot Whether or not to use bootstrapped data to pass along to
#'   \code{func}. Using this option instead of bootstrapping within \code{func}
#'   is preferable to take advantage of parallelization.
#' @param bootParams If \code{boot=TRUE}, then use \code{bootParams} to pass
#'   along a named list of arguments to the \code{\link{boot}} function. The
#'   statistic and R parameters will be filled automatically, but at minimum you
#'   will need to pass along data. Information about parallel processing will
#'   also be passed along automatically.
#' @param parallel The type of parallel operation to be used (if any).
#' @param ncpus Integer: the number of processes to be used in parallel
#'   operation.
#' @param cl An optional \code{parallel} or \code{snow} cluster for use if
#'   \code{parallel = 'snow'}. If not supplied, a cluster on the local machine
#'   is created for the duration of the iterations.
#' @param beep Include a numeric value or character vector indicating the sound
#'   you wish to play once the tests are done running. Requires the "beepr"
#'   package, and information about supported values is available in the
#'   documentation for that package.
#' @param ... Additional arguments to be passed to \code{func}. If you do not
#'   need to vary certain parameters in your model, you can pass them to
#'   \code{func} here.
#' @return Returns a list (by default) with one element per iteration. If
#'   \code{output} is specified as "dataframe", then \code{func} must
#'   return a (named) vector with the results you wish to capture; if
#'   \code{output} is specified as "vector", then \code{func} must return a
#'   one-element vector.
#' @seealso \code{\link{boot}}
#' @examples
#' lm_test <- function(iter, N, b0, b1) {
#'     x <- rnorm(N, 0, 1)
#'     y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))
#'     data <- data.frame(y, x)
#'     model <- lm(y ~ x, data)
#'
#'     # capture output from model summary
#'     est <- coef(summary(model))['x', 'Estimate']
#'     se <- coef(summary(model))['x', 'Std. Error']
#'     p <- coef(summary(model))['x', 'Pr(>|t|)']
#'
#'     return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
#'         sig=est > 0 & p <= .05))
#' }
#'
#' # test power for sample sizes between N=200 and N=300, with 5000 iterations total
#' power_sim <- random_search(lm_test, params=list(N=c(200, 300)), n.iter=5000, b0=0, b1=.15)
#' @export
random_search <- function(func, params=NULL, n.sample=1, n.iter=1,
    output=c('list', 'data.frame', 'vector'), boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, beep=NULL, ...) {

    # create list of param values
    if (!is.null(params)) {
        grid <- list()
        for (p in 1:length(params)) {
            if (!is.null(names(params[[p]])) &&
                    all.equal(names(params[[p]]), c('lower', 'upper'))) {
                # continuous numeric
                grid[[names(params)[p]]] <- stats::runif(n.sample, params[[p]]['lower'],
                    params[[p]]['upper'])
            } else {
                # either integer values or non-numeric values
                grid[[names(params)[p]]] <- sample(params[[p]], n.sample, replace=TRUE)
            }
        }
        grid <- as.data.frame(grid)
    } else {
        grid <- data.frame()  # empty
    }

    output <- run_test(func=func, params=grid, n.iter=n.iter, output=output,
        boot=boot, bootParams=bootParams, parallel=parallel, ncpus=ncpus, cl=cl,
        beep=beep, ...)

    return(output)
}




