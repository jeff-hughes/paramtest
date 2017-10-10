#' Run a function iteratively, with options for parallel processing.
#'
#' \code{run_test} runs a user-defined function iteratively. This function is
#' intentionally kept general and flexible, to allow for a wide variety of
#' applications. This function is the general-purpose function called by
#' functions such as \code{grid_search} and \code{random_search}, which provide
#' different methods for generating the parameters to be tested.
#'
#' @param func A user-defined function. The first argument to this function will
#'   be the iteration number.
#' @param params A list or data frame of parameters to be passed to \code{func}.
#'   Each set of parameters will be passed to \code{func} in turn.
#' @param n.iter Number of iterations (per set of params).
#' @param output Specifies how \code{run_test} provides the ultimate output from
#'   \code{func}: can return a "list" or a "data.frame". Note that if
#'   "data.frame" is specified, the supplied function must return a vector,
#'   matrix, or data frame, so it can be coerced into the data frame format. The
#'   "list" option will accept any type of output.
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
#'   you wish to play once the tests are done running. If set to TRUE, a random
#'   sound will be played. Requires the "beepr" package, and information about
#'   supported values is available in the documentation for that package.
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
#' # test power for sample size N=200 and N=300, with 500 iterations for each
#' power_sim <- run_test(lm_test, params=data.frame(N=c(200, 300)),
#'     n.iter=500, b0=0, b1=.15)
#' @export
run_test <- function(func, params=NULL, n.iter=1,
    output=c('list', 'data.frame'), boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, beep=NULL, ...) {

    dots <- list(...)

    # need to do this to avoid issues with match.arg when only two options
    # if(missing(output) || length(output) > 1) {
    #     outputType <- 'list'
    # } else {
    #     outputType <- match.arg(output)
    # }
    outputType <- match.arg(output, c('list', 'data.frame'))

    # set up combinations of parameters to test
    if (!is.null(params)) {
        if (is.list(params) && !is.data.frame(params)){
            # assume each element is a set of parameters
            if (length(params) > 1) {
                prms <- do.call(rbind.data.frame, params)
            # assume one parameter, multiple values
            } else {
                prms <- as.data.frame(params)
            }
        } else {
            prms <- params
        }
        prms_df_output <- prms
        names(prms_df_output) <- paste0(names(prms_df_output), '.test')
        nSets <- nrow(prms)
    } else {
        prms <- data.frame()  # empty
        prms_output <- NA
        tests_summary <- NA
        nSets <- 1
    }

    # figure out which parallel method to use
    parallel <- match.arg(parallel)
    have_mc <- FALSE
    have_snow <- FALSE
    if (parallel != 'no' && ncpus > 1) {
        if (requireNamespace('parallel', quietly=TRUE)) {
            if (parallel == 'multicore') {
                have_mc <- .Platform$OS.type != 'windows'
                if (!have_mc) {
                    warning('Multicore is not supported on Windows. Try parallel = "snow" instead. Proceeding with tests serially.')
                }
            } else if (parallel == 'snow') {
                have_snow <- TRUE
            }
        } else {
            warning("Loading package 'parallel' failed. Proceeding with tests serially.")
        }
        if (!have_mc && !have_snow) {
            ncpus <- 1
        }
    }

    # set up cluster if necessary
    if (ncpus > 1 && have_snow) {
        if (is.null(cl)) {
            clust <- parallel::makePSOCKcluster(rep('localhost',
                ncpus))
            if (RNGkind()[1] == "L'Ecuyer-CMRG") {
                parallel::clusterSetRNGStream(clust)
            }
        } else {
            clust <- cl
        }
    }

    cat(paste0('Running ',
        prettyNum(nSets * n.iter, big.mark=',', scientific=FALSE),
        ' tests...\n'))

    # cleanup to do when function ends, whether naturally or after error
    on.exit({
        # stop cluster if we created it
        if (ncpus > 1 && have_snow && is.null(cl)) {
            parallel::stopCluster(clust)
        }

        # Ding! Fries are done
        if (!is.null(beep)) {
            if (requireNamespace('beepr', quietly=TRUE)) {
                if (is.logical(beep) && beep == TRUE) {
                    beep <- 0
                }
                beepr::beep(beep)
            }
        }
    })

    allResults <- NULL  # variable to fill with final output
    tests <- NULL  # data frame of same length as output, with params and iterations

    timing <- system.time(
        for (set in 1:nSets) {

            # special case with no parameters to pass to function
            if (nrow(prms) == 0 && length(dots) == 0) {

                # bootstrap data
                if (boot && 'data' %in% names(bootParams)) {
                    boot_output <- do.call(boot::boot, args=c(list(statistic=func,
                        R=n.iter, parallel=parallel, ncpus=ncpus, cl=cl), bootParams))

                    col_names <- names(boot_output$t0)
                    output <- boot_output$t
                    colnames(output) <- col_names

                # simulate data
                } else {
                    if (ncpus > 1 &&
                        (have_mc || have_snow)) {

                        if (have_mc) {
                            output <- do.call(parallel::mclapply,
                                args=c(list(X=1:n.iter, FUN=func, mc.cores=ncpus)))
                        } else if (have_snow) {
                            output <- do.call(parallel::parLapply,
                                args=c(list(cl=clust, X=1:n.iter, fun=func)))
                        }
                    } else {
                        output <- do.call(lapply, args=c(list(X=1:n.iter, FUN=func)))
                    }
                }

            # pass parameters to function
            } else {
                opts <- list()
                if (nrow(prms) > 0) {
                    opts <- as.list(prms[set, ])
                }
                if (length(dots) > 0) {
                    if (length(opts) > 0){
                        opts <- c(opts, dots)
                    } else {
                        opts <- dots
                    }
                }

                # bootstrap data
                if (boot && 'data' %in% names(bootParams)) {
                    boot_output <- do.call(boot::boot, args=c(list(statistic=func,
                        R=n.iter, parallel=parallel, ncpus=ncpus, cl=cl), bootParams,
                        opts))

                    col_names <- names(boot_output$t0)
                    output <- boot_output$t
                    colnames(output) <- col_names

                # simulate data
                } else {
                    if (ncpus > 1 &&
                        (have_mc || have_snow)) {

                        if (have_mc) {
                            output <- do.call(parallel::mclapply,
                                args=c(list(X=1:n.iter, FUN=func, mc.cores=ncpus),
                                    opts))
                        } else if (have_snow) {
                            output <- do.call(parallel::parLapply,
                                args=c(list(cl=clust, X=1:n.iter, fun=func),
                                    opts))
                        }
                    } else {
                        output <- do.call(lapply, args=c(list(X=1:n.iter, FUN=func),
                            opts))
                    }
                }
            }

            rowsEachIter <- 1
            if (outputType == 'data.frame') {
                rowsEachIter <- length(output) / n.iter
            }

            iterations <- rep(1:n.iter, each=rowsEachIter)

            if (nrow(prms) > 0) {
                test <- data.frame(
                    iter=iterations,
                    prms[set, , drop=FALSE],
                    row.names=1:(n.iter*rowsEachIter))
                tests <- rbind(tests, test)
            }


            # convert output to data frame/vector if requested
            if (outputType == 'data.frame') {
                if (is.list(output)) {
                    col_names <- names(output[[1]])
                    output <- do.call(rbind.data.frame, output)
                    colnames(output) <- col_names
                }

                rowsEachIter <- nrow(output) / n.iter
                    # covers case where function outputs more than one row
                if (nrow(prms) > 0) {
                    result <- data.frame(
                        iter=iterations,
                        prms_df_output[set, , drop=FALSE],
                        output,
                        row.names=1:(n.iter*rowsEachIter))
                } else {
                    result <- data.frame(
                        iter=iterations,
                        output)
                }

                allResults <- rbind(allResults, result)
                row.names(allResults) <- NULL
            } else {
                # boot produces output as matrix, so must convert to list
                if (is.matrix(output) || is.data.frame(output)) {
                    output <- as.data.frame(output)
                    result <- NULL
                    for (i in seq(nrow(output))) {
                        result <- c(result, output[i, ])
                    }
                }
                allResults <- c(allResults, output)
            }
        }
    )

    if (outputType == 'data.frame') {
        allResults <- as.data.frame(allResults)
    }

    output <- list(results=allResults, tests=tests, n.iter=n.iter, timing=timing)
    class(output) <- 'paramtest'

    return(output)
}




