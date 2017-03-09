#' Run a function iteratively, with options for parallel processing.
#'
#' \code{run_test} runs a user-defined function iteratively. This function is
#' intentionally kept general and flexible, to allow for a wide variety of
#' applications. This function is the general-purpose function called by
#' functions such as \code{grid_search} and \code{random_search}, which provide
#' different methods for generating the parameters to be tested.
#'
#' @param func A user-defined function.
#' @param params A list or data frame of parameters to be passed to \code{func}.
#'   Each set of parameters will be passed to \code{func} in turn.
#' @param n.iter Number of iterations (per set of params).
#' @param output Specifies how \code{run_test} provides the ultimate output from
#'   func: can return a "list", a "dataframe", or a "vector". Note that
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
#' lm_test <- function(N, b0, b1) {
#' x <- rnorm(N, 0, 1)
#' y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))
#' data <- data.frame(y, x)
#' model <- lm(y ~ x, data)
#'
#' # capture output from model summary
#' est <- coef(summary(model))['x', 'Estimate']
#' se <- coef(summary(model))['x', 'Std. Error']
#' p <- coef(summary(model))['x', 'Pr(>|t|)']
#'
#' return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
#'     sig=est > 0 & p <= .05))
#' }
#'
#' # test power for sample size N=200 and N=300, with 5000 iterations for each
#' power_sim <- run_test(lm_test, params=data.frame(N=c(200, 300)), n.iter=5000, b0=0, b1=.15)
#' @export
run_test <- function(func, params=NULL, n.iter=1,
    output=c('list', 'dataframe', 'vector'), boot=FALSE, bootParams=NULL,
    parallel=c('no', 'multicore', 'snow'), ncpus=1, cl=NULL, beep=NULL, ...) {

    dots <- list(...)
    outputType <- match.arg(output)

    # cross each param value with every other one, to create all combinations
    if (!is.null(params)) {
        grid <- expand.grid(params, KEEP.OUT.ATTRS=FALSE)
        grid_output <- grid
        names(grid_output) <- paste0(names(grid_output), '.test')
        nSets <- nrow(grid)
    } else {
        grid <- data.frame()  # empty
        grid_output <- data.frame()
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

    allResults <- NULL  # variable to fill with final output

    timing <- system.time(
        for (set in 1:nSets) {
            # bootstrap data
            if (boot && 'data' %in% names(bootParams)) {
                boot_output <- do.call(boot::boot, args=c(list(statistic=func,
                    R=n.iter, parallel=parallel, ncpus=ncpus, cl=cl), bootParams,
                    c(as.list(grid[set, , drop=FALSE]), dots)))

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
                                c(as.list(grid[set, , drop=FALSE]), dots)))
                    } else if (have_snow) {
                        output <- do.call(parallel::parLapply,
                            args=c(list(cl=clust, X=1:n.iter, fun=func),
                                c(as.list(grid[set, , drop=FALSE]), dots)))
                    }
                } else {
                    output <- do.call(lapply, args=c(list(X=1:n.iter, FUN=func),
                        c(as.list(grid[set, , drop=FALSE]), dots)))
                }

                # convert output to data frame/vector if requested
                if (outputType == 'dataframe') {
                    col_names <- names(output[[1]])
                    output <- do.call(rbind.data.frame, output)
                    colnames(output) <- col_names
                } else if (outputType == 'vector') {
                    output <- unlist(output)
                }
            }

            if (outputType == 'dataframe') {
                rowsEachIter <- nrow(output) / n.iter
                if (!is.null(params)) {
                    extendGrid <- matrix(rep(unlist(grid_output[set, , drop=FALSE]),
                        each=n.iter), nrow=n.iter)
                    colnames(extendGrid) <- names(grid_output)

                    result <- cbind(sim=rep(1:n.iter, each=rowsEachIter), extendGrid, output)
                } else {
                    result <- cbind(sim=rep(1:n.iter, each=rowsEachIter), output)
                }

                if (is.null(allResults)) {
                    allResults <- result
                } else {
                    allResults <- rbind(allResults, result)
                }
                row.names(allResults) <- NULL
            } else {
                if (is.null(allResults)) {
                    allResults <- output
                } else {
                    allResults <- c(allResults, output)
                }
            }
        }
    )

    # stop cluster if we created it
    if (ncpus > 1 && have_snow && is.null(cl)) {
        parallel::stopCluster(clust)
    }

    if (outputType == 'dataframe') {
        allResults <- as.data.frame(allResults)
    }

    output <- list(results=allResults, tests=grid, n.iter=n.iter, timing=timing)
    class(output) <- 'paramtest'

    # Ding! Fries are done
    if (!is.null(beep)) {
        if (requireNamespace('beepr', quietly=TRUE)) {
            beepr::beep(beep)
        }
    }

    return(output)
}




