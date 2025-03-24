#' Print summary of parameter tests.
#'
#' \code{summary.paramtest} provides a summary of the various combinations of
#' parameter values tested in a given parameter test.
#'
#' @param object An object of class 'paramtest'.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns a data frame with one row per set of unique tests.
#' @seealso \code{\link{run_test}}
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
#' # test power for sample sizes between N=200 and N=300, with 500 iterations total
#' power_sim <- random_search(lm_test, params=list(N=c(200, 300)), n.iter=500, b0=0, b1=.15)
#' summary(power_sim)
#' @export
summary.paramtest <- function(object, ...) {
    tests <- object$tests

    if (is.data.frame(tests)) {
        tests$iter <- NULL
        tests <- unique(tests)
        row.names(tests) <- NULL
        tests$n.iter <- object$n.iter
    } else {
        tests <- data.frame(n.iter=object$n.iter)
    }
    class(tests) <- c('paramtest_summary', class(tests))
    return(tests)
}

#' Print summary of parameter tests.
#'
#' \code{print.paramtest_summary} prints a summary of the various combinations
#' of parameter values tested in a given parameter test.
#'
#' @param x An object of class 'paramtest_summary', from
#'   \code{\link{summary.paramtest}}.
#' @param ... Not currently implemented; used to ensure consistency with S3 generic.
#' @return Returns a data frame with one row per set of unique tests.
#' @seealso \code{\link{summary.paramtest}}
#' @export
print.paramtest_summary <- function(x, ...) {
    if (length(names(x)) == 1) {
        print(paste('No parameters varied.', x$n.iter[1], 'iterations run.'))
    } else {
        print.data.frame(x)
    }
}


