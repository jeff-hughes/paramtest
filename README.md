<!-- README.md is generated from README.Rmd. Please edit that file -->
paramtest
=========

The `paramtest` R package includes a set of functions used to iterate a function across multiple sets of parameters. It allows you to train algorithms, run simulations, etc. while easily varying parameters.

This package offers a flexible way to undergo a grid search or random search when varying parameters. Users must provide a function to iterate over, so all inputs and outputs of the function are specified by the user, offering complete control over the process. `paramtest` encapsulates the search process (along with options to parallelize over multiple processor cores), but is otherwise intentionally left very general-purpose.

However, the package was made with machine learning hyperparameter optimization and statistical power simulations in mind—see the vignettes for examples of these use cases.

Please be aware that this package is still in development, and as such, bugs may still exist, and functions and function parameters may still be subject to change.

To install the most recent stable release, use the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/paramtest", build_vignettes=TRUE)
```

### Installation Issues

Networked computers can sometimes result in installation issues, as the `install_github` function sometimes has difficulty with networked directories. If this happens to you, use the `.libPaths()` function to find the path to your R libraries. That will likely give you a path starting with two backslashes, but you will need to convert that to a path starting with a drive letter (e.g., 'C:', 'D:'). From there, use the following code:

``` r
install.packages("devtools")
devtools::install_github("jeff-hughes/paramtest", build_vignettes=TRUE,
    args=c('--library="N:/path/to/libraries/"'))
```

Obviously, change the path to the path where your R libraries are stored.
