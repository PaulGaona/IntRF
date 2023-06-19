
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IntRF

<!-- badges: start -->

[![R-CMD-check](https://github.com/PaulGaona/IntRF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulGaona/IntRF/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of IntRF is to run tree-based regression for interval-valued data.

## Installation

You can install the development version of IntRF from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PaulGaona/IntRF", build_vignettes = TRUE)
```

## Example

This is a basic example, using the vignette:

``` r
vignette("IntRF_vignette", package = "IntRF")
```

## Additional Information

This package is intended as a learning exercise in R-package
development. We have taken a formally existing CRAN R-package, and made
use of its functionality, we use
[mvpart](https://CRAN.R-project.org/package=mvpart) as a wrapper for
random forests with a pairwise random sampling of predictor variables.

We included all functions of mvpart, as a workaround to
`Additional_repositories` field in the `NAMESPACE` file. `IntRF` passes
`devtools::check()` and `rcmdcheck::rcmdcheck()` without throwing an
error, warning, or note.

After running `rhub::check_for_cran()` and
`usethis::use_github_action_check_standard()` the package fails with an
`install.out` Installation failed error. Which can be attributed to
multiple factors upon closer inspection for each platform failure.

It is important to note that this package is not intended to be CRAN
approved, but instead a training for the package building process.
Github repositories can be found here:
[CRAN](https://github.com/cran/mvpart) and
[mvignon](https://github.com/mvignon/mvpart).
