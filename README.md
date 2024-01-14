
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://docs.mysurvey.solutions/'><img src="man/figures/susotools.png" align="right" height="139" style="float:right; height:139px;"/></a>

# susographql: A Comprehensive R Interface to the Survey Solutions GraphQL API

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/michael-cw/susographql/workflows/R-CMD-check/badge.svg)](https://github.com/michael-cw/susographql/actions)
[![R-CMD-check](https://github.com/michael-cw/susographql/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michael-cw/susographql/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<div data-align="justify">

The `susographql` R package offers a comprehensive interface to the
World Bank’s [Survey Solutions](https://mysurvey.solutions/en/) Computer
Assisted Survey System (CASS) via its GraphQL API. This package supports
all current queries and mutations, seamlessly integrating the latest
advancements like map uploads. It harnesses the power of the modern
[httr2](https://httr2.r-lib.org/index.html) package, ensuring a
streamlined and efficient user experience without the need for external
GraphQL client packages.

Beyond basic API interactions, `susographql` brings an assortment of
helper functions. These are thoughtfully designed to simplify the use of
query filters, elevating the user experience and broadening your data
manipulation and retrieval capabilities. Dive into the details of the
available Survey Solutions GraphQL queries and mutations in our
comprehensive [documentation](https://demo.mysurvey.solutions/graphql/).

While `susographql` is crafted as a “bare-bone” API client, delivering
data largely in its original server format, it offers users unmatched
flexibility. This approach ensures minimal data manipulation in the
background, catering to users who prefer a hands-on approach to data
handling. For those who seek a more deterministic experience with
built-in data transformations and ready-to-use data structures, we
recommend our [SurveySolutionsAPI (httr
version)](https://github.com/michael-cw/SurveySolutionsAPI) or the newer
[SurveySolutionsAPIv2 (httr2
version)](https://github.com/michael-cw/SurveySolutionsAPIv2), which
cover both, the [Survey Solutions REST
API](https://demo.mysurvey.solutions/apidocs/index.html#) and the
GraphQL API handled in this package.

## Installation

  - Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1
    or greater)

  - Install R Studio: <https://rstudio.com/products/rstudio/download/>
    (version 1.2.5001-3 or newer)

  - Make sure the *devtools* package is installed, if not install it
    with:

<!-- end list -->

``` r
install.packages("devtools")
```

  - After that install the actual package:

<!-- end list -->

``` r
devtools::install_github("michael-cw/susographql")
```

</div>
