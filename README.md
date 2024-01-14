
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://docs.mysurvey.solutions/'><img src="man/figures/susotools.png" align="right" height="139"/></a>

# susographql: A Comprehensive R Interface to the Survey Solutions GraphQL API

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/michael-cw/susographql/workflows/R-CMD-check/badge.svg)](https://github.com/michael-cw/susographql/actions)
[![R-CMD-check](https://github.com/michael-cw/susographql/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michael-cw/susographql/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<div align="justify">

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
available Survey Solutions GraphQL queries and mutations in the
comprehensive [API
documentation](https://demo.mysurvey.solutions/graphql/).

While `susographql` is crafted as a “bare-bone” API client, delivering
data largely in its original server format, it offers users unmatched
flexibility in designing their own, more complex processes fully
customized to their needs. This approach also means minimal data
manipulation in the background, catering to users who prefer a hands-on
approach to data handling. For those who seek a more deterministic
experience with built-in data transformations and ready-to-use data
structures including support for integration in an R Shiny application,
we recommend our [SurveySolutionsAPI (httr
version)](https://github.com/michael-cw/SurveySolutionsAPI) or the newer
[SurveySolutionsAPIv2 (httr2
version)](https://github.com/michael-cw/SurveySolutionsAPIv2), which
cover both, the [Survey Solutions REST
API](https://demo.mysurvey.solutions/apidocs/index.html) and the GraphQL
API handled in this package[^1].

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

## Using the API client

1.  Set up your own personal [Survey Solutions
    Server](https://mysurvey.solutions/en/) or request a free [Personal
    Demo Server
    (PDS)](https://pds.mysurvey.solutions/PersonalDemoServerRequest)
    (for testing only\!\!).
2.  Set up your [API user and
    credentials](https://docs.mysurvey.solutions/headquarters/accounts/teams-and-roles-tab-creating-user-accounts/).
3.  Use the credentials to connect to the server.

Contrary to its bigger sisters, the [SurveySolutionsAPI (httr
version)](https://github.com/michael-cw/SurveySolutionsAPI) and the
[SurveySolutionsAPIv2 (httr2
version)](https://github.com/michael-cw/SurveySolutionsAPIv2) this
package does not come with a lot of helpers for credential handling and
setting, nevertheless a light-weight credentials check is included:

``` r
library(susographql)

suso_gql_pwcheck(
  endpoint = "https://demo.mysurvey.solutions/graphql",
  user = "someuser",
  password = "andhispassword",
  workspace = "primary"
)
```

Which returns either **200** if the credentials are correct and **400**
otherwise. Also note, that the endpoint requires the full path including
the **graphql** part at the end.

## Feature requests and bug reports

You can either use the standard GitHub approach by filing a bug
report/feature request
[here](https://github.com/michael-cw/SurveySolutionsAPI/issues) or you
use the Survey Solutions user forum
[here](https://forum.mysurvey.solutions/c/api/13).

Please continue to check for updates, as we are constantly working to
improve the package as well as integrating any new GraphQL queries or
mutations as soon as they are available in [Survey
Solutions](https://docs.mysurvey.solutions/release-notes/).

</div>

[^1]: The [SurveySolutionsAPIv2 (httr2
    version)](https://github.com/michael-cw/SurveySolutionsAPIv2)
    package uses several of the functions from the `susographql`
    package.
