## Package update (Version 0.1.7)

This is an update of an existing CRAN package to update the maintainer email address (requested by Kurt Hornik via GitHub issue #1) and to resolve current CRAN check NOTEs.

### Issues addressed:
* Updated maintainer contact email address in DESCRIPTION to a working address (michael.wild@me.com).
* Fixed 'DESCRIPTION meta-information' NOTE: updated dependency to `R (>= 4.1.0)` due to the use of base pipe `|>` and shorthand `\(...)`.
* Fixed 'dependencies in R code' NOTE: removed unused imports (`jsonlite`, `readr`, `stringr`) from DESCRIPTION.
* Updated demo server URL in DESCRIPTION and README to prevent 404 on HTTP GET.

## Test environments
* macOS (local): R 4.3.1
* win-builder (devel): R Under development (unstable) (2026-09-30 r90605 ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer change NOTE: The maintainer email change was requested by Kurt Hornik / CRAN due to bounced email (GitHub issue #1).