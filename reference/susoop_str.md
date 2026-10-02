# List object for Survey Solutions GraphQl character operator selection

A list of the available transformers

## Usage

``` r
susoop_str
```

## Format

An object of class `list` of length 10.

## Value

A named list with the operator and the value to be passed on as input to
the filter.

## Details

Allows the user to select the operator for the required filter.

## Examples

``` r

# equal to 3
susoop_str$contains("area10")
#> $contains
#> [1] "area10"
#> 

# not equal to 3
susoop_str$startsWith("area")
#> $startsWith
#> [1] "area"
#> 
```
