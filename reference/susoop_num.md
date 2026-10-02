# List object for Survey Solutions GraphQl numeric operator selection

A list of the available transformers

## Usage

``` r
susoop_num
```

## Format

An object of class `list` of length 12.

## Value

A named list with the operator and the value to be passed on as input to
the filter.

## Details

Allows the user to select the operator for the required filter.

## Examples

``` r

# equal to 3
susoop_num$eq(3)
#> $eq
#> [1] 3
#> 

# not equal to 3
susoop_num$neq(3)
#> $neq
#> [1] 3
#> 
```
