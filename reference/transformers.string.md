# Utility Functions for string operator selection

Can be used in filters ("where") for operator selection. If none is
selected, operator always defaults to \`eq()\`. The functions bellow are
valid for the corresponding inputs ComparableInt64OperationFilterInput
and ComparableNullableOfInt32OperationFilterInput.

## Usage

``` r
contains(value_set)

endsWith(value_set)

ncontains(value_set)

nendsWith(value_set)

nstartsWith(value_set)

startsWith(value_set)

inclu(value_set)

ninclu(value_set)
```

## Arguments

- value_set:

  the parameter set for the operator

## Value

a list with a single named element (operator name) to be handed over to
the filter.

## Details

Also see the
[susoop_str](https://michael-cw.github.io/susographql/reference/susoop_str.md)
selector list, which allows you, to just select the function from a
named list.

## Functions

- `contains()`: contains

- `endsWith()`: ends with

- `ncontains()`: not contains

- `nendsWith()`: not ends with

- `nstartsWith()`: not starts with

- `startsWith()`: starts with

- `inclu()`: in

- `ninclu()`: not in

## Examples

``` r

# set filter so that the string contains "area"
contains("area")
#> $contains
#> [1] "area"
#> 

# set filter to string ending with .shp
endsWith(".shp")
#> $endsWith
#> [1] ".shp"
#> 
```
