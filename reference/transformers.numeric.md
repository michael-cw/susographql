# Utility Functions for numeric operator selection

Can be used in filters ("where") for operator selection. If none is
selected, operator always defaults to \`eq()\`. The functions bellow are
valid for the corresponding inputs ComparableInt64OperationFilterInput
and ComparableNullableOfInt32OperationFilterInput.

## Usage

``` r
eq(value_set)

neq(value_set)

inbetw(value_set)

ninbetw(value_set)

gt(value_set)

ngt(value_set)

gte(value_set)

ngte(value_set)

lt(value_set)

nlt(value_set)

lte(value_set)

nlte(value_set)
```

## Arguments

- value_set:

  the parameter set for the operator

## Value

a list with a single named element (operator name) to be handed over to
the filter.

## Details

Also see the
[susoop_num](https://michael-cw.github.io/susographql/reference/susoop_num.md)
selector list, which allows you, to just select the function from a
named list.

## Functions

- `eq()`: equal

- `neq()`: not equal

- `inbetw()`: inbetween (requires numeric vector with upper and lower
  bound, i.e. c(1, 5))

- `ninbetw()`: not inbetween (requires numeric vector with upper and
  lower bound, i.e. c(1,5))

- `gt()`: greater than

- `ngt()`: not greater than

- `gte()`: greater than or equal

- `ngte()`: not greater than or equal

- `lt()`: lower than

- `nlt()`: not lower than

- `lte()`: lower than or equal

- `nlte()`: not lower than or equal

## Examples

``` r

# set filter equal to 0
eq(0)
#> $eq
#> [1] 0
#> 

# set filter to a value from 0 to 5
inbetw(c(0, 5))
#> $`in`
#> [1] 0 5
#> 
```
