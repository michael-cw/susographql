# Utility function to check if credentials are correct

This function returns a 200 status code if credentials are correct and a
400 code otherwise.

## Usage

``` r
suso_gql_pwcheck(
  endpoint = NULL,
  user = NULL,
  password = NULL,
  workspace = NULL,
  token = NULL
)
```

## Arguments

- endpoint:

  Survey Solutions GraphQl

- user:

  API user

- password:

  API password

- workspace:

  endpoint workspace Name, if nothing provided, defaults to primary

- token:

  If Survey Solutions endpoint token is provided *user* and *password*
  will be ignored

## Value

200 code if correct, 400 if incorrect.

## Examples

``` r


# With default credentials returns 400
suso_gql_pwcheck()
#> [1] 400


```
