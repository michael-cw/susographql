# Delete map

Allows the user to delete a map.

## Usage

``` r
suso_gql_deletemap(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  fileName = NULL
)
```

## Arguments

- endpoint:

  GraphQL endpoint of your server

- workspace:

  Server Workspace, if NULL uses default

- user:

  your API username

- password:

  your API user password

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored

- fileName:

  the name of the map file on the server

## Value

if successfull, returns a list with the executed mutation

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Delete map seg_168_ALL.tif

suso_gql_deletemap(endpoint = ep, user = usr,
password = pass, workspace = ws,
fileName = "seg_168_ALL.tif")

}
```
