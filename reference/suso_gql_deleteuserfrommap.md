# Delete user from map

Allows the user to delete an interviewer from a map to be used in CAPI
data collection.

## Usage

``` r
suso_gql_deleteuserfrommap(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  token = NULL,
  password = NULL,
  fileName = NULL,
  userName = NULL
)
```

## Arguments

- endpoint:

  GraphQL endpoint of your server

- workspace:

  Server Workspace, if NULL uses default

- user:

  your API username

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored

- password:

  your API user password

- fileName:

  the name of the map file on the server

- userName:

  the name of the interviewer to whom the map will be assigned to

## Value

if successfull, returns a list with the executed mutation

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Delete map seg_168_ALL.tif from user int0073

suso_gql_deleteuserfrommap(endpoint = ep, user = usr,
password = pass, workspace = ws,
fileName = "seg_168_ALL.tif", userName = "int0073")

}
```
