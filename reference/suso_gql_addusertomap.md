# Assigns a map to a user

Allows the user to assign a map to an interviewer to be used in CAPI
data collection.

## Usage

``` r
suso_gql_addusertomap(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
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

- password:

  your API user password

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored

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

# Assign map seg_168_ALL.tif to user int0073

suso_gql_addusertomap(endpoint = ep, user = usr,
password = pass, workspace = ws,
fileName = "seg_168_ALL.tif", userName = "int0073")

}
```
