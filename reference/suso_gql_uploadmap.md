# Upload map to server

Allows the user to upload 1 or several zip files with base maps and
shapefiles to the server.

## Usage

``` r
suso_gql_uploadmap(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  token = NULL,
  password = NULL,
  path_to_zip = NULL,
  usePar = FALSE,
  n_par = 10
)
```

## Arguments

- endpoint:

  GraphQL endpoint of your server.

- workspace:

  Server Workspace, if NULL uses default.

- user:

  your API username.

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored.

- password:

  API password.

- path_to_zip:

  path to a single zip file or path to a directory of zip files if
  `usePar=TRUE`.

- usePar:

  if TRUE the requests will be performed in parallel EXPERIMENTAL!

- n_par:

  number of parallel requests, required if `usePar = TRUE` EXPERIMENTAL!

## Value

a list with details on successfully processed maps, if `usePar = TRUE`,
the node element of that list will be a data.table with all the
successfully processed uploads.

## Details

If `usePar = TRUE` a path to a directory needs to be provided containing
the zipped map files. This feature may be useful when uploading a large
number of maps, however be careful, with the number of parallel requests
and the size of each zip file, as this may overload the server. Initial
testing and gradually increasing the `n_par` parameter is therefore
strongly recommended.

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Upload a single zip file containing maps

suso_gql_uploadmap(endpoint = ep, user = usr,
password = pass, workspace = ws,
path_to_zip = "./dev/shapes_for_test.zip", usePar = FALSE)

# Upload a directory with zip files containing maps

suso_gql_uploadmap(endpoint = ep, user = usr,
password = pass, workspace = ws,
path_to_zip = "./dev/allzipforupload/", usePar = TRUE)
}
```
