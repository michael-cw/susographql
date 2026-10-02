# Get all maps

Allows the user to retrieve filtered or unfiltered map data.

## Usage

``` r
suso_gql_maps(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  fileName = NULL,
  importDateUtc = NULL,
  size = NULL,
  userName = NULL,
  sortby_filename = NULL,
  sortby_importeddateutc = NULL,
  sortby_size = NULL,
  take = NULL,
  skip = NULL
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

  API password

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored

- fileName:

  name of the map on the server

- importDateUtc:

  Import date of map

- size:

  Size of the map

- userName:

  User name to whom the map(s) are assigned to.

- sortby_filename:

  sort maps by file name, either ASC for ascending or DESC for
  descending

- sortby_importeddateutc:

  sort maps by import date in utc, either ASC for ascending or DESC for
  descending

- sortby_size:

  sort by map size, either ASC for ascending or DESC for descending

- take:

  take the specified integer numeber of maps

- skip:

  skip the first integer number of maps

## Value

if successfull, returns a list with the (filtered) responses

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Get all maps without filter

suso_gql_maps(endpoint = ep, user = usr,
password = pass, workspace = ws)

# Get only boundary files (.shp)

suso_gql_maps(endpoint = ep, user = usr,
password = pass, workspace = ws, fileName = susoop_str$endsWith(".shp"))
}
```
