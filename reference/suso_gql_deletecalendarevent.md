# Delete a calendar event

Delete a calendar event

## Usage

``` r
suso_gql_deletecalendarevent(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  publicKey = NULL
)
```

## Arguments

- endpoint:

  GraphQL endpoint of your server.

- workspace:

  Server Workspace, if NULL uses default.

- user:

  your API username.

- password:

  your API user password.

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored.

- publicKey:

  the publicKey of the calendar event to be deleted.

## Value

if successfull, returns a list with the executed mutation

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Delete a calendar event for a specific event
# in timezone Chile/Continental

suso_gql_deletecalendarevent(endpoint = ep, user = usr,
password = pass, workspace = ws,
publicKey = "ea8b49c0-0d16-4fec-9029-72278e94a233")
}
```
