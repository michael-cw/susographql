# Update a calendar event

Update a calendar event

## Usage

``` r
suso_gql_updatecalendarevent(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  publicKey = NULL,
  comment = NULL,
  newStart = NULL,
  startTimezone = "UTC"
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

  the publicKey of the calendar event to be updated.

- comment:

  a comment string.

- newStart:

  new start date, format must be: `2024-01-16 01:41:14`.

- startTimezone:

  time zone of the tablet device, use
  [`OlsonNames`](https://rdrr.io/r/base/timezones.html).

## Value

if successfull, returns a list with the executed mutation

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Update a calendar event for a specific event to 2024-01-18 01:41:14,
# in timezone Chile/Continental

suso_gql_updatecalendarevent(endpoint = ep, user = usr,
password = pass, workspace = ws, publicKey = "ea8b49c0-0d16-4fec-9029-72278e94a233",
newStart = "2024-01-16 01:41:14",
startTimezone = "Chile/Continental", comment = "Lets Go Again!")
}
```
