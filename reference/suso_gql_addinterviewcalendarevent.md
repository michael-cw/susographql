# Add a calendar event to an Interview

Add a calendar event to an Interview

## Usage

``` r
suso_gql_addinterviewcalendarevent(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  interviewId = NULL,
  comment = NULL,
  newStart = NULL,
  startTimezone = "UTC"
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

- interviewId:

  the interviewId

- comment:

  a comment string

- newStart:

  new start date, format must be: `2024-01-16 01:41:14`

- startTimezone:

  time zone of the tablet device, use
  [`OlsonNames`](https://rdrr.io/r/base/timezones.html)

## Value

if successfull, returns a list with the executed mutation

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Create a calendar event for a selected interview on 2024-01-16 01:41:14,
# in timezone UTC

suso_gql_addinterviewcalendarevent(endpoint = ep, user = usr,
password = pass, workspace = ws, interviewId = "bac4d15253b541dd86a003dc7f854e35",
newStart = "2024-01-16 01:41:14",
startTimezone = "UTC", comment = "Lets Go!")
}
```
