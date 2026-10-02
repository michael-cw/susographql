# Add a calendar event to an Assignment

Add a calendar event to an assignment

## Usage

``` r
suso_gql_addassignmentcalendarevent(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  assignmentId = NULL,
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

- assignmentId:

  assignment id

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

# Create a calendar event for assignment 208 on 2024-01-16 01:41:14,
# in timezone Chile/Continental

suso_gql_addassignmentcalendarevent(endpoint = ep, user = usr,
password = pass, workspace = ws, assignmentId = 208, newStart = "2024-01-16 01:41:14",
startTimezone = "Chile/Continental", comment = "Lets Go!")
}
```
