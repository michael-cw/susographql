# Get all Questionnaires

Get all Questionnaires in the workspace or only a selected one/versions.

## Usage

``` r
suso_gql_questionnaires(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  id = NULL,
  version = NULL,
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

- id:

  Questionnaire ID

- version:

  Questionnaire version

- take:

  take the specified integer numeber of questionnaires

- skip:

  skip the first integer number of questionnaires

## Value

if successfull, returns a list with the responses

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Get all questionnaires in workspace ws

suso_gql_questionnaires(endpoint = ep, user = usr,
password = pass, workspace = ws)
}
```
