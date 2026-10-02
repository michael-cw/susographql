# Get all assignments

Allows the user to retrieve filtered or unfiltered assignment data.

## Usage

``` r
suso_gql_assignments(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  archived = FALSE,
  id = NULL,
  questionnaireId = NULL,
  version = NULL,
  responsibleId = NULL,
  webMode = FALSE,
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

- archived:

  Boolean if the assignment is archived or not

- id:

  Assignment ID

- questionnaireId:

  Questionnaire ID

- version:

  Questionnaire version

- responsibleId:

  ID of the person currently responsible for the assignment

- webMode:

  Boolean for webmode

- take:

  take the specified integer numeber of assignments

- skip:

  skip the first integer number of assignments

## Value

if successfull, returns a list with the (filtered) responses

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Get all assignments without filter

suso_gql_assignments(endpoint = ep, user = usr,
password = pass, workspace = ws)

# Select assignment with id 25
suso_gql_assignments(endpoint = ep, user = usr,
password = pass, workspace = ws, id = 25)

# Select assignment excluding id 25, by using susoop_numeric
suso_gql_assignments(endpoint = ep, user = usr,
password = pass, workspace = ws, id = susoop_num$neq(25))

}
```
