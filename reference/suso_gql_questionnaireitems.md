# Get all Questions in a Questionnaires

Allows the user to retrieve all questions in a questionniare.

## Usage

``` r
suso_gql_questionnaireitems(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  id = NULL,
  version = NULL,
  variable = NULL,
  scope = NULL,
  identifying = NULL,
  title = NULL,
  includedInReportingAtUtc = NULL
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

- variable:

  Get questions for a specific variable

- scope:

  Get questions for a specific scope

- identifying:

  If TRUE only identifying questions are exported

- title:

  the text of the questions

- includedInReportingAtUtc:

  time of the last reporting

## Value

if successfull, returns a list with the (filtered) responses

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

questlist<-suso_gql_questionnaires(
    endpoint = ep, user = usr,
    password = pass,
    workspace = "primary")

id<-questlist$questionnaires$nodes$questionnaireId[1]
v<-questlist$questionnaires$nodes$version[1]

# Get all questions from a questionnaire/version without filter

suso_gql_questionnaireitems(endpoint = ep, user = usr,
     password = pass,
     workspace = ws,
     id = id,
     version = v)

# Select only identifying questions
suso_gql_questionnaireitems(endpoint = ep, user = usr,
     password = pass,
     workspace = ws,
     id = id,
     version = v,
     identifying = TRUE)

# Select only questions which have not interviewer scope
suso_gql_questionnaireitems(endpoint = ep, user = usr,
      password = pass,
      workspace = ws,
      id = id,
      version = v,
      scope = susoop_str$neq("INTERVIEWER"))
}
```
