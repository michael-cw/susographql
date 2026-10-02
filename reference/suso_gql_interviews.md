# Get all interviews

Allows the user to retrieve filtered or unfiltered interview data.

## Usage

``` r
suso_gql_interviews(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  assignmentId = NULL,
  clientKey = NULL,
  createdDate = NULL,
  errorsCount = NULL,
  identifyingData = NULL,
  interviewMode = NULL,
  notAnsweredCount = NULL,
  questionnaireId = NULL,
  questionnaireVersion = NULL,
  questionnaireVariable = NULL,
  responsibleName = NULL,
  responsibleRole = NULL,
  status = NULL,
  supervisorName = NULL,
  sortby_updateDateUtc = NULL,
  sortby_notAnsweredCount = NULL,
  sortby_createdDate = NULL,
  sortby_errorsCount = NULL,
  sortby_assignmentId = NULL,
  sortby_status = NULL,
  sortby_receivedByInterviewerAtUtc = NULL,
  sortby_responsibleName = NULL,
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

- assignmentId:

  Assignment ID

- clientKey:

  Interview key

- createdDate:

  Creation data of the interview

- errorsCount:

  number of errors

- identifyingData:

  Pre-loaded identifying data

- interviewMode:

  Interview mode (CAWI or CAPI)

- notAnsweredCount:

  number of unanswered questions

- questionnaireId:

  the questionnnaire id

- questionnaireVersion:

  the questionnaire version

- questionnaireVariable:

  the variable for the questionnaire

- responsibleName:

  Name of the person responsible

- responsibleRole:

  Role of the person responsible

- status:

  of the interview

- supervisorName:

  Name of the supervisor of the responsible user

- sortby_updateDateUtc:

  sort by updated at UTC time, either ASC for ascending or DESC for
  descending

- sortby_notAnsweredCount:

  sort by errors count, either ASC for ascending or DESC for descending

- sortby_createdDate:

  sort by creation date, either ASC for ascending or DESC for descending

- sortby_errorsCount:

  sort by number of errors, either ASC for ascending or DESC for
  descending

- sortby_assignmentId:

  sort by assignmentId, either ASC for ascending or DESC for descending

- sortby_status:

  sort by interview status, either ASC for ascending or DESC for
  descending

- sortby_receivedByInterviewerAtUtc:

  sort by date the table received the interview, either ASC for
  ascending or DESC for descending

- sortby_responsibleName:

  sort by the current responsible user, either ASC for ascending or DESC
  for descending

- take:

  take the specified integer numeber of interviews

- skip:

  skip the first integer number of interviews

## Value

if successfull, returns a list with the (filtered) responses

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and API credentials

# Get all interviews without filter
suso_gql_interviews(endpoint = ep, user = usr,
password = pass, workspace = "primary")

# Select interviews which are not completed yet
suso_gql_interviews(endpoint = ep, user = usr,
password = pass, workspace = "primary", status = susoop_str$neq("COMPLETED"))

# Select interviews which have 3 or more errors
suso_gql_assignments(endpoint = ep, user = usr,
password = pass, workspace = "primary", errorsCount = susoop_num$gte(3))

# Select interviews which have less than 3 errors
suso_gql_assignments(endpoint = ep, user = usr,
password = pass, workspace = "primary", errorsCount = susoop_num$ngt(3))




}
```
