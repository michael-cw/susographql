# Get the map report

Allows the user to retrieve filtered or unfiltered map report data.

## Usage

``` r
suso_gql_mapreport(
  endpoint = NULL,
  workspace = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  questionnaireId = NULL,
  questionnaireVersion = NULL,
  variable = NULL,
  zoom = 1,
  clientMapWidth = 0,
  west = -180,
  east = 180,
  north = 90,
  south = -90,
  assignmentId = NULL,
  clientKey = NULL,
  createdDate = NULL,
  errorsCount = NULL,
  identifyingData = NULL,
  interviewMode = NULL,
  notAnsweredCount = NULL,
  questionnaireVariable = NULL,
  responsibleName = NULL,
  responsibleRole = NULL,
  status = NULL,
  supervisorName = NULL
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

- questionnaireId:

  the questionnnaire id

- questionnaireVersion:

  the questionnaire version

- variable:

  Variable(s) of interest

- zoom:

  Zoom of the map report

- clientMapWidth:

  width of the client map

- west:

  coordinates for bounding box

- east:

  coordinates for bounding box

- north:

  coordinates for bounding box

- south:

  coordinates for bounding box

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

# Get map report for GPS question start_location

suso_gql_mapreport(endpoint = ep, user = usr,
password = pass, workspace = ws,
questionnaireId = id, questionnaireVersion = v, variable = "start_location")

}
```
