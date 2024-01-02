#' Get the map report
#'
#' Allows the user to retrieve filtered or unfiltered map report data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param questionnaireId the questionnnaire id
#' @param questionnaireVersion the questionnaire version
#' @param variable Variable(s) of interest
#' @param zoom Zoom of the map report
#' @param clientMapWidth width of the client map
#' @param west coordinates for bounding box
#' @param east coordinates for bounding box
#' @param north coordinates for bounding box
#' @param south coordinates for bounding box
#' @param assignmentId Assignment ID
#' @param clientKey Interview key
#' @param createdDate Creation data of the interview
#' @param errorsCount number of errors
#' @param identifyingData Pre-loaded identifying data
#' @param interviewMode Interview mode (CAWI or CAPI)
#' @param notAnsweredCount number of unanswered questions
#' @param questionnaireVariable the variable for the questionnaire
#' @param responsibleName Name of the person responsible
#' @param responsibleRole Role of the person responsible
#' @param status of the interview
#' @param supervisorName Name of the supervisor of the responsible user
#'
#' @export









suso_gql_mapreport <- function(endpoint = NULL,
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
) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # Check for essential variables
  .check_basics(token, endpoint, user, password)


  stopifnot(
    !is.null(questionnaireId),
    !is.null(zoom),
    !is.null(clientMapWidth),
    !is.null(west),
    !is.null(east),
    !is.null(north),
    !is.null(south)
  )


  # define query
  query <- sprintf('
    query($workspace: String $where: MapReportFilter $questionnaireId: UUID! $variable: String $zoom: Int! $clientMapWidth: Int! $questionnaireVersion: Long $west: Float! $east: Float! $north: Float! $south: Float!) {
          mapReport(workspace: $workspace where: $where variable: $variable questionnaireId: $questionnaireId zoom: $zoom clientMapWidth: $clientMapWidth questionnaireVersion: $questionnaireVersion
          west: $west east: $east north: $north south: $south) {
            report {
              totalPoint
              initialBounds {
               north
               south
               east
               west
             }
              featureCollection {
                features {
                  type
                  id
                  geometry
                  properties
                }
                type
              }
            }
          }
    }
  ')
  ################################
  # create the variables list
  # 1. Top level
  variables <- list()
  if (!is.null(workspace)) {
    variables$workspace <- workspace
  }

  variables$questionnaireId<-questionnaireId
  variables$zoom<-zoom
  variables$clientMapWidth<-clientMapWidth
  variables$west<-west
  variables$east<-east
  variables$north<-north
  variables$south<-south


  if (!is.null(questionnaireVersion)) {
    variables$questionnaireVersion <- questionnaireVersion
  }

  if (!is.null(variable)) {
    variables$variable <- variable
  }
  # 2. Filter
  # 2.1 Filter default is NULL
  variables$where<-NULL
  #
  if (!is.null(assignmentId)) {
    variables$where$interviewFilter$assignmentId$eq <- assignmentId
  }
  if (!is.null(clientKey)) {
    variables$where$interviewFilter$clientKey$eq <- clientKey
  }
  # if (!is.null(questionnaireId) && !is.null(questionnaireVersion)) {
  #   variables$where$questionnaireId$id$eq <- questionnaireId
  #   variables$where$questionnaireId$version$eq <- version
  # }

  if (!is.null(responsibleName)) {
    variables$where$interviewFilter$responsibleNameLowerCase$eq <- tolower(responsibleName)
  }
  if (!is.null(supervisorName)) {
    variables$where$interviewFilter$supervisorNameLowerCase$eq <- tolower(supervisorName)
  }
  if (!is.null(errorsCount)) {
    variables$where$interviewFilter$errorsCount$eq <- errorsCount
  }
  if (!is.null(interviewMode)) {
    variables$where$interviewFilter$interviewMode$eq <- interviewMode
  }

  if (!is.null(notAnsweredCount)) {
    variables$where$interviewFilter$notAnsweredCount$neq <- notAnsweredCount
  }

  if (!is.null(status)) {
    variables$where$status$eq <- status
  }


  # create the body of the request
  body <- list(query = query)
  if (!is.null(variables)) {
    body$variables <- variables
  }

  # build the url
  url<-.baseurl_baseauth(endpoint, body, user, password, retry = 3)

  # perform the request
  result<-.perform_request(url)

  return(result$data)
}
