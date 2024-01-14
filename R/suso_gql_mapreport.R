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
#' @return if successfull, returns a list with the (filtered) responses
#'
#' @examplesIf suso_gql_pwcheck()==200
#' ## Requires Survey Solutions Server and API credentials
#'
#' questlist<-suso_gql_questionnaires(
#'     endpoint = ep, user = usr,
#'     password = pass,
#'     workspace = "primary")
#'
#' id<-questlist$questionnaires$nodes$questionnaireId[1]
#'  v<-questlist$questionnaires$nodes$version[1]
#'
#' # Get map report for GPS question start_location
#'
#' suso_gql_mapreport(endpoint = ep, user = usr,
#' password = pass, workspace = ws,
#' questionnaireId = id, questionnaireVersion = v, variable = "start_location")
#'
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
  variables$west<-as.double(west)
  variables$east<-as.double(east)
  variables$north<-as.double(north)
  variables$south<-as.double(south)


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
  if (!is.null(clientKey)) {
    clientKey<-.checkInput(clientKey)
    variables$where$clientKey <- clientKey
  }

  # DOES THIS MAKE SENSE HERE, SINCE QUESTIONNAIRE IS ALREADY PARAMETER
  # if (!is.null(questionnaireId) && !is.null(questionnaireVersion)) {
  #   questionnaireId<-.checkInput(questionnaireId)
  #   variables$where$questionnaireId <- questionnaireId
  #
  #   version<-.checkInput(version)
  #   variables$where$questionnaireVersion <- questionnaireVersion
  # }


  if (!is.null(responsibleName)) {
    # uses lower case name in all cases
    responsibleName <- tolower(responsibleName)
    responsibleName<-.checkInput(responsibleName)
    variables$where$responsibleNameLowerCase <- responsibleName
  }
  if (!is.null(supervisorName)) {
    supervisorName<-tolower(supervisorName)
    supervisorName<-.checkInput(supervisorName)
    variables$where$supervisorNameLowerCase <- supervisorName
  }
  if (!is.null(errorsCount)) {
    errorsCount<-.checkInput(errorsCount)
    variables$where$errorsCount <- errorsCount
  }
  if (!is.null(interviewMode)) {
    interviewMode<-.checkInput(interviewMode)
    variables$where$interviewMode <- interviewMode
  }

  if (!is.null(notAnsweredCount)) {
    notAnsweredCount<-.checkInput(notAnsweredCount)
    variables$where$notAnsweredCount <- notAnsweredCount
  }

  if (!is.null(status)) {
    status<-.checkInput(status)
    variables$where$status <- status
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
