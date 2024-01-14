#' Get all Questions in a Questionnaires
#'
#' Allows the user to retrieve all questions in a questionniare.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param id Questionnaire ID
#' @param version Questionnaire version
#' @param variable Get questions for a specific variable
#' @param scope Get questions for a specific scope
#' @param identifying If TRUE only identifying questions are exported
#' @param title the text of the questions
#' @param includedInReportingAtUtc time of the last reporting
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
#' v<-questlist$questionnaires$nodes$version[1]
#'
#' # Get all questions from a questionnaire/version without filter
#'
#' suso_gql_questionnaireitems(endpoint = ep, user = usr,
#'      password = pass,
#'      workspace = ws,
#'      id = id,
#'      version = v)
#'
#' # Select only identifying questions
#' suso_gql_questionnaireitems(endpoint = ep, user = usr,
#'      password = pass,
#'      workspace = ws,
#'      id = id,
#'      version = v,
#'      identifying = TRUE)
#'
#' # Select only questions which have not interviewer scope
#' suso_gql_questionnaireitems(endpoint = ep, user = usr,
#'       password = pass,
#'       workspace = ws,
#'       id = id,
#'       version = v,
#'       scope = susoop_str$neq("INTERVIEWER"))
#'
#' @export






suso_gql_questionnaireitems <- function(endpoint = NULL,
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
                               includedInReportingAtUtc = NULL) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)


  # define your query
  query <- sprintf('
          query($workspace: String $id: UUID! $version: Long! $language: String $where: QuestionnaireItemsFilter) {
              questionnaireItems (workspace: $workspace id: $id version: $version language: $language where: $where) {
                title
                variable
                scope
                label
                type
                variableType
                identifying
                includedInReportingAtUtc
                options {
                   parentValue
                   title
                   value
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

  if (!is.null(id)) {
    variables$id <- id
  }

  if (!is.null(version)) {
    variables$version <- version
  }

  # 2. Filter
  # 2.1 Filter default is NULL
  variables$where<-NULL

  if (!is.null(variable)) {
    variable<-.checkInput(variable)
    variables$where$variable <- variable
  }
  if (!is.null(scope)) {
    scope<-.checkInput(scope)
    variables$where$scope <- scope
  }
  if (!is.null(identifying)) {
    identifying<-.checkInput(identifying)
    variables$where$identifying <- identifying
  }
  if (!is.null(title)) {
    title<-.checkInput(title)
    variables$where$title <- title
  }
  if (!is.null(includedInReportingAtUtc)) {
    includedInReportingAtUtc<-.checkInput(includedInReportingAtUtc)
    variables$where$includedInReportingAtUtc <- includedInReportingAtUtc
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
