#' Get all assignments
#'
#' Allows the user to retrieve filtered or unfiltered assignment data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param archived Boolean if the assignment is archived or not
#' @param id Assignment ID
#' @param questionnaireId Questionnaire ID
#' @param version Questionnaire version
#' @param responsibleId ID of the person currently responsible for the assignment
#' @param webMode Boolean for webmode
#' @param take take the specified integer numeber of assignments
#' @param skip skip the first integer number of assignments
#'
#' @return if successfull, returns a list with the (filtered) responses
#'
#' @examplesIf suso_gql_pwcheck()==200
#' ## Requires Survey Solutions Server and API credentials
#'
#' # Get all assignments without filter
#'
#' suso_gql_assignments(endpoint = ep, user = usr,
#' password = pass, workspace = ws)
#'
#' # Select assignment with id 25 (note for \code{eq} you do not require the transformer)
#' suso_gql_assignments(endpoint = ep, user = usr,
#' password = pass, workspace = ws, id = 25)
#'
#' # Select assignment excluding id 25, by using susoop_numeric
#' suso_gql_assignments(endpoint = ep, user = usr,
#' password = pass, workspace = ws, id = susoop_num$neq(25))
#'
#'
#' @export


suso_gql_assignments <- function(endpoint = NULL,
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
                                 skip = NULL) {
  # workspace default
  workspace<-.ws_default(ws = workspace)


  # check inputs
  .check_basics(token, endpoint, user, password)


  # define your query
  query <- sprintf('
      query($workspace: String $where: AssignmentsFilter $take: Int $skip: Int){
          assignments(workspace: $workspace where: $where take: $take skip: $skip) {
                            totalCount
                            filteredCount
                            nodes {
                              id
                              email
                              archived
                              createdAtUtc
                              interviewsNeeded
                              receivedByTabletAtUtc
                              responsibleId
                              webMode
                              %s
                            }
                      }
                    }
  ', calenderEvent)
  ################################
  # create the variables list
  # 1. Top level
  variables <- list()
  if (!is.null(workspace)) {
    variables$workspace <- workspace
  }
  # 2. Filter
  # 2.1 Filter default is NULL
  variables$where<-NULL

  if (!is.null(archived)) {
    archived<-.checkInput(archived)
    variables$where$archived <- archived
  }
  if (!is.null(id)) {
    id<-.checkInput(id)
    variables$where$id <- id
  }
  if (!is.null(questionnaireId) && !is.null(version)) {
    questionnaireId<-.checkInput(questionnaireId)
    version<-.checkInput(version)
    variables$where$questionnaireId$id <- questionnaireId
    variables$where$questionnaireId$version <- version
  }
  if (!is.null(responsibleId)) {
    responsibleId<-.checkInput(responsibleId)
    variables$where$responsibleId <- responsibleId
  }
  if (!is.null(webMode)) {
    webMode<-.checkInput(webMode)
    variables$where$webMode <- webMode
  }

  if (!is.null(take)) {
    stopifnot(
      (take%%1==0)
    )
    variables$take <- take
  }

  if (!is.null(skip)) {
    stopifnot(
      (skip%%1==0)
    )
    variables$skip <- skip
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
