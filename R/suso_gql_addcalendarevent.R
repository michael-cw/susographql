#' Add a calendar event to an Assignment
#'
#' Add a calendar event to an assignment
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password your API user password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param assignmentId assignment id
#' @param comment a comment string
#' @param newStart new start date, format must be: \code{2024-01-16 01:41:14}
#' @param startTimezone time zone of the tablet device, use \code{\link[base]{OlsonNames}}
#'
#'
#' @export




suso_gql_addassignmentcalendarevent <- function(endpoint = NULL,
                                  workspace = NULL,
                                  user = NULL,
                                  password = NULL,
                                  token = NULL,
                                  assignmentId = NULL,
                                  comment = NULL,
                                  newStart = NULL,
                                  startTimezone = "UTC") {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)

  if(is.null(assignmentId) || length(assignmentId)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE assignmentId provided."))
  }

  if(is.null(newStart) || length(newStart)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE newStart provided."))
  }

  if(is.null(startTimezone) || length(startTimezone)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE newStart provided."))
  }



  # define your query
  query <- sprintf('
          mutation($workspace: String! $assignmentId: Int! $startTimezone: String! $newStart: DateTime! $comment: String) {
                addAssignmentCalendarEvent(workspace: $workspace assignmentId: $assignmentId startTimezone: $startTimezone newStart: $newStart comment: $comment) {
                  assignmentId
                  comment
                  creatorUserId
                  interviewId
                  interviewKey
                  isCompleted
                  publicKey
                  startTimezone
                  startUtc
                  updateDateUtc
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

  if (!is.null(assignmentId)) {
    variables$assignmentId <- as.integer(assignmentId)
  }

  if (!is.null(startTimezone)) {
    variables$startTimezone <- startTimezone
  }

  if (!is.null(newStart)) {
    newStart<-lubridate::format_ISO8601(as.POSIXct(newStart, tz = startTimezone), usetz = T)
    variables$newStart <- newStart
  }

  if (!is.null(comment)) {
    variables$comment <- comment
  }


  # create the body of the request
  body <- list(query = query)
  if (!is.null(variables)) {
    body$variables <- variables
  }
  #return(body)

  # build the url
  url<-.baseurl_baseauth(endpoint, body, user, password, retry = 3)

  # perform the request
  result<-.perform_request(url)

  return(result$data)

}




#' Add a calendar event to an Interview
#'
#' Add a calendar event to an Interview
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password your API user password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param interviewId the interviewId
#' @param comment a comment string
#' @param newStart new start date, format must be: \code{2024-01-16 01:41:14}
#' @param startTimezone time zone of the tablet device, use \code{\link[base]{OlsonNames}}
#'
#' @export




suso_gql_addinterviewcalendarevent <- function(endpoint = NULL,
                                                workspace = NULL,
                                                user = NULL,
                                                password = NULL,
                                                token = NULL,
                                                interviewId = NULL,
                                                comment = NULL,
                                                newStart = NULL,
                                                startTimezone = "UTC") {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)

  if(is.null(interviewId) || length(interviewId)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE interviewId provided."))
  }

  if(is.null(newStart) || length(newStart)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE newStart provided."))
  }

  if(is.null(startTimezone) || length(startTimezone)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE newStart provided."))
  }



  # define your query
  query <- sprintf('
          mutation($workspace: String! $interviewId: UUID! $startTimezone: String! $newStart: DateTime! $comment: String) {
                addInterviewCalendarEvent(workspace: $workspace interviewId: $interviewId startTimezone: $startTimezone newStart: $newStart comment: $comment) {
                  assignmentId
                  comment
                  creatorUserId
                  interviewId
                  interviewKey
                  isCompleted
                  publicKey
                  startTimezone
                  startUtc
                  updateDateUtc
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

  if (!is.null(interviewId)) {
    variables$interviewId <- interviewId
  }

  if (!is.null(startTimezone)) {
    variables$startTimezone <- startTimezone
  }

  if (!is.null(newStart)) {
    newStart<-lubridate::format_ISO8601(as.POSIXct(newStart))
    variables$newStart <- newStart
  }

  if (!is.null(comment)) {
    variables$comment <- comment
  }


  # create the body of the request
  body <- list(query = query)
  if (!is.null(variables)) {
    body$variables <- variables
  }
  #return(body)

  # build the url
  url<-.baseurl_baseauth(endpoint, body, user, password, retry = 3)

  # perform the request
  result<-.perform_request(url)

  return(result$data)

}
