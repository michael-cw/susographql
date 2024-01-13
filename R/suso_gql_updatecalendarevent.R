#' Update a calendar event
#'
#' Update a calendar event
#'
#' @param endpoint GraphQL endpoint of your server.
#' @param workspace Server Workspace, if NULL uses default.
#' @param user your API username.
#' @param password your API user password.
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored.
#' @param publicKey the publicKey of the calendar event to be updated.
#' @param comment a comment string.
#' @param newStart new start date, format must be: \code{2024-01-16 01:41:14}.
#' @param startTimezone time zone of the tablet device, use \code{\link[base]{OlsonNames}}.
#'
#' @export




suso_gql_updatecalendarevent <- function(endpoint = NULL,
                                         workspace = NULL,
                                         user = NULL,
                                         password = NULL,
                                         token = NULL,
                                         publicKey = NULL,
                                         comment = NULL,
                                         newStart = NULL,
                                         startTimezone = "UTC") {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)

  if(is.null(publicKey) || length(publicKey)!=1) {
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
          mutation($workspace: String! $publicKey: UUID! $startTimezone: String! $newStart: DateTime! $comment: String) {
                updateCalendarEvent(workspace: $workspace publicKey: $publicKey startTimezone: $startTimezone newStart: $newStart comment: $comment) {
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

  if (!is.null(publicKey)) {
    variables$publicKey <- publicKey
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


#' Delete a calendar event
#'
#' Delete a calendar event
#'
#' @param endpoint GraphQL endpoint of your server.
#' @param workspace Server Workspace, if NULL uses default.
#' @param user your API username.
#' @param password your API user password.
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored.
#' @param publicKey the publicKey of the calendar event to be deleted.
#'
#' @export




suso_gql_deletecalendarevent <- function(endpoint = NULL,
                                         workspace = NULL,
                                         user = NULL,
                                         password = NULL,
                                         token = NULL,
                                         publicKey = NULL) {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)

  if(is.null(publicKey) || length(publicKey)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE interviewId provided."))
  }


  # define your query
  query <- sprintf('
          mutation($workspace: String! $publicKey: UUID!) {
                deleteCalendarEvent(workspace: $workspace publicKey: $publicKey) {
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

  if (!is.null(publicKey)) {
    variables$publicKey <- publicKey
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
