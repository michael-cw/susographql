#' Assigns a map to a user
#'
#' Allows the user to assign a map to an interviewer to be used in CAPI data collection.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password your API user password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param fileName the name of the map file on the server
#' @param userName the name of the interviewer to whom the map will be assigned to
#'
#' @return if successfull, returns a list with the executed mutation
#'
#' @examplesIf suso_gql_pwcheck()==200
#' ## Requires Survey Solutions Server and API credentials
#'
#' # Assign map seg_168_ALL.tif to user int0073
#'
#' suso_gql_addusertomap(endpoint = ep, user = usr,
#' password = pass, workspace = ws,
#' fileName = "seg_168_ALL.tif", userName = "int0073")
#'
#'
#' @export




suso_gql_addusertomap <- function(endpoint = NULL,
                                  workspace = NULL,
                                  user = NULL,
                                  password = NULL,
                                  token = NULL,
                                  fileName = NULL,
                                  userName = NULL) {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)

  if(is.null(userName) || length(userName)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE user name provided."))
  }

  if(is.null(fileName) || length(fileName)!=1) {
    cli::cli_abort(c("x" = "No valid SINGLE fileName provided."))
  }


  # define your query
  query <- sprintf('
          mutation($workspace: String $fileName: String! $userName: String!) {
                addUserToMap(workspace: $workspace fileName: $fileName userName: $userName) {
                  fileName
                  size
                  maxScale
                  minScale
                  shapeType
                  shapesCount
                  importDateUtc
                  uploadedBy
                  users {
                    userName
                  }
                  xMaxVal
                  yMaxVal
                  xMinVal
                  yMinVal
                  wkid
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

  if (!is.null(fileName)) {
    variables$fileName <- fileName
  }

  if (!is.null(userName)) {
    variables$userName <- userName
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
