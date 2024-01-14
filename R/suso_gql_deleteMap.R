#' Delete map
#'
#' Allows the user to delete a map.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password your API user password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param fileName the name of the map file on the server
#'
#' @return if successfull, returns a list with the executed mutation
#'
#' @examplesIf suso_gql_pwcheck()==200
#' ## Requires Survey Solutions Server and API credentials
#'
#' # Delete map seg_168_ALL.tif
#'
#' suso_gql_deletemap(endpoint = ep, user = usr,
#' password = pass, workspace = ws,
#' fileName = "seg_168_ALL.tif")
#'
#'
#' @export




suso_gql_deletemap <- function(endpoint = NULL,
                               workspace = NULL,
                               user = NULL,
                               password = NULL,
                               token = NULL,
                               fileName = NULL) {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)

  # define your query
  query <- sprintf('
          mutation($workspace: String $fileName: String!) {
                deleteMap(workspace: $workspace fileName: $fileName) {
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
