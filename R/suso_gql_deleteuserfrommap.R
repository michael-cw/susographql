#' Delete user from map
#'
#' Allows the user to delete an interviewer from a map to be used in CAPI data collection.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password your API user password
#' @param fileName the name of the map file on the server
#' @param userName the name of the interviewer to whom the map will be assigned to
#'
#' @export




suso_gql_deleteuserfrommap <- function(endpoint = NULL,
                                  workspace = NULL,
                                  user = NULL,
                                  password = NULL,
                                  fileName = NULL,
                                  userName = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(endpoint)
  )

  # define your query
  query <- sprintf('
          mutation($workspace: String $fileName: String! $userName: String!) {
                deleteUserFromMap(workspace: $workspace fileName: $fileName userName: $userName) {
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



  response <- httr::POST(endpoint, body = body,
                         encode = "json",
                         httr::content_type_json(),
                         httr::user_agent("r api v2"),
                         httr::accept_json(),
                         httr::authenticate(user, password, type = "basic"))

  # check the status code
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }

  # parse the JSON response
  result <- httr::content(response, "text", encoding = "UTF-8")
  result<-jsonlite::fromJSON(result)
  return(result$data)
}
