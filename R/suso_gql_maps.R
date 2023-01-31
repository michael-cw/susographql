#' Get all maps
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param fileName name of the map on the server
#' @param importDateUtc Import date
#' @param size Size of the map
#' @param users Users to whom the maps are assigned
#'
#' @export

suso_gql_maps <- function(endpoint = NULL,
                                 workspace = NULL,
                                 user = NULL,
                                 password = NULL,
                                 fileName = NULL,
                                 importDateUtc = NULL,
                                 size = NULL,
                                 users = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(endpoint)
  )

  # define your query
  query <- sprintf('
      query($workspace: String $where: MapsFilter){
        maps(workspace: $workspace where: $where) {
              totalCount
              filteredCount
              nodes {
                fileName
                maxScale
                minScale
                shapeType
                shapesCount
                size
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
      }
  ')
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

  if (!is.null(fileName)) {
    variables$where$fileName$eq <- fileName
  }
  if (!is.null(importDateUtc)) {
    variables$where$importDateUtc$eq <- importDateUtc
  }

  if (!is.null(size)) {
    variables$where$size$neq <- size
  }

  ## User must be updated!!
  if (!is.null(users)) {
    variables$where$users$all$userName$eq <- users
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
