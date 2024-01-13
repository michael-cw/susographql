#' Get all maps
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param fileName name of the map on the server
#' @param importDateUtc Import date
#' @param size Size of the map
#' @param users Users to whom the maps are assigned
#' @param sortby_filename sort maps by file name, either ASC for ascending or DESC for descending
#' @param sortby_importeddateutc sort maps by import date in utc, either ASC for ascending or DESC for descending
#' @param sortby_size sort by map size, either ASC for ascending or DESC for descending
#' @param take take the specified integer numeber of maps
#' @param skip skip the first integer number of maps
#'
#' @export

suso_gql_maps <- function(endpoint = NULL,
                          workspace = NULL,
                          user = NULL,
                          password = NULL,
                          token = NULL,
                          fileName = NULL,
                          importDateUtc = NULL,
                          size = NULL,
                          users = NULL,
                          sortby_filename = NULL,
                          sortby_importeddateutc = NULL,
                          sortby_size = NULL,
                          take = NULL,
                          skip = NULL) {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)


  # define your query
  query <- sprintf('
      query($workspace: String $where: MapsFilter $order: [MapsSort!] $take: Int $skip: Int) {
        maps(workspace: $workspace where: $where order: $order take: $take skip: $skip) {
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
    size<-.checkInput(size)
    variables$where$size <- size
  }

  ## User must be updated!!
  if (!is.null(users)) {
    variables$where$users$all$userName$eq <- users
  }

  ## Sort
  if (!is.null(sortby_filename)) {
    stopifnot(
      sortby_filename %in% c("ASC", "DESC")
    )
    variables$order$fileName <- sortby_filename
  }

  if (!is.null(sortby_importeddateutc)) {
    stopifnot(
      sortby_importeddateutc %in% c("ASC", "DESC")
    )
    variables$order$importDateUtc <- sortby_importeddateutc
  }

  if (!is.null(sortby_size)) {
    stopifnot(
      sortby_size %in% c("ASC", "DESC")
    )
    variables$order$size <- sortby_size
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
