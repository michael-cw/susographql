#' Upload map to server
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param path_to_zip path to zip file
#'
#' @export


suso_gql_uploadmap <- function(endpoint = NULL,
                                  workspace = NULL,
                                  user = NULL,
                                  password = NULL,
                                  path_to_zip = NULL) {
  # Check parameters
  stopifnot(
    !is.null(endpoint),
    !is.null(workspace)
  )

  # define the mutation
  mutation<-sprintf('{"query":"mutation($file: Upload! $workspace: String) {uploadMap(file: $file workspace: $workspace) {xMaxVal yMaxVal xMinVal yMinVal wkid fileName size maxScale minScale shapeType importDateUtc uploadedBy users { userName }}}","variables":{"file":null  "workspace": "%s"}}',
                    workspace)

  # create the form
  files = list(
    `operations` = mutation,
    `map` = '{ "0": ["variables.file"] }',
    `0` = httr::upload_file(path_to_zip, type = "application/zip")
  )

  # send the post
  response <- httr::POST(endpoint,
                         body = files,
                         encode = "multipart",
                         #headers = c("Content-Type" = "application/json"),
                         #httr::content_type_json(),
                         httr::user_agent("r api v2"),
                         httr::accept_json(),
                         httr::authenticate(user, password, type = "basic"))
  # check the status code
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }

  result <- httr::content(response, "text", encoding = "UTF-8")
  result<-jsonlite::fromJSON(result)
  return(result$data)
}
