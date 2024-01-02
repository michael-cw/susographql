#' Upload map to server
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param path_to_zip path to zip file
#'
#' @export


suso_gql_uploadmap <- function(endpoint = NULL,
                               workspace = NULL,
                               user = NULL,
                               token = NULL,
                               password = NULL,
                               path_to_zip = NULL) {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # Check for essential variables
  .check_basics(token, endpoint, user, password)

  stopifnot(
    !is.null(path_to_zip)
  )

  # define the mutation
  mutation<-glue::glue('{"query":"mutation(\\
  $file: Upload! $workspace: String) \\
  {uploadMap(file: $file workspace: $workspace) \\
  {xMaxVal\\
   yMaxVal\\
   xMinVal\\
   yMinVal\\
   wkid\\
   fileName\\
   size\\
   maxScale\\
   minScale\\
   shapeType\\
   importDateUtc\\
   uploadedBy\\
   users { userName }}}",\\
  "variables":{"file":null  "workspace": "<<workspace>>"}}',
                       .open = "<<", .close = ">>")

  # build the url with the form
  url<-httr2::request(endpoint) |>
    httr2::req_body_multipart(
      `operations` = mutation,
      `map` = '{ "0": ["variables.file"] }',
      `0` = curl::form_file(path_to_zip, type = "application/zip")
    ) |>
    # !!  IMPORTANT HEADER FOR MAPUPLOAD
    httr2::req_headers(`GraphQL-Preflight` = 1) |>
    httr2::req_method("POST") |>
    httr2::req_user_agent("r api v2") |>
    httr2::req_auth_basic(user, password) |>
    httr2::req_retry(
      max_tries = 3
    )

  # perform the request
  result<-.perform_request(url)

  return(result$data)
}
