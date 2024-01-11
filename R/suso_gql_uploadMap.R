#' Upload map to server
#'
#' Allows the user to upload 1 or several zip files with base maps and shapefiles to the server.
#'
#' @param endpoint GraphQL endpoint of your server.
#' @param workspace Server Workspace, if NULL uses default.
#' @param user your API username.
#' @param password API password.
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored.
#' @param path_to_zip path to a single zip file or path to a directory of zip files if \code{usePar=TRUE}.
#' @param usePar if TRUE the requests will be performed in parallel EXPERIMENTAL!
#' @param n_par number of parallel requests, required if \code{usePar = TRUE} EXPERIMENTAL!
#'
#'
#' @return list with details on successfully processed maps.
#'
#' @export


suso_gql_uploadmap <- function(endpoint = NULL,
                               workspace = NULL,
                               user = NULL,
                               token = NULL,
                               password = NULL,
                               path_to_zip = NULL,
                               usePar = FALSE,
                               n_par = 10) {
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
  if(!usePar) {
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

    # return result data
    return(result$data)


  } else if(usePar) {
    # PERFORM REQUEST IN PARALLEL?
    # ONLY WORKS WITH DIRECTORY
    # 1. list all files in dir wi zip ext
    # 4. loop through all the zip files and create list of request
    # 5. Perform the request in parallel

    # check dir exist & list zip files
    if(dir.exists(path_to_zip)) {
      ziplist<-list.files(path_to_zip, pattern = ".zip", full.names = TRUE)
      if(length(ziplist)>0) {
        # generate the list of requests
        requests<-.genrequests_w_multiform
        requests<-.gen_lapply_with_progress(
          1:length(ziplist),
          .genrequests_w_multiform,
          "requests", "map upload", workspace,
          endpoint, ziplist, user = user, password = password, mutation = mutation
        )
        # execute requests in parallel
        if(!is.numeric(n_par)) cli::cli_abort(c("x" = "Number of parllel requests must be numeric!"))
        responses <- httr2::req_perform_parallel(
          requests,
          pool = curl::new_pool(host_con = n_par, total_con = n_par),
          on_error = "return"
        )
        if(length(responses) == 0) {
          cli::cli_alert_danger(c("x" = "No successfull requests. Please check your inputs. Did you use admin credentials?"))
          return(list(data = NULL))
        } else {
          result<-.gen_lapply_with_progress(
            1:length(responses),
            .transformresponses,
            "responses", "map upload", workspace,
            responses, "uploadMap"
          )
          # create data.table with rbindlist
          result<-data.table::rbindlist(unlist(result, recursive = F))
          return(list(data = result))
        }

      } else {
        cli::cli_abort(c("x" = "No zip archives found!"))
      }

    } else {
      cli::cli_abort(c("x" = "Directory not found, please check the file path again."))
    }

  }

}
