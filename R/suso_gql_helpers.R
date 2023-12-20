#' helper functions  for error handling
#'
#' @keywords internal
#' @noRd
#'

.http_error_handler_gql <- function(error_condition, type = "ass") {
  # Use a switch or if-else to handle different types of errors
  error_type <- class(error_condition)[1]
  # error messages
  msg404<-switch(type,
                 "ass" = c("x" = "Not found!"),
                 "usr" = c("x" = "User not found"),
                 "exp" = c("x" = "Export process was not found")
  )
  msg406<-switch(type,
                 "ass" = c("x" = "Not acceptable!"),
                 "usr" = c("x" = "User is not an interviewer or supervisor")
  )

  msg400<-switch(type,
                 "ass" = c("x" = "Bad request!"),
                 "usr" = c("x" = "User not found"),
                 "exp" = c("x" = "Request is malformed/Export file was not generated yet")
  )

  msg401<-c("x"="Unauthorized/User not authorized.")


  msg403<-c("x" = "Forbidden")

  msg409<-switch(type,
                 "usr" = c("x" = "Conflict!")
  )

  withr::with_options(
    list(rlang_backtrace_on_error = "none"),
    switch(error_type,
           "httr2_http_404" = {
             cli::cli_abort(
               message = msg404,
               call = NULL,
             )
           },
           "httr2_http_406" = {
             cli::cli_abort(
               message = msg406,
               call = NULL,
             )
           },
           "httr2_http_400" = {
             cli::cli_abort(
               message = msg400,
               call = NULL,
             )
           },
           "httr2_http_401" = {
             cli::cli_abort(
               message = msg401,
               call = NULL,
             )
           },
           "httr2_http_403" = {
             cli::cli_abort(
               message = msg403,
               call = NULL,
             )
           },
           "httr2_http_409" = {
             cli::cli_abort(
               message = msg409,
               call = NULL,
             )
           },
           # Default case if the error type is not handled above
           cli::cli_abort(
             message = "The following other error occured:",
             call = NULL,
             parent = error_condition,
             .internal = T
           )
    )
  )
}


#' helper functions  for building the request
#'
#' @keywords internal
#' @noRd
#'

.baseurl_baseauth<-function(endpoint, body, user, password, retry = 5){
  # Build the URL
  url<-httr2::request(endpoint) |>
    httr2::req_body_json(body) |>
    httr2::req_method("POST") |>
    httr2::req_user_agent("r api v2") |>
    httr2::req_auth_basic(user, password) |>
    httr2::req_retry(
      max_tries = retry
    )
  return(url)
}

#' helper functions  for performing the request
#'
#' @keywords internal
#' @noRd
#'

.perform_request<-function(url) {
  tryCatch(
    { resp<-url |>
      httr2::req_perform()

    # get the response data
    if(httr2::resp_has_body(resp)){
      # get body by content type
      if(httr2::resp_content_type(resp) == "application/graphql-response+json") {
        result<-httr2::resp_body_json(resp, simplifyVector = TRUE)
      }
    } else {
      result<-list(data=NULL)
    }
    },
    error = .http_error_handler_gql
  )

  return(result)
}


