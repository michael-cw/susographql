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
    # when 500 return, retry
    httr2::req_retry(is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503), max_tries = 2)
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

#' helper functions  to check basics
#'
#' @keywords internal
#' @noRd
#'
.check_basics<- function(token, server, apiUser, apiPass) {
  withr::with_options(
    list(rlang_backtrace_on_error = "none"),
    {
      if(is.null(token)){
        if(is.null(server) | is.null(apiUser) | is.null(apiPass)){
          cli::cli_abort(c("x" = "Please provide either a token with endpoint, or apiUser and apiPass (with endpoint)."))
        }
      } else {
        if(is.null(server)){
          cli::cli_abort(c("x" = "Please provide a endpoint address"))
        }
      }

      # make sure graphql in endpoint
      if(!(grepl("/graphql$", server))) {
        cli::cli_abort(c("x" = "Endpoint must include /graphql! Like: https://[myserver]/graphql"))
      }

    }
  )
}

#' helper functions  for workspace
#'
#' @keywords internal
#' @noRd
#'
.ws_default<-function(ws=NULL){
  ## workspace default
  if(!is.null(ws)) {
    # margs<-suso_getWorkspace()$Name
    # workspace<-match.arg(workspace, margs)
    ws<-ws
  } else {
    if(interactive()) cli::cli_alert_info("No workspace provided. Using primary workspace.")
    ws<-"primary"
  }
  return(ws)
}



#' request generator with json body
#'
#' @keywords internal
#' @noRd
#'
.genrequests_w_multiform<-function(i, endpoint, path_to_zip, ..., method = "POST") {
  args<-rlang::list2(...)
  req <- httr2::request(endpoint) |>
    httr2::req_body_multipart(
      `operations` = args$mutation,
      `map` = '{ "0": ["variables.file"] }',
      `0` = curl::form_file(path_to_zip[i], type = "application/zip")
    ) |>
    # !!  IMPORTANT HEADER FOR MAPUPLOAD
    httr2::req_headers(`GraphQL-Preflight` = 1) |>
    httr2::req_method(method) |>
    httr2::req_user_agent("r api v2") |>
    httr2::req_auth_basic(args$user, args$password)
  return(req)
}

#' generate lapply wit cli_progress_along/seq_along, function and arguments
#'
#' @keywords internal
#' @noRd
#'
.gen_lapply_with_progress<-function(vec, fun, stage, type ,workspace, ..., call = rlang::caller_env()) {
  force(vec)
  # force(fun)
  if(interactive()){
    pgtext<-sprintf("Creating %s for all %s in workspace %s.", stage, type, workspace)
    cli::cli_alert_info("\n {pgtext} \n")
    requests<-lapply(cli::cli_progress_along(vec, pgtext, total = length(vec), .envir = rlang::current_env()), fun, ...)
  } else {
    requests<-lapply(seq_along(vec), fun, ...)
  }
  return(requests)
}


#' response generator with json body
#'
#' @keywords internal
#' @noRd
#'
.transformresponses<-function(i ,resp, type) {
  # i. Convert to json
  respfull <-resp[[i]] |>
    httr2::resp_body_json(simplifyVector = T)
  return(respfull$data)
}

#' helper functions  for default operator
#'
#' if not operator is provided, defaults to eq (numeric and character)
#'
#' @keywords internal
#' @noRd
#'
.op_default<-function(param) {
  if(!is.list(param)) {
    param<-list(eq = param)
  }
  return(param)
}


#' @noRd
#'
.checkInput<-function(inp) {if(!is.list(inp)) inp<-susoop_num$eq(inp); return(inp)}















