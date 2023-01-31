#' Get all Questions in a Questionnaires
#'
#' Allows the user to retrieve all questions in a questionniare.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param id Questionnaire ID
#' @param version Questionnaire version
#' @param variable Get questions for a specific variable
#' @param scope Get questions for a specific scope
#' @param identifying If TRUE only identifying questions are exported
#' @param title the text of the questions
#' @param includedInReportingAtUtc time of the last reporting
#'
#' @export






suso_gql_questionnaireitems <- function(endpoint = NULL,
                               workspace = NULL,
                               user = NULL,
                               password = NULL,
                               id = NULL,
                               version = NULL,
                               variable = NULL,
                               scope = NULL,
                               identifying = NULL,
                               title = NULL,
                               includedInReportingAtUtc = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(endpoint)
  )

  # define your query
  query <- sprintf('
          query($workspace: String $id: UUID! $version: Long! $language: String $where: QuestionnaireItemsFilter) {
              questionnaireItems (workspace: $workspace id: $id version: $version language: $language where: $where) {
                title
                variable
                scope
                label
                type
                variableType
                identifying
                includedInReportingAtUtc
                options {
                   parentValue
                   title
                   value
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

  if (!is.null(id)) {
    variables$id <- id
  }

  if (!is.null(version)) {
    variables$version <- version
  }

  # 2. Filter
  # 2.1 Filter default is NULL
  variables$where<-NULL

  if (!is.null(variable)) {
    variables$where$variable$eq <- variable
  }
  if (!is.null(scope)) {
    variables$where$scope$eq <- scope
  }
  if (!is.null(identifying)) {
    variables$where$identifying$eq <- identifying
  }
  if (!is.null(title)) {
    variables$where$title$eq <- identifying
  }
  if (!is.null(includedInReportingAtUtc)) {
    variables$where$includedInReportingAtUtc$eq <- identifying
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
