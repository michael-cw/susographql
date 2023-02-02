#' Get all Questionnaires
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param id Questionnaire ID
#' @param version Questionnaire version
#' @param take take the specified integer numeber of questionnaires
#' @param skip skip the first integer number of questionnaires
#'
#' @export






suso_gql_questionnaires <- function(endpoint = NULL,
                                 workspace = NULL,
                                 user = NULL,
                                 password = NULL,
                                 id = NULL,
                                 version = NULL,
                                 take = NULL,
                                 skip = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(endpoint)
  )

  # define your query
  query <- sprintf('
          query($workspace: String $id: UUID $version: Long $take: Int $skip: Int){
            questionnaires(workspace: $workspace id: $id version: $version take: $take skip: $skip){
              totalCount
              filteredCount
              nodes {
                variable
                questionnaireId
                version
                id
                title
                defaultLanguageName
                translations {
                  name
                  id
                }
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
