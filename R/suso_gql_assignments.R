#' Get all assignments
#'
#' Allows the user to retrieve filtered or unfiltered assignment data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param archived Boolean if the assignment is archived or not
#' @param id Assignment ID
#' @param questionnaireId Questionnaire ID
#' @param version Questionnaire version
#' @param responsibleId ID of the person currently responsible for the assignment
#' @param webMode Boolean for webmode
#' @param take take the specified integer numeber of assignments
#' @param skip skip the first integer number of assignments
#'
#' @export


suso_gql_assignments <- function(endpoint = NULL,
                                 workspace = NULL,
                                 user = NULL,
                                 password = NULL,
                                 archived = FALSE,
                                 id = NULL,
                                 questionnaireId = NULL,
                                 version = NULL,
                                 responsibleId = NULL,
                                 webMode = FALSE,
                                 take = NULL,
                                 skip = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(endpoint)
  )

  # define your query
  query <- sprintf('
      query($workspace: String $where: AssignmentsFilter $take: Int $skip: Int){
          assignments(workspace: $workspace where: $where take: $take skip: $skip) {
                            totalCount
                            filteredCount
                            nodes {
                              id
                              email
                              archived
                              createdAtUtc
                              interviewsNeeded
                              receivedByTabletAtUtc
                              responsibleId
                              webMode
                              %s
                            }
                      }
                    }
  ', calenderEvent)
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

  if (!is.null(archived)) {
    variables$where$archived$eq <- archived
  }
  if (!is.null(id)) {
    variables$where$id$eq <- id
  }
  if (!is.null(questionnaireId) && !is.null(version)) {
    variables$where$questionnaireId$id$eq <- questionnaireId
    variables$where$questionnaireId$version$eq <- version
  }
  if (!is.null(responsibleId)) {
    variables$where$responsibleId$eq <- responsibleId
  }
  if (!is.null(webMode)) {
    variables$where$webMode$eq <- webMode
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
