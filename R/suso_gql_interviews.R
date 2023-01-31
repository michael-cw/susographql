#' Get all interviews
#'
#' Allows the user to retrieve filtered or unfiltered interview data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param assignmentId Assignment ID
#' @param clientKey Interview key
#' @param createdDate Creation data of the interview
#' @param errorsCount number of errors
#' @param identifyingData Pre-loaded identifying data
#' @param interviewMode Interview mode (CAWI or CAPI)
#' @param notAnsweredCount number of unanswered questions
#' @param questionnaireId the questionnnaire id
#' @param questionnaireVersion the questionnaire version
#' @param questionnaireVariable the variable for the questionnaire
#' @param responsibleName Name of the person responsible
#' @param responsibleRole Role of the person responsible
#' @param status of the interview
#' @param supervisorName Name of the supervisor of the responsible user
#'
#' @export



suso_gql_interviews <- function(endpoint = NULL,
                                workspace = NULL,
                                user = NULL,
                                password = NULL,
                                assignmentId = NULL,
                                clientKey = NULL,
                                createdDate = NULL,
                                errorsCount = NULL,
                                identifyingData = NULL,
                                interviewMode = NULL,
                                notAnsweredCount = NULL,
                                questionnaireId = NULL,
                                questionnaireVersion = NULL,
                                questionnaireVariable = NULL,
                                responsibleName = NULL,
                                responsibleRole = NULL,
                                status = NULL,
                                supervisorName = NULL
                                ) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(endpoint)
  )


  # define your query
  query <- sprintf('
      query($workspace: String $where: InterviewsFilter){
          interviews(workspace: $workspace where: $where) {
                            totalCount
                            filteredCount
                            nodes {
                              actionFlags
                              assignmentId
                              id
                              status
                              interviewMode
                              responsibleName
                              responsibleId
                              responsibleRole
                              supervisorName
                              wasCompleted
                              createdDate
                              key
                              clientKey
                              updateDateUtc
                              receivedByInterviewerAtUtc
                              errorsCount
                              questionnaireId
                              questionnaireVariable
                              questionnaireVersion
                              notAnsweredCount
                              cawiLink
                              %s
                              identifyingData {
                                answerValue
                                value
                                valueBool
                                valueDate
                                valueDouble
                                valueLong
                                %s
                              }
                            }
                      }
                    }
  ', calenderEvent, entity)
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

  if (!is.null(assignmentId)) {
    variables$where$assignmentId$eq <- assignmentId
  }
  if (!is.null(clientKey)) {
    variables$where$clientKey$eq <- clientKey
  }
  if (!is.null(questionnaireId) && !is.null(questionnaireVersion)) {
    variables$where$questionnaireId$id$eq <- questionnaireId
    variables$where$questionnaireId$version$eq <- version
  }
  if (!is.null(responsibleName)) {
    variables$where$responsibleNameLowerCase$eq <- tolower(responsibleName)
  }
  if (!is.null(supervisorName)) {
    variables$where$supervisorNameLowerCase$eq <- tolower(supervisorName)
  }
  if (!is.null(errorsCount)) {
    variables$where$errorsCount$eq <- errorsCount
  }
  if (!is.null(interviewMode)) {
    variables$where$interviewMode$eq <- interviewMode
  }

  if (!is.null(notAnsweredCount)) {
    variables$where$notAnsweredCount$neq <- notAnsweredCount
  }

  if (!is.null(status)) {
    variables$where$status$eq <- status
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
