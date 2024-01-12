#' Get all interviews
#'
#' Allows the user to retrieve filtered or unfiltered interview data.
#'
#' @param endpoint GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
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
#' @param sortby_updateDateUtc sort by updated at UTC time, either ASC for ascending or DESC for descending
#' @param sortby_notAnsweredCount sort by errors count, either ASC for ascending or DESC for descending
#' @param sortby_createdDate sort by creation date, either ASC for ascending or DESC for descending
#' @param sortby_errorsCount sort by number of errors, either ASC for ascending or DESC for descending
#' @param sortby_assignmentId sort by assignmentId, either ASC for ascending or DESC for descending
#' @param take take the specified integer numeber of interviews
#' @param skip skip the first integer number of interviews
#'
#' @export



suso_gql_interviews <- function(endpoint = NULL,
                                workspace = NULL,
                                user = NULL,
                                password = NULL,
                                token = NULL,
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
                                supervisorName = NULL,
                                sortby_updateDateUtc = NULL,
                                sortby_notAnsweredCount = NULL,
                                sortby_createdDate = NULL,
                                sortby_errorsCount = NULL,
                                sortby_assignmentId = NULL,
                                take = NULL,
                                skip = NULL
                                ) {
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check inputs
  .check_basics(token, endpoint, user, password)



  # define your query
  query <- sprintf('
      query($workspace: String $where: InterviewsFilter $order: [InterviewSort!] $take: Int $skip: Int){
          interviews(workspace: $workspace where: $where order: $order take: $take skip: $skip) {
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
    assignmentId<-checkInput(assignmentId)
    variables$where$assignmentId <- assignmentId
  }
  if (!is.null(clientKey)) {
    variables$where$clientKey$eq <- clientKey
  }
  if (!is.null(questionnaireId) && !is.null(questionnaireVersion)) {
    variables$where$questionnaireId$eq <- questionnaireId

    version<-checkInput(version)
    variables$where$questionnaireVersion <- questionnaireVersion
  }
  if (!is.null(responsibleName)) {
    variables$where$responsibleNameLowerCase$eq <- tolower(responsibleName)
  }
  if (!is.null(supervisorName)) {
    variables$where$supervisorNameLowerCase$eq <- tolower(supervisorName)
  }
  if (!is.null(errorsCount)) {
    errorsCount<-checkInput(errorsCount)
    variables$where$errorsCount <- errorsCount
  }
  if (!is.null(interviewMode)) {
    variables$where$interviewMode$eq <- interviewMode
  }

  if (!is.null(notAnsweredCount)) {
    notAnsweredCount<-checkInput(notAnsweredCount)
    variables$where$notAnsweredCount <- notAnsweredCount
  }

  if (!is.null(status)) {
    variables$where$status$eq <- status
  }

  ## SORTING
  if (!is.null(sortby_updateDateUtc)) {
    stopifnot(
      sortby_updateDateUtc %in% c("ASC", "DESC")
    )
    variables$order$updateDateUtc <- sortby_updateDateUtc
  }

  if (!is.null(sortby_notAnsweredCount)) {
    stopifnot(
      sortby_notAnsweredCount %in% c("ASC", "DESC")
    )
    variables$order$notAnsweredCount <- sortby_notAnsweredCount
  }

  if (!is.null(sortby_createdDate)) {
    stopifnot(
      sortby_createdDate %in% c("ASC", "DESC")
    )
    variables$order$createdDate <- sortby_createdDate
  }

  if (!is.null(sortby_errorsCount)) {
    stopifnot(
      sortby_errorsCount %in% c("ASC", "DESC")
    )
    variables$order$errorsCount <- sortby_errorsCount
  }

  if (!is.null(sortby_assignmentId)) {
    stopifnot(
      sortby_assignmentId %in% c("ASC", "DESC")
    )
    variables$order$assignmentId <- sortby_assignmentId
  }

  ## sorts missing:

  # status: InterviewStatusOperationFilterInput
  # interviewMode: InterviewModeOperationFilterInput
  # questionnaireId: ComparableGuidOperationFilterInput
  # questionnaireVariable: StringOperationFilterInput
  # questionnaireVersion: ComparableInt64OperationFilterInput
  # key: StringOperationFilterInput
  # clientKey: StringOperationFilterInput
  # DONE!! assignmentId: ComparableNullableOfInt32OperationFilterInput
  # responsibleName: StringOperationFilterInput
  # responsibleNameLowerCase: StringOperationFilterInput
  # supervisorName: StringOperationFilterInput
  # supervisorNameLowerCase: StringOperationFilterInput
  # responsibleRole: UserRolesOperationFilterInput
  # receivedByInterviewerAtUtc: ComparableNullableOfDateTimeOperationFilterInput
  # identifyingData: ListFilterInputTypeOfIdentifyEntityValueFilterInput



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








