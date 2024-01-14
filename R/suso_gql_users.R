#' Get user information
#'
#' Allows the user to retrieve filtered or unfiltered user data.
#'
#' @details ATTENTION: This requires admin credentials, regular API credentials won't work
#'
#' @param endpoint GraphQL endpoint of your server
#' @param user your API username
#' @param password API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param userName specific user name
#' @param fullName fullName
#' @param isArchived isArchived
#' @param isLocked isLocked
#' @param creationDate user creation date
#' @param email user email
#' @param phoneNumber user phoneNumber
#' @param id user id
#' @param role user role
#' @param sortby_userName sort users by user name, either ASC for ascending or DESC for descending
#' @param sortby_role sort users by role, either ASC for ascending or DESC for descending
#' @param sortby_creationDate sort users by utc creation date, either ASC for ascending or DESC for descending
#' @param take take the specified integer numeber of maps
#' @param skip skip the first integer number of maps
#'
#'
#' @return if successfull, returns a list with the (filtered) responses
#'
#' @examplesIf suso_gql_pwcheck()==200
#' ## Requires Survey Solutions Server and ADMIN credentials!!
#'
#' # Get all users without filter
#'
#' suso_gql_users(endpoint = ep, user = adminuser,
#' password = adminpass, sortby_userName = "ASC")
#'
#' # Get only supervisors
#'
#' suso_gql_users(endpoint = ep, user = adminuser,
#' password = adminuser, , sortby_userName = "ASC",
#' role = "SUPERVISOR")
#'
#' @export


suso_gql_users <- function(endpoint = NULL,
                          user = NULL,
                          password = NULL,
                          token = NULL,
                          userName = NULL,
                          fullName = NULL,
                          isArchived = NULL,
                          isLocked = NULL,
                          creationDate = NULL,
                          email = NULL,
                          phoneNumber = NULL,
                          id = NULL,
                          role = NULL,
                          sortby_userName = NULL,
                          sortby_role = NULL,
                          sortby_creationDate = NULL,
                          take = NULL,
                          skip = NULL) {

  # check inputs
  .check_basics(token, endpoint, user, password)

  # define your query
  query <- sprintf('
      query($where: UsersFilterInput $order: [UsersSortInput!] $take: Int $skip: Int) {
        users(where: $where order: $order take: $take skip: $skip) {
              totalCount
              filteredCount
              nodes {
                id
                role
                userName
                fullName
                email
                phoneNumber
                creationDate
                isLocked
                isArchived
                isRelinkAllowed
                workspaces
              }
            }
      }
  ')
  ################################
  # create the variables list
  # 1. Top level
  variables <- list()
  # 2. Filter
  # 2.1 Filter default is NULL
  variables$where<-NULL

  if (!is.null(id)) {
    id<-.checkInput(id)
    variables$where$id <- id
  }
  if (!is.null(role)) {
    role<-.checkInput(role)
    variables$where$role <- role
  }
  if (!is.null(userName)) {
    userName<-.checkInput(userName)
    variables$where$userName <- userName
  }

  if (!is.null(fullName)) {
    fullName<-.checkInput(fullName)
    variables$where$fullName <- fullName
  }
  if (!is.null(isArchived)) {
    isArchived<-.checkInput(isArchived)
    variables$where$isArchived <- isArchived
  }
  if (!is.null(isLocked)) {
    isLocked<-.checkInput(isLocked)
    variables$where$isLocked <- isLocked
  }

  if (!is.null(creationDate)) {
    creationDate<-.checkInput(creationDate)
    variables$where$creationDate <- creationDate
  }
  if (!is.null(email)) {
    email<-.checkInput(email)
    variables$where$email <- email
  }
  if (!is.null(phoneNumber)) {
    phoneNumber<-.checkInput(phoneNumber)
    variables$where$phoneNumber <- phoneNumber
  }

  # REST FOLLOWS LATER!!

  ## Sort
  if (!is.null(sortby_userName)) {
    stopifnot(
      sortby_userName %in% c("ASC", "DESC")
    )
    variables$order$userName <- sortby_userName
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
  # build the url
  url<-.baseurl_baseauth(endpoint, body, user, password, retry = 3)

  # perform the request
  result<-.perform_request(url)

  return(result$data)
}




