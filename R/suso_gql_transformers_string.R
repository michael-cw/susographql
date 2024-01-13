#' @title Utility Functions for string operator selection
#'
#' @name transformers.string
#'
#' @description Can be used in filters ("where") for operator selection. If none is selected, operator always defaults to `eq()`. The functions bellow are
#' valid for the corresponding inputs ComparableInt64OperationFilterInput and ComparableNullableOfInt32OperationFilterInput.
#'
#' @details Also see the \link{susoop_str} selector list, which allows you, to just select the function from
#' a named list.
#'
#' @param value_set the parameter set for the operator
#'
#' @examples
#'
#' \dontrun{
#' suso_gql_interviews(endpoint = [srv], user = [user],
#' password = [pass], workspace = "test", errorsCount = eq(0))
#'
#'
#' suso_gql_interviews(endpoint = [srv], user = [user],
#' password = [pass], workspace = "test", errorsCount = gt(0))
#'
#'
#' suso_gql_interviews(endpoint = [srv], user = [user],
#' password = [pass], workspace = "test", errorsCount = inbetw(c(0, 5))
#' }
#'
#' @export
contains<-function(value_set) {list(contains = value_set)}

#' @describeIn transformers.string smaller
#' @export

endsWith<-function(value_set) {list(endsWith = value_set)}

#' @describeIn transformers.string inbetween (requires numeric vector with upper and lower bound, i.e. c(1, 5))
#' @export

ncontains<-function(value_set) {list(ncontains = value_set)}


#' @describeIn transformers.string not inbetween (requires numeric vector with upper and lower bound, i.e. c(1,5))
#' @export
#'
nendsWith<-function(value_set) {list(nendsWith = value_set)}

#' @describeIn transformers.string not inbetween (requires numeric vector with upper and lower bound, i.e. c(1,5))
#' @export
#'
nstartsWith<-function(value_set) {list(nstartsWith = value_set)}

#' @describeIn transformers.string not inbetween (requires numeric vector with upper and lower bound, i.e. c(1,5))
#' @export
#'
startsWith<-function(value_set) {list(startsWith = value_set)}

#' @describeIn transformers.string not inbetween (requires numeric vector with upper and lower bound, i.e. c(1,5))
#' @export
#'
inclu<-function(value_set) {list(`in` = value_set)}

#' @describeIn transformers.string not inbetween (requires numeric vector with upper and lower bound, i.e. c(1,5))
#' @export
#'
ninclu<-function(value_set) {list(nin = value_set)}

