#' @title Utility Functions for numeric operator selection
#'
#' @name transformers.numeric
#'
#' @description Can be used in filters ("where") for operator selection. If none is selected, operator always defaults to `eq()`. The functions bellow are
#' valid for the corresponding inputs ComparableInt64OperationFilterInput and ComparableNullableOfInt32OperationFilterInput.
#'
#' @details Also see the \link{susoop_num} selector list, which allows you, to just select the function from
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


eq<-function(value_set) {list(eq = value_set)}

#' @describeIn transformers.numeric smaller
#' @export

neq<-function(value_set) {list(neq = value_set)}

#' @describeIn transformers.numeric inbetween (requires numeric vector with upper and lower bound, i.e. c(1, 5))
#' @export

inbetw<-function(value_set) {list(`in` = value_set)}


#' @describeIn transformers.numeric not inbetween (requires numeric vector with upper and lower bound, i.e. c(1,5))
#' @export
#'
ninbetw<-function(value_set) {list(nin = value_set)}

#' @describeIn transformers.numeric greater than
#' @export

gt<-function(value_set) {list(gt = value_set)}

#' @describeIn transformers.numeric not greater than
#' @export

ngt<-function(value_set) {list(ngt = value_set)}

#' @describeIn transformers.numeric greater than or equal
#' @export

gte<-function(value_set) {list(gte = value_set)}

#' @describeIn transformers.numeric not greater than or equal
#' @export

ngte<-function(value_set) {list(ngte = value_set)}

#' @describeIn transformers.numeric lower than
#' @export

lt<-function(value_set) {list(lt = value_set)}

#' @describeIn transformers.numeric not lower than
#' @export

nlt<-function(value_set) {list(nlt = value_set)}

#' @describeIn transformers.numeric lower than or equal
#' @export

lte<-function(value_set) {list(lte = value_set)}

#' @describeIn transformers.numeric not lower than or equal
#' @export

nlte<-function(value_set) {list(nlte = value_set)}

