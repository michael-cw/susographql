#' List object for Survey Solutions GraphQl numeric operator selection
#'
#' A list of the available transformers
#'
#' @details Allows the user to select the operator for the required
#' filter.
#'
#' @return A named list with the operator and the value to be passed on as input to
#' the filter.
#'
#' @examples
#'
#' # equal to 3
#' susoop_num$eq(3)
#'
#' # not equal to 3
#' susoop_num$neq(3)
#'
#' @export
susoop_num <- list(
  eq = eq,
  neq = neq,
  inbetw = inbetw,
  ninbetw = ninbetw,
  gt = gt,
  ngt = ngt,
  gte = gte,
  ngte = ngte,
  lt = lt,
  nlt = nlt,
  lte = lte,
  nlte = nlte
)


#' List object for Survey Solutions GraphQl character operator selection
#'
#' A list of the available transformers
#'
#' @details Allows the user to select the operator for the required
#' filter.
#'
#' @return A named list with the operator and the value to be passed on as input to
#' the filter.
#'
#' @examples
#'
#' # equal to 3
#' susoop_str$contains("area10")
#'
#' # not equal to 3
#' susoop_str$startsWith("area")
#'
#' @export
susoop_str <- list(
  eq = eq,
  neq = neq,
  contains = contains,
  endsWith = endsWith,
  ncontains = ncontains,
  nendsWith = nendsWith,
  nstartsWith = nstartsWith,
  startsWith = startsWith,
  inclu = inclu,
  ninclu = ninclu
)
