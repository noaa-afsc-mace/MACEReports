#' @title stick_inputs
#'
#' @description An internal function to check the inputs to build_sf_sticks and stop if there are issues
#' @keywords internal
#' @param x x- position
#' @param y Y- position
#' @param z Abundance value (the value that will be used to scale sticks)
#' @param group_variable If specified, will specify the group for each stick; this can be used
#' to identify sticks by group.
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "x" = c(-152.2, -150.3, -159.4),
#'   "y" = c(55.2, 55.8, 55.6),
#'   "z" = c(75000, 400000, 280000)
#' )
#'
#' stick_inputs(x, y, z)
#' }
#' @export
stick_inputs <- function(x, y, z, group_variable = NULL) {
  # x,y,z must be numeric
  if (!is.numeric(c(x, y, z))) stop("x,y, and z must all be numeric")

  # x, y,z, and, if provided, group_variable must have the same number of observations
  if (!is.null(group_variable)) {
    if (!all(sapply(list(length(x), length(y), length(z), length(group_variable)),
      FUN = identical, length(x)
    ))) {
      stop("x, y, z and group variable must be vectors of the same length.")
    }
  }

  if (is.null(group_variable)) {
    if (!all(sapply(list(length(x), length(y), length(z)),
      FUN = identical, length(x)
    ))) {
      stop("x, y, and z must be vectors of the same length.")
    }
  }
}
