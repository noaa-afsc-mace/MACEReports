#' @title table_nums_format
#' @description An internal function to format catch weights and percents and numbers percents conditionally as
#' we've generally presented them in tables. Specifically, if values = NA, label as '-',
#' If values are <=0.01, label as '<0.01', otherwise, format the value as having a single decimal point for display
#' @keywords internal
#' @param x a numeric value
#' @examples
#' \dontrun{
#'
#' values <- c(1000, NA, .001)
#'
#' formatted_values <- table_nums_format(values)
#' }
#' @return a character for each value.
#' @export
# create a function to format catch weights and percents and numbers percents conditionally:
# if values = NA, label as '-'
# If values are <=0.01, label as '<0.01';
# otherwise, format the value as having a decimal points for display;
table_nums_format <- function(x) {
  # check: make sure each value is numeric
  if (!is(x, "numeric")& !is(x, "logical")) {
    stop(paste0("table_nums_format function requires numeric or logical values, not ", class(x), " values."))
  }

  # conditionally format; return formatted value
  formatted_x <- ifelse(x > 0.01 | is.na(x),
    ifelse(is.na(x),
      "-",
      formatC(x, format = "f", digits = 2, big.mark = ",")
    ),
    "<0.01"
  )

  return(formatted_x)
}
