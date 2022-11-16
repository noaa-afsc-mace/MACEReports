#' @title break_position
#' @description function needed to put lines below each reporting region- identify break positions. from:
#' https://stackoverflow.com/questions/59263028/categorize-and-highlight-table-sections-with-flextable
#' @keywords internal
#' @param x a factor with levels at which breaks will be specified
#' @export
break_position =  function(x){
  z <- data.table::rleidv(x)
  c(z[-length(z)] != z[-1], FALSE)
}
