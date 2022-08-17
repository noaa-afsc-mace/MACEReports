#check inputs: stop if issues
stick_inputs = function(x,y,z, group_variable = NULL){

  #x,y,z must be numeric
  if (!is.numeric(c(x,y,z))) stop('x,y, and z must all be numeric')

  #x, y,z, and, if provided, group_variable must have the same number of observations
  if (!is.null(group_variable)){

    if (!all(sapply(list(length(x), length(y), length(z), length(group_variable)),
                    FUN = identical, length(x))))
      stop("x, y, z and group variable must be vectors of the same length.")
  }

  if (is.null(group_variable)){

    if (!all(sapply(list(length(x), length(y), length(z)),
                    FUN = identical, length(x))))
      stop("x, y, and z must be vectors of the same length.")
  }

}
