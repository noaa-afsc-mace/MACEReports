#' @title check_input_df
#'
#' @description Checks that dataframes required by functions are complete and contain the proper class types.
#' @keywords internal
#' @param template_df A dataframe that contains all the required columns and formats for the target table. This
#' should be in the \code{data} folder and documented in the \code{R} folder.
#' @param input_df The current survey data that you want to generate a table for.
#'
#' @return Nothing, if successful. Returns (hopefully) informative errors if dataframe isn't in the right format.
#'
#' @author Mike Levine
#'
#' @export
check_input_df = function(template_df, input_df){

  #get the name of the input df
  df_name = deparse(substitute(input_df))

  #check that all columns are present
  if (!all(colnames(template_df) %in% colnames(input_df))){

    #report what's missing:
    missing_cols = colnames(template_df)[which(!colnames(template_df) %in% colnames(input_df))]

    stop(paste0('\n', df_name, ' is missing ', missing_cols, ' column'))

  }

  #if all columns are present, check that all data types are correct
  if (all(colnames(template_df) %in% colnames(input_df))){

    wrong_data_type = c()
    correct_data_type = c()
    for (i in 1:ncol(template_df)){

      check_class = class(input_df[colnames(template_df)[i]][[1]])

      if(!all(check_class == class(template_df[i][[1]]))){

        wrong_data_type = rbind(wrong_data_type, colnames(input_df[colnames(template_df)[i]]))
        correct_data_type = rbind(correct_data_type,  class(template_df[i][[1]]))
      }

    }

    if (!is.null(wrong_data_type)){

      stop(paste0('\nWrong data type in ', df_name, ': ', wrong_data_type, '. Values should be: ', correct_data_type))

    }
  }
}
