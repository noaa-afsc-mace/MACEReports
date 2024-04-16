#' @title Create numbers at age table for MACE winter cruise reports
#'
#' @description Build the pollock numbers-at-age table used in winter GOA reports. The data used here is usually
#' limited to the 'selectivity-corrected era' within Shelikof Strait (i.e. 2008-current). This function will report all
#' ages from 1-18, and will do so for any region that was defined for the analysis.
#' If you have an especially old age class (>18 years old), the table will report to this age as opposed
#' to the nomimal age 1- age 18 range.
#' @param biomass_nums_age_data The name of the dataframe that contains all the abundance-at-age values;
#' this dataframe is created in the cruise report markdown process using the function
#' \code{get_biomass_and_nums_age_data_function.R}; this provides all the data that is then used to create a
#' dataframe named \code{shelikof_sel_corr_surveys_biomass_nums_age}. This dataframe is the easiest input to
#' \code{build_numbers_at_age_table_winter_goa} function.
#' @param region_name The name of the reporting region that you want to include in this table
#' (generally "Shelikof Strait" in winter GOA reports)
#'
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' # build the table and the caption
#' nums_age_table_list <- build_numbers_at_age_table_winter_goa(
#'   biomass_nums_age_data = shelikof_sel_corr_surveys_biomass_nums_age,
#'   region_name = "Shelikof Strait"
#' )
#'
#' # pull out the table and caption from the list
#' nums_age_table <- nums_age_table_list[[1]]
#' nums_age_caption <- nums_age_table_list[[2]]
#' }
#' @export
build_numbers_at_age_table_winter_goa <- function(biomass_nums_age_data, region_name) {
  # check input dataframe against the template dataframe: this will make sure the input
  # data can actually be used to create a table, and will return errors if not
  check_data <- MACEReports::template_df_numbers_biomass_at_age_tables_winter_goa
  MACEReports::check_input_df(template_df = check_data, input_df = biomass_nums_age_data)

  # check that the region name is present
  if (!region_name %in% unique(biomass_nums_age_data$region)) {
    stop(paste("The region_name", region_name, "is not present in biomass_nums_age_data"))
  }

  ###########
  # 1. Collect the data for and numbers- at length table

  # convert numbers to numbers (millions fish) and biomass (thousands tons)
  summary_nums_biomass_by_age <- biomass_nums_age_data %>%
    # limit the table data to this reporting region;
    dplyr::filter(.data$region == region_name) %>%
    # get a year from the survey name based on MACE survey naming convention
    dplyr::mutate(year = as.integer(substr(.data$SURVEY, start = 1, stop = 4))) %>%
    dplyr::group_by(.data$year, .data$AGE) %>%
    # convert values to millions of fish; conditionally format to have values <1 as '<1'
    dplyr::summarize(
      number_millions = sum(.data$NUMBERS) / 1e6,
      biomass_thousand_tons = sum(.data$BIOMASS) / 1e6
    ) %>%
    dplyr::arrange(.data$year, .data$AGE)

  # make sure we have the same age vector range as the historic table-
  # historic table reports fish from 1cm-18 years; copy that here as a dataframe;
  # in the unlikely case we have a fish older than 18 years, expand the range to include

  # identify the oldest age in the reported timeseries
  max_survey_age <- max(summary_nums_biomass_by_age$AGE)

  get_age_vector <- function(year) {
    # if this age is <= 18, report to age 18; if greater, expand to this
    max_report_age <- ifelse(max_survey_age <= 18, 18, max_survey_age)

    report_ages <- data.frame(seq(1, max_report_age, 1))
    colnames(report_ages) <- c("AGE")
    report_ages$year <- year

    # return the length vector
    return(report_ages)
  }

  all_reporting_ages <- purrr::map_dfr(unique(summary_nums_biomass_by_age$year), get_age_vector)

  # and add all these ages back to the dataframe; use a right join so that all values are represented
  summary_nums_biomass_by_age <- dplyr::right_join(summary_nums_biomass_by_age,
    all_reporting_ages,
    by = c("AGE", "year")
  ) %>%
    # rename AGE as Age to be nicer looking on tables
    dplyr::rename(Age = .data$AGE) %>%
    dplyr::arrange(.data$year, .data$Age)

  ############
  # 2: build the numbers-at-age table

  # get the numbers data, and make the data 'wide' for the table, with each year as a column
  nums_by_age_selectivity_corrected <- summary_nums_biomass_by_age %>%
    # keep only the columns we care about
    dplyr::select(.data$year, .data$Age, .data$number_millions) %>%
    # make data wide format for table
    tidyr::pivot_wider(id_cols = .data$Age, names_from = .data$year, values_from = .data$number_millions)

  # get a summary row
  totals_footer_row <- nums_by_age_selectivity_corrected %>%
    # get the column sums and round for presentation
    dplyr::summarize_all(list(~ format(round(sum(., na.rm = TRUE), digits = 1), nsmall = 1))) %>%
    # and set the Length column as a blank since the total # of lengths is meaningless here
    dplyr::mutate(Age = "Total")

  # define a border to use above and below caption and at bottom of table
  table_border <- officer::fp_border(color = "black", width = 0.75)

  # make it into a flextable
  nums_age_table <- flextable::flextable(nums_by_age_selectivity_corrected) %>%
    # add the footer row
    flextable::add_footer_row(
      top = FALSE, values = totals_footer_row,
      colwidths = rep(1, length(totals_footer_row))
    ) %>%
    # set the width of the table as 9" for landscape format
    flextable::width(width = 9 / ncol(nums_by_age_selectivity_corrected)) %>%
    # align text: center justify everything
    flextable::align(align = "center", part = "all") %>%
    flextable::padding(padding = 0, part = "all") %>%
    # add horizontal border on top and bottom of table
    flextable::hline_top(part = "all", border = table_border) %>%
    flextable::hline_bottom(part = "all", border = table_border) %>%
    # set the font and font size
    flextable::font(fontname = "times", part = "all") %>%
    flextable::fontsize(size = 9, part = "header") %>%
    flextable::fontsize(size = 9, part = "body") %>%
    flextable::fontsize(size = 9, part = "footer")

  # conditionally format the numbers columns:

  # idea is you pass a list of column names to be formatted as a list, paired with the function they need,
  # and then format everything on this list accoriding to the function

  # get the formatting function
  conditional_formatter <- Vectorize(function(x) {
    if (is.na(x)) {
      x <- 0
    } else if (x >= 1) {
      x <- format(round(x, digits = 1), nsmall = 1)
    } else if (x < 1) {
      x <- "<1"
    } else {
      x <- ""
    }

    return(x)
  })

  # get a list of column keys
  col_keys <- nums_age_table$col_keys[2:length(nums_age_table$col_keys)]

  # create named list of functions
  formatter_func_list <- rep(list(conditional_formatter), length(col_keys))
  names(formatter_func_list) <- col_keys

  ## and conditionally format table: if value is NA (i.e. no fish at this length), set as 0; if value is <1, set as <1
  # function to format each column
  nums_age_table <- flextable::set_formatter(nums_age_table, values = formatter_func_list, part = "body")

  # build the caption:
  # note it is hard-coded to say Shelikof Strait because that's the only region we currently make this table for

  cap_text <- paste0(
    "Numbers-at-age estimates (millions of fish) from acoustic-trawl surveys ",
    "of walleye pollock in the Shelikof Strait area. ",
    "Numbers from ", min(unique(summary_nums_biomass_by_age$year)), " to ",
    max(unique(summary_nums_biomass_by_age$year)),
    " reflect selectivity corrections for escapement of juveniles. "
  )

  # return table and caption
  return(list(nums_age_table, cap_text))
}
