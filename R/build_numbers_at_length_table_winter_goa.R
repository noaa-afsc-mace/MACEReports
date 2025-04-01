#' @title Create numbers at length table for MACE winter cruise reports
#' @description Build the pollock numbers-at-length table used in winter GOA reports. The data used here is usually
#' limited to the 'selectivity-corrected era' within Shelikof Strait (i.e. 2008-current). This function will report all
#' lengths from 5-76 cm, and will do so for any region that was defined for the analysis.
#' If you have an especially small (<5 cm) or large (>76 cm) length class, the table will report these sizes as opposed
#' to the nomimal 5 cm- 76 cm range.
#' @param biomass_nums_length_data The name of the dataframe that contains all the abundance-at-length values;
#' this dataframe is created in the cruise report markdown process using the function
#' \code{get_total_biomass_by_length.R}; this provides all the data that is then used to create a
#' dataframe named \code{shelikof_sel_corr_surveys_totals_by_length_and_region}. This dataframe is the easiest input to
#' \code{build_numbers_at_length_table_winter_goa} function.
#' @param region_name The name of the reporting region that you want to include in this table
#' (generally "Shelikof Strait" in winter GOA reports)
#' @param include_n_years (optional) number of recent years to include in this table. This is useful for long survey time series where you may not need to present all the available years in the table. If not used, all years in the biomass_nums_length_data will be included.
#'
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' # build the table and the caption
#' nums_table_list <- build_numbers_at_length_table_winter_goa(
#'   biomass_nums_length_data = shelikof_sel_corr_surveys_totals_by_length_and_region,
#'   region_name = "Shelikof Strait"
#' )
#'
#' # pull out the table and caption from the list
#' nums_length_table <- nums_table_list[[1]]
#' nums_length_caption <- nums_table_list[[2]]
#' }
#' @export
build_numbers_at_length_table_winter_goa <- function(biomass_nums_length_data,
                                                     region_name,
                                                     include_n_years = NULL) {
  # check input dataframe against the template dataframe: this will make sure the input
  # data can actually be used to create a table, and will return errors if not
  check_data <- MACEReports::template_df_numbers_biomass_at_length_tables_winter_goa
  MACEReports::check_input_df(template_df = check_data, input_df = biomass_nums_length_data)

  # check that the region name is present
  if (!region_name %in% unique(biomass_nums_length_data$region)) {
    stop(paste("The region_name", region_name, "is not present in biomass_nums_length_data"))
  }

  ###########
  # 1. Collect the data for and numbers- at length table

  # if the user has requested to only print n most recent years, limit the data
  if (!is.null(include_n_years)){

    # confirm that they have requested a number
    if (!is.numeric(include_n_years)){
      stop('Please request a number of years for the include_n_years parameter!')
    }

    # limit to the most recent n years for the table
    min_year_for_table <- max(biomass_nums_length_data$year) - include_n_years

    biomass_nums_length_data <- biomass_nums_length_data %>%
      dplyr::filter(.data$year >= min_year_for_table)

  }

  # use this to limit the table data to the requested reporting region;
  # sum everyting up by length bin and year for this region
  nums_and_biomass_by_length <- biomass_nums_length_data %>%
    dplyr::filter(.data$region == region_name) %>%
    dplyr::group_by(.data$year, .data$LENGTH) %>%
    # report values as millions of fish, thousands of tons
    dplyr::summarize(
      number_millions = sum(.data$NUMBERS) / 1e6,
      biomass_thousand_tons = sum(.data$BIOMASS) / 1e6
    ) %>%
    dplyr::arrange(.data$year, .data$LENGTH)

  # we report all lengths from 5-76 cm; it's unlikely that any given survey contains all these,
  # but we want to report a blank in these cases

  # build a dataframe that's simply each possible length per region
  get_all_possible_lengths <- function(year) {
    # set the minimum length at 10 cm UNLESS there are smaller fish in the survey
    min_report_length <- ifelse(min(biomass_nums_length_data$LENGTH) < 5,
      min(biomass_nums_length_data$LENGTH),
      5
    )

    # set the maximum length as 70 cm UNLESS there are larger fish in the survey
    max_report_length <- ifelse(max(biomass_nums_length_data$LENGTH) > 76,
      max(biomass_nums_length_data$LENGTH),
      76
    )

    # build a dataframe that has the length vector and report name
    report_lengths <- data.frame(seq(min_report_length, max_report_length, 1))
    colnames(report_lengths) <- c("LENGTH")
    report_lengths$year <- year

    # and return this dataframe
    return(report_lengths)
  }

  # apply function to get dataframe of lengths
  all_reporting_lengths <- purrr::map_dfr(unique(nums_and_biomass_by_length$year), get_all_possible_lengths)

  # and add all these lengths back to the dataframe; use a right join so that all values are represented
  nums_and_biomass_by_length <- dplyr::right_join(nums_and_biomass_by_length, all_reporting_lengths,
    by = c("LENGTH", "year")
  ) %>%
    # rename LENGTH as Length to be nicer looking on tables
    dplyr::rename(Length = .data$LENGTH) %>%
    dplyr::arrange(.data$year, .data$Length)

  ############
  # 2. Make the table for numbers by length

  # get the numbers data, and make the data 'wide' for the table, with each year as a column
  nums_by_length_wide <- nums_and_biomass_by_length %>%
    # keep only the columns we care about
    dplyr::select(.data$year, .data$Length, .data$number_millions) %>%
    # make data wide format for table
    tidyr::pivot_wider(id_cols = .data$Length, names_from = .data$year, values_from = .data$number_millions)

  # get a summary row
  totals_footer_row <- nums_by_length_wide %>%
    # get the column sums and round for presentation
    dplyr::summarize_all(list(~ format(round(sum(., na.rm = TRUE), digits = 1), nsmall = 1))) %>%
    # and set the Length column as a blank since the total # of lengths is meaningless here
    dplyr::mutate(Length = "Total")

  # define a border to add above and below caption and at bottom of table
  table_border <- officer::fp_border(color = "black", width = 1.0)

  # make it into a flextable
  nums_length_table <- flextable::flextable(nums_by_length_wide) %>%
    # add the footer row
    flextable::add_footer_row(top = FALSE, values = totals_footer_row, colwidths = rep(1, length(totals_footer_row))) %>%
    # set the width of the table as 9" for landscape format
    flextable::width(width = 9 / ncol(nums_by_length_wide)) %>%
    # align text: center justify everything
    flextable::align(align = "center", part = "all") %>%
    # get rid of padding around the cells
    flextable::padding(padding = 0, part = "all") %>%
    # add horizontal border on top and bottom of table
    flextable::hline_top(part = "all", border = table_border) %>%
    flextable::hline_bottom(part = "all", border = table_border) %>%
    # set the font and font size
    flextable::font(fontname = "times", part = "all") %>%
    flextable::fontsize(size = 9, part = "header") %>%
    flextable::fontsize(size = 9, part = "body") %>%
    flextable::fontsize(size = 9, part = "footer")

  # conditionally format the numbers columns
  # idea is you pass a list of column names to be formatted as a list, paired with the function they need,
  # and then format everything on this list according to the function
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
  col_keys <- nums_length_table$col_keys[2:length(nums_length_table$col_keys)]

  # create named list of functions
  formatter_func_list <- rep(list(conditional_formatter), length(col_keys))
  names(formatter_func_list) <- col_keys

  ## and conditionally format table: if value is NA (i.e. no fish at this length), set as 0; if value is <1, set as <1
  # function to format each column
  nums_length_table <- flextable::set_formatter(nums_length_table, values = formatter_func_list, part = "body")

  # build the caption:

  cap_text <- paste0(
    "Numbers-at-length estimates (millions of fish) from acoustic-trawl surveys of pollock in the ",
    region_name, " area from ", min(biomass_nums_length_data$year), "-", max(biomass_nums_length_data$year), "."
  )

  # if include_n_years parameter used, add a note about how many years left off the table
  if (!is.null(include_n_years)){

    cap_text <- paste0(cap_text, paste0(' Note that years prior to ', max(biomass_nums_length_data$year) - include_n_years, ' are excluded due to space constraints.' ))

  }

  # return table and caption
  return(list(nums_length_table, cap_text))
}
