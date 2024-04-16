#' @title Create biomass at age table for MACE summer GOA cruise reports
#' @description Build the pollock biomass-at-age table used in summer GOA reports. This will report biomass at
#' age for ages from 1- maximum reported age in the survey, and will do so for each report number (i.e. survey region)
#' that was defined for the analysis.
#' @param biomass_nums_age_data The name of the dataframe that contains the pollock biomass-at-age data;
#' this dataframe is created in the cruise report markdown process using the function
#' \code{get_biomass_and_nums_age_data_function.R}; this provides all the data that is used to create
#' a 'current survey only/pollock only' dataframe named \code{current_survey_pollock_biomass_nums_age}.
#' This dataframe is the easiest input to the \code{build_biomass_at_age_table_summer_goa} function.
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' # build the table and caption
#' biomass_at_age_table_list <-
#'   build_biomass_at_age_table_summer_goa
#' (biomass_nums_age_data <- current_survey_pollock_biomass_nums_age)
#' #
# #pull out table and caption from the list
#' biomass_at_age_table <- biomass_at_age_table_list[[1]]
#' biomass_at_age_caption <- biomass_at_age_table_list[[2]]
#' }
#' @export
build_biomass_at_age_table_summer_goa <- function(biomass_nums_age_data) {
  ###
  # data checks:

  # check input dataframe against the template dataframe: this will make sure the input
  # data can actually be used to create a table, and will return errors if not
  check_data <- MACEReports::template_df_numbers_biomass_at_age_tables_summer_goa
  MACEReports::check_input_df(template_df = check_data, input_df = biomass_nums_age_data)

  # check- make sure there's only one species, warn if not pollock
  species_list <- unique(biomass_nums_age_data$SPECIES_CODE)

  if (length(species_list) > 1) {
    stop(paste0(
      "biomass_nums_age_data contains multiple species codes: ",
      paste(shQuote(species_list), collapse = ", "), "\n"
    ))
  }

  if (species_list != 21740) {
    warning("You are printing a table for species code ", species_list, " not walleye pollock! Is this okay?")
  }

  # check: make sure only one survey year
  if (length(unique(biomass_nums_age_data$SURVEY)) > 1) {
    stop(paste0(
      "biomass_nums_age_data contains multiple species surveys: ",
      paste(shQuote(unique(biomass_nums_age_data$SURVEY)), collapse = ", "), "\n"
    ))
  }


  #######
  # step 1: sum nums vertically by interval, age; report as biomass (t)
  biomass_summary <- biomass_nums_age_data %>%
    dplyr::group_by(.data$REPORT_NUMBER, .data$region, .data$AGE) %>%
    dplyr::summarize(biomass_t = sum(.data$BIOMASS) / 1e3) %>%
    dplyr::arrange(.data$REPORT_NUMBER, .data$AGE)

  # add a totals column as well
  table_totals <- biomass_summary %>%
    dplyr::group_by(.data$AGE) %>%
    dplyr::summarize(Total = sum(.data$biomass_t))

  #####
  # step 2: get vectors of every possible age in the survey areas:

  # We report a vector from 1-max possible age for each management area/region- it's unlikely that every region contains every age, so report NAs in these cases

  # build a dataframe that's simply each possible length per region
  get_all_possible_ages_for_region <- function(region_name) {
    # build a dataframe that has the length vector and report name
    report_lengths <- data.frame(seq(1, max(biomass_summary$AGE), 1))
    colnames(report_lengths) <- c("AGE")
    report_lengths$region <- region_name

    # and return this dataframe
    return(report_lengths)
  }

  # apply function to get dataframe of lengths
  report_ages_for_regions <- purrr::map_dfr(unique(biomass_summary$region), get_all_possible_ages_for_region)

  # and add the blank rows where no fish were observed
  biomass_summary <- dplyr::left_join(report_ages_for_regions, biomass_summary, by = c("AGE", "region"))

  #######
  # step 3. make the table

  # for presentation, we just want biomass (t) for each age (1- max age in survey within each geographic area)
  biomass_for_presentation <- biomass_summary %>%
    # remove the report_number column- it was only needed to order things by report number
    dplyr::select(.data$AGE, .data$region, .data$biomass_t) %>%
    # also, go from 'long' format (where every region lives in it's own)
    tidyr::pivot_wider(id_cols = .data$AGE, names_from = .data$region, values_from = .data$biomass_t)

  # join totals to the 'wide' dataframe
  biomass_for_presentation <- dplyr::left_join(biomass_for_presentation, table_totals, by = c("AGE"))

  # rename AGE to a nicer title for plotting
  biomass_for_presentation <- biomass_for_presentation %>%
    dplyr::rename("Age" = "AGE")

  # calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there can be
  # na's, for a given age/region, which is fine)

  # this will sum up every column in the survey, except for the first (which is the Length column)
  # we'll just add 'Total' here  to represent the length column, and add up the rest as their own column
  # do this in a loop as the number of survey areas (i.e. columns in table) can differ annually

  col_sums <- c("Total")
  for (i in 2:ncol(biomass_for_presentation)) {
    # get the column summed; format for table
    col_sum_formatted <- format(sum(biomass_for_presentation[, i], na.rm = TRUE),
      big.mark = ",", na_str = "-", nsmall = 2, digits = 2, scientific = FALSE
    )

    # add it to the collection of formatted sums
    col_sums <- cbind(col_sums, col_sum_formatted)
  }

  # define a border to use above and below caption and at bottom of table
  table_border <- officer::fp_border(color = "black", width = 0.75)

  # make the nums-at-length table into a flextable
  biomass_age_table <- flextable::flextable(biomass_for_presentation) %>%
    # append totals as a footer row
    flextable::add_footer_row(top = FALSE, values = col_sums, colwidths = rep(1, length(biomass_for_presentation))) %>%
    # get rid of padding around cells
    flextable::padding(padding = 0, part = "all") %>%
    # align text: center justify everything
    flextable::align(align = "center", part = "all") %>%
    # add horizontal border on top and bottom of table
    flextable::hline_top(part = "all", border = table_border) %>%
    flextable::hline_bottom(part = "all", border = table_border) %>%
    # set the font and font size
    flextable::font(fontname = "times", part = "all") %>%
    flextable::fontsize(size = 9, part = "header") %>%
    flextable::fontsize(size = 9, part = "body") %>%
    # if we have lots of columns/reporting regions, we might need to shrink the total table width to fit
    # 9" margins. Set this as the maximum width
    flextable::fit_to_width(max_width = 9, unit = "in")

  # conditionally format values for table using MACEReports::table_nums_format function:
  # to format all the columns that could exist (i.e. different years will have different numbers of reporting columns),
  # we need a named list of column names and the function to apply to said column;
  # this needs to skip the first column too (no need to format 'Age')
  funs_list <- stats::setNames(
    rep(
      list(MACEReports::table_nums_format),
      length(names(biomass_for_presentation)) - 1
    ),
    names(biomass_for_presentation)[-1]
  )

  # apply this function to the specfied table columns
  biomass_age_table <- flextable::set_formatter(biomass_age_table, values = funs_list)


  table_dims <- flextable::flextable_dim(biomass_age_table)
  if (table_dims$widths > 9) {
    flextable::fit_to_width(biomass_age_table, max_width = 9)
  }

  # build the caption
  cap_text <- paste0(
    "Pollock biomass-at-age estimates (metric tons) by survey region during the ",
    unique(biomass_nums_age_data$year), " GOA survey."
  )

  # return table and caption
  return(list(biomass_age_table, cap_text))
}
