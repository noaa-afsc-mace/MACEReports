#' @title build_nmfs_area_totals_table
#'
#' @description Build the pollock biomass (metric tons) and numbers (millions)- per NMFS management area and survey
#' region. This table has been used in the GOA sumemer report.
#' @param biomass_nums_length_data The name of the dataframe that contains the pollock
#' biomass at length data; this dataframe is created in the cruise report markdown
#' process using the function \code{get_biomass_and_nums_data_function.R}; this provides all
#' the data that is then used to create a 'current survey only/pollock only' dataframe which is
#' the easiest input to the \code{build_nmfs_area_totals_table} function.
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' #build the table and the caption
#'area_totals_table_list =
#'   build_nmfs_area_totals_table(biomass_nums_length_data = current_survey_pollock_biomass_nums)
#'
#' #pull out the table and caption from the list
#' area_totals_table = area_totals_table_list[[1]]
#' area_totals_caption = area_totals_table_list[[2]]#'
#' }
#' @export
build_nmfs_area_totals_table = function(biomass_nums_length_data){

  #data checks:

  #check input dataframe against the template dataframe: this will make sure the input
  #data can actually be used to create a table, and will return errors if not
  check_data = template_df_numbers_biomass_at_length_tables_summer_goa
  check_input_df(template_df = check_data, input_df = biomass_nums_length_data)

  #check- make sure there's only one species, warn if not pollock
  species_list = unique(biomass_nums_length_data$SPECIES_CODE)

  if (length(species_list) > 1){
    stop(paste0('biomass_nums_length_data contains multiple species codes: ',
                paste(shQuote(species_list), collapse=", "), '\n'))
  }

  if (species_list != 21740){
    warning('You are printing a table for species code ', species_list, ' not walleye pollock! Is this okay?')
  }

  #check: make sure only one survey year
  if (length(unique(biomass_nums_length_data$SURVEY)) > 1){
    stop(paste0('biomass_nums_length_data contains multiple species surveys: ',
                paste(shQuote(unique(biomass_nums_length_data$SURVEY)), collapse=", "), '\n'))
  }

  ######
  #step 1: sum things up within NMFS management areas and reporting regions

  #sum things up by region, management; report biomass (t) and numbers (million)
  biomass_nums_by_region_and_nmfs_area = biomass_nums_length_data%>%
    dplyr::group_by(.data$management_area, .data$REPORT_NUMBER, .data$region)%>%
    dplyr::summarize(biomass_t = sum(.data$BIOMASS)/1e3,
              num_million = sum(.data$NUMBERS)/1e6)%>%
    dplyr::arrange(.data$management_area, .data$REPORT_NUMBER)

  #also get a total by management region
  biomass_nums_by_nmfs_area = biomass_nums_length_data%>%
    #label the 'region' as 'total' for merging; set the REPORT_NUMBER really high to be sure it sorts last in tables
    dplyr::mutate(region = 'Total',
           REPORT_NUMBER = 10000)%>%
    dplyr::group_by(.data$management_area, .data$REPORT_NUMBER, .data$region)%>%
    dplyr::summarize(biomass_t = sum(.data$BIOMASS)/1e3,
              num_million = sum(.data$NUMBERS)/1e6)%>%
    dplyr::arrange(.data$management_area)

  #####
  #step 2: make the table

  #merge the totals row with the by-region summary rows
  biomass_nums_summary = dplyr::full_join(biomass_nums_by_region_and_nmfs_area, biomass_nums_by_nmfs_area,
                                   by = c('management_area', 'region', 'REPORT_NUMBER', 'biomass_t', 'num_million'))

  #sort tables by region and report number, then drop the report number for presentation
  biomass_nums_summary = biomass_nums_summary%>%
    dplyr::arrange(.data$management_area, .data$REPORT_NUMBER)%>%
    #turn manaement area to a character so 'Total' can also be used to id management areas
    dplyr::mutate(management_area = as.character(.data$management_area))%>%
    dplyr::ungroup()%>%
    dplyr::select(- .data$REPORT_NUMBER)

  #build a totals row at the bottom too
  survey_totals = data.frame('Survey total',
                             NA,
                             sum(biomass_nums_length_data$BIOMASS)/1e3,
                             sum(biomass_nums_length_data$NUMBERS)/1e6)
  colnames(survey_totals) = c('management_area', 'region', 'biomass_t', 'num_million')
  biomass_nums_summary = dplyr::bind_rows(biomass_nums_summary, survey_totals)

  #define a border to use above and below caption and at bottom of table
  table_border = officer::fp_border(color="black", width = 0.75)

  #make the summary table into a flextable
  biomass_nums_table = flextable::flextable(biomass_nums_summary)

  #get table header rows in a dataframe that : 1) has the column names (col_keys),
  #2), defines first row of header (row1, below)
  header_keys = data.frame(
    col_keys = c(biomass_nums_table$col_keys),
    row1 = c('Reporting area', 'Survey region', 'Biomass (t)', 'Numbers (millions)'),
    stringsAsFactors = FALSE)

  #add the header
  biomass_nums_table = flextable::set_header_df(biomass_nums_table, mapping = header_keys, key = 'col_keys')%>%
    #get rid of padding around cells
    flextable::padding(padding = 0, part = "all")%>%
    #align text: center justify everything
    flextable::align(align = 'center', part = 'all')%>%
    #add horizontal border on top and bottom of table
    flextable::hline_top(part="all", border = table_border)%>%
    flextable::hline_bottom(part="all", border = table_border)%>%
    #apply function to the reporting regions to add spaces below each management area
    flextable::hline(i = ~ break_position(management_area), border = table_border)%>%
    flextable::padding(i = ~ break_position(management_area), padding.bottom = 10)%>%
    #manually fix column widths to end up w/a reasonable table width
    flextable::width(j = c('management_area', 'biomass_t', 'num_million'), width = 1.5)%>%
    flextable::width(j = c('region'), width = 2)%>%
    #format biomass/numbers columns
    flextable::colformat_double(j = c('biomass_t', 'num_million'),  digits = 2, big.mark=",")%>%
    #set the font and font size
    flextable::font(fontname = 'times', part = 'all')%>%
    flextable::fontsize(size = 10, part = 'header')%>%
    flextable::fontsize(size = 10, part = 'body')

  #build the caption
  cap_text = paste0('Pollock biomass (metric tons) and numbers (millions) by NMFS reporting area for the ',
                    unique(biomass_nums_length_data$year), ' GOA summer survey.')

  #return table and caption
  return(list(biomass_nums_table, cap_text))

}
