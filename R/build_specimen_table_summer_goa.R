#' @title Create biological specimen table for MACE summer GOA cruise reports
#'
#' @description Produce the specimen table for use in standard MACE GOA summer reports. This differs from the specimen table in other MACE reports by including the GOA reporting species- currently pollock, capelin, and POP.
#' @param specimen_data a dataframe returned from the 'get_specimen_table_data.R' function, summer GOA cruise reports
#' @param scaling_hauls_list  an (optional) numeric vector of hauls that were used to scale backscatter in your analysis.
#' These hauls are returned from the \code{scaling_hauls} dataframe in summer GOA cruise reports.
#' If not specified, all hauls in the survey will be reported in the table.#'
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' #build the table and caption
#' specimen_table_list = build_specimen_table_summer_goa(specimen_data = specimen_table_data,
#'                                                       scaling_hauls_list = scaling_hauls$EVENT_ID)
#'
#' #unpack tables and captions
#' specimen_table = specimen_table_list[[1]]
#' specimen_caption = specimen_table_list[[2]]
#' }
#' @export
build_specimen_table_summer_goa = function(specimen_data, scaling_hauls_list = NULL){

  #check input dataframes against the template dataframes: this will make sure the input
  #data can actually be used to create a table, and will return errors if not
  check_specimen_data = MACEReports::template_df_specimen_table_summer_goa
  MACEReports::check_input_df(template_df = check_specimen_data, input_df = specimen_data)

  #if only the scaling hauls, limit table to scaling hauls
  if (!is.null(scaling_hauls_list)){

    #check: make sure we have a numeric vector of hauls
    if (!is.numeric(scaling_hauls_list)){
      stop('scaling_hauls_list should be a numeric vector')
    }

    specimen_data = specimen_data%>%
      dplyr::filter(.data$HAUL %in% scaling_hauls_list)

  }

  #pull a survey year out of the specmen data
  tmp_survey_year = substr(unique(specimen_data$SURVEY), 1, 4)

  #reorder table with columns where they should be
  specimen_data = specimen_data[,c('HAUL', 'region', 'POLLOCK_CATCH_LENGTHS', 'POLLOCK_WEIGHTS', 'MATURITIES', 'OTOLITHS',
                                   'O_TAKEN', 'CAPELIN_CATCH_LENGTHS', 'CAPELIN_WEIGHTS', 'POP_CATCH_LENGTHS', 'POP_WEIGHTS',
                                   'OTHER_CATCH_LENGTHS', 'OTHER_WEIGHTS')]

  #get header rows in a dataframe that : 1) has the column names (col_keys),
  #2), defines first row of header (row1, below), 3) defines second row (row2, below)
  header_keys = data.frame(
    col_keys = c('HAUL', 'region', 'POLLOCK_CATCH_LENGTHS', 'POLLOCK_WEIGHTS', 'MATURITIES', 'OTOLITHS',
                 'O_TAKEN', 'CAPELIN_CATCH_LENGTHS', 'CAPELIN_WEIGHTS', 'POP_CATCH_LENGTHS', 'POP_WEIGHTS',
                 'OTHER_CATCH_LENGTHS', 'OTHER_WEIGHTS'),
    row1 = c('Haul','Region', 'Pollock','Pollock', 'Pollock', 'Pollock','Pollock', 'Pacific capelin', 'Pacific capelin',
             'Pacific ocean perch', 'Pacific ocean perch', 'Other', 'Other'),
    row2 = c('No.', 'Name', 'Lengths', 'Weights','Maturities','Otoliths', 'Ovaries', 'Weights', 'Lengths', 'Weights', 'Lengths', 'Weights', 'Lengths'),
    stringsAsFactors = FALSE)

  #calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there are lots
  #of na's, which is fine)
  #also format things for the footer- 1000's separated by commas
  footer_row =c('Total',
                '',
                format(sum(specimen_data$POLLOCK_CATCH_LENGTHS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$POLLOCK_WEIGHTS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$MATURITIES, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$OTOLITHS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$O_TAKEN, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$CAPELIN_CATCH_LENGTHS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$CAPELIN_WEIGHTS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$POP_CATCH_LENGTHS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$POP_WEIGHTS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$OTHER_CATCH_LENGTHS, na.rm = TRUE), big.mark = ",",  na_str = '-'),
                format(sum(specimen_data$OTHER_WEIGHTS, na.rm = TRUE), big.mark = ",",  na_str = '-'))

  #define a border above and below caption and at bottom of table
  table_border = officer::fp_border(color="black")

  #make specimen data into a flextable
  specimen_table = flextable::flextable(specimen_data)%>%
    #add the header
    flextable::set_header_df(mapping = header_keys, key = 'col_keys')%>%
    #merge the repeated header rows
    flextable::merge_h(i = 1, part = 'header')%>%
    #add the footer row
    flextable::add_footer_row(top = FALSE, values = footer_row, colwidths = rep(1, length(specimen_data)))%>%
    #add horizontal border on top and bottom of table
    flextable::hline_top(part="all", border = table_border)%>%
    flextable::hline_bottom(part="all", border = table_border)%>%
    #add a line under categories in the first row too
    flextable::vline(j = c(2,7,9,11), part = 'all')%>%
    #align  text: center justify everything
    flextable::align(align = 'center', part = 'all')%>%
    #format numbers- in the main table; 1000s separated with commas and empty values with '-'
    flextable::colformat_double(j = c('POLLOCK_CATCH_LENGTHS','POLLOCK_WEIGHTS', 'MATURITIES', 'OTOLITHS', 'O_TAKEN',
                                      'CAPELIN_CATCH_LENGTHS', 'CAPELIN_WEIGHTS', 'POP_CATCH_LENGTHS', 'POP_WEIGHTS',
                                      'OTHER_CATCH_LENGTHS', 'OTHER_WEIGHTS'),
                                    big.mark=",", digits = 0,  na_str = '-')%>%
    #limit padding around cells
    #with the exception of the last row of the header, set padding to 0 pts; add a bit to to last row of header
    flextable::padding(i = 1, padding.bottom = 0, part = "header")%>%
    flextable::padding(i = 2, padding.bottom = 2, part = "header")%>%
    flextable::padding(padding = 0, part = "body")%>%
    #set region name to be wider and fit long locations
    flextable::width(j = c('region'), width = 1.5)%>%
    #0.5" columns
    flextable::width(j = c('HAUL'), width = 0.5)%>%
    #0.70" columns
    flextable::width(j = c('POLLOCK_CATCH_LENGTHS','POLLOCK_WEIGHTS', 'MATURITIES', 'OTOLITHS', 'O_TAKEN',
                           'CAPELIN_CATCH_LENGTHS', 'CAPELIN_WEIGHTS', 'POP_CATCH_LENGTHS', 'POP_WEIGHTS',
                           'OTHER_CATCH_LENGTHS', 'OTHER_WEIGHTS'), width = 0.73)%>%
    #set the font and font size
    flextable::font(fontname = 'times', part = 'all')%>%
    flextable::fontsize(size = 10, part = 'all')


  #get some caption text; use 'combine_words' to print the regions in the survey as 'a, b, and c'
  cap_text = paste0('Numbers of specimens measured and biological samples collected during the ',
                    tmp_survey_year, ' GOA acoustic-trawl survey.')

  #return table and caption
  return(list(specimen_table, cap_text))

}

