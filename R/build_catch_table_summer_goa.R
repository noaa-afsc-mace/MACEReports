#' @title Create trawl catch table for MACE summer GOA cruise reports
#'
#' @description build the catch by species and numbers by gear; create a table that is in the format
#' used in the Gulf of Alaska summer reports. This differs from the winter version in the format of the input data.
#'
#' @param catch_data a dataframe returned from the 'get_catch_table_data.R' function, summer GOA cruise reports
#' @param specimen_data a dataframe returned from the 'get_specimen_table_data.R' function, summer GOA cruise reports
#' @param scaling_hauls_list an (optional) numeric vector of hauls that were used to scale backscatter in your analysis.
#' These hauls are returned from the \code{scaling_hauls} dataframe in summer GOA cruise reports.
#' If not specified, all hauls in the survey will be reported in tables.
#' @return A list (or multiple lists, if there are multiple gear types)
#' with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' #build the tables and captions
#' catch_tables = build_catch_table_summer_goa(catch_data = raw_catch_data,
#'                                             specimen_data = specimen_measurements_data)
#'
#' #unpack the tables and the captions
#' table_1 = catch_tables[[1]][[1]]
#' caption_1 = catch_tables[[1]][[2]]
#'
#' table_2 = catch_tables[[2]][[1]]
#' caption_2 = catch_tables[[2]][[2]]
#'
#' #build the tables and captions
#' catch_tables = build_catch_table_summer_goa(catch_data = raw_catch_data,
#'                                             specimen_data = specimen_measurements_data,
#'                                             scaling_hauls_list = scaling_hauls$EVENT_ID)
#'
#'
#' print the tables and the captions
#' for (i in 1:length(catch_tables)){
#'
#'  #get table
#'   print(catch_tables[[i]][[1]])
#'   #get caption
#'   print(catch_tables[[i]][[2]])
#' }
#' @export
build_catch_table_summer_goa = function(catch_data, specimen_data, scaling_hauls_list = NULL){

  #check input dataframes against the template dataframes: this will make sure the input
  #data can actually be used to create a table, and will return errors if not
  check_catch_data = template_df_catch_data_summer_goa
  MACEReports::check_input_df(template_df = check_catch_data, input_df = catch_data)

  check_specimen_data = template_df_specimen_data_summer_goa
  MACEReports::check_input_df(template_df = check_specimen_data, input_df = specimen_data)

  #summarize the catch data for the table: by species, for all the requested hauls

  #if only the scaling hauls, limit catch to scaling hauls PLUS methots
  if (!is.null(scaling_hauls_list)){

    #check: make sure we have a numeric vector of hauls
    if (!is.numeric(scaling_hauls_list)){
      stop('scaling_hauls_list should be a numeric vector')
    }

    catch_data = catch_data%>%
      dplyr::filter(.data$EVENT_ID %in% scaling_hauls_list | .data$GEAR == 'Methot')

  }

  #summarize the specimen data- number of lengths/weights per haul
  specimen_summary = specimen_data%>%
    dplyr::mutate(length_taken = ifelse(!is.na(.data$organism_length), 1, 0),
           weight_taken = ifelse(!is.na(.data$ORGANISM_WEIGHT), 1 ,0))%>%
    dplyr::group_by(.data$HAUL, .data$SPECIES_CODE)%>%
    dplyr::summarize(L = sum(.data$length_taken),
              W = sum(.data$weight_taken))

  #add this summary to the catch data- left join so only hauls in tha catch summary have specimens associated
  #with them (in case user selected scaling hauls only)
  catch_summary = dplyr::left_join(catch_data, specimen_summary, by = c('EVENT_ID' = 'HAUL', 'SPECIES_CODE'))

  #summarize by species- weight, number
  catch_table_data = catch_summary%>%
    dplyr::group_by(.data$GEAR, .data$COMMON_NAME, .data$SCIENTIFIC_NAME)%>%
    dplyr::summarize(TOT_WEIGHT = sum(.data$WEIGHT_IN_HAUL),
              TOT_NUMBER = sum(.data$NUMBER_IN_HAUL),
              L = sum(.data$L, na.rm = TRUE),
              W = sum(.data$W, na.rm = TRUE))

  #clean up a few 'dummy' species names
  catch_table_data$SCIENTIFIC_NAME[catch_table_data$SCIENTIFIC_NAME == 'peanutbutter'] = '-'

  #function to build the table for each gear type

  make_table_for_gear_function = function(gear_type){

    #limit the catch to given gear type, and calculate the percent by wt/number for each catch item too
    catch_table_summary = catch_table_data%>%
      dplyr::filter(.data$GEAR == gear_type)%>%
      dplyr::group_by(.data$COMMON_NAME, .data$SCIENTIFIC_NAME)%>%
      #sum all the weights/numbers up- the data is on a per-region basis,
      #and we want to calculate an overall total
      dplyr::summarize(TOT_WEIGHT = sum(.data$TOT_WEIGHT, na.rm = TRUE),
                TOT_NUMBER = sum(.data$TOT_NUMBER, na.rm = TRUE),
                L = sum(.data$L, na.rm = TRUE),
                W = sum(.data$W, na.rm = TRUE))

    #calculate the percent wt/percent number
    catch_table_summary$percent_wt = catch_table_summary$TOT_WEIGHT/
      sum(catch_table_summary$TOT_WEIGHT, na.rm = TRUE) * 100

    catch_table_summary$percent_num = catch_table_summary$TOT_NUMBER/
      sum(catch_table_summary$TOT_NUMBER, na.rm = TRUE) * 100

    #order by weight
    catch_table_summary = catch_table_summary%>%
      #make sure things are ordered by weight
      dplyr::arrange(dplyr::desc(.data$TOT_WEIGHT))%>%
      #drop the gear column- we don't need that in the table as a column;
      #also reorder columns for making the table
      dplyr::select(.data$COMMON_NAME, .data$SCIENTIFIC_NAME, .data$TOT_WEIGHT,
                    .data$percent_wt, .data$TOT_NUMBER, .data$percent_num,
                    .data$L, .data$W)

    #add a few separators in for table formatting
    catch_table_summary = cbind.data.frame(catch_table_summary$COMMON_NAME, catch_table_summary$SCIENTIFIC_NAME,
                                           catch_table_summary$TOT_WEIGHT, catch_table_summary$percent_wt,
                                           catch_table_summary$TOT_NUMBER, catch_table_summary$percent_num, '',
                                           catch_table_summary$L, catch_table_summary$W)
    colnames(catch_table_summary) = c('COMMON_NAME', 'SCIENTIFIC_NAME', 'TOT_WEIGHT', 'percent_wt',
                                      'TOT_NUMBER', 'percent_num', 'sep', 'L', 'W')

    #get header rows in a dataframe that : 1) has the column names (col_keys),
    #2), defines first row of header (row1, below), 3) defines second row (row2, below)
    header_keys = data.frame(
      col_keys = c('COMMON_NAME', 'SCIENTIFIC_NAME', 'TOT_WEIGHT' ,'percent_wt', 'TOT_NUMBER', 'percent_num', '',
                   'L', 'W'),
      row1 = c('','', 'Catch','', '','','', 'Measurements',''),
      row2 = c('Species name', 'Scientific name', 'Weight (kg)', '%','Number','%','','Length', 'Weight'),
      stringsAsFactors = FALSE)

    #calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there are lots
    #of na's, which is fine)
    #also format things for the footer- 1000's separated by commas
    footer_row =c('Total',
                  '',
                  format(sum(catch_table_summary$TOT_WEIGHT, na.rm = TRUE),
                         big.mark = ",",  na_str = '-', nsmall = 1, digits = 1, scientific = FALSE),
                  '',
                  format(sum(catch_table_summary$TOT_NUMBER, na.rm = TRUE),
                         big.mark = ",",  na_str = '-', nsmall = 0, scientific = FALSE),
                  '',
                  '',
                  format(sum(catch_table_summary$L, na.rm = TRUE),
                         big.mark = ",",  na_str = '-', nsmall = 0, scientific = FALSE),
                  format(sum(catch_table_summary$W, na.rm = TRUE),
                         big.mark = ",",  na_str = '-', nsmall = 0, scientific = FALSE))

    #add a border above and below caption and at bottom of table
    table_border = officer::fp_border(color="black", width = 0.75)

    #make the catch table into a flextable
    catch_table = flextable::flextable(catch_table_summary)%>%
      #add header row
      flextable::set_header_df(mapping = header_keys, key = 'col_keys')%>%
      #add the footer row
      flextable::add_footer_row(top = FALSE, values = footer_row, colwidths = rep(1,length(catch_table_summary)))%>%
      #merge the header cells for 'Catch' label and
      flextable::merge_at(i = 1, j = 3:6, part = 'header')%>%
      flextable::merge_at(i = 1, j = 8:9, part = 'header')%>%
      #get rid of padding around cells
      flextable::padding(padding = 0, part = "all")%>%
      #align text: center justify everything
      flextable::align(align = 'center', part = 'all')%>%
      #align header text: left for common name, scientific name
      flextable::align(j = 1:2, align = 'left', part = 'all')%>%
      #center everything else in 2nd row
      flextable::align(j = 3:9, align = 'center', part = 'all')%>%
      #add horizontal border on top and bottom of table
      flextable::hline_top(part="all", border = table_border)%>%
      flextable::hline_bottom(part="all", border = table_border)%>%
      #add a line under things in the first row too
      flextable::hline(i = 1, j =c(3:6, 8:9), part = 'header', border = table_border)%>%
      #manually fix column widths to end up w/a ~7.5 in table
      #separators can be really thin, make smaller numbers narrow as well
      flextable::width(j = c('sep'), width = 0.05)%>%
      #separators need to be bigger for some columns to fit caption
      flextable::width(j = c('percent_wt', 'TOT_NUMBER', 'percent_num', 'L', 'W'), width = 0.5)%>%
      flextable::width(j = c('COMMON_NAME', 'SCIENTIFIC_NAME'), width = 1.75)%>%
      flextable::width(j = c('TOT_WEIGHT'), width = 0.75)

    #format catch weights and percents and numbers percents conditionally: If values are <=0.1,
    #label as '<0.1'
    #otherwise, format the value as having a single decimal point for display
    catch_table = flextable::set_formatter(catch_table,
                                TOT_WEIGHT = function(x)
                                  ifelse(x > 0.1, formatC(x, format="f", digits = 1, big.mark=","), '<0.1'),
                                TOT_NUMBER = function(x)
                                  ifelse(x > 0.1, formatC(x, format="d", big.mark=","), '<0.1'),
                                percent_wt = function(x)
                                  ifelse(x > 0.1, formatC(x, format="f", digits = 1, big.mark=","), '<0.1'),
                                percent_num = function(x)
                                  ifelse(x > 0.1, formatC(x, format="f", digits = 1, big.mark=","), '<0.1'))


    #format individual measurements columns; add '-' where no data exists
    #whole integer columns
    catch_table = flextable::colformat_double(catch_table, j = c('L', 'W'), digits = 0, na_str = '-', big.mark = ",")

    #try to format the scientific names correctly
    #Rule: if the species name doesn't have '(' as in (class), (order), etc, don't italicize
    #but allow 'sp.' to be italicized
    #this should catch and italicize species names
    catch_table = flextable::italic(catch_table,
                         i = ~ (!grepl('\\(', SCIENTIFIC_NAME)),
                         j = ~ SCIENTIFIC_NAME,
                         part = 'body')

    #set the font and font size
    catch_table = flextable::font(catch_table, fontname = 'times', part = 'all')%>%
      flextable::fontsize(size = 10, part = 'header')%>%
      flextable::fontsize(size = 10, part = 'body')


    #gather the total number of hauls for the caption text
    n_hauls = length(unique(catch_data$EVENT_ID[catch_data$GEAR == gear_type]))

    #build the caption- get the survey year from MACE standard naming convention where surveys are YYYYXX
    cap_text = paste0('Catch by species and numbers of length and weight measurements taken from ', n_hauls,
                      ' ', gear_type,' hauls during the ', substr(unique(specimen_data$SURVEY), 1, 4),
                      ' GOA acoustic-trawl survey.')

    #return table and caption
    return(list(catch_table, cap_text, gear_type))

  }

  #apply this function to make a table for each gear type in the region
  return(purrr::map(unique(catch_table_data$GEAR), make_table_for_gear_function))

}
