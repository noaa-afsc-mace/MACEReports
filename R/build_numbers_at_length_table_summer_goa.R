#' @title build_numbers_at_length_table_summer_goa
#'
#' @description Build the pollock numbers-at-length table used in summer GOA reports. This will report all lengths
#' from 10-70 cm, and will do so for each Report Number (i.e. survey region) that was defined for the analysis.
#' If you have an especially small (<10 cm) or large (>70 cm) length class, the table will report these sizes as opposed
#' to the nomimal 10 cm- 70 cm range.
#' @param biomass_nums_data The name of the dataframe that contains all needed values; this dataframe is created
#' in the cruise report markdown process using the function \code{get_biomass_and_nums_data_function.R}; this provides all
#' the data that is then used to create a 'current survey only/pollock only' dataframe named
#' \code{current_survey_pollock_biomass_nums}. This dataframe is the easiest input to
#'  \code{build_numbers_at_length_table_summer_goa} function.
#'
#'  @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#'  @author Mike Levine
#'
#' @examples
#' \dontrun{
#' #build the table and caption
#' nums_table_list =
#' build_numbers_at_length_table_summer_goa(biomass_nums_data = current_survey_pollock_biomass_nums)
#'
#' #pull out table and caption from the list
#' nums_table =  nums_table_list[[1]]
#' nums_caption = nums_table_list[[2]]
#' }
#' @export
build_numbers_at_length_table_summer_goa = function(biomass_nums_data){

  #data checks:

  #check input dataframe against the template dataframe: this will make sure the input
  #data can actually be used to create a table, and will return errors if not
  check_data = template_df_numbers_biomass_at_length_tables_summer_goa
  check_input_df(template_df = check_data, input_df = biomass_nums_data)

  #check- make sure there's only one species, warn if not pollock
  species_list = unique(biomass_nums_data$SPECIES_CODE)

  if (length(species_list) > 1){
    stop(paste0('biomass_nums_data contains multiple species codes: ',
                paste(shQuote(species_list), collapse=", "), '\n'))
  }

  if (species_list != 21740){
    warning('You are printing a table for species code ', species_list, ' not walleye pollock! Is this okay?')
  }

  #check: make sure only one survey year
  if (length(unique(biomass_nums_data$SURVEY)) > 1){
    stop(paste0('biomass_nums_data contains multiple species surveys: ',
                paste(shQuote(unique(biomass_nums_data$SURVEY)), collapse=", "), '\n'))
  }


  #######
  #step 1: sum nums vertically by interval, length bin; report as numbers (million)
  biomass_nums_summary = biomass_nums_data%>%
    dplyr::group_by(.data$REPORT_NUMBER, .data$region, .data$LENGTH)%>%
    dplyr::summarize(num_million = sum(.data$NUMBERS)/1e6)%>%
    dplyr::arrange(.data$REPORT_NUMBER, .data$LENGTH)

  #add a totals column as well
  table_totals = biomass_nums_summary%>%
    dplyr::group_by(.data$LENGTH)%>%
    dplyr::summarize(Total = sum(.data$num_million))

  #####
  #step 2: get vectors of every possible length in the survey areas:

  #we report all lengths from 10-70 cm; it's unlikely that any given survey contains all these, but we want to report a blank in these cases

  #build a dataframe that's simply each possible length per region
  get_all_possible_lengths_for_region = function(region_name){

    #set the minimum length at 10 cm UNLESS there are smaller fish in the survey
    min_report_length = ifelse(min(biomass_nums_summary$LENGTH) < 10,
                               min(biomass_nums_summary$LENGTH),
                               10)

    #set the maximum length as 70 cm UNLESS there are larger fish in the survey
    max_report_length = ifelse(max(biomass_nums_summary$LENGTH > 70),
                               max(biomass_nums_summary$LENGTH),
                               70)

    #build a dataframe that has the length vector and report name
    report_lengths = data.frame(seq(min_report_length, max_report_length, 1))
    colnames(report_lengths) = c("LENGTH")
    report_lengths$region = region_name

    #and return this dataframe
    return(report_lengths)

  }

  #apply function to get dataframe of lengths
  report_lengths_for_regions = purrr::map_dfr(unique(biomass_nums_summary$region), get_all_possible_lengths_for_region)

  #and add the blank rows where no fish were observed
  biomass_nums_summary = dplyr::left_join(report_lengths_for_regions, biomass_nums_summary, by = c('LENGTH', 'region'))

  #######
  #3. make the table

  #for presentation, we just want numbers (millions) for each length bin (10-70 cm and geographic area)
  nums_for_presentation = biomass_nums_summary%>%
    #remove the REPORT_NUMBER column- it was only needed to order things by report number
    dplyr::select(.data$LENGTH, .data$region, .data$num_million)%>%
    #also, go from 'long' format (where every region lives in it's own)
    tidyr::pivot_wider(id_cols = .data$LENGTH, names_from = .data$region, values_from = .data$num_million)

  #join totals to the 'wide' dataframe
  nums_for_presentation = dplyr::left_join(nums_for_presentation, table_totals, by = c('LENGTH'))

  #rename LENGTH to a nicer title for plotting
  nums_for_presentation = nums_for_presentation%>%
    dplyr::rename('Length' = 'LENGTH')

  #calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there are lots
  #of na's, which is fine)

  #this will sum up every column in the survey, except for the first (which is the Length column)
  #we'll just add 'Total' here  to represent the length column, and add up the rest as their own column
  # do this in a loop as the number of survey areas (i.e. columns in table) can differ annually
  col_sums= c('Total')
  for (i in 2:ncol(nums_for_presentation)){

    #get the column summed; format for table
    col_sum_formatted = format(sum(nums_for_presentation[,i], na.rm = TRUE),
                               big.mark = ",",  na_str = '-', nsmall = 2, digits = 2, scientific = FALSE)

    #add it to the collection of formatted sums
    col_sums = cbind(col_sums, col_sum_formatted)

  }

  #define a border to add above and below caption and at bottom of table
  table_border = officer::fp_border(color="black", width = 0.75)

  #make the nums-at-length table into a flextable
  nums_length_table = flextable::flextable(nums_for_presentation)%>%
    #append totals as a footer row
    flextable::add_footer_row(top = FALSE, values = col_sums, colwidths = rep(1,length(nums_for_presentation)))%>%
    #get rid of padding around cells
    flextable::padding(padding = 0, part = "all")%>%
    #align text: center justify everything
    flextable::align(align = 'center', part = 'all')%>%
    #add horizontal border on top and bottom of table
    flextable::hline_top(part="all", border = table_border)%>%
    flextable::hline_bottom(part="all", border = table_border)


  #create a function to format catch weights and percents and numbers percents conditionally:
  #if values = NA, label as '-'
  #If values are <=0.01, label as '<0.01';
  #otherwise, format the value as having a single decimal point for display;
  pretty_nums_function = function(x) ifelse(x > 0.01 | is.na(x),
                                            ifelse(is.na(x), '-', formatC(x, format="f", digits = 2, big.mark=",")),
                                            '<0.01')

  #to format all the columns that could exist (i.e. different years will have different numbers of reporting columns),
  #we need a named list of column names and the function to apply to said column;
  #this needs to skip the first column too (no need to format 'Length')
  funs_list =  stats::setNames(rep(list(pretty_nums_function),
                                   length(names(nums_for_presentation))-1),
                               names(nums_for_presentation)[-1])

  #apply this function to the specfied table columns
  nums_length_table = flextable::set_formatter(nums_length_table, values = funs_list)%>%
    #set the font and font size
    flextable::font(fontname = 'times', part = 'all')%>%
    flextable::fontsize(size = 10, part = 'header')%>%
    flextable::fontsize(size = 10, part = 'body')

  #build the caption
  cap_text = paste0('Pollock numbers-at-length estimates (millions of fish) by survey region during the ',
                    unique(biomass_nums_data$year), ' GOA survey.' )

  #return table and caption
  return(list(nums_length_table, cap_text))

}
