#' @title Create the old 'pre-selectivity era' biomass at length table for winter MACE cruise reports
#' @description Build the historic pre-selectivity corrected era table for the Shelikof Strait survey. This currently presents values from 1981-2007; these values are stored as static, formatted 'canned' values gleaned from historic cruise reports. If you want to change the static dataframe (for example, removing years as more selectivity-corrected values become available, the data is available within MACEReports package: see \code{shelikof_historic_biomass_by_length_1981_to_2007}.
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#' @author Mike Levine
#' @examples
#' \dontrun{
#' # build the table and the caption
#' shelikof_historical_biomass_length_list <-
#'   build_shelikof_pre_selectivity_corrected_biomass_length_table()
#'
#' # pull out the table and caption from the list
#' shelikof_historical_biomass_length_table <- shelikof_historical_biomass_length_list[[1]]
#' shelikof_historical_biomass_length_caption <- shelikof_historical_biomass_length_list[[2]]
#' }
#' @export
build_shelikof_pre_selectivity_corrected_biomass_length_table <- function() {
  ##############
  # 1. Biomass-at-length table

  # the table data is stored in /data as as pre-formatted/canned object named:
  # shelikof_historic_biomass_by_length_1981_to_2007.rda
  shelikof_historic_biomass_by_length_1981_to_2007 <- MACEReports::shelikof_historic_biomass_by_length_1981_to_2007

  # define a border to use above and below caption and at bottom of table
  table_border <- officer::fp_border(color = "black", width = 1.0)

  # make this into a flextable
  biomass_length_table <- flextable::flextable(shelikof_historic_biomass_by_length_1981_to_2007) %>%
    # set the width of the table as 9" for landscape format
    flextable::fit_to_width(max_width = 9) %>%
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
    flextable::fontsize(size = 9, part = "footer") %>%
    # make sure format is consistent: no decimal, no commas
    flextable::colformat_double(big.mark = "", digits = 0)

  # add a line separating the totals column:
  biomass_length_table <- flextable::hline(biomass_length_table,
    i = biomass_length_table$body$content$content$nrow - 1, part = "body",
    border = table_border
  )

  # build the caption:
  # note it is hard-coded to say Shelikof Strait because that's the only region we currently make this table for

  # get the years for the caption by reshaping data and getting the min and max values
  years_for_caption <- as.numeric(colnames(shelikof_historic_biomass_by_length_1981_to_2007
  [2:length(colnames(shelikof_historic_biomass_by_length_1981_to_2007))]))
  # build the caption
  cap_text <- paste0(
    "Biomass-at-length estimates (thousands of metric tons) from acoustic-trawl surveys of pollock in the ",
    "Shelikof Strait area from ", min(years_for_caption), " to ", max(years_for_caption),
    ". Numbers reflect values that have not been corrected for escapement of juveniles."
  )

  # return table and caption
  return(list(biomass_length_table, cap_text))
}
