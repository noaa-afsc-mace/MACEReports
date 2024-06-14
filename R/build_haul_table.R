#' @title Create trawl haul effort table for MACE cruise reports
#'
#' @description Produce a haul table for use in standard MACE cruise reports.
#' @param haul_data The name of the dataframe that contains all needed values; this dataframe is created
#' in the cruise report markdown process using the get_haul_table_data.R function. \code{haul_data} is
#' checked to make sure that all columns and data types are present and correct; if there are problems with
#' \code{haul_data}, errors will be returned that (hopefully) point out where problems exist.
#'
#' @return A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
#'
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' # build the table and caption
#' haul_table_list <- build_haul_table(haul_data = shelikof_haul_data)
#'
#' # pull out table and caption from the list
#' haul_table <- haul_table_list[[1]]
#' haul_caption <- haul_table_list[[2]]
#' }
#'
#' @export
build_haul_table <- function(haul_data) {
  ###########################################

  # check input dataframe against the template dataframe: this will make sure the input
  # data can actually be used to create a table, and will return errors if not
  check_data <- MACEReports::template_df_haul_data
  MACEReports::check_input_df(template_df = check_data, input_df = haul_data)

  # get the survey year
  survey_year <- min(lubridate::year(haul_data$EQ_TIME))

  # format date more nicely nicely
  haul_data$date <- paste0(lubridate::mday(haul_data$EQ_TIME), "-", lubridate::month(haul_data$EQ_TIME, label = TRUE))

  # make datetime into a character, split string to get the part we want
  haul_data$time_hhmm <- strftime(as.character(haul_data$EQ_TIME))
  # split at the gap between day and time
  haul_data$time_hhmm <- strsplit(haul_data$time_hhmm, " ")
  # and only keep the time
  haul_data$time_hhmm <- lapply(haul_data$time_hhmm, "[", 2)
  # chop the last three characters off (i.e. ':ss'  part) to get hh:mm
  haul_data$time_hhmm <- substr(haul_data$time_hhmm, 1, nchar(haul_data$time_hhmm) - 3)

  # format decimal degrees to 4 decimals
  haul_data$lat <- round(haul_data$EQ_LATITUDE, digits = 4)
  haul_data$lon <- round(haul_data$EQ_LONGITUDE, digits = 4)

  # format table with columns where they should be and small separator rows between haul duration/start lat,
  # start long/gear depth; bottom depth/gear temp; surface temp/pollock kg'; pollock #/other kg'
  haul_data <- cbind.data.frame(
    haul_data$EVENT_ID, haul_data$region, haul_data$GEAR, haul_data$date,
    haul_data$time_hhmm, haul_data$DURATION_MINS, haul_data$lat,
    haul_data$lon, "", haul_data$HEAD_ROPE_DEPTH, haul_data$BOTTOM_DEPTH,
    "", haul_data$HEAD_ROPE_TEMP, haul_data$SURFACE_TEMP, "",
    haul_data$POLLOCK_WEIGHT, haul_data$POLLOCK_NUMBERS, "",
    haul_data$NON_POLLOCK_WEIGHT
  )

  colnames(haul_data) <- c(
    "haul", "area", "gear", "date", "time", "duration", "start_lat", "start_lon",
    "sep1", "gear_depth", "bot_depth", "sep2", "gear_temp", "surface_temp", "sep3",
    "pk_weight", "pk_nums", "sep4", "non_pk_weight"
  )

  # get header rows in a dataframe that : 1) has the column names (col_keys),
  # 2), defines first row of header (row1, below), 3) defines second row (row2, below)
  header_keys <- data.frame(
    col_keys = colnames(haul_data),
    row1 = c(
      "Haul", "Area", "Gear", "Date", "Time", "Duration", "Start Position", "", "", "Depth (m)",
      "", "", paste0("Temp (", intToUtf8(176), "C)"), "", "", "walleye pollock", "", "", "Other"
    ),
    row2 = c(
      "No.", "", "Type", "(GMT)", "(GMT)", "(mins)", "Lat. (N)", "Long. (W)", "", "Headrope",
      "Bottom", "", "Headrope", "Surface", "", "(kg)", "Number", "", "(kg)"
    ),
    stringsAsFactors = FALSE
  )

  # set a table border above and below caption and at bottom of table
  table_border <- officer::fp_border(color = "black", width = 1.0)

  # make into a flextable
  haul_table <- flextable::flextable(haul_data) %>%
    # set the header
    flextable::set_header_df(mapping = header_keys, key = "col_keys") %>%
    # align text: center justify everything
    flextable::align(align = "center", part = "all") %>%
    # add horizontal border on top and bottom of table
    flextable::hline_top(part = "all", border = table_border) %>%
    flextable::hline_bottom(part = "all", border = table_border) %>%
    # add a line under things in the first row too
    flextable::hline(i = 1, j = c(7:8, 10:11, 13:14, 16:17, 19), part = "header", border = table_border) %>%
    # get rid of padding around cells
    flextable::padding(padding = 0, part = "all") %>%
    # let everything  autosize to start at 9 in width, then modify some:
    flextable::fit_to_width(max_width = 9) %>%
    # set some column widths to get format close to right, 10" total width
    # separators can be really thin, make smaller numbers narrow as well,
    flextable::width(j = c("sep1", "sep2", "sep3", "sep4"), width = 0.04) %>%
    # these have to be a min width to fit caption
    flextable::width(j = c("date", "time"), width = 0.5) %>%
    flextable::width(j = c("bot_depth", "surface_temp", "duration", "gear_temp", "gear_depth"), width = 0.5) %>%
    # format numbers for nicer printing; no 1000's comma separator to save space; add '-' where no data exists
    # one decimal point columns:
    flextable::colformat_double(
      j = c("duration", "gear_temp", "surface_temp", "pk_weight", "non_pk_weight"),
      digits = 1, na_str = "-", big.mark = ","
    ) %>%
    # no decimal point columns
    flextable::colformat_double(
      j = c("haul", "gear_depth", "bot_depth", "pk_nums"),
      digits = 0, na_str = "-", big.mark = ","
    )


  # get footnote for each gear type- this function maps the common gear types to nicer names;
  # you might have to add more gear names as things grow over time
  footnote_text_function <- function(gear_type) {
    if (gear_type == "AWT") {
      gear_name <- "Aleutian Wing Trawl"
    } else if (gear_type == "PNE") {
      gear_name <- "poly Nor'eastern bottom trawl"
    } else if (gear_type == "LFS1421") {
      gear_name <- "LFS1421 midwater trawl"
    } else if (gear_type == "83/112") {
      gear_name <- "Bering Sea bottom trawl"
    } else if (gear_type == "Methot") {
      gear_name <- "Methot plankton net"
    } else if (gear_type == "Marinovich") {
      gear_name <- "Marinovich small midwater trawl"
    } else if (gear_type == "StereoDropCam") {
      gear_name <- "camera drop on untrawlable areas."
    } else {
      gear_name <- ""
    }

    return(paste0(gear_type, " = ", gear_name))
  }

  # get all the gear names
  text_list <- purrr::map_chr(unique(haul_data$gear), footnote_text_function)

  # and combine them in a sentence
  gear_footnote <- paste(c(text_list), collapse = ", ")

  # add footnotes
  haul_table <- flextable::footnote(haul_table,
    i = 2, j = c("gear", "gear_depth", "surface_temp"),
    value = flextable::as_paragraph(c(
      gear_footnote,
      "Headrope depth obtained from SBE temperature logger. In hauls without SBE temperature logger records, depth was obtained from scientist notes when possible.",
      "Average temperature measured from an SBE temperature logger"
    )),
    ref_symbols = c("a", "b", "c"), part = "header"
  )


  # set the font and font size after we make the footnotes
  haul_table <- flextable::font(haul_table, fontname = "times", part = "all") %>%
    flextable::fontsize(size = 8, part = "header") %>%
    flextable::fontsize(size = 8, part = "body")

  # get some caption text; use 'combine_words' to print the regions in the survey as 'a, b, and c'
  cap_text <- paste0(
    "Trawl stations and catch data summary from the ", survey_year,
    " acoustic-trawl survey of walleye pollock in the ",
    knitr::combine_words(unique(as.character(haul_data$area))), " regions."
  )

  # return table and caption
  return(list(haul_table, cap_text))
}
