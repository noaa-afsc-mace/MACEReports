#' @title Get biomass- and numbers- by length plots with dual y-axis
#' @description Returns a dual-axis plot with numbers as bars and biomass as lines; each abundance measure is plotted on independent y-axis scales. Plot units are automatically scaled to billions of fish, millions of fish OR thousands of fish/ millions of tons, 1000s of tons OR tons based on the magnitude of the biomass or numbers.
#' @param length_vector A vector of fish lengths (units are assumed to be cm in 1 cm increments). This vector must be equal in length to \code{biomass_vector} and \code{numbers_vector}.
#' @param biomass_vector A vector of fish weights (units should be KG. Be sure to provide weights in KG!). This must should be equal in length to \code{length_vector} and \code{numbers_vector}.
#' @param numbers_vector A vector of fish numbers (units should be individual fish. Be sure to provide weights in individual fish!). This must should be equal in length to \code{length_vector} and \code{biomass_vector}.
#' @param add_totals Option to add the totals as the title of the plot. Default is to not add a title. Set to TRUE if you'd like a title with the total biomass and number values.
#' @param cex_label_size the font size of the axis labels (default 1.0). These are specified as scaling factors from the default of 1.0 (i.e. 2 is twice as large).
#' @return A ggplot plot object. Note that the actual plot is a R baseplot; it is returned as a ggplot object so that it can be more easily integrated into Rmarkdown documents.
#'
#' @examples
#' # generate a simulated fish population
#' lengths <- round(rnorm(mean = 30, sd =3, n = 100000), digits = 0)
#' weights <- lengths^3 * 0.01
#' numbers <- sample(c(0, 3e3), size = 100000, replace = TRUE)
#'
#' # plot it
#' plot_biomass_nums_by_length(length_vector = lengths,
#'  biomass_vector = weights,
#'   numbers_vector = numbers)
#'
#' # plot it with totals added as text
#' plot_biomass_nums_by_length(length_vector = lengths,
#' biomass_vector = weights,
#' numbers_vector = numbers,
#' add_totals = TRUE)
#'
#' @export
plot_biomass_nums_by_length <- function(length_vector,
                                        biomass_vector,
                                        numbers_vector,
                                        add_totals = FALSE,
                                        cex_label_size = 1.0){

  # checks: make sure each vector is the same length
  if (!(all(sapply(list(length(length_vector),
                  length(biomass_vector),
                  length(numbers_vector)),
             FUN = identical,
             length(length_vector))))){
    stop("length_vector, biomass_vector, and numbers_vector must be vectors of equal length!")
  }

  # combine the vectors as a dataframe
  abundance_data <- cbind.data.frame(length_vector, biomass_vector, numbers_vector)

  # sum biomass/nums vertically by length bin
  biomass_nums_summary <- abundance_data %>%
    dplyr::group_by(.data$length_vector) %>%
    dplyr::summarize(num = sum(.data$numbers_vector), wt = sum(.data$biomass_vector))

  # we need to return a GGPLOT object so that it can be combined with ggplot objects in markdown documents! Write plot as a function,
  # which can then allow plot to 'draw' to a ggplot object
  make_base_plot <- function() {

    # plot with base r plots- biomass as line, numbers as bars
    # this is done by making the barplot (weight) first, and using the x-axis from the barplot as the x-axis
    # for the lineplot (numbers); this allows for the lines to be centered properly over the bars

    # bigger margins to allow room for text
    graphics::par(mar = c(5, 5, 3, 5))
    # and smaller outer margins to shrink spaces between plots
    graphics::par(oma = c(1, 1, 1, 2))

    # enable Times New Roman font to be used (for windows only!)
    grDevices::windowsFonts("Times" = grDevices::windowsFont("Times New Roman"))

    # get the total vector for plotting- so that bins with no fish get a 0

    # plot from 1-max length +5 cm (to add a buffer at end visually)
    length_bins <- as.data.frame(seq(0, max(biomass_nums_summary$length_vector) + 5, 1))
    colnames(length_bins) <- c("length_vector")

    # if there's no fish at a given length bin from 1-85, make it a 0 instead of NA for plotting
    plot_data <- dplyr::left_join(length_bins, biomass_nums_summary, by = c("length_vector")) %>%
      tidyr::replace_na(list(wt = 0, num = 0))

    # get a common maximum for x axis for all plots in survey;
    x_axis_max <- max(plot_data$length_vector)

    # plot with base r plots- biomass as line, numbers as bars
    # this is done by making the barplot (weight) first, and using the x-axis from the barplot as the x-axis
    # for the lineplot (numbers); this allows for the lines to be centered properly over the bars

    # we need to deal with very small totals and very large areas differently for nice plotting- most
    # areas are best as millions of fish + 1000s of tons; for very small areas thousands of fish and tons are better
    # if there's less than an million tons total, use the smaller scale, if more, use the larger
    units_scaler_numbers <- ifelse(max(plot_data$num) > 1e9,
                                   1e9,
                                   ifelse(max(plot_data$wt) <= 1e9 & max(plot_data$wt) > 10e6,
                                          1e6,
                                          1e3))

    units_scaler_biomass <- ifelse(max(plot_data$wt) > 1e9,
                                   1e9,
                                   ifelse(max(plot_data$wt) <= 1e9 & max(plot_data$wt) > 10e6,
                                          1e6,
                                          1e3))

    units_number_id <- ifelse(max(plot_data$num) > 1e9,
                               "(billions)",
                               ifelse(max(plot_data$wt) <= 1e9 & max(plot_data$wt) > 10e6,
                                      "(millions)",
                                      "(thousands)"))

    units_biomass_id <- ifelse(max(plot_data$wt) > 1e9,
                               "(million t)",
                                 ifelse(max(plot_data$wt) <= 1e9 & max(plot_data$wt) > 10e6,
                                        "(1000s t)",
                                        "(t)"))

    # specify left y-axis limit & define interval width for Numbers
    y_axis_max <- max(plot_data$num / units_scaler_numbers) * 1.05
    # y_axis_int <- ifelse(units_scaler_numbers == 1e3,
    #                      round(y_axis_max/5, digits = -2),
    #                      ifelse(y_axis_max <= 100,
    #                      5,
    #                      round(y_axis_max / 5, digits = -1))
    #                      )

    y_axis_int <- ifelse(units_number_id ==  "(thousands)",
                          round(y_axis_max/5, digits = -1),
                          ifelse(units_number_id == "(millions)" & y_axis_max >= 5 & y_axis_max <= 500,
                                 round(y_axis_max / 5, digits = 0),
                                 round(y_axis_max / 5, digits = 1)))

    # add the numbers bars
    plot_bars <- graphics::barplot(
      height = plot_data$num / units_scaler_numbers, xlab = NA, ylab = NA,
      # up font sizes, rotate labels to perpendicular
      col = "#6baed6", cex.axis = 0.75, cex.names = 0.75, border = "white",
      # make bars fill length bins
      xpd = FALSE, space = 0.0,
      # set xlims as the entire length vector
      xlim = c(0, x_axis_max),
      # add a buffer above max value
      ylim = c(0, y_axis_max),
      # turn off y-axis labels
      yaxt = "n",
      # use times font
      family = "Times"
    )

    # major tick marks every 10 cm, center bars over ticks
    graphics::axis(side = 1, at = seq(0, x_axis_max, 1) + .5, labels = F, col.ticks = "gray50", lwd = 1, tck = -0.02)
    graphics::axis(side = 1, at = seq(5, x_axis_max, 10) + .5, labels = F, col.ticks = "gray50", lwd = 0, lwd.ticks = 1, tck = -0.04)
    graphics::axis(side = 1, at = seq(0, x_axis_max, 10) + .5, labels = seq(0, x_axis_max, 10), col.ticks = "gray50", lwd = 0, lwd.ticks = 1, tck = -0.04, cex.axis = cex_label_size, family = "Times")
    graphics::axis(side = 2, las = 2, at = seq(0, y_axis_max, y_axis_int), cex.axis = cex_label_size, family = "Times", col.axis = "#0072B2")

    # specify right y-axis limit & define interval width for biomass
    y2_axis_max <- max(plot_data$wt / units_scaler_biomass) * 1.05
    y2_axis_int <- ifelse(units_biomass_id ==  "(thousands)",
                          round(y2_axis_max/5, digits = -1),
                          ifelse(units_biomass_id == "(1000s t)" & y2_axis_max >= 5 & y2_axis_max <= 100,
                                 round(y2_axis_max / 5, digits = 0),
                                 round(y2_axis_max / 5, digits = 1)))

    # add lines
    graphics::par(new = T)
    plot_lines <- graphics::plot(plot_bars, plot_data$wt / units_scaler_biomass,
                       col = "#cb181d",
                       type = "l", axes = F, xlab = NA, ylab = NA, yaxs = "i",
                       ylim = c(0, y2_axis_max), lwd = 3,
                       xlim = c(0, x_axis_max),
                       family = "Times"
    )

    # add second axis, format the initial axis correctly too

    # add axis ticks on x axis too
    # major tick marks every 10 cm, center bars over ticks
    graphics::axis(side = 4, las = 2, at = seq(0, y2_axis_max, y2_axis_int), cex.axis = cex_label_size, family = "Times", col.axis = "#cb181d")

    # set the line to put the captions on if cex >1, move them out a bit
    text_pos_r <- ifelse(cex_label_size > 1, 2.5, 2.0)
    text_pos_l <- ifelse(cex_label_size > 1, 4.5, 4.0)
    text_pos_bot <- ifelse(cex_label_size > 1, 2.2, 1.2)

    graphics::mtext(side = 4, line = text_pos_r, paste0("Biomass ", units_biomass_id), cex = cex_label_size, padj = 1, family = "Times", col = "#cb181d")
    graphics::mtext(side = 2, line = text_pos_l, paste0("Numbers of fish ", units_number_id), cex = cex_label_size, padj = 1, family = "Times", col = "#0072B2")
    graphics::mtext(side = 1, line = text_pos_bot, "Length (cm)", cex = cex_label_size, padj = 1, family = "Times")

    # add a box around plot
    graphics::box(lty = "solid", col = "black")

    # add title - either with or without the summed values
    if (add_totals == TRUE) {
      graphics::title(main = paste0(
        "Total abundance: \n", formatC(round(sum(plot_data$num) / units_scaler_numbers, digits = 1),
                                       big.mark = ",", format = "f", digits = 1
        ), " ", stringr::str_remove_all(units_number_id, paste( c('\\(', '\\)', 's'), collapse = "|")),
        " fish and ", formatC(round(sum(plot_data$wt) / units_scaler_biomass, digits = 1),
                                      big.mark = ",", format = "f", digits = 1
        ), " ", stringr::str_remove_all(units_biomass_id, paste( c('\\(', '\\)'), collapse = "|"))
      ), cex.main = 1.5, family = "Times")
    }
  }

  # draw plot to a ggplot object
  tmp_plot <- cowplot::ggdraw(make_base_plot) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))

  # return ggplot object
  return(tmp_plot)


}
