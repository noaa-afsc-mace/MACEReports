#' @title Get biomass- and numbers- by age plots with dual y-axis
#' @description Returns a dual-axis plot with numbers as bars and biomass as lines; each abundance measure is plotted on independent y-axis scales. Plot units are automatically scaled to millions of fish OR thousands of fish/ 1000s of tons OR tons based on the magnitude of the biomass (if there is greater than 1 million KG at any length, the larger scale is used).
#' @param age_vector A vector of fish ages (units are assumed to be cm in 1 year increments). This vector must be equal in length to \code{biomass_vector} and \code{numbers_vector}.
#' @param biomass_vector A vector of fish weights (units are assumed to be KG). This must should be equal in length to \code{age_vector} and \code{numbers_vector}.
#' @param numbers_vector A vector of fish numbers (units are assumed to be individual fish). This must should be equal in length to \code{age_vector} and \code{biomass_vector}.
#' @param add_totals Option to add the totals as the title of the plot. Default is to not add a title. Set to TRUE if you'd like a title with the total biomass and number values.
#' @param cex_label_size the font size of the axis labels (default 1.0). These are specified as scaling factors from the default of 1.0 (i.e. 2 is twice as large).
#' @return A ggplot plot object. Note that the actual plot is a R baseplot; it is returned as a ggplot object so that it can be more easily integrated into Rmarkdown documents.
#'
#' @examples
#' # generate a simulated fish population
#' ages <- round(rnorm(mean = 5, sd =2, n = 100000), digits = 0)
#' numbers <- sample(c(0, 3e5), size = 100000, replace = TRUE)
#' weights <- (ages^3 * .02) * numbers
#'
#' # plot it
#' plot_biomass_nums_by_age(age_vector = ages,
#'  biomass_vector = weights,
#'    numbers_vector = numbers)
#'
#' # plot it with totals added as text
#' plot_biomass_nums_by_age(age_vector = ages,
#' biomass_vector = weights,
#' numbers_vector = numbers,
#' add_totals = TRUE)
#'
#' @export
plot_biomass_nums_by_age <- function(age_vector,
                                     biomass_vector,
                                     numbers_vector,
                                     add_totals = FALSE,
                                     cex_label_size = 1.0){

    # checks: make sure each vector is the same length
    if (!(all(sapply(list(length(age_vector),
                          length(biomass_vector),
                          length(numbers_vector)),
                     FUN = identical,
                     length(age_vector))))){
      stop("age_vector, biomass_vector, and numbers_vector must be vectors of equal length!")
    }

    # combine the vectors as a dataframe
    abundance_data <- cbind.data.frame(age_vector, biomass_vector, numbers_vector)

    # first, sum biomass/nums vertically by interval, length bin
    biomass_nums_age_summary <- abundance_data %>%
      dplyr::group_by(.data$age_vector) %>%
      dplyr::summarize(num = sum(.data$numbers_vector),
                wt = sum(.data$biomass_vector))

    # we need to return a GGPLOT object so that it can be combined with ggplot objects in markdown documents! Write plot as a function,
    # which can then allow plot to 'draw' to a ggplot object
    make_base_plot <- function() {

      # get a common maximum for x axis for all plots in survey
      x_axis_max <- max(biomass_nums_age_summary$age_vector[biomass_nums_age_summary$num > 0])
      # for each region, make the plot

      # plot from 1-max age; first, get a age vector for x axis from 1-max age
      age_bins <- as.data.frame(seq(1, max(biomass_nums_age_summary$age_vector), 1))
      colnames(age_bins) <- c("age_vector")

      # if there's no fish at a given age bin from 1-85, make it a 0 instead of NA for plotting
      biomass_nums_age_summary <- dplyr::left_join(age_bins, biomass_nums_age_summary, by = c("age_vector")) %>%
        tidyr::replace_na(list(wt = 0, num = 0))

      # plot with base r plots- biomass as line, numbers as bars
      # this is done by making the barplot (weight) first, and using the x-axis from the barplot as the x-axis
      # for the lineplot (numbers); this allows for the lines to be centered properly over the bars

      # bigger margins to allow room for text
      # par(mar = c(3, 5, 3, 5))
      graphics::par(mar = c(5, 5, 3, 5))
      # and smaller outer margins to shrink spaces between plots
      graphics::par(oma = c(1, 1, 1, 2))

      # enable Times New Roman font to be used (for windows only!)
      grDevices::windowsFonts("Times" = grDevices::windowsFont("Times New Roman"))

      # specify left y-axis limit & define interval width
      y_axis_max <- round(max(biomass_nums_age_summary$num / 1e6) * 1.05, 0)
      y_axis_int <- ifelse(y_axis_max <= 275, 50, 500)

      # add the numbers bars
      plot_bars <- graphics::barplot(
        height = biomass_nums_age_summary$num / 1e6, xlab = NA, ylab = NA,
        # up font sizes, rotate labels to perpendicular
        col = "#6baed6", cex.axis = 0.75, cex.names = 0.75, border = "white",
        # make bars fill age bins
        xpd = FALSE, space = 0.0,
        # set xlims as the entire age vector
        xlim = c(0, x_axis_max),
        # add a buffer above max value
        ylim = c(0, y_axis_max),
        # turn off y-axis labels
        yaxt = "n",
        # use times font
        family = "Times"
      )

      # major tick marks every 10 cm, center bars over ticks
      graphics::axis(side = 1, at = seq(0, x_axis_max, 1) + .5, labels = seq(1, x_axis_max + 1, 1), cex.axis = 1.0, col = "grey50", col.ticks = "grey50", )
      graphics::axis(side = 2, las = 2, at = seq(0, y_axis_max, y_axis_int), cex.axis = 1.0, family = "Times", col.axis = "#0072B2")

      # add lines
      graphics::par(new = T)
      plot_lines <- graphics::plot(plot_bars, biomass_nums_age_summary$wt / 1e6,
                         col = "#cb181d",
                         type = "l", axes = F, xlab = NA, ylab = NA, yaxs = "i",
                         ylim = c(0, max(biomass_nums_age_summary$wt / 1e6) * 1.05), lwd = 3,
                         xlim = c(0, x_axis_max),
                         family = "Times"
      )

      # add second axis, format the initial axis correctly too
      graphics::axis(side = 4, las = 2, cex.axis = 1.0, family = "Times", col.axis = "#cb181d")

      graphics::mtext(side = 4, line = 2.5, "Biomass (1000s t)", cex = cex_label_size, padj = 1, family = "Times", col = "#cb181d")
      graphics::mtext(side = 2, line = 4.5, "Numbers of fish (millions)", cex = cex_label_size, padj = 1, family = "Times", col = "#0072B2")
      graphics::mtext(side = 1, line = 2, "Age", cex = cex_label_size, padj = 1, family = "Times")

      # add a box around plot
      graphics::box(lty = "solid", col = "black")

      # add title
      # add title - either with or without the summed values
      if (add_totals == TRUE) {
        graphics::title(main = paste0(
          "Total abundance: \n", formatC(round(sum(biomass_nums_age_summary$num) / 1e6, digits = 1),
                                         big.mark = ",", format = "f", digits = 1
          ),
          " million fish and ", formatC(round(sum(biomass_nums_age_summary$wt) / 1e6, digits = 1),
                                        big.mark = ",",
                                        format = "f", digits = 1
          ),
          " thousand metric tons"
        ), cex.main = 1.5, family = "Times")
      }
    }

    # draw plot to a ggplot object
    tmp_plot <- cowplot::ggdraw(make_base_plot) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))

    # return ggplot object
    return(tmp_plot)

}
