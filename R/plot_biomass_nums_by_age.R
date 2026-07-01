#' @title Get biomass- and numbers- by age plots with dual y-axis
#' @description Returns a dual-axis plot with numbers as bars and biomass as lines; each abundance measure is plotted on independent y-axis scales. Plot units are automatically scaled to millions of fish OR thousands of fish/ 1000s of tons OR tons based on the magnitude of the biomass (if there is greater than 1 million KG at any length, the larger scale is used).
#' @param age_vector A vector of fish ages (units are assumed to be cm in 1 year increments). This vector must be equal in length to \code{biomass_vector} and \code{numbers_vector}.
#' @param biomass_vector A vector of fish weights (units should be KG. Be sure to provide weights in KG!). This must should be equal in length to \code{age_vector} and \code{numbers_vector}.
#' @param numbers_vector A vector of fish numbers (units should be individual fish. Be sure to provide weights in individual fish!). This must should be equal in length to \code{age_vector} and \code{biomass_vector}.
#' @param add_totals Option to add the totals as the title of the plot. Default is to not add a title. Set to TRUE if you'd like a title with the total biomass and number values.
#' @param label_report_region Option to add the report region the title of your plot. Default is not to add the report region. If you'd like the report region in your title, provide a vector of report regions. This must be equal in length to \code{length_vector} and \code{biomass_vector}.
#' @param label_size the font size of the axis labels (default 12 point font).
#' @return A ggplot plot object.
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
                                     label_report_region = NULL,
                                     label_size = 12){

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

    # plot from ages 1-15 (unless there's fish older than 15 YO, then plot to there)
    x_axis_max <- ifelse(max(biomass_nums_age_summary$age_vector) > 15,
                         max(biomass_nums_age_summary$age_vector),
                         15)

    # get a age vector for x axis from 1-max age
    age_bins <- as.data.frame(seq(1, x_axis_max, 1))
    colnames(age_bins) <- c("age_vector")

    ##################
    # plot totals on a 2-axis ggplot plot- numbers as bars, biomass as lines.

    # enable Times New Roman font to be used (for windows only!)
    grDevices::windowsFonts(Times = grDevices::windowsFont("Times New Roman"))

    # if there's no fish at a given age bin from 1-85, make it a 0 instead of NA for plotting
    biomass_nums_age_summary <- dplyr::left_join(age_bins, biomass_nums_age_summary, by = c("age_vector")) %>%
      tidyr::replace_na(list(wt = 0, num = 0))

    # we need to deal with very small totals and very large areas differently for nice plotting-
        # Scale y-axis for numbers and biomass
    units_scaler_numbers <- ifelse(max(biomass_nums_age_summary$num) > 1e+09,
                                   1e+09,
                                   ifelse(max(biomass_nums_age_summary$num) <= 1e+09 & max(biomass_nums_age_summary$num) > 1e+06,
                                          1e+06,
                                          1e3))

    # and add the scaled numbers for plotting
    biomass_nums_age_summary$plot_num <- biomass_nums_age_summary$num / units_scaler_numbers

    units_scaler_biomass <- ifelse(max(biomass_nums_age_summary$wt) > 1e+09,
                                   1e+09,
                                   ifelse(max(biomass_nums_age_summary$wt) <= 1e+09 & max(biomass_nums_age_summary$wt) > 1e+06,
                                          1e+06,
                                          1e9))

    # and add the scaled numbers for plotting
    biomass_nums_age_summary$plot_biomass <- biomass_nums_age_summary$wt / units_scaler_biomass

    units_number_id <- ifelse(max(biomass_nums_age_summary$num) > 1e+09,
                              "(billions)",
                              ifelse(max(biomass_nums_age_summary$num) <= 1e+09 &  max(biomass_nums_age_summary$num) > 1e+06,
                                     "(millions)",
                                     "(thousands)"))

    units_biomass_id <- ifelse(max(biomass_nums_age_summary$wt) > 1e+09,
                               "(million t)",
                               ifelse(max(biomass_nums_age_summary$wt) <= 1e+09 &  max(biomass_nums_age_summary$wt) > 1e+06,
                                      "(thousand t)",
                                      "(t)"))



    #######
    # make a title (or not) based on user arguments
    plot_title_text <- NULL

    # if user requested a region name but not the totals, add that as the title
    if (!is.null(label_report_region) & !isTRUE(add_totals)){

      plot_title_text <- unique(label_report_region)

    }

    # if user requested the totals, add totals as the title
    if (isTRUE(add_totals)){

      # if they also requested a report region, add that
      if (!is.null(label_report_region)){

        plot_title_text <-   paste0(unique(label_report_region), " total abundance: \n",
                                    formatC(round(sum(biomass_nums_age_summary$plot_num), digits = 1), big.mark = ",", format = "f", digits = 1), " ", stringr::str_remove_all(units_number_id, paste(c("\\(", "\\)", "s"), collapse = "|")), " fish and ", formatC(round(sum(biomass_nums_age_summary$plot_biomass), digits = 1), big.mark = ",", format = "f", digits = 1), " ", stringr::str_remove_all(units_biomass_id, paste(c("\\(", "\\)"), collapse = "|")))

      }

      # if they didn't request the report region, don't add it
      if (is.null(label_report_region)){

        plot_title_text <-   paste0("Total abundance: \n",
                                    formatC(round(sum(biomass_nums_age_summary$plot_num), digits = 1), big.mark = ",", format = "f", digits = 1), " ", stringr::str_remove_all(units_number_id, paste(c("\\(", "\\)", "s"), collapse = "|")), " fish and ", formatC(round(sum(biomass_nums_age_summary$plot_biomass), digits = 1), big.mark = ",", format = "f", digits = 1), " ", stringr::str_remove_all(units_biomass_id, paste(c("\\(", "\\)"), collapse = "|")))

      }

    }

    ######
    #actually make the plot

    # add a second axis by transforming the numbers data to
    # ratio of the maximum numbers and maximum biomass values
    trans <- max(biomass_nums_age_summary$plot_num)/max(biomass_nums_age_summary$plot_biomass)

    # make the plot
    bio_nums_plot <-
      ggplot2::ggplot() +
      # add the biomass, scaled for plotting
      ggplot2::geom_col(data = biomass_nums_age_summary, ggplot2::aes(x = .data$age_vector, y = .data$plot_num),
                        fill = "#0072B2") +
      # add the numbers as a line, transformed and scaled for plotting
      ggplot2::geom_line(data = biomass_nums_age_summary, ggplot2::aes(x = .data$age_vector, y = .data$plot_biomass * trans),
                         color = "#cb181d", linewidth = 1.0) +
      ggplot2::scale_y_continuous(
        # force label rounding
        labels = scales::label_number(accuracy = 1, big.mark = ","),
        name = paste0("Numbers of fish ", units_number_id),
        # Apply inverse transformation to get y axis units back
        sec.axis = ggplot2::sec_axis(~ . / trans, name =  paste0("Biomass ", units_biomass_id),
                                     # force label rounding
                                     labels = scales::label_number(accuracy = 1, big.mark = ",")),
        # force bars/lines to start at 0
        expand = ggplot2::expansion(mult = c(0, 0.05))

      ) +
      # add x- axis label and define axis ticks as every 10 cm with minor (1 cm) ticks
      ggplot2::guides(x = ggplot2::guide_axis(minor.ticks = TRUE)) +
      ggplot2::scale_x_continuous(
        name = "Age",
        breaks = seq(0, max(biomass_nums_age_summary$age_vector), 1),
        minor_breaks = seq(min(biomass_nums_age_summary$age_vector), max(biomass_nums_age_summary$age_vector), 1),
        # force bars/lines to start at 0
        expand = ggplot2::expansion(mult = c(0.01, 0.01))
      ) +
      # add a title if the user requested it
      {if (!is.null(plot_title_text)) ggplot2::labs(title = plot_title_text)} +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.y.right = ggplot2::element_text(color = "#cb181d"),
                     axis.text.y.right = ggplot2::element_text(color = "#cb181d"),
                     axis.title.y.left = ggplot2::element_text(color = "#0072B2"),
                     axis.text.y.left = ggplot2::element_text(color = "#0072B2"),
                     strip.background = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     text = ggplot2::element_text(family = "Times"),
                     axis.text = ggplot2::element_text(size = label_size),
                     axis.title = ggplot2::element_text(size = label_size),
                     plot.title = ggplot2::element_text(size = label_size))

    # return plot as a ggplot object
    return(bio_nums_plot)

}
