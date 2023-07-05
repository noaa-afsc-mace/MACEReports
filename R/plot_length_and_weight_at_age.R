#' @title Plot mean fish length- and weight- at age for the current survey, with historical length- and weight- at age for context.
#' @description Present the current length- and weight- at age (mean +/- 1 s.d., with lengths binned at 1 cm increments and ages reported in 1 year increments) with historical survey observations plotted as the range of past observations as well the historical mean +/- 1 s.d. Note that the plots automatically scale to the current survey ages + 1 (so, if your survey has fish up to 12 years old, your plots will extend to 13 years- old outlier ages may be excluded from plotting).
#' @param survey_vector a vector identifying the survey a given length and weight value is from. This vector must be equal in length to \code{length_vector}, \code{weight_vector}, and \code{age_vector}.
#' @param length_vector A vector of fish lengths (units are assumed to be cm). This vector must be equal in length to \code{survey_vector}, \code{weight_vector}, and \code{age_vector}.
#' @param weight_vector A vector of fish weights (units are assumed to be kg). This vector must be equal in length to \code{survey_vector}, \code{length_vector}, and \code{age_vector}.
#' @param age_vector A vector of fish ages (units are assumed to be 1 year increments). This vector must be equal in length to \code{survey_vector}, \code{length_vector}, and \code{weight_vector}.
#' @param current_survey Identify which survey is the current survey so that it can be highlighted in plots (and removed from the historical comparison).
#' @return A list with 4 items:
#' Item 1 = A length at age plot (ggplot object).
#' Item 2 = A weight at age plot (ggplot object).
#' Item 3 = A dataframe summarizing the current survey length- and weight- at age
#' Item 4 = A dataframe summarizing the historical length- and weight- at age
#' @examples
#' \dontrun{
#' #load example data
#' library(MACEReports)
#' data("pollock_length_weight_age_data")
#'
#' # gather plot and summary dataframes
#' l_w_age <- plot_length_and_weight_at_age(survey_vector =  pollock_length_weight_age_data$SURVEY,
#' length_vector =pollock_length_weight_age_data$FORK_LENGTH,
#' weight_vector = pollock_length_weight_age_data$ORGANISM_WEIGHT,
#' age_vector = pollock_length_weight_age_data$AGE,
#' current_survey = 202104)
#'
#' # print the length at age plot
#' l_w_age[[1]]
#'
#' # print the weight at age plot
#' l_w_age[[2]]
#'
#' # get the current survey summary
#' current_survey_l_w_age <- l_w_age[[3]]
#'
#' # and the historical summary
#' historical_l_w_age <- l_w_age[[4]]
#' }
#' @export
plot_length_and_weight_at_age <- function(survey_vector, length_vector, weight_vector, age_vector, current_survey){

  # checks: make sure each vector is the same length
  if (!(all(sapply(list(length(survey_vector),
                        length(length_vector),
                        length(weight_vector),
                        length(age_vector)),
                   FUN = identical,
                   length(survey_vector))))){
    stop("survey_vector, length_vector, and weight_vector must be vectors of equal length!")
  }

  # make sure a current survey is identified, and is present in the data
  if (!current_survey %in% unique(survey_vector)){
    stop("The current survey you have identified is not present in survey_vector; check your data!")
  }

  # create a dataframe from the data vectors
  length_weight_age_data <- cbind.data.frame(survey_vector, length_vector, weight_vector, age_vector)

  # if there's no current age data, warn user (and return blank dataframes as a placeholder for reports)
  if (length(length_weight_age_data$age_vector[length_weight_age_data$survey_vector == current_survey &
                                               !is.na(length_weight_age_data$age_vector)]) < 1){
    stop("There is no age data for the current survey!")
  }

  # split the dataset up into a current survey and historical surveys
  current_survey_age_summary <- length_weight_age_data %>%
    dplyr::filter(survey_vector == current_survey) %>%
    # only retain the aged fish
    dplyr::filter(!is.na(.data$age_vector)) %>%
    dplyr::group_by(.data$age_vector) %>%
    dplyr::summarize(
      mean_wt_at_age = mean(.data$weight_vector, na.rm = TRUE),
      sd_wt_at_age = stats::sd(.data$weight_vector, na.rm = TRUE),
      mean_length_at_age = mean(.data$length_vector, na.rm = TRUE),
      sd_length_at_age = stats::sd(.data$length_vector, na.rm = TRUE),
      n_wt_at_age = dplyr::n()
    )

  # for plotting, plot the 'old' surveys separately than the current survey as points; keep all regions
  historical_survey_comparison <- length_weight_age_data %>%
    dplyr::filter(survey_vector != current_survey & !is.na(age_vector))

  # add a simple mean line
  historical_mean <- historical_survey_comparison %>%
    dplyr::group_by(.data$age_vector) %>%
    dplyr::summarize(
      mean_length = mean(.data$length_vector),
      sd_length = stats::sd(.data$length_vector),
      mean_wt = mean(.data$weight_vector),
      sd_wt = stats::sd(.data$weight_vector)
    )

  # enable Times New Roman font to be used (for windows only!)
  grDevices::windowsFonts("Times" = grDevices::windowsFont("Times New Roman"))

  # plot length at age
  mean_length_at_age <-
    ggplot2::ggplot() +
    # add the historical point data
    ggplot2::geom_point(
      data = historical_survey_comparison, ggplot2::aes(x = .data$age_vector, y = .data$length_vector), color = "grey80",
      shape = 0, size = 2
    ) +
    # and the historical confidence intervals
    ggplot2::geom_ribbon(
      data = historical_mean, ggplot2::aes(
        x = .data$age_vector, ymin = .data$mean_length - .data$sd_length,
        ymax = .data$mean_length + .data$sd_length
      ),
      color = "grey80", fill = "grey80", alpha = 0.5
    ) +
    # add the current survey point data
    ggplot2::geom_point(
      data = current_survey_age_summary, ggplot2::aes(x = .data$age_vector, y = .data$mean_length_at_age), color = "red",
      fill = "red", shape = 22, size = 2
    ) +
    # and the current survey confidence intervals
    ggplot2::geom_errorbar(
      data = current_survey_age_summary, ggplot2::aes(
        x = .data$age_vector, ymin = .data$mean_length_at_age - .data$sd_length_at_age,
        ymax = .data$mean_length_at_age + .data$sd_length_at_age
      ),
      color = "red", width = 0.2
    ) +
    # add the historical mean trendline
    ggplot2::geom_line(data = historical_mean, ggplot2::aes(x = .data$age_vector, y = .data$mean_length), color = "black") +
    # set x axis limits as 1 year greater than the oldest fish in the current survey; this will exclude bigger fish from the past
    # and you'll get a 'removed XXXXX rows' warning; this is intentional
    ggplot2::scale_x_continuous(limits = c(1, max(current_survey_age_summary$age_vector) + 1),
                                breaks = seq(0, max(current_survey_age_summary$age_vector) + 1, 1)) +
    # on y-axis, label every 10 cm, from 0-80 cm
    ggplot2::scale_y_continuous(breaks = seq(0, 80, 10)) +
    ggplot2::labs(x = "Age", y = "Fork length (cm)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      text = ggplot2::element_text(family = "Times"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )

  # for wt-at-age, set y axis as the maximum weight in the historical comparison, unless there isn't any historical data in this region!
  if (nrow(historical_survey_comparison) > 1) {
    wt_age_y_lims <- ggplot2::scale_y_continuous(breaks = seq(0, ceiling(max(historical_survey_comparison$weight_vector)), 0.5))
  }

  if (nrow(historical_survey_comparison) <= 1) {
    wt_age_y_lims <- ggplot2::scale_y_continuous(breaks = seq(0, ceiling(max(current_survey_age_summary$mean_wt_at_age)), 0.5))
  }

  #plot the weight at age
  mean_wt_at_age <-
    ggplot2::ggplot() +
    # add the historical point data
    ggplot2::geom_point(
      data = historical_survey_comparison, ggplot2::aes(x = .data$age_vector, y = .data$weight_vector),
      color = "grey80", shape = 0, size = 2
    ) +
    # add the historical confidence intervals
    ggplot2::geom_ribbon(
      data = historical_mean, ggplot2::aes(
        x = .data$age_vector, ymin =.data$ mean_wt - .data$sd_wt,
        ymax = .data$mean_wt + .data$sd_wt
      ),
      color = "grey80", fill = "grey80", alpha = 0.5
    ) +
    # add the current survey point data
    ggplot2::geom_point(
      data = current_survey_age_summary, ggplot2::aes(x = .data$age_vector, y = .data$mean_wt_at_age),
      color = "red", fill = "red", shape = 22, size = 2
    ) +
    # add the current survey confidence intervals
    ggplot2::geom_errorbar(
      data = current_survey_age_summary, ggplot2::aes(x = .data$age_vector,
                                                      ymin = .data$mean_wt_at_age - .data$sd_wt_at_age,
                                                      ymax = .data$mean_wt_at_age + .data$sd_wt_at_age),
      color = "red", width = 0.2
    ) +
    # add the historical mean trendline
    ggplot2::geom_line(data = historical_mean, ggplot2::aes(x = .data$age_vector, y = .data$mean_wt), color = "black") +
    # set x axis limits as 1 year greater than the oldest fish in the current survey; this will exclude bigger fish from the past
    # and you'll get a 'removed XXXXX rows' warning; this is intentional
    ggplot2::scale_x_continuous(limits = c(1, max(current_survey_age_summary$age_vector) + 1),
                       breaks = seq(0, max(current_survey_age_summary$age_vector) + 1, 1)) +
    # on y-axis, label every 0.5 kg
    wt_age_y_lims +
    ggplot2::labs(x = "Age", y = "Weight (kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      text = ggplot2::element_text(family = "Times"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )

  #return a list with plots and summary dataframe
  return(list(mean_length_at_age, mean_wt_at_age, current_survey_age_summary, historical_mean))

}


