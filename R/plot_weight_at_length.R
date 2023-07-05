#' @title Plot mean fish weight at length for the current survey, with historical weight at length context.
#' @description Present the current weight at length (mean +/- 1 s.d., with lengths binned at 1 cm increments) with historical survey observations plotted as the range of past observations as well the historical mean +/- 1 s.d. To be consistent with the MACE standard approach, only classes with >= 5 fish are plotted; in cases with < 5 fish the estimated weight at length based on a linear regression is used instead (see De Robertis and Williams 2008: Weight–Length Relationships in Fisheries Studies: The Standard Allometric Model Should Be Applied with Caution.).
#' @param survey_vector a vector identifying the survey a given length and weight value is from. This vector must be equal in length to \code{length_vector} and \code{weight_vector}.
#' @param length_vector A vector of fish lengths (units are assumed to be cm). This vector must be equal in length to \code{survey_vector} and \code{weight_vector}.
#' @param weight_vector A vector of fish weights (units are assumed to be kg). This vector must be equal in length to \code{survey_vector} and \code{length_vector}.
#' @param current_survey Identify which survey is the current survey so that it can be highlighted in plots (and removed from the historical comparison).
#' @return A list with 3 items:
#' Item 1 = A ggplot object
#' Item 2 = A dataframe of the current survey weight at length
#' Item 3 = A dataframe summarizing the historical survey weight at length
#' @examples
#' \dontrun{
#' #load example data
#' library(MACEReports)
#' data("pollock_length_weight_age_data")
#'
#' #gather plot and summary dataframes
#' pollock_wt_len <- plot_weight_at_length(
#' survey_vector = pollock_length_weight_age_data$SURVEY,
#'  length_vector = pollock_length_weight_age_data$FORK_LENGTH,
#'  weight_vector = pollock_length_weight_age_data$ORGANISM_WEIGHT,
#'   current_survey = 202104)
#'
#' # show the plot
#' pollock_wt_len[[1]]
#' # get the current survey summary
#' current_survey_pollock_wt_len <- pollock_wt_len[[2]]
#' # and the historical survey summary
#' historical_survey_pollock_wt_len <- pollock_wt_len[[3]]
#' }
#' @export
plot_weight_at_length <- function(survey_vector, length_vector, weight_vector, current_survey){

  # checks: make sure each vector is the same length
  if (!(all(sapply(list(length(survey_vector),
                        length(length_vector),
                        length(weight_vector)),
                   FUN = identical,
                   length(survey_vector))))){
    stop("survey_vector, length_vector, and weight_vector must be vectors of equal length!")
  }

  # make sure a current survey is identified, and is present in the data
  if (!current_survey %in% unique(survey_vector)){
    stop("The current survey you have identified is not present in survey_vector; check your data!")
  }

  # create a dataframe from the data vectors
  length_weight_data <- cbind.data.frame(survey_vector, length_vector, weight_vector)

  # split the dataset up into a current survey and historical surveys
  length_weight_current_survey <- length_weight_data %>%
    dplyr::filter(.data$survey_vector == current_survey) %>%
    # Round lengths to 1cm bins
    dplyr::mutate(length_bin = round(.data$length_vector, digits = 0)) %>%
    dplyr::group_by(.data$survey_vector, .data$length_bin) %>%
    dplyr::summarize(count = dplyr::n(),
                     mean_wt_at_len = mean(.data$weight_vector),
                     sd_wt_at_len = stats::sd(.data$weight_vector))

  # for plotting, plot the 'old' surveys separately than the current survey as points
  historical_survey_comparison <- length_weight_data %>%
    dplyr::filter(.data$survey_vector != current_survey) %>%
    # Round lengths to 1cm bins
    dplyr::mutate(length_bin = round(.data$length_vector, digits = 0))

  # In the current survey, don't plot cases with less than 5 fish in a category (to be consistent w past figures)
  length_weight_current_survey <- length_weight_current_survey[length_weight_current_survey$count >= 5, ]

  #########
  # add a regression line from the past- based suggestions in De Robertis and Williams 2008:
  # Weight–Length Relationships in Fisheries Studies: The Standard Allometric Model Should Be Applied with Caution...
  # regression is a simple average-weight-at-length in each survey, and, in cases where there were <5 samples available,
  # fitting a linear model

  # #also create a vector of fish that spans the smallest-largest in the total timeseries,
  # #and use this to fill in weight-at-length cases where there isn't enough data
  # #(i.e. any 1cm length increment from min-max without >6 fish)
  fit_lengths <- seq(min(historical_survey_comparison$length_bin), max(historical_survey_comparison$length_bin), 1)

  # use modelled values in cases with <5 fish
  get_modelled_wts <- function(survey) {
    # get data for survey
    mod_data <- historical_survey_comparison %>%
      dplyr::filter(.data$survey_vector == survey)

    # fit the model as: lm(log(weight)~log(length))
    lw_mod <- stats::lm(formula = log(weight_vector) ~ log(length_bin), data = mod_data)

    # get the residual standard error for the model
    var_residual <- stats::var(lw_mod$residuals)
    corr_fact <- exp(0.5 * var_residual)

    # and back-transform to linear space, at the 1cm length vector for covering all survey lengths
    # mod_wt = exp(lw_mod$coefficients[1]) * (fit_lengths^lw_mod$coefficients[2])

    # and back-transform to linear space, at the 1cm length vector for covering all survey lengths,
    # with correction for back-transformation
    mod_wt <- corr_fact * (exp(lw_mod$coefficients[1]) * (fit_lengths^lw_mod$coefficients[2]))

    # turn this into a dataframe
    lw_fit <- cbind.data.frame(survey, fit_lengths, mod_wt)

    # and return dataframe
    return(lw_fit)
  }

  # get all the modelled values
  mod_wt_lengths <- purrr::map_dfr(unique(historical_survey_comparison$survey_vector), get_modelled_wts)

  ###############
  # now calculate get the average weight at length, as we'll use this in cases with >5 fish
  survey_av_at_length <- historical_survey_comparison %>%
    dplyr::group_by(.data$survey_vector, .data$length_bin) %>%
    # get the average wt @ each length
    dplyr::summarize(
      n_fish = dplyr::n(),
      mean_wt_at_len = mean(.data$weight_vector),
      sd_wt_at_len = stats::sd(.data$weight_vector)
    )

  # and add these back to the historical dataframe
  survey_av_at_length <- dplyr::right_join(survey_av_at_length, mod_wt_lengths,
                                           by = c("survey_vector" = "survey", "length_bin" = "fit_lengths")) %>%
    dplyr::arrange(.data$survey_vector, .data$length_bin)

  # use calculated mean at length UNLESS <5 fish, then use modeled
  survey_av_at_length$plot_wt <- ifelse(survey_av_at_length$n_fish < 5 | is.na(survey_av_at_length$n_fish),
                                        survey_av_at_length$mod_wt, survey_av_at_length$mean_wt_at_len
  )

  # and take average wt at each length across surveys
  historical_av_wt_at_len <- survey_av_at_length %>%
    dplyr::group_by(.data$length_bin) %>%
    dplyr::summarize(
      survey_type = "historical_comp",
      mean_wt_at_len = mean(.data$plot_wt),
      sd_wt_at_len = stats::sd(.data$plot_wt)
    )

  #########
  # plot it all
  # enable Times New Roman font to be used (for windows only!)
  grDevices::windowsFonts("Times" = grDevices::windowsFont("Times New Roman"))

  length_weight_comparison_plot <-
    ggplot2::ggplot() +
    # add all the historical data in grey boxes
    ggplot2::geom_point(
      data = historical_survey_comparison, ggplot2::aes(x = .data$length_bin, y = .data$weight_vector),
      color = "grey80", shape = 0, size = 2
    ) +
    # add the sd as a ribbon
    ggplot2::geom_ribbon(
      data = historical_av_wt_at_len, ggplot2::aes(
        x = .data$length_bin, ymin = .data$mean_wt_at_len - .data$sd_wt_at_len,
        ymax = .data$mean_wt_at_len + .data$sd_wt_at_len
      ),
      color = "grey60", fill = "grey60", alpha = 0.5
    ) +
    # add the current survey in red
    ggplot2::geom_point(
      data = length_weight_current_survey,
                        ggplot2::aes(x = .data$length_bin, y = .data$mean_wt_at_len),
      color = "red", fill = "red", shape = 22, size = 2) +
    ggplot2::geom_errorbar(
      data = length_weight_current_survey, ggplot2::
        aes(x = .data$length_bin, ymin = .data$mean_wt_at_len - .data$sd_wt_at_len,
            ymax = .data$mean_wt_at_len + .data$sd_wt_at_len),
      color = "red", width = 0.2
    ) +
    # and add the above model fit as a line
    ggplot2::geom_line(data = historical_av_wt_at_len,
                       ggplot2::aes(x = .data$length_bin, y = .data$mean_wt_at_len),
                       color = "black", linewidth = 1.0) +
    # on x-axis, label every 10 cm, from 0-80 cm
    ggplot2::scale_x_continuous(breaks = seq(0, 80, 10)) +
    # on y-axis, label every 0.5 kg
    ggplot2::scale_y_continuous(breaks = seq(0, ceiling(max(historical_survey_comparison$weight_vector)), 0.5)) +
    ggplot2::labs(x = "Fork length (cm)", y = "Weight (kg)", ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )

  # return the plot and summary stats dataframe
  return(list(length_weight_comparison_plot, length_weight_current_survey, historical_av_wt_at_len))

}
