#' @title Plot 'ridgelines' comparing the survey timeseries biomass and numbers at length.
#' @description Compare a survey timeseries numbers- and biomass- at length to visualize cohorts and compare trends over time. Because abundances can vary greatly over time, there are options to transform abundance values for more informative ridgeline plots. To be comparable to our typical plots, you can summarize your biomass and numbers as biomass (thousand tons) and numbers (million fish) by 1-cm length bins.
#' @param survey_year_vector  A vector identifying the year a given numbers or abundance value is from. This vector must be equal in length to \code{length_vector}, \code{biomass_vector}, and \code{numbers_vector}.
#' @param length_vector A vector of fish lengths (units are assumed to be cm). This vector must be equal in length to \code{survey_year_vector}, \code{biomass_vector}, and \code{numbers_vector}.
#' @param biomass_vector A vector of fish weights (units are not specified, but should be consistent.) This vector must be equal in length to \code{survey_year_vector}, \code{length_vector}, and \code{numbers_vector}.
#' @param numbers_vector A vector of fish numbers (units are not specified, but should be consistent.) This vector must be equal in length to \code{survey_year_vector}, \code{length_vector}, and \code{biomass_vector}.
#' @param numbers_scale_transform An optional transformation for the numbers values; options are "log10" and "sqrt".
#' @param biomass_scale_transform An optional transformation for the numbers values; options are "log10" and "sqrt".
#' @return A ggplot plot object.
#' @examples
#' \dontrun{
#' library(MACEReports)
#' library(dplyr)
#' data("pollock_length_weight_age_data")

#' # summarize by year, 1 cm length bins
#' plot_data <- pollock_length_weight_age_data %>%
#' mutate(length_bin = round(FORK_LENGTH, digits = 0),
#'        year = as.numeric(substr(SURVEY, 1, 4))) %>%
#' group_by(year, length_bin) %>%
#' summarize(biomass = sum(ORGANISM_WEIGHT),
#'           numbers = n())
#'
#' #return a plot
#' ridgeline_plot <- plot_historical_biomass_nums_length_ridges(survey_year_vector = plot_data$year,
#' length_vector = plot_data$length_bin,
#' biomass_vector = plot_data$biomass,
#' numbers_vector = plot_data$numbers)
#'
#' #plot it
#' ridgeline_plot
#' }
#' @export
plot_historical_biomass_nums_length_ridges <- function(survey_year_vector,
                                                       length_vector,
                                                       biomass_vector,
                                                       numbers_vector,
                                                       numbers_scale_transform = NULL,
                                                       biomass_scale_transform = NULL){

  # checks: make sure each vector is the same length
  if (!(all(sapply(list(length(survey_year_vector),
                        length(length_vector),
                        length(biomass_vector),
                        length(numbers_vector)),
                   FUN = identical,
                   length(survey_year_vector))))){
    stop("survey_year_vector, length_vector, biomass_vector, and numbers_vector must be vectors of equal length!")
  }

  # handle any requested transformations
  if (!is.null(numbers_scale_transform)){

    if (!numbers_scale_transform %in% c("log10", "sqrt")){

      stop(paste0("available numbers transformations are log10 and sqrt, not ", numbers_scale_transform))

    }

    # if the user has requested a valid transformation, make it
    if (numbers_scale_transform == "log10"){

      numbers_vector <- get(numbers_scale_transform)(numbers_vector + 1)

    }

    if (numbers_scale_transform == "sqrt"){

      numbers_vector <- get(numbers_scale_transform)(numbers_vector)

    }


  }

  if (!is.null(biomass_scale_transform)){

    if (!biomass_scale_transform %in% c("log10", "sqrt")){

      stop(paste0("available biomass transformations are log10 and sqrt, not ", biomass_scale_transform))

    }

    # if the user has requested a valid transformation, make it
    if (biomass_scale_transform == "log10"){

      biomass_vector <- get(biomass_scale_transform)(biomass_vector + 1)

    }

    if (biomass_scale_transform == "sqrt"){

      biomass_vector <- get(biomass_scale_transform)(biomass_vector)

    }

  }

  # combine vectors into a dataframe for plotting
  historical_data <- data.frame(survey_year_vector,
                                length_vector,
                                biomass_vector,
                                numbers_vector)

  # we want to have all lengths represented within every year- so, in the case of an unobserved length bin within a year, set this at 0.
  all_ages_df <- c()
  for (i in unique(historical_data$survey_year_vector)){

    # get a length vector that is equal to the 'traditional' historical data spread; get this as a dataframe to add to the current data
    length_vec <- data.frame("length_vector" = seq(min(historical_data$length), max(historical_data$length), 1))
    # add the year as well- this is needed to show where there were no fish in a given year
    length_vec$survey_year_vector<- i

    # compile dataframes
    all_ages_df <- rbind(all_ages_df, length_vec)
  }

  # add the length vector that contains all possible length- populate the length bins that have no data with 0s
  historical_data <- dplyr::left_join(all_ages_df, historical_data, by = c("length_vector", "survey_year_vector")) %>%
    tidyr::replace_na(list(biomass_vector = 0, numbers_vector = 0))

  # to scale plots for comparison, reshape data so that we can use the measurement type (biomass or nums) as a faceting variable (go from 'long' to 'wide')
  historical_plot <- historical_data %>%
    tidyr::pivot_longer(cols = c(numbers_vector, biomass_vector), names_to = "measurement_type", values_to = "measurement_value")

  # title numbers plots based on their values:
  if (!is.null(numbers_scale_transform)){

    if (numbers_scale_transform == "log10"){

      num_plot_name <- expression(paste(" log"["10"], " Numbers"))

    }

    if (numbers_scale_transform == "sqrt"){

      num_plot_name <- expression(paste(sqrt("Numbers")))

    }

  }

  if (is.null(numbers_scale_transform)){

    num_plot_name <- "Numbers"

  }

  # title biomass plots based on their values:
  if (!is.null(biomass_scale_transform)){

    if (biomass_scale_transform == "log10"){

      biomass_plot_name <- expression(paste("Numbers", " (log"["10"], " fish)"))

    }

    if (biomass_scale_transform == "sqrt"){

      biomass_plot_name <- expression(paste("Numbers (", sqrt("fish"), ")"))

    }

  }

  if (is.null(biomass_scale_transform)){

    biomass_plot_name <- "Biomass"

  }

  # add a column containing the new facet label names, and use this to make a 'biomass' and a 'numbers' plot
  historical_plot$facets <- factor(historical_plot$measurement_type,
                                   labels = c(biomass_plot_name, num_plot_name))

  # enable Times New Roman font to be used (for windows only!)
  grDevices::windowsFonts("Times" = grDevices::windowsFont("Times New Roman"))

  # make the plot
  ridges_plot <-
    ggplot2::ggplot(historical_plot, ggplot2::aes(x = .data$length_vector, y = .data$survey_year_vector,
                                                  height = .data$measurement_value, group = .data$survey_year_vector, fill = .data$measurement_type)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 6, alpha = 0.8) +
    ggplot2::facet_wrap(~facets, ncol = 2, labeller = ggplot2::label_parsed) +
    ggplot2::scale_y_reverse(breaks = seq(min(historical_data$survey_year_vector), max(historical_data$survey_year_vector), 1), expand = c(0.01, 0.01)) +
    ggplot2::scale_x_continuous(breaks = seq(min(historical_data$length_vector), max(historical_data$length_vector), 10), expand = c(0.01, 0.01)) +
    ggplot2::scale_fill_manual(values = c("#a50f15", "#08519c")) +
    ggplot2::labs(x = "Fork length (cm)", y = "Survey year") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      legend.position = "none",
      text = ggplot2::element_text(family = "Times"),
      axis.text =  ggplot2::element_text(size = 10),
      axis.title =  ggplot2::element_text(size = 10),
      panel.grid.minor =  ggplot2::element_blank(),
      panel.grid.major =  ggplot2::element_blank())

}





