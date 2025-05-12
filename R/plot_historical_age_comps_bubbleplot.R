#' @title Bubbleplot of the historical numbers- and biomass- at age for the survey timeseries.
#' @description Compare a survey timeseries numbers- and biomass- at age to visualize cohorts and compare trends over time. Because abundances can vary greatly over time, abundances are presented as the square-root transform of numbers and biomass values. These plots are taken from work done by Cole Monnahan (cole.monnahan@noaa.gov) and Dave McGowan (david.mcgowan@noaa.gov). You should input biomass values in KG and numbers as numbers of individuals! They will be summarized and plotted as biomass (1000s t) and numbers (millions) as these units work well for our typical pollock surveys.
#' @param survey_year_vector A vector identifying the year a given numbers or abundance value is from. This vector must be equal in length to \code{age_vector}, \code{biomass_vector}, and \code{numbers_vector}.
#' @param age_vector A vector of fish ages (units are assumed to be years). This vector must be equal in length to \code{survey_year_vector}, \code{biomass_vector}, and \code{numbers_vector}.
#' @param biomass_vector A vector of fish weights (units are expected as KG.) This vector must be equal in length to \code{survey_year_vector}, \code{age_vector}, and \code{numbers_vector}.
#' @param numbers_vector A vector of fish numbers (units are expected as individual fish.) This vector must be equal in length to \code{survey_year_vector}, \code{age_vector}, and \code{biomass_vector}.
#' @param max_age Any years >= than this age will be grouped for plotting, and presented as a 'plus' category. Defaults to 12, as this is consistent with how we generally present pollock age values.
#' @return A base r plot.
#' @examples
#' \dontrun{
#' # simulate some fish populations
#' years <- rep(c(2013,2015, 2017, 2019, 2021), 100000/5)
#' ages <- round(rpois(n = 100000, lambda = seq(1,6,1)), digits = 0) + 1
#' numbers <- sample(seq(0, 3e6, 100), size = 100000, replace = TRUE)
#' weights <- (ages^3 * .005) * numbers
#'
#' #plot it
#' plot_historical_age_comps_bubbleplot(survey_year_vector = years,
#' age_vector = ages,
#' numbers_vector = numbers,
#' biomass_vector = weights)
#'
#' #plot with a higher maximum age grouping
#' plot_historical_age_comps_bubbleplot(survey_year_vector = years,
#'  age_vector = ages,
#'  numbers_vector = numbers,
#'  biomass_vector = weights,
#'  max_age = 14)
#' }
#' @export
plot_historical_age_comps_bubbleplot <- function(survey_year_vector,
                                                 age_vector,
                                                 numbers_vector,
                                                 biomass_vector,
                                                 max_age = 12){

  # checks: make sure each vector is the same length
  if (!(all(sapply(list(length(survey_year_vector),
                        length(age_vector),
                        length(numbers_vector),
                        length(biomass_vector)),
                   FUN = identical,
                   length(survey_year_vector))))){
    stop("survey_year_vector, length_vector, biomass_vector, and numbers_vector must be vectors of equal length!")
  }

  # make sure max_age is a numeric value
  if (!is.numeric(max_age)){
    stop(paste0("max_age must be numeric, not ", class(max_age)))
  }

  # combine vectors into a dataframe
  biomass_and_numbers_age_data <- data.frame(survey_year_vector, age_vector, numbers_vector, biomass_vector)

  # sum biomass/nums by year and age; group anything greater than the specified max_age in one category
  biomass_and_nums <- biomass_and_numbers_age_data %>%
    # add anything >= max_age to a "max_age +" category
    dplyr::mutate(age_class = ifelse(.data$age_vector < max_age, .data$age_vector, max_age)) %>%
    # sum values within these categories
    dplyr::group_by(.data$survey_year_vector, .data$age_class) %>%
    # sum the weight and numbers,
    # present biomass (kg) as 1000s of tons, and fish (individuals) as millions of fish
    dplyr::summarize(number_million_fish = sum(.data$numbers_vector) / 1e6,
                     biomass_thousand_tons = sum(.data$biomass_vector) / 1e6)

  # we want to ensure there's a value at each year/age combination- so put zeros anywhere there's a year/age combination without an observation

  #get the total range of available ages
  all_age_classes <- seq(min(biomass_and_nums$age_class), max(biomass_and_nums$age_class), 1)

  # plot using base R

  # enable Times New Roman font to be used (for windows only!)
  grDevices::windowsFonts("Times" = grDevices::windowsFont("Times New Roman"))

  graphics::par(mfrow = c(2, 1), mar = c(2, 2, 1, 1), oma = c(2, 2.5, 1, 0), family = "Times")
  ylim <- c(0, max_age + 2)

  # Plot numbers age comps
  graphics::plot(x = biomass_and_nums$survey_year_vector, y = biomass_and_nums$age_class, pch = 1,
                 cex = sqrt(biomass_and_nums$number_million_fish / 100) / 2,
                 ann = F, ylim = ylim, xaxt = "n", yaxt = "n", yaxs = "i")
  graphics::axis(side = 2, las = 2, at = c(1:max_age), labels = c(1:(max_age - 1), paste0(max_age, "+")), cex.axis = 0.8)
  graphics::axis(
    side = 1, at = seq(min(biomass_and_nums$survey_year_vector), max(biomass_and_nums$survey_year_vector), 1),
    labels = F, lwd = 1, lwd.ticks = 0.75, tck = -0.02)
  graphics::axis(side = 1, at = unique(biomass_and_nums$survey_year_vector), lwd = 0, lwd.ticks = 1, tck = -0.04, cex.axis = 0.8)
  graphics::text(x = stats::median(biomass_and_nums$survey_year_vector), y = max_age + 1.25, "Age composition by numbers of fish (millions)")
  graphics::mtext(side = 2, las = 2, outer = F, line = 1, font = 2, at = max_age + 2, "a)")

  # Plot biomass age comps
  graphics::plot(x = biomass_and_nums$survey_year_vector, y = biomass_and_nums$age_class, pch = 1,
                 cex = sqrt(biomass_and_nums$biomass_thousand_tons) / 10,
                 ann = F, ylim = ylim, xaxt = "n", yaxt = "n", yaxs = "i")
  graphics::axis(side = 2, las = 2, at = c(1:max_age), labels = c(1:(max_age - 1), paste0(max_age, "+")), cex.axis = 0.8)
  graphics::axis(
    side = 1, at = seq(min(biomass_and_nums$survey_year_vector), max(biomass_and_nums$survey_year_vector), 1),
    labels = F, lwd = 1, lwd.ticks = 0.75, tck = -0.02)
  graphics::axis(side = 1, at = unique(biomass_and_nums$survey_year_vector), lwd = 0, lwd.ticks = 1, tck = -0.04, cex.axis = 0.8)
  graphics::text(x = stats::median(biomass_and_nums$survey_year_vector), y = max_age + 1.25, "Age composition by biomass (1000s t)")
  graphics::mtext(side = 1, outer = F, line = 2.5, "Year")
  graphics::mtext(side = 2, outer = T, line = 1, "Age")
  graphics::mtext(side = 2, las = 2, outer = F, line = 1, font = 2, at = max_age + 2, "b)")

}
