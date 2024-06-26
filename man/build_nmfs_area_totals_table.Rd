% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_nmfs_area_totals_table.R
\name{build_nmfs_area_totals_table}
\alias{build_nmfs_area_totals_table}
\title{Create pollock biomass and numbers per NMFS management area table for MACE GOA summer cruise reports}
\usage{
build_nmfs_area_totals_table(biomass_nums_length_data)
}
\arguments{
\item{biomass_nums_length_data}{The name of the dataframe that contains the pollock
biomass at length data; this dataframe is created in the cruise report markdown
process using the function \code{get_biomass_and_nums_data_function.R}; this provides all
the data that is then used to create a 'current survey only/pollock only' dataframe which is
the easiest input to the \code{build_nmfs_area_totals_table} function.}
}
\value{
A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
}
\description{
Build the pollock biomass (metric tons) and numbers (millions)- per NMFS management area and survey
region. This table has been used in the GOA summer report.
}
\examples{
\dontrun{
# build the table and the caption
area_totals_table_list <-
  build_nmfs_area_totals_table(biomass_nums_length_data = current_survey_pollock_biomass_nums)

# pull out the table and caption from the list
area_totals_table <- area_totals_table_list[[1]]
area_totals_caption <- area_totals_table_list[[2]] #'
}
}
\author{
Mike Levine
}
