% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_ts_relationships_table.R
\name{build_ts_relationships_table}
\alias{build_ts_relationships_table}
\title{Create the standard MACE target strength relationships table}
\usage{
build_ts_relationships_table(ts_relationships_used = NULL)
}
\arguments{
\item{ts_relationships_used}{An optional character vector of relationships used in the survey.
If used, this will attempt to limit the table to the values used in the survey. Current options are available at:
\code{macebase2.ts_relationships}; the most straightforward way to specify is to use the
\code{ts_relationships_data$TS_RELATIONSHIP} vector returned from the get_ts_relationships function generated
in the get_macebase_data step of report generation.}
}
\value{
A list with two items: item 1 is the Flextable table object, item 2 is the table caption.
}
\description{
Produce a TS-length relationships table for use in standard MACE cruise reports. This stores all of the commonly used TS-length relationships, but will need to be updated as we add more TS-length relationships to analyses.Note that this is formatted for use with MS Word output, and references are formatted for use with a \code{.bib} bibliography file. Standard cruise reports include an appropriate \code{.bib} file.
}
\examples{
\dontrun{
# build a table for all commonly used TS relationships
ts_table_list <- build_ts_relationships_table()

# return table and caption
ts_table <- ts_table_list[[1]]
ts_caption <- ts_table_list[[2]]

# limit based on the relationships used (see macebase2.ts_relationships)
ts_relationships_list <- c("standard_pollock", "chrysaora_melanaster", "euphausiids_15_65mm_38khz")

ts_table_list <- build_ts_relationships_table(ts_relationships_used = ts_relationships_list)

# return table and caption
ts_table <- ts_table_list[[1]]
ts_caption <- ts_table_list[[2]]
}

}
\author{
Mike Levine
}
