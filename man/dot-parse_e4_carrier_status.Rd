% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apoe_genetics_helpers.R
\name{.parse_e4_carrier_status}
\alias{.parse_e4_carrier_status}
\title{Parse e4 carrier status values to logical}
\usage{
.parse_e4_carrier_status(status_vector, true_values, false_values)
}
\arguments{
\item{status_vector}{Vector of e4 status values}

\item{true_values}{Vector of values that represent e4 positive}

\item{false_values}{Vector of values that represent e4 negative}
}
\value{
Logical vector of standardized e4 status
}
\description{
Parse e4 carrier status values to logical
}
\keyword{internal}
