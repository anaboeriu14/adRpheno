% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apoe_genetics.R
\name{standardize_e4_status}
\alias{standardize_e4_status}
\title{Standardize e4 carrier status values}
\usage{
standardize_e4_status(status_vector, true_values, false_values)
}
\arguments{
\item{status_vector}{Vector of e4 status values}

\item{true_values}{Vector of values that represent e4 positive}

\item{false_values}{Vector of values that represent e4 negative}
}
\value{
Logical vector of standardized e4 status (TRUE = e4 positive)
}
\description{
Standardize e4 carrier status values
}
\keyword{internal}
