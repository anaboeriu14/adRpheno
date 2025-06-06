% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers.R
\name{detect_outlier_thresholds}
\alias{detect_outlier_thresholds}
\title{Detect Outlier Thresholds}
\usage{
detect_outlier_thresholds(dataf, var_name, multiplier = 1.5, label = NULL)
}
\arguments{
\item{dataf}{A data frame containing the numeric variable}

\item{var_name}{Character. Name of the numeric variable to analyze}

\item{multiplier}{Numeric. Multiplier for IQR (default: 1.5)}

\item{label}{Character. Optional custom label for the output (default: NULL, uses var_name)}
}
\value{
Data frame with lower bound (LB), upper bound (UB), and notes
}
\description{
Calculates lower and upper bounds for outlier detection using the
IQR method (Q1 - k\emph{IQR and Q3 + k}IQR).
}
