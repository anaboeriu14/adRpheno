% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clinical_calculations.R
\name{calculate_bp_metrics}
\alias{calculate_bp_metrics}
\title{Calculate blood pressure metrics}
\usage{
calculate_bp_metrics(
  dataf,
  systolic1,
  systolic2,
  diastolic1,
  diastolic2,
  calculate_pp = TRUE,
  calculate_map = FALSE,
  decimal_places = 2,
  force = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{dataf}{A data frame containing blood pressure measurements}

\item{systolic1}{String. Name of the first systolic BP measurement column}

\item{systolic2}{String. Name of the second systolic BP measurement column}

\item{diastolic1}{String. Name of the first diastolic BP measurement column}

\item{diastolic2}{String. Name of the second diastolic BP measurement column}

\item{calculate_pp}{Logical. If TRUE, calculates pulse pressure (default: TRUE)}

\item{calculate_map}{Logical. If TRUE, calculates mean arterial pressure (default: FALSE)}

\item{decimal_places}{Integer specifying decimal places for rounding (default: 2)}

\item{force}{Logical. If TRUE, recalculates even if outputs already exist (default: FALSE)}

\item{verbose}{Logical. If TRUE, prints informative messages (default: TRUE)}
}
\value{
Data frame with added average BP columns and selected derived metrics.
New columns are named \code{mean_arterial_pressure} or \code{pulse_pressure}
}
\description{
Calculates average systolic and diastolic blood pressure from two measurements,
and optionally calculates mean arterial pressure (MAP) and/or pulse pressure.
Uses output-based tracking to prevent duplicate calculations.
}
\details{
Mean arterial pressure (MAP) is calculated as:
MAP = DBP + (SBP - DBP) / 3

Pulse pressure (PP) is calculated as:
PP = SBP - DBP

where:
\itemize{
\item DBP = diastolic blood pressure
\item SBP = systolic blood pressure
}
}
\examples{
\dontrun{
test_df <- data.frame(
  age = c(50, 54, 32),
  systolic1 = c(120, 130, 125),
  diastolic1 = c(80, 85, 82),
  systolic2 = c(130, 140, 135),
  diastolic2 = c(85, 90, 88)
)
# Calculate PP only
calculate_bp_metrics(test_df, "systolic1", "systolic2", "diastolic1", "diastolic2")

# Calculate both MAP and pulse pressure
calculate_bp_metrics(test_df, "systolic1", "systolic2", "diastolic1", "diastolic2",
                    calculate_map = TRUE, calculate_pp = TRUE)

# Calculate MAP only
calculate_bp_metrics(test_df, "systolic1", "systolic2", "diastolic1", "diastolic2",
                    calculate_map = TRUE, calculate_pp = FALSE)
}
}
