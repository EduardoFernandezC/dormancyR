% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle_greenhouse.R
\name{handle_greenhouse}
\alias{handle_greenhouse}
\title{Get weather data from the greenhouses at CKA in \code{\link[chillR:chillR-package]{chillR}} format}
\usage{
handle_greenhouse(
  path_data,
  time_step = "hourly",
  vars = c("Temp", "Humidity", "CO2"),
  check_data = T,
  n_hours = 24,
  threshold = 10,
  fix_data = T,
  na_strings = c("FAULT", "")
)
}
\arguments{
\item{path_data}{Character string. This is the directory in which the data is stored. It is important to note
that the file must have either \strong{'.xls'} or \strong{'.xlsx'} extension}

\item{time_step}{Character string. This is the time step in which the data must be retrieved. It has two options;
\emph{'hourly'} (the default) and \emph{'daily'}}

\item{vars}{Character string. Variables that must be retrieved by the function. In general, the options are
\emph{'Temperatures'}, \emph{'Humidity'}, and \emph{'CO2 concentration'}. Regarding \emph{'Temperatures'}, 
the options differed depending on the \code{time_step} parameter. For \code{time_step = 'daily'}, the options
are \emph{'Tmean'}, \emph{'Tmin'}, and \emph{'Tmax'}}

\item{check_data}{Boolean parameter to check the observations for inconsistencies. Unfortunately,
the dataloggers in the greenhouses often define missing records as 0. This makes exremely complicated 
to know if one record was actually 0 or missing. This check for that inconsistency by comparing the
temperature at a given hour 'x' with the mean for the previous hours (defined by the \code{n_hours}).
If this difference is greater than a given threshold (i.e. \code{threshold}) the record is corrected to
\code{NA}}

\item{n_hours}{Numeric input. This is the number of previous hours used to compute the mean and to check for
inconsistencies. Default is set to 24 hours}

\item{threshold}{Numeric input. This is the limit to define whether the record was a true observation or not.
Default is set to 10 celsius degree since greenhouse temperatures are controlled}

\item{fix_data}{Boolean parameter. If \code{fix_data = TRUE}, fill in the missing hours
by the function \code{\link[chillR:interpolate_gaps_hourly]{interpolate_gaps_hourly}} from 
\code{\link[chillR:chillR-package]{chillR}}}

\item{na_strings}{Character vector of strings to interpret as missing values. By default,
\code{handle_greenhouse} treats the word \emph{'FAULT'} and blank cells as missing data.}
}
\description{
This function is to handle weather data obtained from the greenhouses located in Campus Klein Altendorf.
For now, this works for input files stored as \strong{'.xls'} or \strong{'.xlsx'} format
}
\note{
In the \emph{'daily'} mode, this function can returns the variables \emph{'Tmin'}, \emph{'Tmean'} and \emph{'Tmax'}. In contrast, under the \emph{'hourly'}
mode, it can returns the mean temperature for each hour as \emph{'Temp'}
}
\examples{
# As each user has different path for the folder this example is not running until "#"
# is removed

# path <- "C:/Users/...../...."

# handle_greenhouse(path_data = path, time_step = "daily", vars = c("Tmin", "Tmax"),
#                   check_data = T, n_hours = 12, threshold = 5, fix_data = T)

}
