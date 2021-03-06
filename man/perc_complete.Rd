% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perc_complete.R
\name{perc_complete}
\alias{perc_complete}
\title{Compute the percentage of data complete within a dataframe}
\usage{
perc_complete(x)
}
\arguments{
\item{x}{a dataframe normally produced through weather functions retrieving climate data. However, this is also
possible to use with any dataframe.}
}
\description{
Often it is relevant to compute the percentage of data complete in a dataframe to have an idea of the 
quality of the data. This is specially important for weather data where climate databases often have missing 
days. This function only works for dataframes. For lists, see details.
}
\details{
For list of dataframes, it is useful to combine this function with \code{\link[base]{lapply}}. The output is a dataframe
containing two columns (variable and percentage) and rows equal to the number of columns in the input dataframe.
}
\examples{

df <- data.frame(year = rep(2011, 5),
                 month = rep(1, 5),
                 day = seq(1, 5),
                 Tmin = c(10, 12, 11, NA, 21))
                 
perc_complete(df)           

}
