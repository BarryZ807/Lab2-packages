% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Function.R
\name{stats_average_payment}
\alias{stats_average_payment}
\title{stats_average_payment}
\usage{
stats_average_payment(data, option)
}
\arguments{
\item{data}{is the dataset}

\item{option}{is statistics types for mean, median, standard deviation}
}
\value{
required statistics
}
\description{
Function that calculates statistics over all of the DRG codes for average Medicare payments. variable \code{option}
}
\examples{
stats_average_payment(DRG, 'mean')
}
