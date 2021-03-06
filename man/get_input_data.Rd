% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_data.R
\name{get_input_data}
\alias{get_input_data}
\title{Get netcdf data from files}
\usage{
get_input_data(reachid, data_dir)
}
\arguments{
\item{reachid}{string reach identifier}

\item{data_dir}{directory that contains SWOT and SoS data}
}
\value{
list matrix of reach data (both valid and invalid)
}
\description{
Retrieve geoBAM input variables and replace fill values with NA.
}
