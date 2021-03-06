% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getUserName.R
\name{getUserName}
\alias{getUserName}
\title{Get User Name Based On Computer User Name}
\usage{
getUserName(datasourcename = c("PacFIN"))
}
\arguments{
\item{datasourcename}{The sql data base name that is stored on your
computer. The default is \code{"PacFIN"} which will pull data from
the Pacific Fisheries Information Network.}
}
\value{
A single character value to use as input in functions
calling for \code{username = }.
}
\description{
Query the user name of your local machine to generate
a suspected user name for the \code{datasourcename}.
}
\examples{
getUserName()

}
\author{
Kelli Faye Johnson
}
