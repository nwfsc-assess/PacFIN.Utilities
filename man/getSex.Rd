% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSex.R
\name{getSex}
\alias{getSex}
\title{Get sex codes and convert to character values}
\usage{
getSex(data.vector, verbose = FALSE)
}
\arguments{
\item{data.vector}{A vector of data that contains information on SEX.}

\item{verbose}{A logical value specifying if output should be written
to the screen or not. Good for testing and exploring your data but
can be turned off when output indicates errors.
The printing doesn't affect any of the returned objects, instead,
printing makes it easier to see if the data has errors and what was
done to rectify them in the given function.
Sorry, but the default is to always print to the screen,
i.e., \code{verbose = TRUE}, so you do not miss out on anything.
This is standard practice for packages in the nwfsc-assess organization.}
}
\value{
A vector the same length as the input vector where all the entries
will be character values of either \code{M}, \code{F}, or \code{U}.
}
\description{
Get current codes used for SEX and transcribe to character values only.
Males (1), females (2), and unknown sex (3, 9, NA, NULL) will be coded
as \code{M}, \code{F}, and \code{U}, respectively. Conversion codes were gleaned from
\href{https://wdfw.wa.gov/sites/default/files/publications/01754/wdfw01754.pdf}{Washington Department of Fish and Wildlife documentation}.
}
\seealso{
\link{cleanPacFIN} uses this function to convert the column \code{SEX} to
standardized values. Note that PacFIN does this now as well with the
comprehensive data base, but the function is retained for historical data
and to provide a summary of sexes by using \code{verbose = TRUE}.
}
