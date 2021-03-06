% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/SearchFunctions.R
\name{ConvertTrueFalse}
\alias{ConvertTrueFalse}
\title{ConvertTrueFalse}
\usage{
ConvertTrueFalse(TrueorFalse, True = "1", False = "0")
}
\arguments{
\item{TrueorFalse}{Boolean, either TRUE or FALSE}

\item{True}{Conversion for TRUE variable}

\item{False}{Conversion for FALSE variable}
}
\value{
Will return conversion
}
\description{
Convert True/False to other variables
}
\details{
This function will convert TRUE and FALSE to something esle.  Even though you can add 0 (for example, TRUE+0)
to get 1 and 0, this function works by specifying any T/F variables.
}
\examples{
ConvertTrueFalse(TRUE)
ConvertTrueFalse(TRUE, True="ppp", False="qqq")
}
\seealso{
\link{MakeDataAvailabilityTable}
}

