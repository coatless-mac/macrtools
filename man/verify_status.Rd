% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{verify_status}
\alias{verify_status}
\title{Verify Status of Operation}
\usage{
verify_status(status, program, url, type = c("uninstall", "install"))
}
\arguments{
\item{status}{Status code from operation}

\item{program}{Name of the program being installed or uninstalled}

\item{url}{Optional URL for manual instructions}

\item{type}{Type of operation ("uninstall" or "install")}
}
\value{
TRUE if status is successful, FALSE otherwise (invisibly)
}
\description{
Verify Status of Operation
}
\keyword{internal}
