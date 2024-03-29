% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert}
\alias{assert}
\alias{assert_mac}
\alias{assert_macos_supported}
\alias{assert_aarch64}
\alias{assert_x86_64}
\title{Assert a condition}
\usage{
assert(condition, message = NULL)

assert_mac()

assert_macos_supported()

assert_aarch64()

assert_x86_64()
}
\arguments{
\item{condition}{A logical indicating the status of the condition.}

\item{message}{A string to display.}
}
\description{
\code{assert()} allows a function state to be checked and stopped.
}
\keyword{internal}
