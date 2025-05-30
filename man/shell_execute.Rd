% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shell.R
\name{shell_execute}
\alias{shell_execute}
\title{Execute a shell command}
\usage{
shell_execute(
  cmd,
  sudo = FALSE,
  password = NULL,
  verbose = TRUE,
  timeout = 300
)
}
\arguments{
\item{cmd}{The command to execute}

\item{sudo}{Whether to use sudo (default: FALSE)}

\item{password}{User password for sudo (only required when sudo=TRUE)}

\item{verbose}{Display the command being executed}

\item{timeout}{Timeout in seconds (default: 300)}
}
\value{
The exit status of the command (0 for success)
}
\description{
Execute a command with or without sudo privileges
}
\keyword{internal}
