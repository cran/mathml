% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mathml.R
\name{hook}
\alias{hook}
\title{Hook for custom symbols}
\usage{
hook(term, display, quote = TRUE, as.rolog = TRUE)
}
\arguments{
\item{term}{an R call or symbol/number. This is the expression to replace.}

\item{display}{an R call or symbol/number. This is shown instead of \emph{term}.}

\item{quote}{(default is TRUE)
indicates that \emph{term} and \emph{display} should be quoted.}

\item{as.rolog}{(default is TRUE)
indicates that simplified quasi-quotoation is to be used.}
}
\value{
TRUE on success
}
\description{
Hook for custom symbols
}
\examples{
hook(t0, subscript(t, 0))
mathml(quote(t0))

hook(term=quote(t0), display=quote(subscript(t, 0)), quote=FALSE)
mathml(quote(t0))

}
