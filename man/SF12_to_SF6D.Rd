% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.R
\name{SF12_to_SF6D}
\alias{SF12_to_SF6D}
\title{Predict sample mean SF-6D utility}
\usage{
SF12_to_SF6D(means, SDs, corr = NULL, dimensions = NULL,
  values = NULL, version = 2)
}
\arguments{
\item{means}{A vector: \code{(mean(PCS), mean(MCS))}}

\item{SDs}{A vector: \code{(sd(PCS), sd(MCS))}}

\item{corr}{\strong{(optional)} A matrix: \code{(Cor(PCS, MCS))}. Default is to use the correlation matrix obtained
from the estimation sample.}

\item{dimensions}{\strong{(optional)} A character vector: one or more of "utility", "PF", "RL", "SF", "PAIN", "MH",
"VIT", "MOST", indicating which dimensions should be predicted ("utility" (the default) gives the overall SF-6D
utility score); or "all", indicating all of the above.}

\item{values}{\strong{(optional)} A list with items \code{PF}, \code{RL}, .... SF-6D utility values (additive
detriments from perfect health). The default is to use the UK general population valuations provided by Brazier &
Roberts (2004).}

\item{version}{\strong{(optional)} An integer: 1 = PCS/MCS scores are from SF-12-v1; 2 (default) = PCS/MCS scores are
from SF-12-v2. Other values give an error.}
}
\value{
A numeric vector of length \code{length(dimensions)} (or length 8 for \code{dimensions == "all"}): predicted
  sample mean utility value and/or contributions to mean utility of each dimension.
}
\description{
\code{SF12_to_SF6D} provides an estimate of the predicted sample mean SF-6D utility value from the sample means and
  standard deviations of SF-12 PCS and MCS scores (and optionally their correlation).
}
\section{References}{

Brazier, John E and Roberts, Jennifer. 2004. The estimation of a preference-based measure of health from the SF-12.
  \emph{Med Care}, 42(9):851-859.
}

\seealso{
\code{\link{SF6D_distributions}} for the dimension-specific functions, and \code{\link{SF6D_uncertainty}}
for uncertainty intervals around the predicted mean utility values.
}
