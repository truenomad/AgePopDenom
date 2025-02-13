% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling.R
\name{extract_betas}
\alias{extract_betas}
\title{Extract Beta Parameters from Model Output}
\usage{
extract_betas(
  params_result,
  params = c("gamma", "log_sigma2", "log_phi", "log_tau1")
)
}
\arguments{
\item{params_result}{A model parameter object containing parameter estimates}

\item{params}{A character vector specifying parameter names, defaults to
c("gamma", "log_sigma2", "log_phi", "log_tau1")}
}
\value{
A list with two components:
  \itemize{
    \item beta1: First set of beta coefficients
    \item beta2: Second set of beta coefficients
  }
}
\description{
This function extracts beta coefficients from a model parameter object,
separating them into beta1 and beta2 components.
}
\details{
The function assumes the parameter vector contains beta coefficients
followed by other model parameters. It splits the betas into two equal
groups after removing the last 4 parameters.
}
