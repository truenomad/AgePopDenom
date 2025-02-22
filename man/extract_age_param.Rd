% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{extract_age_param}
\alias{extract_age_param}
\title{Extract Parameters and Optimization Details with Log-Likelihood}
\usage{
extract_age_param(
  dir_path = here::here("03_outputs", "3a_model_outputs"),
  output_file = here::here("03_outputs", "3d_compiled_results", "model_params.csv")
)
}
\arguments{
\item{dir_path}{A character string specifying the directory containing the
files.}

\item{output_file}{A character string specifying the path to save the output
data frame. If NULL, the output will not be saved.}
}
\value{
A data frame with the extracted parameters, log-likelihood,
        and optimization details.
}
\description{
Reads files matching the pattern "age_param_spatial_urban" in a specified
directory, extracts the country name, parameter values, and optimization
details, combines the results into a data frame, and optionally saves
the output to a file.
}
\examples{
\dontrun{
# Not run to avoid lengthy data downloads, processing and modelling
# in examples
params_df <- extract_age_param(
  dir_path = "03_outputs/3a_model_outputs",
  output_file = "03_outputs/age_parameters.csv"
 )
}

}
