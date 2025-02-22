% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{generate_gamma_raster_plot}
\alias{generate_gamma_raster_plot}
\title{Generate and Save Raster Plot for Gamma Predictions}
\usage{
generate_gamma_raster_plot(
  predictor_data,
  pred_list,
  country_code,
  output_dir,
  save_raster = TRUE,
  file_name_suffix = "gamma_prediction_rasters",
  width = 2500,
  height = 2000,
  png_resolution = 300
)
}
\arguments{
\item{predictor_data}{A data frame containing `web_x` and `web_y`
coordinates and associated prediction values.}

\item{pred_list}{A list containing predictions (`shape_hat`, `scale_hat`,
`mean_age_pred`) for creating rasters.}

\item{country_code}{A string representing the lowercase country code,
used for naming the output file.}

\item{output_dir}{A string specifying the directory where the plot should
be saved.}

\item{save_raster}{A logical input specifying whether to save output or not.
Default is TRUE.}

\item{file_name_suffix}{A string specifying the suffix for the file name
(default is "gamma_prediction_rasters").}

\item{width}{Numeric. Width of output plot in pixels (default: 2500).}

\item{height}{Numeric. Height of output plot in pixels (default: 2000).}

\item{png_resolution}{An integer specifying the resolution of the plot in DPI
(default: 300).}
}
\value{
The path to the saved raster plot.
}
\description{
This function creates rasters from prediction data, combines them into a
stack, and saves the faceted plot to a specified file.
}
\examples{
\dontrun{
# Not run to avoid lengthy data downloads, processing and modelling
# in examples
raster_path <- generate_gamma_raster_plot(
   predictor_data = predictor_data,
   pred_list = pred_list,
   country_code = "ken",
   output_dir = "03_outputs/3b_visualizations"
 )
}

}
