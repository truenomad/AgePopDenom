% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\name{download_shapefile}
\alias{download_shapefile}
\title{Download WHO ADM2 Boundaries with Partial Update}
\usage{
download_shapefile(
  country_codes,
  dest_file = here::here("01_data", "1c_shapefiles", "district_shape.gpkg")
)
}
\arguments{
\item{country_codes}{Character vector of ISO3 country codes
(e.g. c("KEN","UGA")).}

\item{dest_file}{File path where data is saved (default:
"01_data/1c_shapefiles/district_shape.gpkg").}
}
\value{
An `sf` object of combined boundaries for all requested country
  codes.
}
\description{
This function ensures the specified shapefile (`dest_file`) contains
boundaries for all requested ISO3 codes (`country_codes`). It dynamically
Downloades only the missing codes and appends them to the file, ensuring no
duplication.
}
\examples{

\donttest{

tf <- file.path(tempdir(), "test_env")

# Download population rasters from worldpop
download_shapefile(
  country_codes = "COM",
  dest_file = here::here(tf, "district_shape.gpkg")
)
}
}
