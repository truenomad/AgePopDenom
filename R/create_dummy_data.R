#' Simulate Processed DHS Survey Personal Records Data
#'
#' @description
#' This function simulates a synthetic DHS survey dataset, generating age
#' distribution parameters, coordinates, and metadata. It mimics typical
#' DHS/MICS output and saves the result to disk. The metadata includes:
#' - `country`: Name of the country ("Gambia" by default)
#' - `country_code_iso3`: ISO3 country code ("GMB" by default)
#' - `country_code_dhs`: DHS country code ("GM" by default)
#' - `year_of_survey`: Year of the simulated survey (2024 by default)
#'
#' @param country Character. Country name. Default is "Gambia".
#' @param country_code_iso3 Character. ISO3 country code. Default is "GMB".
#' @param country_code_dhs Character. DHS country code. Default is "GM".
#' @param year_of_survey Integer. Year of the survey. Default is 2024.
#' @param total_population Integer. Total number of individuals to simulate.
#'    Default is 266.
#' @param urban_proportion Numeric. Proportion of the population in urban areas.
#'    Default is 0.602.
#' @param lon_range Numeric vector of length 2. Longitude bounds for simulation.
#'    Default is c(-16.802, -13.849).
#' @param lat_range Numeric vector of length 2. Latitude bounds for simulation.
#'    Default is c(13.149, 13.801).
#' @param mean_web_x Numeric. Mean Web Mercator X coordinate. Default is
#'    -1764351.
#' @param mean_web_y Numeric. Mean Web Mercator Y coordinate. Default is
#'    1510868.
#' @param log_scale_mean Numeric. Mean of log-scale parameter. Default is 2.82.
#' @param log_scale_sd Numeric. SD of log-scale parameter. Default is 0.2.
#' @param log_shape_mean Numeric. Mean of log-shape parameter. Default is 0.331.
#' @param log_shape_sd Numeric. SD of log-shape parameter. Default is 0.1.
#' @param b1_mean Numeric. Mean of b1. Default is 0.0142.
#' @param b1_sd Numeric. SD of b1. Default is 0.002.
#' @param c_mean Numeric. Mean of c. Default is -0.00997.
#' @param c_sd Numeric. SD of c. Default is 0.001.
#' @param b2_mean Numeric. Mean of b2. Default is 0.00997.
#' @param b2_sd Numeric. SD of b2. Default is 0.002.
#' @param nsampled_range Integer vector of length 2. Range of sample sizes per
#'    cluster. Default is c(180, 220).
#' @param output_path Character. Path to save the resulting RDS file.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#'
#' @return Saves a list containing a tibble (`age_param_data`) to the specified
#'    path.
#'
#' @export
simulate_dummy_dhs_pr <- function(
  country = "Gambia",
  country_code_iso3 = "GMB",
  country_code_dhs = "GM",
  year_of_survey = 2024,
  total_population = 266,
  urban_proportion = 0.602,
  lon_range = c(-16.802, -13.849),
  lat_range = c(13.149, 13.801),
  mean_web_x = -1764351,
  mean_web_y = 1510868,
  log_scale_mean = 2.82,
  log_scale_sd = 0.2,
  log_shape_mean = 0.331,
  log_shape_sd = 0.1,
  b1_mean = 0.0142,
  b1_sd = 0.002,
  c_mean = -0.00997,
  c_sd = 0.001,
  b2_mean = 0.00997,
  b2_sd = 0.002,
  nsampled_range = c(180, 220),
  output_path = here::here(
    "01_data",
    "1a_survey_data",
    "processed",
    "dhs_pr_records_combined.rds"
  ),
  seed = 123
) {
  stopifnot(
    is.numeric(total_population),
    is.numeric(urban_proportion),
    length(lon_range) == 2,
    length(lat_range) == 2,
    is.numeric(mean_web_x),
    is.numeric(mean_web_y),
    length(nsampled_range) == 2,
    is.character(output_path)
  )

  set.seed(seed)

  total_coords <- total_population

  df_gambia <- list()
  df_gambia$age_param_data <- dplyr::tibble(
    country = country,
    country_code_iso3 = country_code_iso3,
    country_code_dhs = country_code_dhs,
    year_of_survey = year_of_survey,
    id_coords = rep(1:total_coords, length.out = total_population),
    lon = stats::runif(total_population, lon_range[1], lon_range[2]),
    lat = stats::runif(total_population, lat_range[1], lat_range[2]),
    web_x = rnorm(total_population, mean_web_x, 50000),
    web_y = rnorm(total_population, mean_web_y, 50000),
    log_scale = rnorm(total_population, log_scale_mean, log_scale_sd),
    log_shape = rnorm(total_population, log_shape_mean, log_shape_sd),
    urban = rep(
      c(1, 0),
      c(
        round(total_population * urban_proportion),
        total_population - round(total_population * urban_proportion)
      )
    ),
    b1 = rnorm(total_population, b1_mean, b1_sd),
    c = rnorm(total_population, c_mean, c_sd),
    b2 = rnorm(total_population, b2_mean, b2_sd),
    nsampled = sample(
      nsampled_range[1]:nsampled_range[2],
      total_population,
      replace = TRUE
    )
  )

  saveRDS(df_gambia, file = output_path)
}
