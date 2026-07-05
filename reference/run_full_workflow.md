# Run Country-Specific Spatial Modeling Workflow with Logging

This function runs the entire spatial modeling workflow for a given
country code and logs the results. It processes Survey data, fits a
spatial model, generates predictions, creates population tables, and
produces raster outputs. The function is modular and can be reused for
different countries with minimal adjustments.

## Usage

``` r
run_full_workflow(
  country_code,
  survey_data_path = here::here("01_data", "1a_survey_data", "processed"),
  survey_data_suffix = "dhs_pr_records_combined.rds",
  shape_path = here::here("01_data", "1c_shapefiles"),
  shape_suffix = "district_shape.gpkg",
  pop_raster_path = here::here("01_data", "1b_rasters", "pop_raster"),
  pop_raster_suffix = "_ppp_2020_constrained.tif",
  ur_raster_path = here::here("01_data", "1b_rasters", "urban_extent"),
  ur_raster_suffix = "afurextent.asc",
  pred_save_file = FALSE,
  raster_width = 2500,
  raster_height = 2000,
  raster_resolution = 300,
  save_raster = TRUE,
  generate_pop_raster = FALSE,
  pyramid_line_color = "#67000d",
  pyramid_fill_high = "#fee0d2",
  pyramid_fill_low = "#a50f15",
  pyramid_caption = paste0("Note: Total population includes ",
    "ages 99+, pyramid shows ages 0-99"),
  output_paths = list(),
  model_params = list(),
  return_results = FALSE,
  n_cores = max(1, parallel::detectCores() - 2, na.rm = TRUE),
  ...
)
```

## Arguments

- country_code:

  Character. The ISO3 country code (e.g., "TZA").

- survey_data_path:

  Character. Path to Survey data. Default:
  "01_data/1a_survey_data/processed".

- survey_data_suffix:

  Character. Suffix for Survey data files. Default:
  "dhs_pr_records_combined.rds".

- shape_path:

  Character. Path to shapefile data. Default: "01_data/1c_shapefiles".

- shape_suffix:

  Character. Suffix for shapefile data. Default: "district_shape.gpkg".

- pop_raster_path:

  Character. Path to population raster data. Default:
  "01_data/1b_rasters/pop_raster".

- pop_raster_suffix:

  Character. Suffix for population raster files. Default:
  "\_ppp_2020_constrained.tif".

- ur_raster_path:

  Character. Path to urban-rural extent data. Default:
  "01_data/1b_rasters/urban_extent".

- ur_raster_suffix:

  Character. Suffix for urban-rural raster. Default: "afurextent.asc".

- pred_save_file:

  Logical. Whether to save prediction files. Default: FALSE

- raster_width:

  Integer. Width of raster plots in pixels. Default: 2500

- raster_height:

  Integer. Height of raster plots in pixels. Default: 2000

- raster_resolution:

  Integer. Resolution of PNG outputs. Default: 300

- save_raster:

  Logical. Whether to save raster outputs to disk. Default: TRUE

- generate_pop_raster:

  Logical. Whether to generate population raster. Default: FALSE

- pyramid_line_color:

  Character. Hex color code for the age pyramid's outline. Default:
  "#67000d"

- pyramid_fill_high:

  Character. Hex color code for the age pyramid's higher values fill.
  Default: "#fee0d2"

- pyramid_fill_low:

  Character. Hex color code for the age pyramid's lower values fill.
  Default: "#a50f15"

- pyramid_caption:

  Character. Caption text for the age pyramid plot. Default: "Note:
  Total population includes ages 99+, pyramid shows ages 0-99"

- output_paths:

  List of output paths:

  - model: Path for model outputs. Default:
    "03_outputs/3a_model_outputs"

  - plot: Path for plots. Default: "03_outputs/3b_visualizations"

  - raster: Path for rasters. Default: "03_outputs/3c_raster_outputs"

  - table: Path for tables. Default: "03_outputs/3c_table_outputs"

  - compiled: Path for compiled results. Default:
    "03_outputs/3d_compiled_results"

  - excel: Path for Excel outputs. Default:
    "03_outputs/3d_compiled_results/age_pop_denom_compiled.xlsx"

  - log: Path for logs. Default:
    "03_outputs/3a_model_outputs/modelling_log.rds"

- model_params:

  List of model parameters:

  - cell_size: Cell size in meters. Default: 5000

  - n_sim: Number of simulations. Default: 5000

  - ignore_cache: Whether to ignore cache. Default: FALSE

  - age_range: Age range vector. Default: c(0, 99)

  - age_interval: Age interval. Default: 1

  - return_prop: Return proportions. Default: TRUE

  - scale_outcome: Scale outcome variable. Default: "log_scale"

  - shape_outcome: Shape outcome variable. Default: "log_shape"

  - covariates: Model covariates. Default: "urban"

  - cpp_script: C++ script path. Default: "02_scripts/model"

  - control_params: Control parameters. Default: list(trace = 2)

  - manual_params: Manual parameters. Default: NULL

  - verbose: Verbose output. Default: TRUE

  - age_range_raster: Age range for raster output. Default: c(0, 10)

  - age_interval_raster: Age interval for raster output. Default: 1

- return_results:

  Logical. Whether to return results. Default: FALSE.

- n_cores:

  Integer number of cores for parallel processing for age population
  table, default max(1, detectCores()-2)

- ...:

  Additional arguments passed to subfunctions.

## Value

If return_results is TRUE, a list containing:

- spat_model_param: Fitted spatial model parameters

- predictor_data: Predictor dataset

- gamma_prediction: Generated gamma predictions

- pred_list: Processed gamma prediction results

- final_age_pop_table: Age-population table data

- final_pop: Compiled population data

- all_mod_params: Compiled model parameters

If return_results is FALSE, the function saves all outputs to disk and
returns NULL invisibly.

## See also

- [`fit_spatial_model`](https://truenomad.github.io/AgePopDenom/reference/fit_spatial_model.md):
  Fits the spatial model for age parameters

- [`create_prediction_data`](https://truenomad.github.io/AgePopDenom/reference/create_prediction_data.md):
  Creates predictor dataset from spatial inputs

- [`generate_gamma_predictions`](https://truenomad.github.io/AgePopDenom/reference/generate_gamma_predictions.md):
  Generates predictions using fitted model

- [`process_gamma_predictions`](https://truenomad.github.io/AgePopDenom/reference/process_gamma_predictions.md):
  Processes gamma prediction results

- [`generate_gamma_raster_plot`](https://truenomad.github.io/AgePopDenom/reference/generate_gamma_raster_plot.md):
  Creates prediction raster plots

- [`generate_age_pop_table`](https://truenomad.github.io/AgePopDenom/reference/generate_age_pop_table.md):
  Generates age-population tables

- [`generate_age_pyramid_plot`](https://truenomad.github.io/AgePopDenom/reference/generate_age_pyramid_plot.md):
  Creates age pyramid plots

- [`extract_age_param`](https://truenomad.github.io/AgePopDenom/reference/extract_age_param.md):
  Extracts and compiles model parameters

- [`process_final_population_data`](https://truenomad.github.io/AgePopDenom/reference/process_final_population_data.md):
  Processes final population data

- [`generate_variogram_plot`](https://truenomad.github.io/AgePopDenom/reference/generate_variogram_plot.md):
  Creates variogram plots showing spatial dependence structure

## Examples

``` r

if (FALSE) { # \dontrun{
# set country code
country_codeiso <- "GMB"

set.seed(123)
# Set parameters for simulation
total_population <- 266
urban_proportion <- 0.602
total_coords <- 266
lon_range <- c(-16.802, -13.849)
lat_range <- c(13.149, 13.801)
mean_web_x <- -1764351
mean_web_y <- 1510868

# Simulate processed survey dataset for Gambia
df_gambia <- NULL
df_gambia$age_param_data <- dplyr::tibble(
  country = "Gambia",
  country_code_iso3 = "GMB",
  country_code_dhs = "GM",
  year_of_survey = 2024,
  id_coords = rep(1:total_coords, length.out = total_population),
  lon = runif(total_population, lon_range[1], lon_range[2]),
  lat = runif(total_population, lat_range[1], lat_range[2]),
  web_x = rnorm(total_population, mean_web_x, 50000),
  web_y = rnorm(total_population, mean_web_y, 50000),
  log_scale = rnorm(total_population, 2.82, 0.2),
  log_shape = rnorm(total_population, 0.331, 0.1),
  urban = rep(c(1, 0), c(
    round(total_population * urban_proportion),
    total_population - round(total_population * urban_proportion)
  )),
  b1 = rnorm(total_population, 0.0142, 0.002),
  c = rnorm(total_population, -0.00997, 0.001),
  b2 = rnorm(total_population, 0.00997, 0.002),
 nsampled = sample(180:220, total_population, replace = TRUE)
)


# Create temp directory with normalized path
tf <- file.path(tempdir(), "test_env")
dir.create(tf, recursive = TRUE, showWarnings = FALSE)
tf <- normalizePath(tf, winslash = "/", mustWork = FALSE)

AgePopDenom::init(
  r_script_name = "full_pipeline.R",
  cpp_script_name = "model.cpp",
  path = tf,
  open_r_script = FALSE
)

# save as processed dhs data
saveRDS(
  df_gambia,
  file = file.path(
    tf, "01_data", "1a_survey_data", "processed",
    "dhs_pr_records_combined.rds"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE)
)

# Download shapefiles
download_shapefile(
  country_codes = country_codeiso,
  dest_file = file.path(
    tf, "01_data", "1c_shapefiles",
    "district_shape.gpkg"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE)
)

# Download population rasters from worldpop
download_pop_rasters(
 country_codes = country_codeiso,
  dest_dir = file.path(tf, "01_data", "1b_rasters", "pop_raster") |>
    normalizePath(winslash = "/", mustWork = FALSE)
)
# Extract urban extent raster
extract_afurextent(
  dest_dir = file.path(tf, "01_data", "1b_rasters", "urban_extent") |>
    normalizePath(winslash = "/", mustWork = FALSE)
)

# Modelling --------------------------------------------------------------

run_full_workflow(
  country_code = country_codeiso,
  survey_data_path = file.path(
    tf, "01_data", "1a_survey_data", "processed"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE),
  survey_data_suffix = "dhs_pr_records_combined.rds",
  shape_path = file.path(
    tf, "01_data", "1c_shapefiles"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE),
  shape_suffix = "district_shape.gpkg",
  pop_raster_path = file.path(
    tf, "01_data", "1b_rasters", "pop_raster"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE),
  pop_raster_suffix = "_ppp_2020_constrained.tif",
  ur_raster_path = file.path(
    tf, "01_data", "1b_rasters", "urban_extent"
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE),
  ur_raster_suffix = "afurextent.asc",
  pred_save_file = FALSE,
  raster_width = 2500,
  raster_height = 2000,
  raster_resolution = 300,
  save_raster = TRUE,
  pyramid_line_color = "#67000d",
  pyramid_fill_high = "#fee0d2",
  pyramid_fill_low = "#a50f15",
  pyramid_caption = paste0(
    "Note: Total population includes ",
    "ages 99+, pyramid shows ages 0-99"
  ),
  generate_pop_raster = TRUE,
  output_paths = list(
    model = file.path(tf, "03_outputs", "3a_model_outputs"),
    plot = file.path(tf, "03_outputs", "3b_visualizations"),
    raster = file.path(tf, "03_outputs", "3c_raster_outputs"),
    table = file.path(tf, "03_outputs", "3c_table_outputs"),
    compiled = file.path(tf, "03_outputs", "3d_compiled_results"),
    excel = file.path(
      tf, "03_outputs", "3d_compiled_results",
      "age_pop_denom_compiled.xlsx"
    ),
    log = file.path(
      tf, "03_outputs", "3a_model_outputs", "modelling_log.rds"
    )
  ) |> lapply(\(x) normalizePath(x, winslash = "/", mustWork = FALSE)),
  model_params = list(
    cell_size = 5000,
    n_sim = 10,
    ignore_cache = FALSE,
    age_range = c(0, 1),
    age_interval = 1,
    return_prop = TRUE,
    scale_outcome = "log_scale",
    shape_outcome = "log_shape",
    covariates = "urban",
    cpp_script = file.path(tf, "02_scripts", "model") |>
      normalizePath(winslash = "/", mustWork = FALSE),
    control_params = list(trace = 2),
    manual_params = NULL,
    verbose = TRUE
  ),
  return_results = FALSE,
  n_cores = 1
)
} # }
```
