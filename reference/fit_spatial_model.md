# Fit a Spatial Model for Age Parameters using TMB

Fits a spatial model for age parameters using Template Model Builder
(TMB) and C++. The model incorporates spatial correlation through
distance matrices and handles both scale and shape parameters
simultaneously. Progress is tracked with clear status updates. Can
optionally load from cache.

## Usage

``` r
fit_spatial_model(
  data,
  country_code = NULL,
  scale_outcome,
  shape_outcome,
  covariates,
  cpp_script_name,
  verbose = TRUE,
  control_params = list(trace = 2),
  manual_params = NULL,
  output_dir = NULL,
  ignore_cache = FALSE
)
```

## Arguments

- data:

  A data frame containing the response variables, covariates, and
  spatial coordinates (web_x, web_y)

- country_code:

  Optional country code to save/load cached model. Default NULL runs
  model without caching.

- scale_outcome:

  Character string specifying the column name for the scale parameter
  response variable

- shape_outcome:

  Character string specifying the column name for the shape parameter
  response variable

- covariates:

  Character vector of covariate names to include in both scale and shape
  models

- cpp_script_name:

  Character string specifying the name of the C++ file (without
  extension) containing the TMB model definition

- verbose:

  Logical indicating whether to show progress updates. Default TRUE

- control_params:

  List of control parameters passed to nlminb optimizer. Default:
  list(trace = 2)

- manual_params:

  Optional list of manual parameter values. If NULL (default), initial
  parameters are estimated from linear regression. The list should
  contain:

  - beta1: Vector of coefficients for scale model

  - beta2: Vector of coefficients for shape model

  - gamma: Scalar value (default 1.0)

  - log_sigma2: Log of sigma squared (default log(1.0))

  - log_phi: Log of phi (estimated from variogram)

  - log_tau2_1: Log of tau squared (default log(1.0))

- output_dir:

  Directory to save cached models. Only used if country_code is
  provided.

- ignore_cache:

  Whether to ignore existing cache. Default FALSE.

## Value

An object of class 'nlminb' containing:

- par - Optimized parameter values

- objective - Final value of objective function

- convergence - Convergence code

- message - Convergence message

- iterations - Number of iterations

- evaluations - Number of function/gradient evaluations

- scale_formula - Formula used for scale model

- shape_formula - Formula used for shape model

- variogram - Fitted variogram model from automap containing:

  - range - Spatial correlation range parameter

  - psill - Partial sill (structured variance)

  - nugget - Nugget effect (unstructured variance)

  - kappa - Smoothness parameter for Matern models

## Details

The function performs the following steps with progress tracking: 1.
Fits initial linear models for scale and shape parameters 2. Calculates
spatial distance matrix from web coordinates 3. Estimates optimal phi
parameter using variogram: - Computes empirical variogram using
automap - Automatically selects best theoretical variogram model - Range
parameter is used to initialize spatial correlation - Default range of
100 used if estimation fails 4. Compiles and loads the TMB C++ template
5. Optimizes the joint likelihood using nlminb

The spatial correlation is modeled using an exponential variogram with
parameters estimated from the data. The distance matrix is computed from
the web coordinates (web_x, web_y) and used in the spatial covariance
structure.

The C++ template should implement the joint spatial model for both
parameters.

## Note

Requires TMB package and a working C++ compiler. The C++ template must
be properly structured for TMB. The automap package is required for
variogram fitting.

## Examples

``` r

if (FALSE) { # \dontrun{
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


tf <- file.path(tempdir(), "test_env")
dir.create(tf, recursive = TRUE, showWarnings = FALSE)

#initialise files and key scripts
init(
  r_script_name = "full_pipeline.R",
  cpp_script_name = "model.cpp",
  path = tf,
  open_r_script = FALSE
)

mod <- fit_spatial_model(
  df_gambia$age_param_data,
  scale_outcome = "log_scale",
  shape_outcome = "log_shape",
  covariates = "urban",
  cpp_script_name = file.path(tf, "02_scripts/model"),
  country_code = "GMB",
  output_dir = file.path(tf, "03_outputs/3a_model_outputs")
)

} # }
```
