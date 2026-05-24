# Predict Gamma Distribution Parameters for Spatial Grid

This function predicts the scale and shape parameters of a Gamma
distribution across a spatial grid using a bivariate spatial model. It
can either generate new predictions or load cached results if available.

## Usage

``` r
generate_gamma_predictions(
  country_code,
  age_param_data,
  model_params,
  predictor_data,
  shapefile,
  cell_size = 5000,
  n_sim = 5000,
  ignore_cache = FALSE,
  save_file = FALSE,
  output_dir = here::here("03_outputs", "3a_model_outputs")
)
```

## Arguments

- country_code:

  A string representing the country code (e.g., "KEN").

- age_param_data:

  A data frame containing:

  - web_x, web_y: Spatial coordinates

  - urban: Urban/rural indicator

  - log_scale: Log of scale parameter at observed locations

  - log_shape: Log of shape parameter at observed locations

- model_params:

  A list containing model parameters:

  - par: Named vector with gamma, log_sigma2, log_phi, log_tau1

  - Additional parameters for extracting beta coefficients

- predictor_data:

  A data object containing the predictors data.

- shapefile:

  An sf object defining the boundary for predictions

- cell_size:

  Numeric. Grid cell size in meters (default: 5000)

- n_sim:

  Integer. Number of simulations for prediction (default: 5000)

- ignore_cache:

  A boolean input which is set to determine whether to ignore the
  existing cache and write over it. Default is set to FALSE.

- save_file:

  A boolean to determine whether to save prediction or not. Default is
  FALSE as this will require lots of space.

- output_dir:

  A string specifying the directory where the predictions file should be
  saved (default is "03_outputs/3a_model_outputs").

## Value

A list containing:

- scale_pred: Matrix of simulated scale parameters

- shape_pred: Matrix of simulated shape parameters
