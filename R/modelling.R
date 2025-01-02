#' Fit a Spatial Model for Age Parameters using TMB
#'
#' @description
#' Fits a spatial model for age parameters using Template Model Builder (TMB)
#' and C++. The model incorporates spatial correlation through distance matrices
#' and handles both scale and shape parameters simultaneously. Progress is
#' tracked with clear status updates.
#'
#' @param data A data frame containing the response variables, covariates, and
#'   spatial coordinates (web_x, web_y)
#' @param scale_outcome Character string specifying the column name for the
#'   scale parameter response variable
#' @param shape_outcome Character string specifying the column name for the
#'   shape parameter response variable
#' @param covariates Character vector of covariate names to include in both
#'   scale and shape models
#' @param cpp_script_name Character string specifying the name of the C++ file
#'   (without extension) containing the TMB model definition
#' @param verbose Logical indicating whether to show progress updates. Default
#'    TRUE
#' @param control_params List of control parameters passed to nlminb optimizer.
#'   Default: list(trace = 2)
#' @param manual_params Optional list of manual parameter values. If NULL
#'   (default), initial parameters are estimated from linear regression. The list
#'   should contain:
#'   \itemize{
#'     \item beta1: Vector of coefficients for scale model
#'     \item beta2: Vector of coefficients for shape model
#'     \item gamma: Scalar value (default 1.0)
#'     \item log_sigma2: Log of sigma squared (default log(1.0))
#'     \item log_phi: Log of phi (default log(100))
#'     \item log_tau2_1: Log of tau squared (default log(1.0))
#'   }
#'
#' @return An object of class 'nlminb' containing:
#'   \itemize{
#'     \item par - Optimized parameter values
#'     \item objective - Final value of objective function
#'     \item convergence - Convergence code
#'     \item message - Convergence message
#'     \item iterations - Number of iterations
#'     \item evaluations - Number of function/gradient evaluations
#'   }
#'
#' @details
#' The function performs the following steps with progress tracking:
#' 1. Fits initial linear models for scale and shape parameters
#' 2. Calculates spatial distance matrix from web coordinates
#' 3. Compiles and loads the TMB C++ template
#' 4. Optimizes the joint likelihood using nlminb
#'
#' The C++ template should implement the joint spatial model for both
#' parameters.
#'
#' @note
#' Requires TMB package and a working C++ compiler. The C++ template must be
#' properly structured for TMB.
#'
#' @examples
#' \dontrun{
#' fit <- fit_spatial_model(
#'   data = my_data,
#'   scale_outcome = "scale_param",
#'   shape_outcome = "shape_param",
#'   covariates = c("elevation", "temperature"),
#'   cpp_script_name = "spatial_model"
#' )
#' }
#'
#' @export
fit_spatial_model <- function(data, scale_outcome,
                              shape_outcome, covariates,
                              cpp_script_name,
                              verbose = TRUE,
                              control_params = list(trace = 2),
                              manual_params = NULL) {

  # Section 1: Initial Linear Models -------------------------------------------

  cli::cli_process_start(msg = "Fitting initial linear models...",
                         msg_done = "Fitted initial linear models.")

  # Create model formulas
  scale_formula <- reformulate(covariates,
                               response = scale_outcome,
                               intercept = FALSE
  )

  shape_formula <- reformulate(covariates,
                               response = shape_outcome,
                               intercept = FALSE
  )

  # Fit
  lm_scale <- tryCatch(
    {
      lm(scale_formula, data = data, x = TRUE)
    },
    error = function(e) {
      stop("Error fitting scale model: ", e$message)
    }
  )

  lm_shape <- tryCatch(
    {
      lm(shape_formula, data = data, x = TRUE)
    },
    error = function(e) {
      stop("Error fitting shape model: ", e$message)
    }
  )

  # Extract design matrices
  design_scale <- lm_scale$x
  design_shape <- lm_shape$x

  cli::cli_process_done()

  # Section 2: Distance Matrix -------------------------------------------------

  cli::cli_process_start(
    msg = "Calculating distance matrix...",
    msg_done =  "Calculated distance matrix.")

  dist_matrix <- data |>
    dplyr::select(web_x, web_y) |>
    as.matrix() |>
    dist() |>
    as.matrix()

  cli::cli_process_done()

  # Section 3: Parameter Setup ------------------------------------------------

  cli::cli_process_start(
    msg = "Initializing data and parameters for optimisation...",
    msg_done = "Initialization complete."
  )

  # set up phi based on the country type
  if (any(data$country_code_iso3 %in% c("BFA", "CAF"))) {
    log_phi = 4000 } else {log_phi = 100}

  # Use manual parameters if provided,
  # otherwise use linear regression estimates
  parameters <- if (!is.null(manual_params)) {
    # Validate manual parameters structure
    required_params <- c("beta1", "beta2", "gamma", "log_sigma2",
                         "log_phi", "log_tau2_1")
    if (!all(required_params %in% names(manual_params))) {
      stop("manual_params must contain all required parameters: ",
           paste(required_params, collapse = ", "))
    }
    manual_params
  } else {
    list(
      beta1 = as.vector(coef(lm_scale)),
      beta2 = as.vector(coef(lm_shape)),
      gamma = 1.0,
      log_sigma2 = log(1.0),
      log_phi = log(log_phi),
      log_tau2_1 = log(1.0)
      # log_sigma2 = log(1.0),
      # log_phi = log(100),
      # log_tau2_1 = log(1.0)
    )
  }

  tmb_data <- list(
    design_scale = design_scale,
    design_shape = design_shape,
    y = c(data[[scale_outcome]], data[[shape_outcome]]),
    dist_matrix = dist_matrix,
    b1 = data$b1,
    c = data$c,
    b2 = data$b2
  )

  cli::cli_process_done()

  # Section 4: TMB Compilation ------------------------------------------------

  cli::cli_process_start(msg = "Compiling TMB model",
                         msg_done  = "Compiled TMB model")

  if (!verbose) {
    suppressWarnings(
      suppressMessages({
        system2("R",
                args = c(
                  "CMD", "SHLIB",
                  paste0(cpp_script_name, ".cpp"), "-O2"
                ),
                stdout = FALSE, stderr = FALSE
        )
        dyn.load(TMB::dynlib(cpp_script_name))
      })
    )
  } else {
    TMB::compile(paste0(cpp_script_name, ".cpp"))
    dyn.load(TMB::dynlib(cpp_script_name))
  }

  cli::cli_process_done()

  # Section 5: Optimization --------------------------------------------------

  cli::cli_process_start(msg = "Optimizing model",
                         msg_done  = "Optimized model")

  obj <- TMB::MakeADFun(
    data = tmb_data,
    parameters = parameters,
    DLL = stringr::str_extract(
      cpp_script_name, "[^/]+$"),
    silent = TRUE
  )

  if (!verbose) {
    control_params$trace <- 0
  }

  opt <- nlminb(obj$par, obj$fn, obj$gr,
                control = control_params
  )

  names(opt$par) <- c(
    names(coef(lm_scale)),
    names(coef(lm_shape)),
    "gamma", "log_sigma2", "log_phi", "log_tau1"
  )

  cli::cli_process_done()

  # include formula to output
  opt$scale_formula <- scale_formula
  opt$shape_formula <- shape_formula

  return(opt)
}

#' Run or Load a Cached Spatial Model
#'
#' This function fits a spatial model for age parameters or loads a cached
#' result if the model output file already exists. It saves the fitted model
#' to a specified directory and provides progress updates.
#'
#' @param country_code A string representing the country code (e.g., "KEN").
#' @param age_param_data A data frame or tibble containing the data for the model.
#' @param scale_outcome A string specifying the outcome variable for the scale
#'   parameter (default is "log_scale").
#' @param shape_outcome A string specifying the outcome variable for the shape
#'   parameter (default is "log_shape").
#' @param covariates A string or vector of strings representing the covariates
#'   to include in the model (default is "urban").
#' @param cpp_script_name A string specifying the path to the CPP script used
#'   for optimization (default is "02_scripts/model_optim").
#' @param output_dir A string specifying the directory where the model results
#'   should be saved (default is "03_outputs/3a_model_outputs").
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#'
#' @return The fitted spatial model object (`spat_model_param`).
#'
#' @examples
#' \dontrun{
#' result <- run_spatial_model(
#'   country_code = "KEN",
#'   age_param_data = my_data,
#'   scale_outcome = "log_scale",
#'   shape_outcome = "log_shape",
#'   covariates = "urban",
#'   cpp_script_name = "02_scripts/model_optim",
#'   output_dir = "03_outputs/3a_model_outputs"
#' )
#'}
#' @export
run_spatial_model <- function(country_code,
                              age_param_data,
                              scale_outcome = "log_scale",
                              shape_outcome = "log_shape",
                              covariates = "urban",
                              cpp_script_name = "02_scripts/model_optim",
                              output_dir = "03_outputs/3a_model_outputs",
                              ignore_cache = FALSE) {

  # Create lowercase country code
  country_code <- tolower(country_code)

  # Construct the model parameter path
  model_param_path <- file.path(
    output_dir,
    glue::glue("{country_code}_age_param_spatial_urban.rds")
  )


  # Check if the model file exists
  if (ignore_cache || !file.exists(model_param_path)) {

    # Fit the spatial model
    spat_model_param <- fit_spatial_model(
      data = age_param_data,
      scale_outcome = scale_outcome,
      shape_outcome = shape_outcome,
      covariates = covariates,
      cpp_script_name = cpp_script_name
    )

    # Save the model output
    saveRDS(spat_model_param, file = model_param_path)

    cli::cli_alert_success("Model fitted and saved at {model_param_path}")
  } else {

    # Notify user about cached results
    cli::cli_process_start(
      msg = "Importing cached model results...",
      msg_done = "Successfully imported cached model results."
    )

    # Read cached model results
    spat_model_param <- readRDS(model_param_path)

    cli::cli_process_done()
  }

  return(spat_model_param)
}


#' Predict Gamma Distribution Parameters for Spatial Grid
#'
#' This function predicts the scale and shape parameters of a Gamma distribution
#' across a spatial grid using a bivariate spatial model.
#'
#' @param age_param A data frame containing:
#'   \itemize{
#'     \item web_x, web_y: Spatial coordinates
#'     \item urban: Urban/rural indicator
#'     \item log_scale: Log of scale parameter at observed locations
#'     \item log_shape: Log of shape parameter at observed locations
#'   }
#' @param model_params A list containing model parameters:
#'   \itemize{
#'     \item par: Named vector with gamma, log_sigma2, log_phi, log_tau1
#'     \item Additional parameters for extracting beta coefficients
#'   }
#' @param shapefile An sf object defining the boundary for predictions
#' @param cell_size Numeric. Grid cell size in meters (default: 5000)
#' @param n_sim Integer. Number of simulations for prediction (default: 5000)
#' @param predictor_data A data object containing the predictors data.
#' @return A list containing:
#'   \itemize{
#'     \item country_grid: Data frame with prediction grid coordinates
#'     \item scale_pred: Matrix of simulated scale parameters
#'     \item shape_pred: Matrix of simulated shape parameters
#'   }
#'
#' @export
predict_gamma_params <- function(age_param, predictor_data,
                                 model_params, shapefile,
                                 cell_size = 5000, n_sim = 5000) {

  # Make grid ----------------------------------------------------------------

  cli::cli_process_start(
    msg = "Making a prediction grid...",
    msg_done = "Prediction grid successfully made.")

  country_grid <- predictor_data |> dplyr::select(web_x, web_y)

  cli::cli_process_done()

  # Set parameters from models -------------------------------------------------

  predictor_data <- predictor_data |>
    dplyr::mutate(log_scale = 1, log_shape = 1)

  cli::cli_process_start(
    msg = "Setting model parameters...",
    msg_done = "Model parameters set successfully.")

  gamma <- model_params$par['gamma']
  sigma2 <- exp(model_params$par['log_sigma2'])
  phi <- exp(model_params$par['log_phi'])
  tau <- exp(model_params$par['log_tau1'])
  beta1 <- extract_betas(model_params)$beta1
  beta2 <- extract_betas(model_params)$beta2
  y <- c(age_param$log_scale, age_param$log_shape)
  d1 <- d2 <- model.matrix(model_params$scale_formula,
                           data = age_param)
  mu1 <- as.numeric(d1 %*% beta1)
  mu2 <- as.numeric(d2 %*% beta2)
  mu <- c(mu1, mu2)

  # Predict the gamma parameters
  d_pred <- model.matrix(model_params$scale_formula,
                         data = predictor_data)

  mu1_pred <- as.numeric(d_pred %*% beta1)
  mu2_pred <- as.numeric(d_pred %*% beta2)

  u_dist <- as.matrix(dist(age_param[, c("web_x", "web_y")]))
  n_x <- nrow(u_dist)

  cli::cli_process_done()

  # Predict the random effects ----------------------------------------------

  cli::cli_process_start(
    msg = "Calculating pairwise distances for prediction grid...",
    msg_done = "Computed pairwise distances for prediction grid.")

  u_pred <- pdist::pdist(country_grid,
                         age_param[,c("web_x","web_y")]) |>
    as.matrix()

  n_pred <- nrow(country_grid)

  cli::cli_process_done()

  cli::cli_process_start(
    msg = "Computing and inverting covariance matrix...",
    msg_done = "Computed and inverted covariance matrix.")

  c_s_star <- cbind(sigma2 * exp(-u_pred / phi),
                    gamma * sigma2 * exp(-u_pred / phi))

  matrix_inv <- compute_cov(
    gamma = gamma,
    phi = phi,
    sigma2 = sigma2,
    u_dist = u_dist,
    n_x = n_x,
    tau2_1 = tau,
    age_param_data = age_param
  ) |>
    chol() |>
    chol2inv()

  cli::cli_process_done()

  cli::cli_process_start(
    msg = "Computing mean and standard deviation of predictions...",
    msg_done = "Computed mean and standard deviation of predictions.")

  a <- c_s_star %*% matrix_inv
  mean_s_pred <- a %*% (y - mu)
  sd_s_pred <- sqrt(sigma2 - apply(a * c_s_star, 1, sum))

  cli::cli_process_done()

  # Simulate random effects ----------------------------------------------------

  cli::cli_process_start(
    msg = "Simulating random effects...",
    msg_done = "Simulated random effects.")

  s_samples <- sapply(1:n_sim,
                      function(i) rnorm(n_pred, mean_s_pred, sd_s_pred))

  cli::cli_process_done()

  # Predict the gamma parameters -----------------------------------------------

  cli::cli_process_start(
    msg = "Predicting gamma, scale, and shape parameters...",
    msg_done = "Predicted gamma, scale, and shape parameters.")

  scale_pred <- exp(sapply(1:n_sim,
                           function(i) mu1_pred + s_samples[, i]))
  shape_pred <- exp(sapply(1:n_sim,
                           function(i) mu2_pred + gamma * s_samples[, i]))

  cli::cli_process_done()

  # Return results -------------------------------------------------------------

  list(
    scale_pred = scale_pred,
    shape_pred = shape_pred
  )
}

#' Generate or Load Cached Predictors Data
#'
#' This function creates predictors data based on spatial inputs or loads cached
#' predictors data if the file already exists. It saves the generated data to a
#' specified directory for reuse and provides progress updates.
#'
#' @param country_code A string representing the country code (e.g., "KEN").
#' @param country_shape An `sf` object representing the country's administrative
#'   boundaries.
#' @param pop_raster A `terra` raster object representing the population raster.
#' @param ur_raster A `terra` raster object representing the urban extent
#'  raster.
#' @param adm2_shape An `sf` object representing the administrative level 2
#'  boundaries.
#' @param cell_size An integer specifying the cell size for the prediction grid
#'   in meters (default is 5000).
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#' @param output_dir A string specifying the directory where the predictors data
#'   file should be saved (default is "03_outputs/3a_model_outputs").
#'
#' @return A data object (`predictor_data`) containing the generated predictors.
#'
#' @examples
#' \dontrun{
#' predictors <- generate_predictor_data(
#'   country_code = "KEN",
#'   country_shape = country_sf,
#'   pop_raster = pop_raster,
#'   ur_raster = urban_raster,
#'   adm2_shape = adm2_sf,
#'   cell_size = 5000,
#'   output_dir = "03_outputs/3a_model_outputs"
#' )
#'}
#'
#' @export
generate_predictor_data <- function(country_code,
                                    country_shape,
                                    pop_raster,
                                    ur_raster,
                                    adm2_shape,
                                    cell_size = 5000,
                                    ignore_cache = FALSE,
                                    output_dir = "03_outputs/3a_model_outputs") {

  # Create lowercase country code
  country_code <- tolower(country_code)


  # Construct the predictors data path
  predictor_data_path <- file.path(
    output_dir,
    glue::glue("{country_code}_predictor_data_urban.rds")
  )

  # Check if predictors data file exists
  if (!!ignore_cache || !file.exists(predictor_data_path)) {

    # Create predictors data
    predictors_data <- create_prediction_data(
      country_shape, pop_raster, ur_raster,
      adm2_shape, cell_size = cell_size
    )

    # Save predictors data
    saveRDS(predictors_data, file = predictor_data_path)

    cli::cli_alert_success(
      "Predictors data created and saved at {predictor_data_path}")
  } else {

    # Notify user about cached predictors
    cli::cli_process_start(
      msg = "Importing cached predictors data...",
      msg_done = "Successfully imported cached predictors data."
    )

    # Load cached predictors data
    predictors_data <- readRDS(predictor_data_path)

    cli::cli_process_done()
  }

  return(predictors_data)
}


#' Compute Covariance Matrix for Spatial Model
#'
#' This function computes a block covariance matrix for a bivariate spatial
#' model with age-structured parameters.
#'
#' @param gamma Correlation parameter between the two spatial processes
#' @param sigma2 Variance parameter for the spatial processes
#' @param phi Range parameter for the spatial correlation
#' @param u_dist Distance matrix between locations
#' @param n_x Number of spatial locations
#' @param age_param_data List containing age-structured parameters:
#'   \itemize{
#'     \item b1: Vector of age parameters for first process
#'     \item b2: Vector of age parameters for second process
#'     \item c: Vector of cross-process age parameters
#'   }
#' @param tau2_1 Variance parameter for first process (default = 1)
#' @param tau2_2 Variance parameter for second process (default = 1)
#'
#' @return A sparse symmetric matrix of dimension 2n_x × 2n_x
#'
#' @export
compute_cov <- function(gamma, sigma2, phi, u_dist,
                        n_x, tau2_1 = 1, tau2_2 = 1,
                        age_param_data) {

  sigma_s <- sigma2 * exp(-u_dist / phi)
  m <- matrix(NA, 2 * n_x, 2 * n_x)

  m[1:n_x, 1:n_x] <- sigma_s + diag(age_param_data$b1) * tau2_1

  m[(n_x + 1):(2 * n_x), 1:n_x] <-
    m[1:n_x, (n_x + 1):(2 * n_x)] <-
    gamma * sigma_s + diag(age_param_data$c) * sqrt(tau2_1 * tau2_2)

  m[(n_x + 1):(2 * n_x), (n_x + 1):(2 * n_x)] <-
    (gamma^2) * sigma_s + diag(age_param_data$b2) * tau2_2

  m
}


#' Log-Likelihood Function for Spatial Model
#'
#' Computes the log-likelihood for a spatial statistical model with a covariance
#' structure determined by parameters including spatial decay and variance.
#'
#' @param par A numeric vector of parameters to estimate. The vector contains:
#'   \itemize{
#'     \item \code{par[1:p1]}: Coefficients for fixed effects in dataset 1
#'     (\eqn{\beta_1}).
#'     \item \code{par[(p1 + 1):(p1 + p2)]}: Coefficients for fixed effects in
#'     dataset 2 (\eqn{\beta_2}).
#'     \item \code{par[p1 + p2 + 1]}: Spatial decay parameter (\eqn{\gamma}).
#'     \item \code{par[p1 + p2 + 2]}: Log of the variance parameter
#'     (\eqn{\sigma^2}).
#'     \item \code{par[p1 + p2 + 3]}: Log of the range parameter (\eqn{\phi}).
#'   }
#' @param p1 An integer. The number of fixed-effect parameters in dataset 1.
#' @param p2 An integer. The number of fixed-effect parameters in dataset 2.
#' @param d1 A numeric matrix. Design matrix for dataset 1 used to model the
#'    mean structure.
#' @param d2 A numeric matrix. Design matrix for dataset 2 used to model the
#'    mean structure.
#' @param y A numeric vector. Observed response variable, including both datasets.
#' @param u_dist A numeric matrix. Distance matrix for spatial locations.
#' @param n_x An integer. The number of unique spatial locations.
#' @param age_param_data A numeric matrix or vector. Additional parameters
#'    specific to age-based modeling.
#' @param tau2_1 Variance parameter for first process (default = 1)
#' @param tau2_2 Variance parameter for second process (default = 1)
#'
#' @return A numeric scalar. The computed log-likelihood value.
#'
#' @details
#' The log-likelihood is computed as:
#' \deqn{
#' -0.5 \left[ \log(\det(M)) + (y - \mu)^T M^{-1} (y - \mu) \right]
#' }
#' where:
#' \itemize{
#'   \item \eqn{M} is the covariance matrix, computed using \code{compute_cov}.
#'   \item \eqn{\mu} is the mean structure, determined by the design matrices
#'    \code{d1}, \code{d2} and coefficients \eqn{\beta_1, \beta_2}.
#' }
#'
#' The covariance matrix \eqn{M} is computed using spatial parameters
#'  (\eqn{\gamma, \sigma^2, \phi}) and the distance matrix \code{u_dist}.
#'
#' @note
#' This function requires a helper function, \code{compute_cov}, to compute
#'  the covariance matrix based on spatial parameters.
#'
#' @export
log_lik <- function(par, p1, p2, d1, d2, y, u_dist, n_x, tau2_1 = 1, tau2_2 = 1,
                    age_param_data) {
  beta1 <- par[1:p1]
  beta2 <- par[(p1 + 1):(p1 + p2)]
  gamma <- par[p1 + p2 + 1]
  sigma2 <- exp(par[p1 + p2 + 2])
  phi <- exp(par[p1 + p2 + 3])

  mu1 <- as.numeric(d1 %*% beta1)
  mu2 <- as.numeric(d2 %*% beta2)
  mu <- c(mu1, mu2)

  m <- compute_cov(gamma, sigma2, phi, u_dist, n_x,
                   tau2_1, tau2_2, age_param_data)
  m_inv <- chol2inv(chol(m))

  -0.5 * as.numeric(determinant(m)$modulus +
                      t(y - mu) %*% m_inv %*% (y - mu))
}

#' Extract Beta Parameters from Model Output
#'
#' This function extracts beta coefficients from a model parameter object,
#' separating them into beta1 and beta2 components.
#'
#' @param params_result A model parameter object containing parameter estimates
#' @param params A character vector specifying parameter names, defaults to
#'   c("gamma", "log_sigma2", "log_phi", "log_tau1")
#' @return A list with two components:
#'   \itemize{
#'     \item beta1: First set of beta coefficients
#'     \item beta2: Second set of beta coefficients
#'   }
#'
#' @details
#' The function assumes the parameter vector contains beta coefficients
#' followed by other model parameters. It splits the betas into two equal
#' groups after removing the last 4 parameters.
#' @export
extract_betas <- function(params_result,
                          params = c("gamma", "log_sigma2",
                                     "log_phi", "log_tau1")) {

  params_length <- length(params)
  par_start <- as.numeric(params_result$par)
  par_length <- length(par_start)
  beta_length <- par_length - 4
  beta1_locend <- (beta_length/2)
  beta2_locstart <- beta1_locend + 1
  beta2_locend <- beta_length

  list(
    beta1 = par_start[1:beta1_locend],
    beta2 = par_start[beta2_locstart:beta2_locend]
  )
}

#' Create Prediction Data
#'
#' @param country_shape sf object containing country boundary shapefile
#' @param pop_raster terra raster object containing population density data
#' @param ur_raster terra raster object containing urban/rural classification
#' @param adm2_shape sf object containing admin level 2 boundaries
#' @param cell_size numeric grid cell size in meters (default 5000)
#'
#' @return data.frame containing processed grid data with predictors
#' @export
create_prediction_data <- function(country_shape, pop_raster, ur_raster,
                                   adm2_shape, cell_size = 5000) {

  # Process shapefiles and grids -----------------------------------------------

  cli::cli_process_start(
    msg = "Processing shapefiles and grids...",
    msg_done = "Processed shapefiles and grids.")

  grid_polygons <- sf::st_make_grid(
    sf::st_transform(
      country_shape, crs = 3857),
    cellsize = cell_size,
    square = TRUE) |>
    sf::st_intersection(
      sf::st_transform(country_shape, crs = 3857)) |>
    #  correct small overlaps by applying a small buffer
    sf::st_buffer(dist = 1e-9)

  # Generate country_grid points from grid_polygons
  country_grid <- sf::st_centroid(grid_polygons) |>
    sf::st_coordinates() |>
    as.data.frame() |>
    setNames(c("web_x", "web_y"))

  cli::cli_process_done()

  # Process pop raster data ----------------------------------------------------

  cli::cli_process_start(
    msg = "Processing population raster...",
    msg_done = "Processed population raster.")

  pop_raster_dens <- pop_raster |>
    terra::project("EPSG:3857")

  cli::cli_process_done()

  # Process urban/rural classification --------------------------------------

  cli::cli_process_start(
    msg = "Processing urban-rural raster...",
    msg_done = "Processed urban-rural raster.")

  ur_raster_web <- ur_raster |>
    terra::project("EPSG:3857")

  cli::cli_process_done()

  # Extract and process grid data ----------------------------------------------

  cli::cli_process_start(
    msg = "Extracting data from rasters onto grid...",
    msg_done = "Extracted data from rasters onto grid.")

  # Use exact_extract to calculate total population per grid cell
  pop_values <- exactextractr::exact_extract(
    pop_raster_dens, grid_polygons, fun = 'sum',
    max_cells_in_memory = 3e+08,  progress = FALSE)

  urb_values <- terra::extract(
    ur_raster_web, country_grid |>
      terra::vect(geom = c("web_x", "web_y"),
                  crs = "EPSG:3857"))[[2]] |>
    tidyr::replace_na(0)

  cli::cli_process_done()

  # Create predictors dataframe ------------------------------------------------

  cli::cli_process_start(
    msg = "Creating grided data with predictors...",
    msg_done = "Created grided data with predictors.")

  predictors <- data.frame(
    web_x = country_grid[, 1],
    web_y = country_grid[, 2],
    pop = pop_values,
    urban = urb_values

  ) |>
    dplyr::mutate(
      lat = web_y,
      lon = web_x
    ) |>
    sf::st_as_sf(
      coords = c(x = "lon", y = "lat"),
      crs = 3857
    ) |>
    sf::st_transform(crs = 3857) |>
    sf::st_join(sf::st_transform(adm2_shape, crs = 3857)) |>
    dplyr::select(
      country, region, district,
      web_x, web_y, urban, pop
    )

  # Assign unmatched points to the nearest boundary
  predictors <- predictors |>
    dplyr::filter(is.na(country)) |>
    dplyr::select(web_x, web_y, pop, urban) |>
    sf::st_join(
      sf::st_transform(adm2_shape,
                       crs = 3857),
      join = sf::st_nearest_feature) |>
    dplyr::bind_rows(
      predictors |>
        dplyr::filter(!is.na(country))
    ) |>
    sf::st_drop_geometry()

  cli::cli_process_done()

  return(predictors)
}

#' Generate or Load Cached Gamma Predictions
#'
#' This function generates gamma parameter predictions using spatial model
#' parameters and predictors data, or loads cached predictions if the file
#' already exists. It saves the predictions to a specified directory and
#' provides progress updates.
#'
#' @param country_code A string representing the country code (e.g., "KEN").
#' @param age_param A data frame or tibble containing the age parameter data.
#' @param model_params The fitted spatial model object (e.g.,
#'    `spat_model_param`).
#' @param predictor_data A data object containing the predictors data.
#' @param shapefile An `sf` object representing the country shape.
#' @param cell_size An integer specifying the cell size for prediction in
#'    meters (default is 5000).
#' @param n_sim An integer specifying the number of simulations for predictions
#'   (default is 10000).
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#' @param output_dir A string specifying the directory where the predictions
#'   file should be saved (default is "03_outputs/3a_model_outputs").
#' @param save_file A boolean to determine whetehr to save prediction or not.
#'   Default is FALSE as this will requre lots of space.
#'
#' @return A data object (`gamma_prediction`) containing the gamma parameter
#'   predictions.
#'
#' @examples
#'
#' # gamma_results <- generate_gamma_predictions(
#' #  country_code = "KEN",
#' #  age_param = age_param_data,
#' #  model_params = spat_model_param,
#' #  predictor_data = predictors_data,
#' #  shapefile = country_sf,
#' #  cell_size = 5000,
#' #  n_sim = 10000,
#' # output_dir = "03_outputs/3a_model_outputs"
#' #  )
#'
#' @export
generate_gamma_predictions <- function(country_code,
                                       age_param,
                                       model_params,
                                       predictor_data,
                                       shapefile,
                                       cell_size = 5000,
                                       n_sim = 10000,
                                       ignore_cache = FALSE,
                                       save_file = FALSE,
                                       output_dir =
                                         "03_outputs/3a_model_outputs") {

  # Create lowercase country code
  country_code <- tolower(country_code)

  # Construct the gamma prediction path
  gamma_prediction_path <- file.path(
    output_dir,
    glue::glue("{country_code}_gamma_prediction_urban.rds")
  )

  # Check if prediction file exists
  if (ignore_cache || !file.exists(gamma_prediction_path)) {

    # Generate gamma parameter predictions
    gamma_prediction <- predict_gamma_params(
      age_param = age_param,
      model_params = model_params,
      predictor_data = predictor_data,
      shapefile = shapefile,
      cell_size = cell_size,
      n_sim = n_sim
    )

    if (save_file) {

      # Save predictions
      saveRDS(gamma_prediction, file = gamma_prediction_path)
    }

    cli::cli_alert_success(
      "Gamma predictions generated and saved at {gamma_prediction_path}")
  } else {

    # Notify user about cached results
    cli::cli_process_start(
      msg = "Importing cached prediction results...",
      msg_done = "Successfully imported cached prediction results."
    )

    # Load cached prediction results
    gamma_prediction <- readRDS(gamma_prediction_path)

    cli::cli_process_done()
  }

  return(gamma_prediction)
}


#' Rasterize Spatial Data
#'
#' This function converts spatial data with x, y coordinates and a value field
#' into a raster using a specified resolution and CRS.
#'
#' @param x_coords Numeric vector of x-coordinates (e.g., longitude).
#' @param y_coords Numeric vector of y-coordinates (e.g., latitude).
#' @param values Numeric vector of values associated with each point.
#' @param resolution Numeric, the resolution of the raster in map units
#'                   (e.g., meters for EPSG:3857).
#' @param crs Character, the coordinate reference system in EPSG format
#'            (e.g., "EPSG:3857").
#' @param fun Function to aggregate values in cells (default is `mean`).
#'
#' @return A `terra::SpatRaster` object.
#' @examples
#' # rast <- rasterize_data(
#' #   predictor_data$web_x, predictor_data$web_y, pred_list$shape_hat,
#' #   resolution = 5000, crs = "EPSG:3857"
#' # )
#' @export
rasterize_data <- function(x_coords, y_coords, values,
                           resolution, crs, fun = mean) {
  # Combine inputs into a data frame
  spatial_data <- data.frame(x = x_coords, y = y_coords, value = values)

  # Create a SpatVector
  spat_vector <- terra::vect(spatial_data, geom = c("x", "y"), crs = crs)

  # Create a raster template with the correct extent and resolution
  raster_template <- terra::rast(spat_vector,
                                 resolution = resolution, crs = crs)

  # Rasterize the data
  terra::rasterize(spat_vector, raster_template, field = "value", fun = fun)
}

#' Generate and Save Raster Plot for Gamma Predictions
#'
#' This function creates rasters from prediction data, combines them into a
#' stack, and saves the faceted plot to a specified file.
#'
#' @param predictor_data A data frame containing `web_x` and `web_y`
#'   coordinates and associated prediction values.
#' @param pred_list A list containing predictions (`shape_hat`, `scale_hat`,
#'   `mean_age_pred`) for creating rasters.
#' @param country_code A string representing the lowercase country code,
#'   used for naming the output file.
#' @param output_dir A string specifying the directory where the plot should
#'   be saved.
#' @param file_name_suffix A string specifying the suffix for the file name
#'   (default is "gamma_prediction_rasters").
#' @param resolution An integer specifying the resolution of the plot in DPI
#'   (default is 300).
#' @param save_raster A logical input specifying whether to save output or not.
#'    Default is TRUE.
#' @param width Width of output plot. width is 2500.
#' @param height Width of output plot. width is 2000
#' @return The path to the saved raster plot.
#'
#' @examples
#' # raster_path <- generate_gamma_raster_plot(
#' #  predictor_data = predictor_data,
#' #  pred_list = pred_list,
#' #  country_code = "ken",
#' #  output_dir = "03_outputs/3b_visualizations"
#' # )
#'
#' @export
generate_gamma_raster_plot <- function(predictor_data,
                                       pred_list,
                                       country_code,
                                       output_dir,
                                       save_raster = TRUE,
                                       file_name_suffix =
                                         "gamma_prediction_rasters",
                                       width = 2500,
                                       height = 2000,
                                       resolution = 300) {

  rast_shape <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$shape_hat,
    resolution = 5000,
    crs = "EPSG:3857"
  )

  rast_scale <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$scale_hat,
    resolution = 5000,
    crs = "EPSG:3857"
  )

  rast_mean_age <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$mean_age_pred,
    resolution = 5000,
    crs = "EPSG:3857"
  )

  # Combine rasters into a stack
  raster_stack <- c(rast_shape, rast_scale, rast_mean_age)

  # Set names for visualization
  names(raster_stack) <- c("Shape Parameter",
                           "Scale Parameter",
                           "Mean Age Prediction")

  if (save_raster) {

    # Define output path
    output_file <- file.path(
      output_dir,
      glue::glue("{country_code}_{file_name_suffix}.png")
    )

    # Create output directory if it doesn't exist
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save the plot
    png(output_file, width = width, height = height, res = resolution)
    terra::plot(raster_stack,
                main = c("Shape Parameter",
                         "Scale Parameter",
                         "Mean Age Prediction"))
    dev.off()

    cli::cli_alert_success("Raster plot saved to {output_file}")

  }

}

#' Process Gamma Prediction Results
#'
#' This function processes gamma prediction results to calculate the mean age
#' predictions, scale, and shape parameters efficiently.
#'
#' @param gamma_prediction A list containing `scale_pred` and `shape_pred`
#'   matrices from the gamma prediction model.
#'
#' @return A list containing the following elements:
#'   - `mean_age_pred`: A vector of mean age predictions.
#'   - `scale_hat`: A vector of mean scale parameters.
#'   - `shape_hat`: A vector of mean shape parameters.
#'
#' @examples
#' # results <- process_gamma_predictions(
#' #            gamma_prediction = gamma_prediction)
#' # print(results$mean_age_pred)
#'
#' @export
process_gamma_predictions <- function(gamma_prediction) {
  # Extract scale and shape predictions
  scale_pred <- gamma_prediction$scale_pred
  shape_pred <- gamma_prediction$shape_pred

  # Compute mean age predictions
  mean_age_pred <- rowMeans(scale_pred * shape_pred)

  # Compute mean scale and shape parameters
  scale_hat <- rowMeans(scale_pred)
  shape_hat <- rowMeans(shape_pred)

  # Return results as a list
  return(list(
    mean_age_pred = mean_age_pred,
    scale_hat = scale_hat,
    shape_hat = shape_hat
  ))
}

#' Generate Age Population Data Across Intervals
#'
#' This function computes population estimates for different age intervals
#' based on gamma distributions, aggregates results across simulations,
#' and returns a summarized data frame with mean, lower, and upper quantiles.
#'
#' @param predictor_data A data frame containing columns: `country`, `region`,
#'   `district`, and `pop`.
#' @param scale_pred A matrix of scale predictions (rows correspond to data
#'   points, columns to simulations).
#' @param shape_pred A matrix of shape predictions (rows correspond to data
#'   points, columns to simulations).
#' @param age_range A numeric vector of length 2 specifying the minimum and
#'   maximum ages for the intervals (e.g., `c(0, 100)`).
#' @param interval A numeric value specifying the size of each age interval.
#' @param country_code A string representing the lowercase country code,
#'   used for naming the output file.
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#' @param output_dir A string specifying the directory where the output file
#'   should be saved.
#' @param n_cores An integer specifying the number of cores for parallel
#'   processing (default is `parallel::detectCores() - 2`).
#'
#' @return A data frame with population estimates and quantiles across age
#'   intervals.
#'
#' @examples
#' # final_data <- generate_age_pop_table(
#' #  predictor_data = predictor_data,
#' #  scale_pred = scale_pred,
#' #  shape_pred = shape_pred,
#' #  age_range = c(0, 100),
#' #  interval = 5,
#' #  country_code = "ken",
#' #  output_dir = "03_outputs/3a_model_outputs"
#' # )
#'
#' @export
generate_age_pop_table <- function(predictor_data,
                                   scale_pred,
                                   shape_pred,
                                   age_range = c(0, 100),
                                   interval = 5,
                                   country_code,
                                   ignore_cache = FALSE,
                                   output_dir,
                                   n_cores = parallel::detectCores() - 2) {

  # Construct the output file path dynamically
  output_path <- file.path(
    output_dir,
    glue::glue(
      "{country_code}_age_tables_pop_",
      "{age_range[1]}_{age_range[2]}yrs_by_{interval}yrs.rds")
  )

  # Check if prediction file exists
  if (!ignore_cache && file.exists(output_path)) {
    message("Loading cached data from ", output_path)
    return(readRDS(output_path))
  }

  # Ensure the output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Generate limslow and limsup based on age_range and interval
  limslow <- seq(age_range[1], age_range[2], interval)
  limsup <- seq(age_range[1] + interval, age_range[2] + interval, interval)

  n_sim <-  ncol(shape_pred)


  # Notify user about cached results
  cli::cli_process_start(
    msg = "Setting up parallelisation for pgamma simulation...",
    msg_done = "Successfully set up parallelisation for pgamma simulation"
  )

  # Start an asynchronous backend with progress tracking
  backend <- parabar::start_backend(
    cores = n_cores, # auto choose clustertype
    cluster_type = ifelse(Sys.info()["sysname"] == "Windows",
                          "psock", "fork"),
    backend_type = "async")

  # Configure the progress bar
  parabar::configure_bar(type = "modern",
                         format = "[:bar] :percent")

  # Export required variables to the backend
  parabar::export(backend,
                  variables = c("limsup", "limslow", "n_sim",
                                "scale_pred", "shape_pred",
                                "predictor_data"),
                  environment = environment())


  cli::cli_process_done()

  # Notify user about cached results
  cli::cli_process_start(
    msg = "Parallelising pgamma simulation...",
    msg_done = "Successfully parallelised pgamma simulation"
  )

  # Run tasks in parallel with progress tracking
  all_results <- parabar::par_lapply(
    backend,
    x = seq_len(length(limsup) - 1),
    fun = function(runnum) {
      # Compute age class proportions for this interval
      prop_age_pred <- lapply(
        1:n_sim,
        function(i) {
          pgamma(
            limsup[runnum], scale = scale_pred[, i],
            shape = shape_pred[, i]) -
            pgamma(
              limslow[runnum], scale = scale_pred[, i],
              shape = shape_pred[, i])
        }
      )

      # Aggregate across simulations
      prop_age_pred_binded <- do.call(cbind, prop_age_pred)
      mean_prop_age <- rowMeans(prop_age_pred_binded)

      # Calculate upper and lower quantiles for mean_prop_age
      quantiles_mean_prop_age <- matrixStats::rowQuantiles(
        prop_age_pred_binded,
        probs = c(0.025, 0.975)
      )

      # Build data frame with population and quantiles
      age_pop_class <-
        data.frame(
          country = predictor_data$country,
          region = predictor_data$region,
          district = predictor_data$district,
          pop = predictor_data$pop
        ) |>
        dplyr::mutate(
          pop = mean_prop_age * pop,
          lower_quantile = quantiles_mean_prop_age[, 1] * pop,
          upper_quantile = quantiles_mean_prop_age[, 2] * pop
        ) |>
        dplyr::group_by(country, region, district) |>
        dplyr::summarize(
          dplyr::across(
            dplyr::everything(),
            sum,
            .names = "{.col}"),
          .groups = "drop") |>
        dplyr::rename_with(
          ~ c(
            paste0(limslow[runnum], "_",
                   limsup[runnum], "y_mean"),
            paste0(limslow[runnum], "_",
                   limsup[runnum], "y_low_interval_2.5%"),
            paste0(limslow[runnum], "_",
                   limsup[runnum], "y_upper_interval_97.5%")
          ),
          c("pop", "lower_quantile", "upper_quantile")
        ) |>
        as.data.frame()

      age_pop_class

    }
  );
  parabar::clear(backend);  # Clear the backend
  parabar::stop_backend(backend)  # Stop the backend

  cli::cli_process_done()

  # Combine all interval results into the final data.frame
  final_df <- Reduce(
    function(df1, df2) dplyr::full_join(
      df1, df2,
      by = c("country", "region", "district")),
    all_results)

  # Save the final data frame
  saveRDS(final_df, file = output_path)

  cli::cli_alert_success(
    "Final age population data saved to {output_path}")

  return(final_df)
}

#' Generate and Save Age Pyramid Plot
#'
#' This function processes an input dataset to compute age distribution,
#' generates an age pyramid plot by region, and saves the plot to a specified
#' directory.
#'
#' @param dataset A data frame containing population data, with columns for
#'   `country`, `region`, `district`, and columns ending with "mean".
#' @param country_code A string representing the country code (e.g., "ken").
#' @param output_dir A string specifying the directory where the plot should
#'  be saved.
#' @param line_color A string specifying the color of the plot's lines. Default
#'   is `"#67000d"`.
#' @param fill_high A string specifying the fill color for high values. Default
#'   is `"#fee0d2"`.
#' @param fill_low A string specifying the fill color for low values. Default
#'   is `"#a50f15"`
#' @return The file path of the saved plot.
#'
#' @examples
#' # generate_age_pyramid_plot(
#' #  dataset = results,
#' #  country_code = "ken",
#' #  output_dir = "03_outputs/3b_figures"
#' # )
#'
#' @export
generate_age_pyramid_plot <- function(dataset,
                                      country_code,
                                      output_dir,
                                      line_color = "#67000d",
                                      fill_high = "#fee0d2",
                                      fill_low = "#a50f15") {

  # Process the dataset for age structure
  age_struct <- dataset |>
    dplyr::select(country, region, district,
                  dplyr::ends_with("mean")) |>
    tidyr::pivot_longer(
      cols = -c(country, region,
                district),
      names_to = "age_group"
    ) |>
    dplyr::mutate(
      age_group = stringr::str_remove(age_group, "_mean"),
      age_group = stringr::str_replace(age_group, "_", "-"),
      region = stringr::str_to_title(region),
      region = stringr::str_remove(region, " County")
    ) |>
    dplyr::group_by(country, region, age_group) |>
    dplyr::summarise(
      tot_pop = sum(value, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    dplyr::mutate(
      age_group = factor(age_group, levels = c(
        "0-5y", "5-10y", "10-15y", "15-20y",
        "20-25y", "25-30y", "30-35y", "35-40y",
        "40-45y", "45-50y", "50-55y", "55-60y",
        "60-65y", "65-70y", "70-75y", "75-80y",
        "80-85y", "85-90y", "90-95y", "95-100y"
      ))
    ) |>
    dplyr::filter(!is.na(age_group))

  # get total pop for country
  total_pop <- sum(age_struct$tot_pop, na.rm = TRUE) |>
    round() |> format(big.mark = ",")

  # Generate the plot
  plot <- age_struct |>
    ggplot2::ggplot(
      ggplot2::aes(x = age_group, y = tot_pop, fill = as.numeric(age_group))
    ) +
    ggplot2::geom_bar(stat = "identity",
                      color = line_color, linewidth = 0.4,
                      position = "identity", width = 1) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_x_discrete(
      breaks = levels(
        age_struct$age_group)[seq(1, length(levels(
          age_struct$age_group)), by = 2)]
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~region) +
    ggplot2::labs(
      title = glue::glue(
        "Age Pyramid by Region",
        ", {stringr::str_to_title(age_struct$country[1])} (N = ",
        "{total_pop}) \n"),
      x = "Age Group \n",
      y = "\n Population",
      fill = "Region"
    ) +
    # ggplot2::scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
    ggplot2::scale_fill_gradient(high = fill_high,
                                 low = fill_low) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      # axis.text.x = ggplot2::element_text(angle = 65, hjust = 0.80),
      plot.title = ggplot2::element_text(size = 15, face = "bold")
    )

  # Define the output file path
  output_file <- file.path(
    output_dir, glue::glue("{country_code}_age_pyramid.png"))

  # Save the plot
  ggplot2::ggsave(
    output_file, plot = plot, width = 14,
    height = 14, dpi = 500, scale = 1, device = "png"
  )

  cli::cli_alert_success("Age pyramid plot saved to {output_file}")

  return(plot)
}

#' Run Country-Specific Spatial Modeling Workflow
#'
#' This function runs the entire spatial modeling workflow for a given country
#' code. It processes DHS data, fits a spatial model, generates predictions,
#' creates population tables, and produces raster outputs. The function is
#' modular and can be reused for different countries with minimal adjustments.
#'
#' @param country_code Character. The ISO3 country code (e.g., "TZA" for
#'    Tanzania).
#' @param dhs_data_path Character. Path to DHS data. Default:
#'     "01_data/1a_dhs_data/processed/dhs_pr_records_combined.rds".
#' @param shape_path Character. Path to DHS data. Default:
#'     "01_data/1c_shapefiles/district_shape.gpkg".
#' @param pop_raster_path Character. Path to DHS data. Default:
#'     "01_data/1b_rasters/worldpop_100m".
#' @param ur_raster_path Character. Path to urban-rural extent data. Default:
#'     "01_data/1b_rasters/urban_extent/afurextent.asc".
#' @param model_output_path Character. Path to save model outputs. Default:
#'    "03_outputs/3a_model_outputs".
#' @param plot_output_path Character. Path to save visualization outputs.
#'    Default: "03_outputs/3b_visualizations".
#' @param raster_output_path Character. Path to save raster outputs. Default:
#'    "03_outputs/3c_raster_outputs".
#' @param table_output_path Character. Path to save table outputs. Default:
#'    "03_outputs/3c_table_outputs".
#' @param cell_size Numeric. Cell size (meters) for predictor generation.
#'    Default: 5000.
#' @param n_sim Numeric. Number of simulations for gamma predictions.
#'    Default: 10000.
#' @param ignore_cache Logical. Whether to ignore cached data. Default: FALSE.
#' @param age_range_table Numeric vector. Age range for table generation.
#'   Default: c(0, 100).
#' @param interval_table Numeric. Age interval for table generation.
#'   Default: 5.
#' @param age_range_raster Numeric vector. Age range for raster generation.
#'   Default: c(0, 50).
#' @param interval_raster Numeric. Age interval for raster generation.
#'   Default: 5.
#' @param return_prop Logical. Whether to return proportion in raster output.
#'   Default: TRUE.
#' @param scale_outcome Character. Outcome variable for scaling. Default:
#'   "log_scale".
#' @param shape_outcome Character. Outcome variable for shaping. Default:
#'   "log_shape".
#' @param covariates Character. Covariates for the spatial model. Default:
#'   "urban".
#' @param cpp_script_name Character. Path to the C++ script for the model.
#'   Default: "02_scripts/model_optim".
#' @param return_results Logical. Whether to return the function outputs or not.
#'   Default: TRUE.
#'
#' @return A list containing the following components:
#'   \item{spat_model_param}{Fitted spatial model parameters.}
#'   \item{predictor_data}{Predictor dataset used for modeling.}
#'   \item{gamma_prediction}{Generated gamma predictions.}
#'   \item{pred_list}{Processed gamma prediction results.}
#'   \item{final_data}{Age-population table data.}
#'   \item{age_pop_raster}{Generated gridded age-population raster.}
#'
#' @examples
#' # Run the model for Tanzania with default parameters
#' # results <- run_country_model(country_code = "TZA")
#'
#' # Access specific components from the results
#' # spat_model_param <- results$spat_model_param
#' # age_pop_raster <- results$age_pop_raster
#'
#' # Visualize the raster
#' # terra::plot(age_pop_raster)
#' @export
run_country_model <- function(
    country_code,
    dhs_data_path =
      "01_data/1a_dhs_data/processed/dhs_pr_records_combined.rds",
    shape_path = "01_data/1c_shapefiles/district_shape.gpkg",
    pop_raster_path =  "01_data/1b_rasters/worldpop_100m",
    ur_raster_path = "01_data/1b_rasters/urban_extent/afurextent.asc",
    model_output_path = "03_outputs/3a_model_outputs",
    plot_output_path = "03_outputs/3b_visualizations",
    raster_output_path = "03_outputs/3c_raster_outputs",
    table_output_path = "03_outputs/3c_table_outputs",
    cell_size = 5000,
    n_sim = 5000,
    ignore_cache = FALSE,
    age_range_table = c(0, 100),
    interval_table = 5,
    age_range_raster = c(0, 4),
    interval_raster = 1,
    return_prop = TRUE,
    scale_outcome = "log_scale",
    shape_outcome = "log_shape",
    covariates = "urban",
    cpp_script_name = "02_scripts/model",
    return_results = FALSE
) {

  country_code_lw <- tolower(country_code)

  if (!file.exists(dhs_data_path)) {
    stop(glue::glue(
      "DHS data file not found at {dhs_data_path}"))
  }

  age_param_data <- readRDS(dhs_data_path)$age_param_data |>
    dplyr::filter(country_code_iso3 == country_code)

  # Get regional shapefile
  cntry_code <- country_code
  adm2_shape <- sf::read_sf(shape_path) |>
    dplyr::filter(country_code %in% cntry_code
    ) |>
    sf::st_transform(crs = 3857)

  # Make country shapefile from adm2
  country_shape <- sf::st_union(adm2_shape)

  country_name <- adm2_shape$country[1]
  country_name_clr <- stringr::str_to_title(country_name) |>
    crayon::blue()

  # Get population raster
  pop_raster_path <- here::here(
    pop_raster_path,
    glue::glue("{country_code_lw}_ppp_2020_constrained.tif")
  )

  if (!file.exists(pop_raster_path)) {
    stop(glue::glue(
      "Population raster not found for country code ",
      "{country_name_clr} (country_code) at {pop_raster_path}"))
  }

  pop_raster <- terra::rast(pop_raster_path)

  if (!file.exists(ur_raster_path)) {
    stop(glue::glue("Urban extent raster not found at {ur_raster_path}"))
  }

  ur_raster <- terra::rast(ur_raster_path) |>
    # Crop raster to pop extent
    terra::crop(terra::ext(pop_raster))

  # Fit Spatial Model (urban adjusted)
  cli::cli_h1(glue::glue("Fitting Spatial Model for {country_name_clr}"))

  spat_model_param <- run_spatial_model(
    country_code,
    age_param_data,
    scale_outcome = scale_outcome,
    shape_outcome = shape_outcome,
    covariates = covariates,
    cpp_script_name = cpp_script_name,
    output_dir = model_output_path,
    ignore_cache = ignore_cache
  )

  # Create predictor data
  cli::cli_h1(glue::glue("Creating Predictor Data for {country_name_clr}"))

  predictors_data <- generate_predictor_data(
    country_code,
    country_shape,
    pop_raster,
    ur_raster,
    adm2_shape,
    cell_size = cell_size,
    ignore_cache = ignore_cache,
    output_dir = model_output_path
  )

  # Run prediction
  cli::cli_h1(glue::glue(
    "Running Prediction for {country_name_clr}"))

  gamma_prediction <- generate_gamma_predictions(
    country_code,
    age_param_data,
    spat_model_param,
    predictors_data,
    adm2_shape,
    cell_size = cell_size,
    n_sim = n_sim,
    ignore_cache = ignore_cache,
    output_dir = model_output_path
  )

  # Prediction rasters
  cli::cli_h1(glue::glue(
    "Producing Prediction Rasters for {country_name_clr}"))

  # Process gamma prediction results
  pred_list <- process_gamma_predictions(gamma_prediction)

  # Show and save prediction raster
  generate_gamma_raster_plot(
    predictors_data,
    pred_list,
    country_code_lw,
    plot_output_path,
    save_raster = TRUE
  )

  # Create age-population tables
  cli::cli_h1(glue::glue(
    "Producing District-level Age-Population Tables for {country_name_clr}"))

  # pull scale and shape and
  # then drop gamma_prediction
  scale_pred = gamma_prediction$scale_pred
  shape_pred = gamma_prediction$shape_pred
  rm(gamma_prediction)

  final_age_pop_table <- generate_age_pop_table(
    predictor_data = predictors_data,
    scale_pred = scale_pred,
    shape_pred = shape_pred,
    country_code = country_code_lw,
    age_range = age_range_table,
    interval = interval_table,
    ignore_cache = ignore_cache,
    output_dir = table_output_path
  )

  # Create age-population tables
  cli::cli_h1(glue::glue(
    "Producing Regional-level Age-pyramid for {country_name_clr}"))

  generate_age_pyramid_plot(
    dataset = final_age_pop_table,
    country_code = country_code_lw,
    output_dir = plot_output_path
  )

  # Create age-population raster
  # cli::cli_h1(glue::glue(
  #   "Producing Gridded Age-Population Raster for {country_name_clr}"))

  # age_pop_raster <- generate_age_pop_raster(
  #   predictor_data = predictors_data,
  #   scale_pred = scale_pred,
  #   shape_pred = shape_pred,
  #   age_range = age_range_raster,
  #   interval = interval_raster,
  #   return_prop = return_prop,
  #   country_code = country_code_lw,
  #   output_dir = raster_output_path
  # )


  # Optionally, return results
  if (return_results) {
    return(list(
      spat_model_param = spat_model_param,
      predictor_data = predictors_data,
      gamma_prediction = gamma_prediction,
      pred_list = pred_list,
      final_age_pop_table = final_age_pop_table
      # age_pop_raster = age_pop_raster
    ))
  }
}

#' Run Models for Multiple Countries and Log Results
#'
#' Runs a model for each country in the provided list, logs the status,
#' duration, and any errors, and merges with an existing log before saving.
#'
#' @param country_codes A character vector of country codes to process.
#' @param dhs_data_path Character. Path to DHS data. Default:
#'     "01_data/1a_dhs_data/processed/dhs_pr_records_combined.rds".
#' @param shape_path Character. Path to DHS data. Default:
#'     "01_data/1c_shapefiles/district_shape.gpkg".
#' @param pop_raster_path Character. Path to DHS data. Default:
#'     "01_data/1b_rasters/worldpop_100m".
#' @param ur_raster_path Character. Path to urban-rural extent data. Default:
#'     "01_data/1b_rasters/urban_extent/afurextent.asc".
#' @param log_path A character string specifying the file path for the log file.
#' @param ignore_cache A boolean input which is set to determine whether
#'  to ignore the existing cache and write over it. Default is set to FALSE.
#' @return None. The function updates the log file at `log_path`.
#' @examples
#' # run_models_with_logging(
#' #  country_codes = c("KEN", "UGA"),
#' #  log_path = "03_outputs/3a_model_outputs/modelling_log.rds"
#' # )
#' @export
run_models_with_logging <- function(
    country_codes,
    dhs_data_path =
      "01_data/1a_dhs_data/processed/dhs_pr_records_combined.rds",
    shape_path = "01_data/1c_shapefiles/district_shape.gpkg",
    pop_raster_path =  "01_data/1b_rasters/worldpop_100m",
    ur_raster_path = "01_data/1b_rasters/urban_extent/afurextent.asc",
    log_path = "03_outputs/3a_model_outputs/modelling_log.rds",
    ignore_cache = FALSE) {

  log_data <- list()

  # Import historical log
  if (file.exists(log_path)) {
    hist_log <- readRDS(log_path)
  } else {hist_log <- list()}

  # Loop through country codes
  for (country in country_codes) {
    start_time <- Sys.time() # Record start time

    tryCatch(
      {
        # Run the model
        run_country_model(
          dhs_data_path = dhs_data_path,
          shape_path = shape_path,
          pop_raster_path = pop_raster_path,
          ur_raster_path = ur_raster_path,
          country_code = country,
          ignore_cache = ignore_cache
        )

        # Record success
        log_data[[country]] <- list(
          status = "Success",
          start_time = start_time,
          end_time = Sys.time(),
          duration = difftime(Sys.time(), start_time, units = "mins"),
          error_message = NA
        )
      },
      error = function(e) {
        # Record error
        log_data[[country]] <- list(
          status = "Error",
          start_time = start_time,
          end_time = Sys.time(),
          duration = difftime(Sys.time(), start_time, units = "mins"),
          error_message = e$message
        )

        # Print error message
        cli::cli_alert_danger(
          glue::glue("Error running model for {country}: {e$message}")
        )
      }
    )
  }

  # Convert log_data to a data frame
  log_data_df <- do.call(rbind, lapply(names(log_data), function(country) {
    data.frame(
      Country = country,
      Status = log_data[[country]]$status,
      Start_Time = as.character(log_data[[country]]$start_time),
      End_Time = as.character(log_data[[country]]$end_time),
      Duration_Minutes = as.numeric(log_data[[country]]$duration),
      Error_Message = log_data[[country]]$error_message
    )
  }))

  # Merge historical and new logs, then save
  logs <- dplyr::bind_rows(hist_log, log_data_df) |>
    dplyr::mutate(
      Error_Message = ifelse(is.na(Error_Message), "None", Error_Message)
    )
  saveRDS(logs, log_path)
}

#' Process Final Population Data
#'
#' Reads population data from RDS files in a specified directory, processes the
#' data to generate age-group-based population summaries at different
#' administrative levels (Country, Region, District), and writes the results
#' to an Excel file with separate sheets for each level.
#'
#' @param input_dir A character string specifying the directory containing RDS
#'  files.
#' @param output_file A character string specifying the output Excel file path.
#' @return None. The function writes an Excel file to the specified output
#'  location.
#' @examples
#' # process_final_population_data(
#' # input_dir = "03_outputs/3c_table_outputs",
#' # output_file = "03_outputs/afro_population_2020.xlsx"
#' #)
#' @export
process_final_population_data <- function(
    input_dir = "03_outputs/3c_table_outputs",
    output_file = "03_outputs/3d_compiled_results/age_pop_denom_2020.xlsx") {

  # Helper function to process individual files
  process_file <- function(file) {
    readRDS(file) |>
      dplyr::select(country, region, district,
                    dplyr::ends_with("mean")) |>
      tidyr::pivot_longer(
        cols = -c(country, region, district),
        names_to = "age_group"
      ) |>
      dplyr::mutate(
        age_group = stringr::str_remove(age_group, "_mean") |>
          stringr::str_replace("_", "-"),
        age_group = factor(age_group, levels = c(
          "0-5y", "5-10y", "10-15y", "15-20y", "20-25y", "25-30y",
          "30-35y", "35-40y", "40-45y", "45-50y", "50-55y", "55-60y",
          "60-65y", "65-70y", "70-75y", "75-80y", "80-85y", "85-90y",
          "90-95y", "95-100y"
        ))
      ) |>
      dplyr::filter(!is.na(age_group))
  }

  # Read and process all files
  files <- list.files(input_dir, full.names = TRUE)
  pop_df <- lapply(files, process_file) |> dplyr::bind_rows()

  # Summarize data at different levels
  summarize_by <- function(data, group_vars) {
    data |>
      dplyr::group_by(dplyr::across(all_of(group_vars))) |>
      dplyr::summarise(
        population = sum(value, na.rm = TRUE) |> round(),
        .groups = "drop"
      )
  }

  res_adm0 <- summarize_by(pop_df, c("country", "age_group"))
  res_adm1 <- summarize_by(pop_df, c("country", "region", "age_group"))
  res_adm2 <- summarize_by(pop_df, c("country", "region",
                                     "district", "age_group"))

  # Create Excel workbook and write data
  wb <- openxlsx::createWorkbook()

  add_sheet <- function(wb, sheet_name, data) {
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, data)
  }

  add_sheet(wb, "Country", res_adm0)
  add_sheet(wb, "Region", res_adm1)
  add_sheet(wb, "District", res_adm2)

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  cli::cli_alert_success(
    "Final age-structured population is saved to {output_file}.")

}

#' Extract Parameters and Optimization Details with Log-Likelihood
#'
#' Reads files matching the pattern "age_param_spatial_urban" in a specified
#' directory, extracts the country name, parameter values, and optimization
#' details, combines the results into a data frame, and optionally saves
#' the output to a file.
#'
#' @param dir_path A character string specifying the directory containing the
#'  files.
#' @param output_file A character string specifying the path to save the output
#'                    data frame. If NULL, the output will not be saved.
#' @return A data frame with the extracted parameters, log-likelihood,
#'         and optimization details.
#' @examples
#' # params_df <- extract_age_param(
#' #  dir_path = "03_outputs/3a_model_outputs",
#' #  output_file = "03_outputs/age_parameters.csv"
#' # )
#' @export
extract_age_param <- function(
    dir_path = "03_outputs/3a_model_outputs",
    output_file = "03_outputs/3d_compiled_results/model_params.csv") {

  # List all relevant files
  files <- list.files(dir_path, full.names = TRUE)

  # Filter files matching the desired pattern
  param_files <- grep("age_param_spatial_urban", files, value = TRUE)

  # Initialize a list to store data frames
  param_list <- lapply(param_files, function(file) {
    param <- readRDS(file)

    # Extract parameters into a data frame
    data.frame(
      country = basename(file) |> substr(1, 3) |> toupper(),
      beta1 = param$par[1],
      beta2 = param$par[2],
      gamma = param$par[3],
      log_sigma2 = param$par[4],
      log_phi = param$par[5],
      log_tau1 = param$par[6],
      log_likelihood = param$objective,
      convergence = param$convergence,
      iterations = param$iterations,
      eval_function = param$evaluations["function"],
      eval_gradient = param$evaluations["gradient"],
      message = param$message
    )
  })

  # Combine all data frames into one
  params_df <- do.call(rbind, param_list)

  # remove row names
  rownames(params_df) <- NULL

  # Save to file if output_file is provided
  if (!is.null(output_file)) {
    write.csv(params_df, output_file, row.names = FALSE)
  }

  cli::cli_alert_success(
    "Model paramters extracted and saved to {output_file}.")

  return(params_df)
}



