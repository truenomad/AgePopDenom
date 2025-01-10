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

  # Section 5: Optimization ----------------------------------------------------

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
#'   for optimization (default is "02_scripts/model").
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
#'   cpp_script_name = "02_scripts/model",
#'   output_dir = "03_outputs/3a_model_outputs"
#' )
#'}
#' @export
run_spatial_model <- function(
    country_code,
    age_param_data,
    scale_outcome = "log_scale",
    shape_outcome = "log_shape",
    covariates = "urban",
    cpp_script_name = here::here("02_scripts", "model"),
    output_dir = here::here("03_outputs", "3a_model_outputs"),
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
#' predictors <- create_prediction_data(
#'   country_code = "KEN",
#'   country_shape = country_sf,
#'   pop_raster = pop_raster,
#'   ur_raster = urban_raster,
#'   adm2_shape = adm2_sf,
#'   cell_size = 5000,
#'   output_dir = "03_outputs/3a_model_outputs"
#' )
#' }
#'
#' @export
create_prediction_data <- function(country_code, country_shape, pop_raster,
                                   ur_raster, adm2_shape, cell_size = 5000,
                                   ignore_cache = FALSE,
                                   output_dir = here::here(
                                     "03_outputs", "3a_model_outputs"
                                   )) {
  # Check cache and import if it exists ------------------------------------------

  # Create lowercase country code
  country_code <- tolower(country_code)

  # Construct the predictors data path
  predictor_data_path <- file.path(
    output_dir,
    glue::glue("{country_code}_predictor_data_urban.rds")
  )

  # Check if predictors data file exists and return if cached
  if (!ignore_cache && file.exists(predictor_data_path)) {
    # Notify user about cached predictors
    cli::cli_process_start(
      msg = "Importing cached predictors data...",
      msg_done = "Successfully imported cached predictors data."
    )
    # Load cached predictors data
    predictors_data <- readRDS(predictor_data_path)
    cli::cli_process_done()
    return(predictors_data)
  }

  # Process shapefiles and grids -----------------------------------------------

  cli::cli_process_start(
    msg = "Processing shapefiles and grids...",
    msg_done = "Processed shapefiles and grids."
  )

  grid_polygons <- sf::st_make_grid(
    sf::st_transform(
      country_shape,
      crs = 3857
    ),
    cellsize = cell_size,
    square = TRUE
  ) |>
    sf::st_intersection(
      sf::st_transform(country_shape, crs = 3857)
    ) |>
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
    msg_done = "Processed population raster."
  )

  pop_raster_dens <- pop_raster |>
    terra::project("EPSG:3857")

  cli::cli_process_done()

  # Process urban/rural classification --------------------------------------

  cli::cli_process_start(
    msg = "Processing urban-rural raster...",
    msg_done = "Processed urban-rural raster."
  )

  ur_raster_web <- ur_raster |>
    terra::project("EPSG:3857")

  cli::cli_process_done()

  # Extract and process grid data ----------------------------------------------

  cli::cli_process_start(
    msg = "Extracting data from rasters onto grid...",
    msg_done = "Extracted data from rasters onto grid."
  )

  # Use exact_extract to calculate total population per grid cell
  pop_values <- exactextractr::exact_extract(
    pop_raster_dens, grid_polygons,
    fun = "sum",
    max_cells_in_memory = 3e+08, progress = FALSE
  )

  urb_values <- terra::extract(
    ur_raster_web, country_grid |>
      terra::vect(
        geom = c("web_x", "web_y"),
        crs = "EPSG:3857"
      )
  )[[2]] |>
    tidyr::replace_na(0)

  cli::cli_process_done()

  # Create predictors dataframe ------------------------------------------------

  cli::cli_process_start(
    msg = "Creating grided data with predictors...",
    msg_done = "Created grided data with predictors."
  )

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
  predictors_data <- predictors |>
    dplyr::filter(is.na(country)) |>
    dplyr::select(web_x, web_y, pop, urban) |>
    sf::st_join(
      sf::st_transform(adm2_shape,
                       crs = 3857
      ),
      join = sf::st_nearest_feature
    ) |>
    dplyr::bind_rows(
      predictors |>
        dplyr::filter(!is.na(country))
    ) |>
    sf::st_drop_geometry()

  cli::cli_process_done()

  # Save predictors data
  saveRDS(predictors_data, file = predictor_data_path)

  cli::cli_alert_success(
    "Predictors data created and saved at {predictor_data_path}"
  )

  return(predictors_data)
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
                                       output_dir = here::here(
                                         "03_outputs", "3a_model_outputs")) {

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

#' Run Country-Specific Spatial Modeling Workflow
#'
#' This function runs the entire spatial modeling workflow for a given country
#' code. It processes Survey data, fits a spatial model, generates predictions,
#' creates population tables, and produces raster outputs. The function is
#' modular and can be reused for different countries with minimal adjustments.
#'
#' @param country_code Character. The ISO3 country code (e.g., "TZA" for
#'    Tanzania).
#' @param survey_data_path Character. Path to Survey data. Default:
#'     "01_data/1a_survey_data/processed".
#' @param survey_data_suffix Character. Suffix for Survey data files. Default:
#'     "dhs_pr_records_combined.rds".
#' @param shape_path Character. Path to shapefile data. Default:
#'     "01_data/1c_shapefiles".
#' @param shape_suffix Character. Suffix for shapefile data. Default:
#'     "district_shape.gpkg".
#' @param pop_raster_path Character. Path to population raster data. Default:
#'     "01_data/1b_rasters/pop_raster".
#' @param pop_raster_suffix Character. Suffix for population raster files. Default:
#'   "_ppp_2020_constrained.tif".
#' @param ur_raster_path Character. Path to urban-rural extent data. Default:
#'     "01_data/1b_rasters/urban_extent/afurextent.asc".
#' @param ur_raster_suffix Character. Suffix for urban-rural raster. Default:
#'    "afurextent.asc".
#' @param model_output_path Character. Path to save model outputs. Default:
#'    "03_outputs/3a_model_outputs".
#' @param plot_output_path Character. Path to save visualization outputs.
#'    Default: "03_outputs/3b_visualizations".
#' @param raster_output_path Character. Path to save raster outputs. Default:
#'    "03_outputs/3c_raster_outputs".
#' @param table_output_path Character. Path to save table outputs. Default:
#'    "03_outputs/3c_table_outputs".
#' @param compiled_output_path Character. Path to save compiled results. Default:
#'    "03_outputs/3d_compiled_results".
#' @param excel_output_file A character string specifying the output Excel file path.
#'   Default is "03_outputs/3d_compiled_results/age_pop_denom_2020.xlsx" in the
#'   project directory.
#' @param cell_size Numeric. Cell size (meters) for predictor generation.
#'    Default: 5000.
#' @param n_sim Numeric. Number of simulations for gamma predictions.
#'    Default: 5000.
#' @param ignore_cache Logical. Whether to ignore cached data. Default: FALSE.
#' @param age_range_table Numeric vector. Age range for table generation.
#'   Default: c(0, 99).
#' @param interval_table Numeric. Age interval for table generation.
#'   Default: 1.
#' @param return_prop Logical. Whether to return proportion in raster output.
#'   Default: TRUE.
#' @param scale_outcome Character. Outcome variable for scaling. Default:
#'   "log_scale".
#' @param shape_outcome Character. Outcome variable for shaping. Default:
#'   "log_shape".
#' @param covariates Character. Covariates for the spatial model. Default:
#'   "urban".
#' @param cpp_script_name Character. Path to the C++ script for the model.
#'   Default: "02_scripts/model".
#' @param return_results Logical. Whether to return the function outputs or not.
#'   Default: FALSE.
#'
#' @return If return_results is TRUE, a list containing the following components:
#'   \item{spat_model_param}{Fitted spatial model parameters.}
#'   \item{predictor_data}{Predictor dataset used for modeling.}
#'   \item{gamma_prediction}{Generated gamma predictions.}
#'   \item{pred_list}{Processed gamma prediction results.}
#'   \item{final_age_pop_table}{Age-population table data.}
#'   \item{final_pop}{Compiled population data for all countries.}
#'   \item{all_mod_params}{Compiled model parameters for all countries.}
#'
#' @examples
#' # Run the model for Tanzania with default parameters
#' # results <- run_country_model(country_code = "TZA")
#'
#' # Access specific components from the results
#' # spat_model_param <- results$spat_model_param
#' # final_pop <- results$final_pop
#'
#' @export
run_country_model <- function(
    country_code,
    survey_data_path = here::here("01_data", "1a_survey_data", "processed"),
    survey_data_suffix = "dhs_pr_records_combined.rds",
    shape_path = here::here("01_data", "1c_shapefiles"),
    shape_suffix = "district_shape.gpkg",
    pop_raster_path = here::here("01_data", "1b_rasters", "pop_raster"),
    pop_raster_suffix = "_ppp_2020_constrained.tif",
    ur_raster_path = here::here("01_data", "1b_rasters",
                                "urban_extent", "afurextent.asc"),
    ur_raster_suffix = "afurextent.asc",
    model_output_path = here::here("03_outputs", "3a_model_outputs"),
    plot_output_path = here::here("03_outputs", "3b_visualizations"),
    raster_output_path = here::here("03_outputs", "3c_raster_outputs"),
    table_output_path = here::here("03_outputs", "3c_table_outputs"),
    compiled_output_path = here::here("03_outputs", "3d_compiled_results"),
    excel_output_file = here::here(
      "03_outputs", "3d_compiled_results",
      "age_pop_denom_compiled.xlsx"
    ),
    cell_size = 5000,
    n_sim = 5000,
    ignore_cache = FALSE,
    age_range_table = c(0, 99),
    interval_table = 1,
    return_prop = TRUE,
    scale_outcome = "log_scale",
    shape_outcome = "log_shape",
    covariates = "urban",
    cpp_script_name = here::here("02_scripts", "model"),
    return_results = FALSE
) {

  country_code_lw <- tolower(country_code)

  survey_data_path <- file.path(survey_data_path, survey_data_suffix)
  if (!file.exists(survey_data_path)) {
    stop(glue::glue(
      "Survey data file not found at {survey_data_path}"))
  }

  age_param_data <- readRDS(survey_data_path)$age_param_data |>
    dplyr::filter(country_code_iso3 == country_code)

  # Get regional shapefile
  cntry_code <- country_code
  shape_path <- file.path(shape_path, shape_suffix)
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
    glue::glue("{country_code_lw}{pop_raster_suffix}")
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

  predictors_data <- create_prediction_data(
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

  # Create age-population pyramid
  cli::cli_h1(glue::glue(
    "Producing Regional-level Age-pyramid for {country_name_clr}"))


  # set up the y axis label distance
  axis_by <- (age_range_table/interval_table)[2]/10

  generate_age_pyramid_plot(
    dataset = final_age_pop_table,
    country_code = country_code_lw,
    output_dir = plot_output_path,
    break_axis_by = axis_by
  )

  # Create age-population pyramid
  cli::cli_h1(glue::glue(
    "Compiling the model parameter data for all countries"))

  # Compile all the model param data for
  all_mod_params <- extract_age_param(
    dir_path = model_output_path,
    output_file = compiled_output_path
  )

  # Finalise population age structured data
  cli::cli_h1(glue::glue(
    "Compiling age-structured population data for all countries"))

  final_pop <- process_final_population_data(
    input_dir = table_output_path,
    excel_output_file = excel_output_file
  )

  # Optionally, return results
  if (return_results) {
    return(list(
      spat_model_param = spat_model_param,
      predictor_data = predictors_data,
      gamma_prediction = gamma_prediction,
      pred_list = pred_list,
      final_age_pop_table = final_age_pop_table,
      final_pop = final_pop,
      all_mod_params = all_mod_params
    ))
  }
}

#' Run Models for Multiple Countries and Log Results
#'
#' Runs a model for each country in the provided list, logs the status,
#' duration, and any errors, and merges with an existing log before saving.
#'
#' @param country_codes A character vector of country codes to process.
#' @param survey_data_path Character. Path to Survey data. Default:
#'     "01_data/1a_survey_data/processed".
#' @param survey_data_suffix Character. Suffix for Survey data files. Default:
#'     "dhs_pr_records_combined.rds".
#' @param shape_path Character. Path to shapefile data. Default:
#'     "01_data/1c_shapefiles".
#' @param shape_suffix Character. Suffix for shapefile data. Default:
#'     "district_shape.gpkg".
#' @param pop_raster_path Character. Path to population raster data. Default:
#'     "01_data/1b_rasters/pop_raster".
#' @param pop_raster_suffix Character. Suffix for population raster files. Default:
#'    "_ppp_2020_constrained.tif".
#' @param ur_raster_path Character. Path to urban-rural extent data. Default:
#'     "01_data/1b_rasters/urban_extent/afurextent.asc".
#' @param ur_raster_suffix Character. Suffix for urban-rural raster. Default:
#'    "afurextent.asc".
#' @param model_output_path Character. Path to save model outputs. Default:
#'    "03_outputs/3a_model_outputs".
#' @param plot_output_path Character. Path to save visualization outputs.
#'    Default: "03_outputs/3b_visualizations".
#' @param raster_output_path Character. Path to save raster outputs. Default:
#'    "03_outputs/3c_raster_outputs".
#' @param table_output_path Character. Path to save table outputs. Default:
#'    "03_outputs/3c_table_outputs".
#' @param compiled_output_path Character. Path to save compiled results. Default:
#'    "03_outputs/3d_compiled_results".
#' @param excel_output_file A character string specifying the output Excel file path.
#'   Default is "03_outputs/3d_compiled_results/age_pop_denom_2020.xlsx" in the
#'   project directory.
#' @param log_path Character. Path to save log file. Default:
#'    "03_outputs/3a_model_outputs/modelling_log.rds".
#' @param cell_size Numeric. Cell size in meters for predictor generation.
#'    Default: 5000.
#' @param n_sim Numeric. Number of simulations for gamma predictions.
#'    Default: 5000.
#' @param age_range_table Numeric vector. Age range for table generation.
#'    Default: c(0, 99).
#' @param interval_table Numeric. Age interval for table generation.
#'    Default: 1.
#' @param return_prop Logical. Whether to return proportions in raster output.
#'    Default: TRUE.
#' @param scale_outcome Character. Outcome variable for scaling.
#'    Default: "log_scale".
#' @param shape_outcome Character. Outcome variable for shaping.
#'    Default: "log_shape".
#' @param covariates Character. Covariates for spatial model.
#'    Default: "urban".
#' @param cpp_script_name Character. Path to C++ model script.
#'    Default: "02_scripts/model".
#' @param return_results Logical. Whether to return function outputs.
#'    Default: FALSE.
#' @param ignore_cache Logical. Whether to ignore cached data.
#'    Default: FALSE.
#' @examples
#' # run_models_with_logging(
#' #  country_codes = c("KEN", "UGA"),
#' #  log_path = "03_outputs/3a_model_outputs/modelling_log.rds"
#' # )
#' @export
run_models_with_logging <- function(
    country_codes,
    survey_data_path = here::here("01_data", "1a_survey_data", "processed"),
    survey_data_suffix = "dhs_pr_records_combined.rds",
    shape_path = here::here("01_data", "1c_shapefiles"),
    shape_suffix = "district_shape.gpkg",
    pop_raster_path = here::here("01_data", "1b_rasters", "pop_raster"),
    pop_raster_suffix = "_ppp_2020_constrained.tif",
    ur_raster_path = here::here("01_data", "1b_rasters", "urban_extent",
                               "afurextent.asc"),
    ur_raster_suffix = "afurextent.asc",
    model_output_path = here::here("03_outputs", "3a_model_outputs"),
    plot_output_path = here::here("03_outputs", "3b_visualizations"),
    raster_output_path = here::here("03_outputs", "3c_raster_outputs"),
    table_output_path = here::here("03_outputs", "3c_table_outputs"),
    compiled_output_path = here::here("03_outputs", "3d_compiled_results"),
    excel_output_file = here::here(
      "03_outputs", "3d_compiled_results",
      "age_pop_denom_compiled.xlsx"
    ),
    log_path = here::here("03_outputs", "3a_model_outputs", "modelling_log.rds"),
    cell_size = 5000,
    n_sim = 5000,
    age_range_table = c(0, 99),
    interval_table = 1,
    return_prop = TRUE,
    scale_outcome = "log_scale",
    shape_outcome = "log_shape",
    covariates = "urban",
    cpp_script_name = here::here("02_scripts", "model"),
    return_results = FALSE,
    ignore_cache = FALSE) {

  log_data <- list()

  # Import historical log if exists
  hist_log <- if (file.exists(log_path)) readRDS(log_path) else list()

  # Loop through country codes
  for (country in country_codes) {
    start_time <- Sys.time()

    tryCatch({
      # Run the model
      run_country_model(
        country_code = country,
        survey_data_path = survey_data_path,
        survey_data_suffix = survey_data_suffix,
        shape_path = shape_path,
        shape_suffix = shape_suffix,
        pop_raster_path = pop_raster_path,
        pop_raster_suffix = pop_raster_suffix,
        ur_raster_path = ur_raster_path,
        ur_raster_suffix = ur_raster_suffix,
        model_output_path = model_output_path,
        plot_output_path = plot_output_path,
        raster_output_path = raster_output_path,
        table_output_path = table_output_path,
        compiled_output_path = compiled_output_path,
        excel_output_file = excel_output_file,
        cell_size = cell_size,
        n_sim = n_sim,
        age_range_table = age_range_table,
        interval_table = interval_table,
        return_prop = return_prop,
        scale_outcome = scale_outcome,
        shape_outcome = shape_outcome,
        covariates = covariates,
        cpp_script_name = cpp_script_name,
        return_results = return_results,
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

      cli::cli_alert_danger(
        glue::glue("Error running model for {country}: {e$message}")
      )
    })
  }

  # Convert log_data to data frame
  log_data_df <- do.call(rbind, lapply(names(log_data), function(country) {
    data.frame(
      Country = country,
      Status = log_data[[country]]$status,
      Start_Time = as.character(log_data[[country]]$start_time),
      End_Time = as.character(log_data[[country]]$end_time),
      Duration_Minutes = as.numeric(log_data[[country]]$duration),
      Error_Message = log_data[[country]]$error_message,
      stringsAsFactors = FALSE
    )
  }))

  # Merge historical and new logs, then save
  logs <- dplyr::bind_rows(hist_log, log_data_df) |>
    dplyr::mutate(Error_Message = ifelse(is.na(Error_Message), "None", Error_Message))
  saveRDS(logs, log_path)
}
