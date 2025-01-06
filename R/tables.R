#' Compute Age Proportions
#'
#' @description
#' Calculates the proportion of population in each age interval using gamma
#' distribution parameters. Handles both regular intervals and the final
#' open-ended interval.
#'
#' @param sim Integer index for current simulation
#' @param scale Matrix of scale parameters for gamma distribution
#' @param shape Matrix of shape parameters for gamma distribution
#' @param run Integer index for current age interval being processed
#' @param limslow Numeric vector of lower age limits for intervals
#' @param limsup Numeric vector of upper age limits for intervals
#'
#' @return Numeric vector of proportions for the given age interval
#'
#' @noRd
compute_age_proportions <- function(sim, scale, shape, run, limslow, limsup) {
  if (run == length(limslow)) {
    # For the last interval (open-ended)
    return(
      1 - stats::pgamma(limslow[run],
                        scale = scale[, sim],
                        shape = shape[, sim])
    )
  } else {
    # For regular intervals
    return(
      stats::pgamma(limsup[run],
                    scale = scale[, sim],
                    shape = shape[, sim]) -
        stats::pgamma(limslow[run],
                      scale = scale[, sim],
                      shape = shape[, sim])
    )
  }
}

#' Run parallel computation wrapper
#' @noRd
run_parallel_mac <- function(n_sim, scale_pred, shape_pred, runnum, n_cores) {
  pbmcapply::pbmclapply(
    1:n_sim,
    function(sim) {
      compute_age_proportions(sim, scale_pred, shape_pred, runnum,
                              limslow, limsup)
    },
    mc.cores = n_cores,
    mc.preschedule = FALSE
  )
}

#' Run parallel computation wrapper for non-Mac
#' @noRd
run_parallel_other <- function(n_sim, scale_pred, shape_pred, runnum, n_cores) {
  future.apply::future_lapply(
    1:n_sim,
    function(sim) {
      compute_age_proportions(sim, scale_pred, shape_pred, runnum,
                              limslow, limsup)
    }
  )
}

#' Generate Age Population Tables
#'
#' @description
#' Creates age-stratified population tables from predictor data and gamma
#' distribution parameters. Supports parallel processing and caching of results.
#'
#' @param predictor_data Data frame containing population data with columns:
#'    country, region, district, pop
#' @param scale_pred Matrix of scale parameters for gamma distribution
#'    predictions
#' @param shape_pred Matrix of shape parameters for gamma distribution
#'    predictions
#' @param age_range Numeric vector of length 2 specifying min and max ages,
#'    default c(0,99)
#' @param interval Numeric interval size between age groups in years, default 1
#' @param country_code Character ISO3 country code
#' @param ignore_cache Logical whether to ignore cached results, default FALSE
#' @param output_dir Character path to output directory
#' @param n_cores Integer number of cores for parallel processing, default
#'    detectCores()-2
#'
#' @return Data frame with age-stratified population estimates and
#'    uncertainty intervals
#'
#' @export
generate_age_pop_table <- function(predictor_data,
                                   scale_pred,
                                   shape_pred,
                                   age_range = c(0, 99),
                                   interval = 1,
                                   country_code,
                                   ignore_cache = FALSE,
                                   output_dir,
                                   n_cores = parallel::detectCores() - 2) {
  # Construct output path
  output_path <- file.path(
    output_dir,
    glue::glue(
      "{country_code}_age_tables_pop_",
      "{age_range[1]}_{age_range[2]}plus_yrs_by_{interval}yrs.rds"
    )
  )

  # Check cache
  if (!ignore_cache && file.exists(output_path)) {
    cli::cli_process_start(
      msg = "Importing cached age population data...",
      msg_done = "Successfully imported cached age population data."
    )
    final_df <- readRDS(output_path)
    cli::cli_process_done()
    return(final_df)
  }

  # Ensure output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Initialize age population data frame
  final_df <- predictor_data |>
    dplyr::group_by(country, region = region, district = district) |>
    dplyr::summarize(popsize = sum(pop, na.rm = TRUE), .groups = "drop") |>
    as.data.frame()

  # Define age intervals including the final open-ended interval
  limslow <- seq(age_range[1], age_range[2], interval)
  limsup <- c(seq(age_range[1] + interval, age_range[2] + interval, interval), Inf)

  n_sim <- ncol(shape_pred)

  # Validate inputs
  if (!identical(dim(scale_pred), dim(shape_pred))) {
    stop("scale_pred and shape_pred must have the same dimensions")
  }

  if (nrow(scale_pred) != nrow(predictor_data)) {
    stop("Number of rows in predictions must match predictor_data")
  }

  # Processing loop
  for (runnum in seq_along(limslow)) {
    cli::cli_process_start(
      msg = glue::glue("Processing interval {runnum}/{length(limslow)}..."),
      msg_done = glue::glue("Completed interval {runnum}/{length(limslow)}.")
    )

    # Run parallel computation
    prop_age_pred <- pbmcapply::pbmclapply(
      1:n_sim,
      function(sim) {
        compute_age_proportions(
          sim = sim, scale = scale_pred, shape = shape_pred,
          run = runnum, limslow = limslow, limsup = limsup
        )
      },
      mc.cores = n_cores
    )

    # Combine simulation results
    prop_age_pred <- base::do.call(base::cbind, prop_age_pred)
    mean_prop_age <- base::rowMeans(prop_age_pred, na.rm = TRUE)
    quantiles_mean_prop_age <- matrixStats::rowQuantiles(
      prop_age_pred,
      probs = c(0.025, 0.975)
    )

    # Create age class data frame
    age_pop_class <- base::data.frame(
      country = predictor_data$country,
      region = predictor_data$region,
      district = predictor_data$district,
      pop = mean_prop_age * predictor_data$pop,
      lower_quantile = quantiles_mean_prop_age[, 1] * predictor_data$pop,
      upper_quantile = quantiles_mean_prop_age[, 2] * predictor_data$pop
    ) |>
      dplyr::group_by(country, region, district) |>
      dplyr::summarize(
        dplyr::across(dplyr::everything(),
                      base::sum,
                      .names = "{.col}_tot_class"
        ),
        .groups = "drop"
      )

    # Update column names
    interval_name <- if (runnum == length(limslow)) {
      paste0(limslow[runnum], "plus")
    } else {
      paste0(limslow[runnum], "_", limsup[runnum], "y")
    }
    colnames(age_pop_class) <- c(
      "country", "region", "district",
      paste0(interval_name, "_mean"),
      paste0(interval_name, "_low_interval_2.5%"),
      paste0(interval_name, "_upper_interval_97.5%")
    )

    # Merge with final data frame
    final_df <- final_df |>
      dplyr::left_join(age_pop_class,
                       by = c("country", "region", "district")
      )

    cli::cli_process_done()
  }

  # Save results
  base::saveRDS(final_df, file = output_path)
  cli::cli_alert_success("Final age population data saved to {output_path}")
  return(final_df)
}

#' Process Final Population Data
#'
#' Reads population data from RDS files in a specified directory, processes the
#' data to generate age-group-based population summaries at different
#' administrative levels (Country, Region, District), and writes the results
#' to an Excel file with separate sheets for each level.
#'
#' @param input_dir A character string specifying the directory containing RDS
#'   files. Default is "03_outputs/3c_table_outputs" in the project directory.
#' @param output_file A character string specifying the output Excel file path.
#'   Default is "03_outputs/3d_compiled_results/age_pop_denom_2020.xlsx" in the
#'   project directory.
#' @return None. The function writes an Excel file to the specified output
#'   location with three sheets: "Country", "Region", and "District", containing
#'   age-structured population data at different administrative levels.
#' @examples
#' # process_final_population_data(
#' #   input_dir = "03_outputs/3c_table_outputs",
#' #   output_file = "03_outputs/afro_population_2020.xlsx"
#' # )
#' @export
process_final_population_data <- function(
    input_dir = here::here("03_outputs", "3c_table_outputs"),
    output_file = here::here(
      "03_outputs", "3d_compiled_results",
      "age_pop_denom_2020.xlsx"
    )) {
  # Helper function to process individual files
  process_file <- function(file) {
    readRDS(file) |>
      dplyr::select(
        country, region, district,
        dplyr::contains("mean")
      ) |>
      dplyr::rename_with(
        ~ stringr::str_replace_all(
          ., c("_mean" = "", "plus" = "+y")
        ),
        dplyr::contains("mean")
      )
  }

  # Read and process all files
  files <- list.files(input_dir, full.names = TRUE)
  pop_df <- lapply(files, process_file) |> dplyr::bind_rows()
  # Reshape the data to long format
  pop_long <- pop_df |>
    tidyr::pivot_longer(
      cols = -c(country, region, district),
      names_to = "age_group",
      values_to = "population"
    )

  # Summarize data at different levels
  summarize_by <- function(data, group_vars) {
    data |>
      dplyr::group_by(dplyr::across(all_of(group_vars))) |>
      dplyr::summarise(
        population = sum(population, na.rm = TRUE) |> round(),
        .groups = "drop"
      ) |>
      # Reshape back to wide format
      tidyr::pivot_wider(
        names_from = age_group,
        values_from = population
      )
  }

  res_adm0 <- summarize_by(pop_long, c("country", "age_group"))
  res_adm1 <- summarize_by(pop_long, c("country", "region", "age_group"))
  res_adm2 <- summarize_by(
    pop_long,
    c("country", "region", "district", "age_group")
  )

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
    "Final age-structured population is saved to {output_file}."
  )
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
    dir_path = here::here("03_outputs", "3a_model_outputs"),
    output_file = here::here("03_outputs", "3d_compiled_results",
                             "model_params.csv")) {

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
    write.csv(params_df, file.path(output_file, "afro_model_params.csv"),
              row.names = FALSE)
  }

  cli::cli_alert_success(
    "Model paramters extracted and saved to {output_file}.")

  return(params_df)
}
