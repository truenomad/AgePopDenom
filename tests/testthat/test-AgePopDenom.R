cleanup_tmb_artifacts <- function() {
  # Find and remove symbols.rds files in R check directories
  temp_files <- list.files(
    path = tempdir(),
    pattern = "symbols.rds$",
    full.names = TRUE,
    recursive = TRUE
  )

  # Filter to only include files in .Rcheck directories
  rcheck_files <- temp_files[grep("\\.Rcheck", temp_files)]

  # Remove files
  if (length(rcheck_files) > 0) {
    message(
      "Removing ",
      length(rcheck_files),
      " TMB artifacts in .Rcheck directories"
    )
    for (file in rcheck_files) {
      tryCatch(
        {
          unlink(file, force = TRUE)
          message("Removed: ", file)
        },
        error = function(e) {
          warning("Could not remove file: ", file, "\n", e$message)
        }
      )
    }
  }

  # Additional cleanup for local symbols.rds
  if (file.exists("symbols.rds")) {
    unlink("symbols.rds", force = TRUE)
  }
}

testthat::test_that("process_dhs_data correctly processes DHS survey data", {

  testthat::skip_if_not_installed("numDeriv")

  # Setup temporary test environment
  # Create temp directory with normalized path
  tf <- file.path(tempdir(), "test_env")
  dir.create(tf, recursive = TRUE, showWarnings = FALSE)
  tmp_rds_dir <- file.path(tf, "rds")
  tmp_shp_dir <- file.path(tf, "shp")
  tmp_output <- file.path(tf, "output.rds")

  dir.create(tmp_rds_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_shp_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit(
    {
      unlink(tf, recursive = TRUE)
      cleanup_tmb_artifacts()
    },
    add = TRUE
  )

  # Create fake DHS data
  create_fake_dhs_data <- function(country_code) {
    set.seed(123) # For reproducibility
    n <- 100

    # Create labelled vectors
    hv007 <- haven::labelled(
      sample(c(2015, 2016), n, replace = TRUE),
      labels = c("2015" = 2015, "2016" = 2016)
    )

    hv001 <- haven::labelled(
      sample(1:20, n, replace = TRUE),
      labels = setNames(1:20, paste("Cluster", 1:20))
    )

    hv105 <- haven::labelled(
      sample(c(1:97, 98), n, replace = TRUE),
      labels = c(setNames(1:97, paste("Age", 1:97)), "Don't know" = 98)
    )

    # Combine into data frame
    data.frame(
      hv007 = hv007,
      hv001 = hv001,
      hv105 = hv105
    )
  }

  # Create fake shapefile data
  # Create fake shapefile data with explicit CRS
  create_fake_shapefile <- function(country_code) {
    set.seed(123)
    n_clusters <- 20

    # Create spatial data frame with explicit CRS
    sf_data <- sf::st_as_sf(
      dplyr::tibble(
        DHSCLUST = 1:n_clusters,
        URBAN_RURA = sample(c("R", "U"), n_clusters, replace = TRUE),
        LATNUM = runif(n_clusters, -10, 10),
        LONGNUM = runif(n_clusters, -10, 10)
      ),
      coords = c("LONGNUM", "LATNUM"),
      crs = 4326 # WGS84
    ) |>
      dplyr::mutate(
        LATNUM = runif(n_clusters, -10, 10),
        LONGNUM = runif(n_clusters, -10, 10)
      )
  }

  # Save test data for two countries
  countries <- c("KE", "TZ")
  for (country in countries) {
    saveRDS(
      create_fake_dhs_data(country),
      file = file.path(tmp_rds_dir, paste0(country, "HR71FL.rds"))
    )
    saveRDS(
      create_fake_shapefile(country),
      file = file.path(tmp_shp_dir, paste0(country, "HR7SHP.rds"))
    )
  }

  # Run the function
  process_dhs_data(
    rds_dir = tmp_rds_dir,
    shp_dir = tmp_shp_dir,
    output_path = tmp_output
  )

  # Read results
  results <- readRDS(tmp_output)

  # Test expectations
  testthat::expect_true(file.exists(tmp_output))
  testthat::expect_named(
    results,
    c("outlier_free_data", "age_param_data")
  )

  # Test outlier_free_data structure
  testthat::expect_true(
    is.data.frame(results$outlier_free_data)
  )

  testthat::expect_true(all(
    c(
      "country",
      "country_code_iso3",
      "country_code_dhs",
      "year_of_survey",
      "dhs_clust_num",
      "ageyrs",
      "survey_code",
      "urban",
      "lat",
      "long",
      "country_year"
    ) %in%
      names(results$outlier_free_data)
  ))

  # Test age_param_data structure
  testthat::expect_true(is.data.frame(results$age_param_data))

  testthat::expect_true(all(
    c(
      "country",
      "country_code_iso3",
      "country_code_dhs",
      "year_of_survey",
      "survey_code",
      "urban",
      "lat",
      "lon",
      "b1",
      "country_year"
    ) %in%
      names(results$age_param_data)
  ))

  # Test data quality
  testthat::expect_true(all(results$outlier_free_data$ageyrs != 98))
  testthat::expect_true(all(!is.na(results$outlier_free_data$country_code_dhs)))
  testthat::expect_true(all(!is.na(results$age_param_data$country_code_dhs)))

  # Test for expected number of countries
  testthat::expect_equal(
    length(unique(results$outlier_free_data$country_code_dhs)),
    length(countries)
  )
})


testthat::test_that("Test the full pipeline works", {

  # Skip on CRAN and if TMB not available
  testthat::skip_on_cran()
  testthat::skip_if(
    !requireNamespace("TMB", quietly = TRUE),
    "TMB not available"
  )

  country_codeiso <- "GMB"

  suppressWarnings({
    # Create temp directory with normalized path
    tf <- file.path(tempdir(), "test_env")
    dir.create(tf, recursive = TRUE, showWarnings = FALSE)

    # Initialize with normalized path
    cpp_path <- file.path(tf, "02_scripts", "model")
    dir.create(cpp_path, recursive = TRUE, showWarnings = FALSE)
    cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = FALSE)

    init(
      r_script_name = "full_pipeline.R",
      cpp_script_name = "model.cpp",
      path = tf,
      open_r_script = FALSE
    )

    # Simulate and save processed survey dataset for Gambia
    AgePopDenom::simulate_dummy_dhs_pr(
      country = "Gambia",
      country_code_iso3 = country_codeiso,
      country_code_dhs = "GM",
      year_of_survey = 2024,
      output_path = here::here(
        tf,
        "01_data",
        "1a_survey_data",
        "processed",
        "dhs_pr_records_combined.rds"
      ) |>
        normalizePath(winslash = "/", mustWork = FALSE)
    )

    # Download shapefiles
    download_shapefile(
      country_codes = country_codeiso,
      dest_file = file.path(
        tf,
        "01_data",
        "1c_shapefiles",
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
        tf,
        "01_data",
        "1a_survey_data",
        "processed"
      ) |>
        normalizePath(winslash = "/", mustWork = FALSE),
      survey_data_suffix = "dhs_pr_records_combined.rds",
      shape_path = file.path(
        tf,
        "01_data",
        "1c_shapefiles"
      ) |>
        normalizePath(winslash = "/", mustWork = FALSE),
      shape_suffix = "district_shape.gpkg",
      pop_raster_path = file.path(
        tf,
        "01_data",
        "1b_rasters",
        "pop_raster"
      ) |>
        normalizePath(winslash = "/", mustWork = FALSE),
      pop_raster_suffix = "_ppp_2020_constrained.tif",
      ur_raster_path = file.path(
        tf,
        "01_data",
        "1b_rasters",
        "urban_extent"
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
          tf,
          "03_outputs",
          "3d_compiled_results",
          "age_pop_denom_compiled.xlsx"
        ),
        log = file.path(
          tf,
          "03_outputs",
          "3a_model_outputs",
          "modelling_log.rds"
        )
      ) |>
        lapply(\(x) normalizePath(x, winslash = "/", mustWork = FALSE)),
      model_params = list(
        cell_size = 5000,
        n_sim = 100,
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

    # Model outputs test -----------------------------------------------------
    # Test existence of model output files
    testthat::expect_true(
      file.exists(file.path(
        tf,
        "03_outputs",
        "3a_model_outputs",
        "gmb_age_param_spatial.rds"
      ))
    )

    testthat::expect_true(
      file.exists(file.path(
        tf,
        "03_outputs",
        "3a_model_outputs",
        "gmb_predictor_data.rds"
      ))
    )

    # Test model output contents
    model_output <- readRDS(file.path(
      tf,
      "03_outputs",
      "3a_model_outputs",
      "gmb_age_param_spatial.rds"
    ))
    testthat::expect_type(model_output, "list")

    testthat::expect_true(all(
      c(
        "par",
        "objective",
        "convergence",
        "iterations",
        "evaluations",
        "message",
        "scale_formula",
        "shape_formula",
        "variogram"
      ) %in%
        names(model_output)
    ))

    # Test for variogram components
    testthat::expect_true("variogram" %in% names(model_output))
    testthat::expect_type(model_output$variogram, "list")

    predictor_data <- readRDS(file.path(
      tf,
      "03_outputs",
      "3a_model_outputs",
      "gmb_predictor_data.rds"
    ))
    testthat::expect_s3_class(predictor_data, "data.frame")

    testthat::expect_true(all(
      c(
        "urban",
        "web_x",
        "web_y",
        "pop",
        "urban",
        "country",
        "region",
        "district",
        "country_code"
      ) %in%
        names(predictor_data)
    ))

    # Visualization outputs test
    testthat::expect_true(
      file.exists(file.path(
        tf,
        "03_outputs",
        "3b_visualizations",
        "gmb_gamma_prediction_rasters.png"
      ))
    )
    testthat::expect_true(
      file.exists(file.path(
        tf,
        "03_outputs",
        "3b_visualizations",
        "gmb_age_pyramid_count.png"
      ))
    )
    testthat::expect_true(
      file.exists(file.path(
        tf,
        "03_outputs",
        "3b_visualizations",
        "gmb_age_pyramid_prop.png"
      ))
    )

    testthat::expect_true(
      file.exists(file.path(
        tf,
        "03_outputs",
        "3b_visualizations",
        "gmb_variogram.png"
      ))
    )

    # Test image properties
    for (img_file in c(
      "gmb_gamma_prediction_rasters.png",
      "gmb_age_pyramid_count.png",
      "gmb_age_pyramid_prop.png"
    )) {
      img_path <- file.path(tf, "03_outputs", "3b_visualizations", img_file)
      img_info <- file.info(img_path)
      testthat::expect_gt(img_info$size, 0)
    }

    # Table outputs test
    table_path <- file.path(
      tf,
      "03_outputs",
      "3c_table_outputs",
      "gmb_age_tables_pop_0_1plus_yrs_by_1yrs.rds"
    )
    testthat::expect_true(file.exists(table_path))

    # Test table contents
    age_tables <- readRDS(table_path)
    testthat::expect_type(age_tables, "list")
    testthat::expect_true(all(
      c("prop_df", "pop_df") %in%
        names(age_tables)
    ))

    testthat::expect_true(all(
      c(
        "country",
        "region",
        "district",
        "popsize"
      ) %in%
        c(
          names(age_tables$prop_df),
          names(age_tables$pop_df)
        )
    ))

    # Raster outputs test
    rast_path <- file.path(
      tf,
      "03_outputs",
      "3b_visualizations",
      "gmb_age_pop_grid_0_10_yrs_by_1yrs.tif"
    )
    testthat::expect_true(file.exists(table_path))

    # Check if output is raster
    rast <- terra::rast(rast_path)
    testthat::expect_equal(class(rast)[1], "SpatRaster")

    # Compiled results test
    excel_path <- file.path(
      tf,
      "03_outputs",
      "3d_compiled_results",
      "age_pop_denom_compiled.xlsx"
    )
    testthat::expect_true(file.exists(excel_path))
    testthat::expect_gt(file.info(excel_path)$size, 0)

    params_path <- file.path(
      tf,
      "03_outputs",
      "3d_compiled_results",
      "afro_model_params.csv"
    )
    testthat::expect_true(file.exists(params_path))

    # Test CSV contents
    params_data <- read.csv(params_path)
    testthat::expect_true(all(
      c(
        "country",
        "beta1",
        "beta2",
        "gamma",
        "log_sigma2",
        "log_phi",
        "log_tau1",
        "log_likelihood",
        "convergence",
        "iterations",
        "eval_function",
        "eval_gradient",
        "message"
      ) %in%
        names(params_data)
    ))

    testthat::expect_equal(params_data$country[1], "GMB")

    # Test log file
    log_path <- file.path(
      tf,
      "03_outputs",
      "3a_model_outputs",
      "modelling_log.rds"
    )
    testthat::expect_true(file.exists(log_path))
    log_data <- readRDS(log_path)
    testthat::expect_type(log_data, "list")
  })

})

testthat::test_that("simulate_dummy_dhs_pr() creates expected DHS dataset", {

  testthat::skip_if_not_installed("dplyr")

  # setup: temporary output path
  temp_path <- tempfile(fileext = ".rds")

  # run simulation
  AgePopDenom::simulate_dummy_dhs_pr(
    total_population = 100,
    output_path = temp_path,
    seed = 42
  )

  # check file was created
  testthat::expect_true(file.exists(temp_path))

  # read the output
  result <- readRDS(temp_path)

  # check structure
  testthat::expect_type(result, "list")
  testthat::expect_named(result, "age_param_data")
  testthat::expect_s3_class(result$age_param_data, "tbl_df")

  # check column names
  expected_cols <- c(
    "country",
    "country_code_iso3",
    "country_code_dhs",
    "year_of_survey",
    "id_coords",
    "lon",
    "lat",
    "web_x",
    "web_y",
    "log_scale",
    "log_shape",
    "urban",
    "b1",
    "c",
    "b2",
    "nsampled"
  )
  testthat::expect_true(all(expected_cols %in% names(result$age_param_data)))

  # cleanup
  unlink(temp_path)
})

testthat::test_that("autofitVariogram() returns valid variogram model", {
  testthat::skip_if_not_installed("sp")

  # prepare data
  data("meuse", package = "sp")
  meuse_sf <- sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

  # run function
  result <- AgePopDenom:::autofitVariogram(zinc ~ 1, meuse_sf)

  # basic checks
  testthat::expect_s3_class(result, "autofitVariogram")
  testthat::expect_type(result, "list")
  testthat::expect_named(
    result,
    c("exp_var", "var_model", "sserr"),
    ignore.order = TRUE
  )

  # check contents
  testthat::expect_s3_class(result$exp_var, "data.frame")
  testthat::expect_true(nrow(result$exp_var) > 1)
  testthat::expect_s3_class(result$var_model, "variogramModel")
  testthat::expect_true(is.numeric(result$sserr))
})

testthat::test_that("autofitVariogram() prints expected output in verbose mode", {
  testthat::skip_if_not_installed("sp")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("gstat")

  data("meuse", package = "sp")
  meuse_sf <- sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

  testthat::expect_output(
    AgePopDenom:::autofitVariogram(zinc ~ 1, meuse_sf, verbose = TRUE),
    regexp = "Selected:"
  )

  testthat::expect_output(
    AgePopDenom:::autofitVariogram(zinc ~ 1, meuse_sf, verbose = TRUE),
    regexp = "Tested models, best first:"
  )
})


cleanup_tmb_artifacts()
