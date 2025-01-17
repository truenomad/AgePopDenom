testthat::test_that(
  "`fit_spatial_model()` returns valid results for Comoros", {


    df_comoros <- dplyr::tibble(
      country = "Comoros",
      country_code_iso3 = "COM",
      country_code_dhs = "KM",
      year_of_survey = 2024L,
      id_coords = 1:500,
      lon = runif(500, 43.2, 44.6),
      lat = runif(500, -12.4, -11.2),
      web_x = runif(500, 4.2e5, 4.4e5),
      web_y = runif(500, -1.2e6, -1.1e6),
      log_scale = runif(500, 2.7, 3.2),
      log_shape = runif(500, 0.3, 0.5),
      urban = sample(c(0, 1), 500, TRUE),
      b1 = runif(500, 0.008, 0.02),
      c = runif(500, -0.012, -0.005),
      b2 = runif(500, 0.0002, 0.02),
      nsampled = sample(100:300, 500, TRUE),
      country_year = "Comoros - 2024"
    )

    # Skip on CRAN and if TMB not available
    testthat::skip_on_cran()
    testthat::skip_if(!requireNamespace("TMB", quietly = TRUE),
                      "TMB not available")

    suppressWarnings({
      # Create temp directory with normalized path
      tf <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
      dir.create(tf)

      # Initialize with normalized path
      cpp_path <- file.path(tf, "02_scripts", "model")
      cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = FALSE)

      AgePopDenom::init(
        r_script_name = "full_pipeline.R",
        cpp_script_name = "model.cpp",
        path = tf,
        open_r_script = FALSE
      )

      # Test with normalized paths
      results <- AgePopDenom::fit_spatial_model(
        country_code = "COM",
        data = df_comoros,
        scale_outcome = "log_scale",
        shape_outcome = "log_shape",
        covariates = "urban",
        cpp_script_name = cpp_path,
        output_dir = normalizePath(
          file.path(tf, "03_outputs", "3a_model_outputs"),
          winslash = "/",
          mustWork = FALSE),
        ignore_cache = TRUE
      )

    })
  })
