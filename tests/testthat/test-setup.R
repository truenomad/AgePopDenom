testthat::test_that(
  "`fit_spatial_model()` returns valid results for Comoros", {

    suppressWarnings({

      set.seed(123)

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

      tf <- tempfile()
      dir.create(tf)

      AgePopDenom::init(
        r_script_name = "full_pipeline.R",
        cpp_script_name = "model.cpp",
        path = tf,
        open_r_script = FALSE
      )

      results <- AgePopDenom::fit_spatial_model(
        country_code = "COM",
        data = df_comoros,
        scale_outcome = "log_scale",
        shape_outcome = "log_shape",
        covariates = "urban",
        cpp_script_name = here::here(tf, "02_scripts", "model"),
        output_dir = file.path(tf, "03_outputs", "3a_model_outputs"),
        ignore_cache = TRUE
      )

      testthat::expect_type(results, "list")
      testthat::expect_true(
        all(c("par", "objective", "convergence") %in% names(results)))

      testthat::expect_equal(results$convergence, 0)
      testthat::expect_lt(results$objective, 0) # objective should be negative
      testthat::expect_length(results$par, 6)   # e.g. if you expect 6 paraMs

      testthat::expect_match(deparse(results$scale_formula),
                             "log_scale ~ urban - 1", fixed = TRUE)
      testthat::expect_match(deparse(results$shape_formula),
                             "log_shape ~ urban - 1", fixed = TRUE)
    })
  })
