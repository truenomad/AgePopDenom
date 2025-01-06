#' Create a Standardized Project Folder Structure
#'
#' This function creates a standardized folder structure for organizing
#' data, scripts, and outputs within a project directory. It ensures
#' consistency and reproducibility for data-related workflows.
#'
#' @param base_path A character string specifying the root directory where
#'   the folder structure will be created. Defaults to `here::here()`
#'   to use the current project directory.
#'
#' @return Creates directories under the specified `base_path`. Returns
#'   invisible `NULL` and prints messages about folder creation status.
#'
#' @details The function generates the following folder structure:
#' \preformatted{
#' # 01_data/
#' # ├── 1a_dhs_data/
#' # │   ├── processed/
#' # │   └── raw/
#' # ├── 1b_rasters/
#' # │   ├── urban_extent/
#' # │   └── worldpop_100m/
#' # ├── 1c_shapefiles/
#' # 02_scripts/
#' # 03_outputs/
#' # ├── 3a_model_outputs/
#' # ├── 3b_visualizations/
#' # ├── 3c_table_outputs/
#' # └── 3d_compiled_results/
#' }
#'
#' @examples
#' # Create the project folder structure in the current directory
#' # create_project_structure()
#'
#' # Create the folder structure in a specific directory
#' # create_project_structure(base_path = "~/my_project")
#'
#' @export
create_project_structure <- function(base_path = here::here()) {

  # Define your directories using here::here for each subfolder.
  dirs <- c(
    here::here("01_data", "1a_dhs_data", "processed"),
    here::here("01_data", "1a_dhs_data", "raw"),
    here::here("01_data", "1b_rasters", "urban_extent"),
    here::here("01_data", "1b_rasters", "worldpop_100m"),
    here::here("01_data", "1c_shapefiles"),
    here::here("02_scripts"),
    here::here("03_outputs", "3a_model_outputs"),
    here::here("03_outputs", "3b_visualizations"),
    here::here("03_outputs", "3c_raster_outputs"),
    here::here("03_outputs", "3c_table_outputs"),
    here::here("03_outputs", "3d_compiled_results")
  )

  for (dir in dirs) {
    dir_path <- file.path(base_path, dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cli::cli_alert_info("Created: {dir_path}")
    } else {
      cli::cli_alert_warning("Exists: {dir_path}")
    }
  }
  cli::cli_alert_success("Folder structure created successfully.")
}

#' Download DHS Datasets for Specified Countries
#'
#' Downloads DHS datasets of a specified survey type and file type for given
#' countries.
#'
#' @param country_codes A character vector of ISO3 country codes.
#' @param survey_type A character string. Type of DHS survey (e.g., "DHS").
#' @param file_type A character string. Type of file (e.g., "PR" or "GE").
#' @param file_format A character string. Format of the file (e.g., "DT",
#'    "FLAT").
#' @param output_dir A character string specifying the directory to save
#'    downloaded datasets.
#' @param clear_cache Logical. Whether to clear the cache after downloading.
#'    Default is TRUE.
#'
#' @return Invisibly returns a vector of downloaded dataset filenames.
#' @export
download_dhs_datasets_by_type <- function(country_codes, survey_type, file_type,
                                          file_format, output_dir, clear_cache = TRUE) {
  cli::cli_h1("Downloading {file_type} datasets for {survey_type} survey...")

  # Retrieve dataset filenames
  datasets <- rdhs::dhs_datasets(
    countryIds = country_codes,
    surveyType = survey_type,
    fileFormat = file_format,
    fileType = file_type
  ) |>
    dplyr::group_by(DatasetType, CountryName) |>
    dplyr::slice_max(SurveyYear) |>
    dplyr::distinct() |>
    dplyr::pull(FileName)

  if (length(datasets) == 0) {
    cli::cli_alert_warning("No datasets found for {survey_type} - {file_type}.")
    return(invisible(character(0)))
  }

  # Download datasets
  rdhs::get_datasets(
    datasets,
    download_option = "rds",
    output_dir_root = output_dir,
    clear_cache = clear_cache
  )

  cli::cli_alert_success("Datasets successfully downloaded to: {output_dir}")
  invisible(datasets)
}

#' Main Function to Download DHS Datasets
#'
#' Downloads the latest PR (household) and GE (geographic) DHS datasets for
#'  specified countries.
#'
#' @param country_codes A character vector of ISO3 country codes.
#' @param cache_path A character string specifying the cache path for RDHS.
#' @param output_dir_root A character string specifying the root directory for
#'    output.
#' @param email A character string. Email registered with DHS.
#' @param project A character string. Project name as registered with DHS.
#' @param verbose Logical for rdhs setup and messages to be printed and dataset
#'    download progress bars to be shown.
#'
#' @return Invisibly returns a list of downloaded dataset filenames.
#' @export
download_dhs_datasets <- function(
    country_codes,
    cache_path = here::here("01_data", "1a_dhs_data", "raw"),
    output_dir_root = here::here("01_data", "1a_dhs_data", "raw"),
    email, project,
    verbose = TRUE) {

  # ´get the dhs country codes
  dhs_country_code <- countrycode::codelist |>
    dplyr::filter(iso3c %in% country_codes) |>
    dplyr::pull(dhs)

  cli::cli_alert_info("Configuring RDHS settings...")

  # Set RDHS configuration
  rdhs::set_rdhs_config(
    email = email,
    project = project,
    config_path = "rdhs.json",
    cache_path = cache_path,
    data_frame = "data.table::as.data.table",
    global = FALSE,
    verbose_setup = verbose,
    timeout = 120,
    verbose_download = verbose
  )

  cli::cli_alert_success("RDHS configuration complete.")

  # Download PR (household) datasets
  pr_output_dir <- file.path(output_dir_root, "pr_records")
  pr_datasets <- download_dhs_datasets_by_type(
    country_codes = dhs_country_code,
    survey_type = "DHS",
    file_type = "PR",
    file_format = "DT",
    output_dir = pr_output_dir
  )

  # Download GE (geographic) datasets
  ge_output_dir <- file.path(output_dir_root, "shapefiles")
  ge_datasets <- download_dhs_datasets_by_type(
    country_codes = dhs_country_code,
    survey_type = "DHS",
    file_type = "GE",
    file_format = "FLAT",
    output_dir = ge_output_dir
  )

  cli::cli_alert_success("All datasets successfully Downloaded and saved.")
  invisible(list(PR = pr_datasets, GE = ge_datasets))
}

#' Aggregate Individual DHS Data and Extract Gamma Parameters by Location
#'
#' This script aggregates the individual DHS data to location level and extracts
#' the gamma parameters for the locations.
#'
#' @param data Data frame containing individual-level age data with
#'        coordinates and urban/rural classification
#' @param lat_column Column name for latitude coordinates (default: "lat")
#' @param long_column Column name for longitude coordinates (default: "long")
#' @param age_column Column name for age values (default: "ageyrs")
#' @param urban_column Column name for urban/rural classification (default:
#'      "urban")
#' @return List containing:
#'         \itemize{
#'           \item outlier_free_data: Original data with added spatial
#'                  coordinates
#'           \item age_param_data: Location-level gamma parameters with columns:
#'                 lon, lat, web_x, web_y, log_scale, log_shape, urban,
#'                 b1, c, b2, nsampled
#'         }
aggregate_and_extract_gamma <- function(data,
                                        lat_column = "lat",
                                        long_column = "long",
                                        age_column = "ageyrs",
                                        urban_column = "urban") {

  # Convert coordinates
  coords_sf <- data |>
    dplyr::select(
      !!rlang::sym(long_column), !!rlang::sym(lat_column)
    ) |>
    sf::st_as_sf(coords = c(long_column, lat_column), crs = 4236) |>
    sf::st_transform(crs = 3857)

  data <- data |>
    dplyr::mutate(
      web_x = sf::st_coordinates(coords_sf)[, 1],
      web_y = sf::st_coordinates(coords_sf)[, 2]
    ) |>
    # Create groups based on unique coordinate combinations
    dplyr::group_by(web_x, web_y) |>
    dplyr::mutate(id_coords = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  # Parameter estimation
  names_dt <- c(
    "lon", "lat", "web_x", "web_y",
    "log_scale", "log_shape", "urban",
    "b1", "c", "b2", "nsampled"
  )

  age_param_data <- matrix(ncol = length(names_dt), nrow = 0) |>
    as.data.frame() |>
    setNames(names_dt)

  loglik_x_i <- function(y, par) {
    sum(dgamma(y, scale = exp(par[1]), shape = exp(par[2]), log = TRUE))
  }

  age_param_data <- data |>
    dplyr::group_by(id_coords) |>
    dplyr::group_modify(~ {
      sub_data <- .x |>
        dplyr::filter(is.finite(!!rlang::sym(age_column)),
                      !!rlang::sym(age_column) != 0)

      age_x_i <- sub_data[[age_column]]
      shape_start <- mean(age_x_i)^2 / var(age_x_i)
      scale_start <- var(age_x_i) / mean(age_x_i)

      estim_x_i <- nlminb(
        log(c(scale_start, shape_start)),
        function(par) -loglik_x_i(age_x_i, par)
      )
      sigma_i <- solve(numDeriv::hessian(
        function(par) -loglik_x_i(age_x_i, par),
        x = estim_x_i$par
      ))

      tibble::tibble(
        lon = dplyr::first(sub_data[[long_column]]),
        lat = dplyr::first(sub_data[[lat_column]]),
        web_x = dplyr::first(sub_data$web_x),
        web_y = dplyr::first(sub_data$web_y),
        log_scale = estim_x_i$par[1],
        log_shape = estim_x_i$par[2],
        urban = dplyr::first(sub_data[[urban_column]]),
        b1 = diag(sigma_i)[1],
        c = sigma_i[1, 2],
        b2 = diag(sigma_i)[2],
        nsampled = nrow(sub_data)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      dplyr::select(
        data,
        country,
        year_of_survey, survey_code,
        country_code_iso3,
        country_code_dhs,
        lat, lon = long,
      ), by = c("lat", "lon")
    ) |>
    dplyr::select(
      country,
      country_code_iso3,
      country_code_dhs,
      survey_code,
      year_of_survey,
      dplyr::everything()
    )

  # Return list of results
  list(
    outlier_free_data = data,
    age_param_data = age_param_data
  )
}

#' Process DHS Data: Merge RDS Files with Shapefiles and Extract Gamma
#' Parameters
#'
#' This function processes DHS (Demographic and Health Survey) data by:
#' 1. Reading RDS files and shapefiles for each country.
#' 2. Merging demographic data with geographic information.
#' 3. Cleaning and aggregating the data.
#' 4. Extracting gamma parameters for age-related analysis.
#'
#' @param rds_dir Character. Path to the directory containing raw RDS files.
#' @param shp_dir Character. Path to the directory containing shapefiles.
#' @param output_path Character. Path to save the final processed dataset as an
#'  RDS file.
#'
#' @details
#' The function loops through RDS files, processes each country's data by
#' merging demographic information with shapefile data, and computes gamma
#' parameters for age-related analysis. The progress is tracked and displayed
#' for each country.
#'
#' The function also filters out incomplete data (e.g., age values of `98`) and
#' handles labelled data using the `haven::zap_labels` function.
#'
#' The final output includes two datasets:
#' 1. Outlier-free data.
#' 2. Aggregated age parameter data.
#'
#' @return None. Saves the final combined dataset to the specified output path.
#'
#' @examples
#' \dontrun{
#' process_dhs_data(
#'   rds_dir = "01_data/1a_dhs_data/raw/pr_records",
#'   shp_dir = "01_data/1a_dhs_data/raw/shapefiles/",
#'   output_path = "01_data/1a_dhs_data/processed/dhs_pr_records_combined.rds"
#' )
#' }
#'
#' @export
process_dhs_data <- function(
    rds_dir = here::here("01_data", "1a_dhs_data", "raw", "pr_records"),
    shp_dir = here::here("01_data", "1a_dhs_data", "raw", "shapefiles"),
    output_path = here::here("01_data", "1a_dhs_data",
                             "processed", "dhs_pr_records_combined.rds")) {

  # Create age-population raster
  cli::cli_h1("Processing DHS data and joining with shapefile")

  # Get a list of RDS and Shapefiles
  rds_files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)
  shp_files <- list.files(shp_dir, pattern = "\\.rds$", full.names = TRUE)

  # Initialize a list to store processed datasets
  all_data <- list()
  total_countries <- length(rds_files)
  processed_count <- 0

  # Process each RDS file
  for (rds_path in rds_files) {
    rds_file_name <- basename(rds_path)
    country_code <- substr(rds_file_name, 1, 2)

    # Display processing message
    cli::cli_process_start(
      msg = glue::glue("Processing country: {country_code}"),
      msg_done = glue::glue(
        "Processed country: {country_code} ",
        "({processed_count + 1} of {total_countries})"
      )
    )

    # Read the RDS file
    rds_data <- readRDS(rds_path)

    # Select and rename required columns
    rds_processed <- rds_data |>
      dplyr::mutate(country_code_dhs = country_code) |>
      dplyr::select(
        country_code_dhs,
        year_of_survey = hv007,
        dhs_clust_num = hv001,
        ageyrs = hv105
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(haven::is.labelled), haven::zap_labels)) |>
      dplyr::mutate(
        ageyrs = as.integer(ageyrs),
        year_of_survey = ifelse(year_of_survey == 94, 1994, year_of_survey),
        year_of_survey = ifelse(year_of_survey == 95, 1995, year_of_survey),
        survey_code = sub("\\.rds$", "", rds_file_name)
      ) |>
      dplyr::filter(ageyrs != 98)


    # Find the matching shapefile
    shp_file <- shp_files[grepl(
      paste0(here::here("shapefiles"), here::here(), country_code, ".*L\\.rds$"),
      shp_files)]

    if (length(shp_file) == 1) {
      shp_data <- readRDS(shp_file) |>
        dplyr::mutate(urban = ifelse(URBAN_RURA == "R", 0, 1)) |>
        dplyr::select(
          dhs_clust_num = DHSCLUST,
          urban,
          lat = LATNUM, long = LONGNUM
        ) |>
        sf::st_drop_geometry()

      merged_data <- rds_processed |>
        dplyr::left_join(shp_data, by = "dhs_clust_num")

      all_data[[rds_file_name]] <- merged_data
      processed_count <- processed_count + 1
    } else {
      cli::cli_alert_warning(
        glue::glue(
          "No matching shapefile found for {country_code} ({rds_file_name})")
      )
    }

    cli::cli_process_done()
  }

  # Bind all datasets together -------------------------------------------------

  binded_dataset <- dplyr::bind_rows(all_data) |>
    dplyr::left_join(
      dplyr::select(countrycode::codelist, country.name.en, iso3c, dhs),
      by = c("country_code_dhs" = "dhs")
    ) |>
    dplyr::select(
      country = country.name.en,
      country_code_iso3 = iso3c,
      country_code_dhs,
      dplyr::everything()
    )

  # Process gamma parameters----------------------------------------------------

  # Create age-population raster
  cli::cli_h1("Process gamma parameters")

  processed_count <- 0

  final_data <- purrr::map(
    unique(binded_dataset$country_code_iso3), function(country_code) {
      processed_count <<- processed_count + 1
      cli::cli_process_start(
        msg = glue::glue(
          "Aggregating and extracting gamma for: {country_code}"),
        msg_done = glue::glue(
          "Aggregated and extracted gamma for: {country_code} ",
          "({processed_count} of {
          length(unique(binded_dataset$country_code_iso3))})"
        )
      )

      processed_data <- binded_dataset |>
        dplyr::filter(
          country_code_iso3 == country_code,
          !is.na(urban) | !is.na(lat) | !is.na(long)
        ) |>
        aggregate_and_extract_gamma()


      # Get the latest year for each survey_code
      outlier_free_data <- processed_data$outlier_free_data |>
        dplyr::filter(long != lat) |>
        dplyr::group_by(survey_code) |>
        dplyr::mutate(
          year_of_survey = max(year_of_survey, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        # Add the combined country and year label
        dplyr::mutate(
          country_year = paste0(
            country, " - ", year_of_survey, " (", survey_code, ")"
          )
        )

      # Get the latest year for each survey_code
      age_param_data <- processed_data$age_param_data |>
        dplyr::filter(lon != lat) |>
        dplyr::group_by(survey_code) |>
        dplyr::mutate(
          year_of_survey = max(year_of_survey, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        # Add the combined country and year label
        dplyr::mutate(
          country_year = paste0(
            country, " - ", year_of_survey, " (", survey_code, ")"
          )
        )

      list(
        outlier_free_data = outlier_free_data,
        age_param_data = age_param_data
      )
    })

  # Combine final data
  final_dataset <- list(
    outlier_free_data = purrr::map_dfr(final_data, "outlier_free_data") |>
      dplyr::distinct(),
    age_param_data = purrr::map_dfr(final_data, "age_param_data") |>
      dplyr::distinct()
  )

  # Save the final dataset
  saveRDS(final_dataset, file = output_path, compress = "xz")

  cli::cli_alert_success(
    "All countries processed. Combined data saved to {output_path}")
}


#' Download population rasters for given country codes.
#'
#' This function attempts to download population rasters from WorldPop for the
#' specified country codes. If `dest_files` is not provided, file paths will be
#' generated based on the given country codes and saved into the current
#' project directory (using `here::here()`). It first checks if a local file
#' already exists, and if so, it will skip downloading.
#'
#' The function tries a baseline URL (BSGM) for each country code. If the file
#' is not found there, it then tries a secondary URL (maxar_v1). If neither
#' location provides the file, it returns NA and prompts you to check the
#' WorldPop website directly.
#'
#' @param country_codes A character vector of ISO3 country codes.
#' @param dest_files A character vector of file paths for saving rasters.
#'   If NULL, defaults to "<cc>_ppp_2020_constrained2.tif" in the project dir.
#' @param quiet Logical; if TRUE, suppress status messages.
#'
#' @return Invisibly returns a vector of downloaded file paths
#'  (or NA if not found).
#'
#' @examples
#'# download_pop_rasters(c("DZA", "KEN"))
#' @export
download_pop_rasters <- function(
    country_codes,
    dest_files = here::here("01_data", "1b_rasters", "worldpop_100m"),
    quiet = FALSE) {

  dest_files <- here::here(
    dest_files,
    paste0(tolower(country_codes), "_ppp_2020_constrained.tif")
  )

  if (length(country_codes) != length(dest_files)) {
    stop("country_codes and dest_files must have same length")
  }

  base_url <- paste0(
    "https://data.worldpop.org/GIS/Population/",
    "Global_2000_2020_Constrained/2020/BSGM/"
  )
  base_url_maxar <- paste0(
    "https://data.worldpop.org/GIS/Population/",
    "Global_2000_2020_Constrained/2020/maxar_v1/"
  )

  check_url <- function(url) {
    h <- curl::new_handle(nobody = TRUE, timeout = 600)
    res <- tryCatch(
      curl::curl_fetch_memory(url, handle = h),
      error = function(e) NULL
    )
    !is.null(res) && res$status_code == 200
  }

  out_files <- mapply(function(cc, df) {
    if (file.exists(df)) {
      if (!quiet) {
        cli::cli_alert_info(
          "Raster file already exists: {crayon::blue(basename(df))}"
        )
      }
      return(df)
    }

    cc_lower <- tolower(cc)
    url_bsgm <- here::here(
      base_url, cc, cc_lower,
      "_ppp_2020_constrained.tif"
    )
    url_maxar <- here::here(
      base_url_maxar, cc, cc_lower,
      "_ppp_2020_constrained.tif"
    )

    final_url <- NA_character_
    if (check_url(url_bsgm)) {
      final_url <- url_bsgm
    } else if (check_url(url_maxar)) {
      final_url <- url_maxar
    }

    if (!is.na(final_url)) {
      curl::curl_download(
        url = final_url, destfile = df, quiet = quiet,
        handle = curl::new_handle(timeout = 600)
      )
      df
    } else {
      if (!quiet) {
        cli::cli_alert_warning(
          glue::glue(
            "No file found for {crayon::blue(toupper(cc))}. ",
            "Please download from the WorldPop site."
          )
        )
      }
      NA_character_
    }
  }, country_codes, dest_files, USE.NAMES = FALSE)

  cli::cli_alert_success(
    "Population raster file successfully downloaded to: {.file {dest_files}}"
  )

  invisible(out_files)
}

#' Extract Urban/Rural Extent Raster
#'
#' Extracts the `afurextent.asc` raster file from the package's `inst/extdata`
#' directory to a specified destination.
#'
#' @param dest_dir A character string specifying the directory to save the
#'   extracted raster file.
#' @param overwrite Logical. Whether to overwrite an existing file in the
#'   destination directory. Default is FALSE.
#'
#' @return A character string representing the full path to the extracted raster file.
#' @details
#' This function extracts the `afurextent.asc` file from the package's
#' `extdata` directory, where it is stored as a compressed `.zip` file. It
#' requires the `raster` package to load the raster file.
#'
#' @examples
#' # Extract the raster to a temporary directory
#' # raster_path <- extract_afurextent(tempdir(), overwrite = TRUE)
#' # print(raster_path)
#'
#' @export
extract_afurextent <- function(
    dest_dir = here::here("01_data", "1b_rasters", "urban_extent"),
    overwrite = FALSE) {

  # Ensure the destination directory exists
  if (!dir.exists(dest_dir)) {
    cli::cli_abort(
      glue::glue("Destination directory {dest_dir} does ",
                 "not exist. Please create it first."))
  }

  # Locate the compressed raster file in the package
  raster_zip <- system.file("extdata", "afurextent.asc.zip",
                            package = "AgePopDenom")
  if (raster_zip == "") {
    cli::cli_abort(
      glue::glue("The raster file {raster_zip} ",
                 "is not found in the package."))
  }

  # Define the output path
  raster_file <- file.path(dest_dir, "afurextent.asc")

  # Check if the file already exists
  if (file.exists(raster_file) && !overwrite) {
    cli::cli_alert_info(
      glue::glue("The file already exists at {raster_file} ",
                 "and {overwrite} is FALSE."))
    return(raster_file)
  }

  # Extract the raster file
  cli::cli_alert_info("Extracting raster file to {dest_dir}...")
  utils::unzip(raster_zip, exdir = dest_dir)

  # Validate extraction
  if (!file.exists(raster_file)) {
    cli::cli_abort(
      glue::glue("Extraction failed. The raster file could ",
                 "not be found at {dest_dir}."))
  }


  # Remove any extraneous files in the directory
  dir_files <- list.files(dest_dir, full.names = TRUE)
  extraneous_files <- dir_files[basename(dir_files) != "afurextent.asc"]
  if (length(extraneous_files) > 0) {
    file.remove(extraneous_files)
  }

  cli::cli_alert_success(
    "Raster file successfully extracted to: {raster_file}")
  return(raster_file)
}

#' Download WHO ADM2 Boundaries with Partial Update
#'
#' This function ensures the specified shapefile (`dest_file`) contains
#' boundaries for all requested ISO3 codes (`country_codes`). It dynamically
#' Downloades only the missing codes and appends them to the file, ensuring no
#' duplication.
#'
#' @param country_codes Character vector of ISO3 country codes
#'   (e.g. c("KEN","UGA")).
#' @param dest_file File path where data is saved (default:
#'   "01_data/1c_shapefiles/district_shape.gpkg").
#'
#' @return An `sf` object of combined boundaries for all requested country
#'   codes.
#' @export
download_shapefile <- function(
    country_codes,
    dest_file = here::here("01_data", "1c_shapefiles", "district_shape.gpkg")) {

  # Initialize existing codes if the file exists
  if (file.exists(dest_file)) {
    existing_sf <- sf::st_read(dest_file, quiet = TRUE)
    existing_codes <- unique(existing_sf$country_code)
  } else {
    existing_codes <- character(0)  # No existing codes if file doesn't exist
  }

  # Calculate missing codes
  diff_codes <- setdiff(country_codes, existing_codes)

  # Assign only the missing codes back to `country_codes`
  country_codes <- diff_codes

  if (length(country_codes) == 0) {
    cli::cli_alert_info(
      glue::glue(
        "All requested country codes are already ",
        "in {dest_file}. No updates needed.")
    )
    return(invisible(NULL))
  }

  # Download missing country codes
  codes_str <- paste0("'", paste(country_codes, collapse = "','"), "'")
  where_clause <- paste0("ISO_3_CODE IN (", codes_str, ")")
  base_url <- paste0(
    "https://services.arcgis.com/5T5nSi527N4",
    "F7luB/arcgis/rest/services/Detailed_Boundary_ADM2/",
    "FeatureServer/0"
  )

  cli::cli_alert_info(
    glue::glue(
      "Downloading missing WHO ADM2 data for: ",
      "{paste(country_codes, collapse=', ')}")
  )

  # Download data from the ArcGIS API
  new_sf <- esri2sf::esri2sf(
    url       = base_url,
    where     = where_clause,
    outFields = c("ISO_3_CODE", "ADM0_NAME", "ADM1_NAME",
                  "ADM2_NAME", "ENDDATE"),
    progress  = TRUE
  ) |>
    dplyr::filter(ENDDATE == "253402214400000") |>
    dplyr::transmute(
      country_code = ISO_3_CODE,
      country      = ADM0_NAME,
      region       = ADM1_NAME,
      district     = ADM2_NAME
    )
  # Append new data to the file or create a new one
  if (file.exists(dest_file)) {
    sf::st_write(new_sf, dest_file, append = TRUE, quiet = TRUE)
    cli::cli_alert_success(
      glue::glue(
        "Appended missing country codes to existing ",
        "shapefile: {paste(country_codes, collapse=', ')}"
      )
    )
  } else {
    sf::st_write(new_sf, dest_file, append = FALSE, quiet = TRUE)
    cli::cli_alert_success(
      glue::glue(
        "Created new shapefile with country codes: ",
        "{paste(country_codes, collapse=', ')}"
      )
    )
  }
}

#' Initialize Full Pipeline Script and Model Script
#'
#' This function creates a full pipeline R script and a model C++ script,
#' saving them to the appropriate folders within the project directory
#' structure. The folder structure is created using
#' `AgePopDenom::create_project_structure()`. The R script is automatically
#' opened in RStudio after creation.
#'
#' @param r_script_name Character. The name of the R script file to be created.
#' Defaults to `"full_pipeline.R"`.
#' @param cpp_script_name Character. The name of the C++ script file to be
#' created. Defaults to `"model.cpp"`.
#' @param path Character. The directory in which to create the scripts.
#'   Defaults to `"."`.
#' @param open_r_script Logical. Whether to open the R script automatically in
#'   RStudio (if available). Defaults to `TRUE`.
#'
#' @return A list of character strings with the file paths of the created
#' scripts. The function also generates success messages upon completion.
#'
#' @examples
#' # Create the full pipeline script and model script
#' # init()
#'
#' @export
init <- function(r_script_name = "full_pipeline.R",
                 cpp_script_name = "model.cpp",
                 path = here::here(),
                 open_r_script = TRUE) {

  # Ensure the project structure is created
  AgePopDenom::create_project_structure(base_path = path)

  # Define the script directories
  r_script_dir <- file.path(path, "02_scripts")
  cpp_script_dir <- r_script_dir

  # Create the C++ folder if not already created by the project structure
  if (!dir.exists(cpp_script_dir)) {
    dir.create(cpp_script_dir, recursive = TRUE)
  }

  # Define the R script content
  r_script_content <- '
# set up country of interest
cntry_codes = c("GMB", "COM")

# Gather and process datasets --------------------------------------------------

# Download DHS datasets
AgePopDenom::download_dhs_datasets(
  country_codes = cntry_codes,
  email = "my_email@exmaple.com",
  project = "My populaiton denom project")

# Process DHS datasets
AgePopDenom::process_dhs_data()

# Download shapefiles
AgePopDenom::download_shapefile(cntry_codes)

# Download population rasters from worldpop
AgePopDenom::download_pop_rasters(cntry_codes)

# Extract urban extent raster
AgePopDenom::extract_afurextent()

# Run models and get outputs ---------------------------------------------------

# Run the model
AgePopDenom::run_models_with_logging(cntry_codes)

# Compile all the model param data for
AgePopDenom::extract_age_param()

# Now get final age-structured population data
AgePopDenom::process_final_population_data()
'

  # Define the C++ script content
  cpp_script_content <- '
#include <TMB.hpp>

// Covariance computation function
template<class Type>
matrix<Type> compute_cov_init(Type gamma, Type sigma2, Type phi, Type tau2_1,
                              matrix<Type> dist_matrix,
                              vector<Type> b1, vector<Type> c, vector<Type> b2){
  int n_x = dist_matrix.rows();

  // Compute the covariance scaling
  matrix<Type> sigma_s = sigma2 * (-dist_matrix / phi).array().exp().matrix();

  Type tau2_2 = tau2_1;
  Type sqrt_tau = sqrt(tau2_1 * tau2_2);

  // Initialize the full covariance matrix
  matrix<Type> M(2 * n_x, 2 * n_x);
  M.setZero();

  // Block [1,1]: sigma_s + diag(b1) * tau2_1
  M.block(0, 0, n_x, n_x) = sigma_s;
  for (int i = 0; i < n_x; i++) {
    M(i, i) += tau2_1 * b1(i);
  }

  // Block [1,2] and [2,1]: gamma * sigma_s + diag(c) * sqrt_tau
  matrix<Type> cross_sigma_s = gamma * sigma_s;
  for (int i = 0; i < n_x; i++) {
    cross_sigma_s(i, i) += c(i) * sqrt_tau;
  }
  M.block(0, n_x, n_x, n_x) = cross_sigma_s;
  M.block(n_x, 0, n_x, n_x) = cross_sigma_s.transpose();

  // Block [2,2]: (gamma^2) * sigma_s + diag(b2) * tau2_2
  M.block(n_x, n_x, n_x, n_x) = pow(gamma, 2) * sigma_s;
  for (int i = 0; i < n_x; i++) {
    M(i + n_x, i + n_x) += tau2_2 * b2(i);
  }

  // Add small jitter for numerical stability
  M.diagonal().array() += 1e-6;

  return M;
}

// Objective function
template<class Type>
Type objective_function<Type>::operator() () {
  // INPUT DATA ----------------------------------------------------------------
  DATA_MATRIX(design_scale);   // Design matrix for scale
  DATA_MATRIX(design_shape);   // Design matrix for shape
  DATA_VECTOR(y);              // Response vector
  DATA_MATRIX(dist_matrix);    // Distance matrix
  DATA_VECTOR(b1);             // Diagonal components for scale
  DATA_VECTOR(c);              // Shared diagonal components
  DATA_VECTOR(b2);             // Diagonal components for shape

  // PARAMETERS ----------------------------------------------------------------
  PARAMETER_VECTOR(beta1);     // Regression coefficients for scale
  PARAMETER_VECTOR(beta2);     // Regression coefficients for shape
  PARAMETER(gamma);            // Cross-covariance scaling factor
  PARAMETER(log_sigma2);       // Log-transformed variance parameter
  PARAMETER(log_phi);          // Log-transformed spatial decay
  PARAMETER(log_tau2_1);       // Log-transformed variance for the process

  // TRANSFORM PARAMETERS -----------------------------------------------------
  Type sigma2 = exp(log_sigma2);
  Type tau2_1 = exp(log_tau2_1);
  Type phi = exp(log_phi);

  // LINEAR PREDICTOR ---------------------------------------------------------
  vector<Type> mu(design_scale.rows() + design_shape.rows());
  mu.head(design_scale.rows()) = design_scale * beta1;
  mu.tail(design_shape.rows()) = design_shape * beta2;

  // COVARIANCE MATRIX --------------------------------------------------------
  matrix<Type> M = compute_cov_init(gamma, sigma2, phi, tau2_1,
                                    dist_matrix, b1, c, b2);

  // NEGATIVE LOG-LIKELIHOOD --------------------------------------------------
  using namespace density;
  MVNORM_t<Type> neg_log_dmvnorm(M);
  parallel_accumulator<Type> nll(this);  // Parallelize likelihood computation
  nll += neg_log_dmvnorm(y - mu);

  // Subtract the constant term to match the R code
  Type n = y.size();
  Type log2pi = log(2.0 * M_PI);
  Type const_term = 0.5 * n * log2pi;
  nll -= const_term;

  return nll;  // Return the negative log-likelihood
}
'

# Write the R script
r_script_path <- file.path(r_script_dir, r_script_name)
writeLines(r_script_content, r_script_path)

# Write the C++ script
cpp_script_path <- file.path(cpp_script_dir, cpp_script_name)
writeLines(cpp_script_content, cpp_script_path)

# Open the R script in RStudio (if available)
if (open_r_script && requireNamespace("rstudioapi", quietly = TRUE)) {
  rstudioapi::navigateToFile(r_script_path)
} else {
  cli::cli_alert_info(
    "Scripts created but could not open automatically: RStudio not available.")
}

# Return success messages
cli::cli_alert_success(
  "R script '{r_script_path}' successfully created and opened.")
cli::cli_alert_success(
  "C++ script '{cpp_script_path}' successfully created.")

}
