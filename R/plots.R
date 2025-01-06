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
#' @param break_axis_by break axis to show less clutterd age groups.  Default
#'   is 10
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
                                      fill_low = "#a50f15",
                                      break_axis_by = 10) {

  # Validate required columns --------------------------------------------------
  required_cols <- c("country", "region", "popsize")
  if (!all(required_cols %in% names(dataset))) {
    stop("Dataset must contain the following columns: ",
         paste(required_cols, collapse = ", "))
  }

  # Process the dataset for age structure --------------------------------------

  age_struct <- dataset |>
    dplyr::select(
      country, region, district,
      dplyr::contains("mean")
    ) |>
    dplyr::select(-dplyr::contains("plus")) |>
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        ., c("_mean" = "", "plus" = "+y")
      ),
      dplyr::contains("mean")
    ) |>
    tidyr::pivot_longer(
      cols = -c(country, region, district),
      names_to = "age_group",
      values_to = "population"
    ) |>
    dplyr::mutate(
      age_group = stringr::str_remove(age_group, "_mean"),
      age_group = stringr::str_replace_all(
        age_group, c("_" = "-", "to" = "-", " " = "")),
      region = stringr::str_to_title(region),
      region = stringr::str_remove(region, " County"))

  # get levels
  levels_age <- unique(age_struct$age_group)

  age_struct <- age_struct |>
    dplyr::group_by(country, region, age_group) |>
    dplyr::summarise(
      tot_pop = sum(population, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    dplyr::filter(!is.na(age_group)) |>
    dplyr::mutate(
      age_group = factor(age_group, levels = levels_age)
    )

  # Calculate total population for the country ---------------------------------

  total_pop <- sum(age_struct$tot_pop, na.rm = TRUE) |>
    round() |> format(big.mark = ",")

  # Generate the plot ----------------------------------------------------------

  # Dynamically generate breaks for the x-axis
  x_breaks <- levels(age_struct$age_group)
  x_break_labels <- levels(age_struct$age_group)[seq(
    1, length(x_breaks), by = break_axis_by)]

  # x_break_labels <-   sub("-.*", "", x_break_labels)

  plot <- age_struct |>
    ggplot2::ggplot(
      ggplot2::aes(x = age_group, y = tot_pop, fill = as.numeric(age_group))
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      color = line_color,
      linewidth = 0.4,
      position = "identity",
      width = 1
    ) +
    ggplot2::scale_y_continuous(
      labels = \(x) format(x, scientific = FALSE, big.mark = ",")
    ) +
    ggplot2::scale_x_discrete(
      breaks = x_break_labels,
      labels = x_break_labels
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
    ggplot2::scale_fill_gradient(high = fill_high, low = fill_low) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(size = 15, face = "bold")
    )

  # Save the plot --------------------------------------------------------------
  output_file <- file.path(
    output_dir, glue::glue("{country_code}_age_pyramid.png")
  )

  ggplot2::ggsave(
    output_file, plot = plot, width = 14,
    height = 14, dpi = 500, scale = 1, device = "png"
  )

  cli::cli_alert_success("Age pyramid plot saved to {output_file}")

  return(plot)
}
