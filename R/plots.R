#' Rasterize Spatial Data
#'
#' This function converts spatial data with x, y coordinates and a value field
#' into a raster using a specified resolution and CRS.
#'
#' @param x_coords Numeric vector of x-coordinates (e.g., longitude).
#' @param y_coords Numeric vector of y-coordinates (e.g., latitude).
#' @param values Numeric vector of values associated with each point.
#' @param cell_size Numeric. Grid cell size in meters (default: 5000).
#' @param crs Character, the coordinate reference system in EPSG format
#'            (e.g., "EPSG:3857").
#' @param fun Function to aggregate values in cells (default is `mean`).
#'
#' @return A `terra::SpatRaster` object.
#' @examples
#' # rast <- rasterize_data(
#' #   predictor_data$web_x, predictor_data$web_y, pred_list$shape_hat,
#' #   cell_size = 5000, crs = "EPSG:3857"
#' # )
#' @export
rasterize_data <- function(x_coords, y_coords, values,
                           cell_size = 5000, crs, fun = mean) {
  # Combine inputs into a data frame
  spatial_data <- data.frame(x = x_coords, y = y_coords, value = values)

  # Create a SpatVector
  spat_vector <- terra::vect(spatial_data, geom = c("x", "y"), crs = crs)

  # Create a raster template with the correct extent and resolution
  raster_template <- terra::rast(spat_vector,
                                 resolution = cell_size, crs = crs
  )

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
#' @param save_raster A logical input specifying whether to save output or not.
#'   Default is TRUE.
#' @param file_name_suffix A string specifying the suffix for the file name
#'   (default is "gamma_prediction_rasters").
#' @param width Numeric. Width of output plot in pixels (default: 2500).
#' @param height Numeric. Height of output plot in pixels (default: 2000).
#' @param png_resolution An integer specifying the resolution of the plot in DPI
#'   (default: 300).
#' @return The path to the saved raster plot.
#'
#' @examples
#' # raster_path <- generate_gamma_raster_plot(
#' #   predictor_data = predictor_data,
#' #   pred_list = pred_list,
#' #   country_code = "ken",
#' #   output_dir = "03_outputs/3b_visualizations"
#' # )
#'
#' @export
generate_gamma_raster_plot <- function(predictor_data,
                                       pred_list,
                                       country_code,
                                       output_dir,
                                       save_raster = TRUE,
                                       file_name_suffix = "gamma_prediction_rasters",
                                       width = 2500,
                                       height = 2000,
                                       png_resolution = 300) {
  rast_shape <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$shape_hat,
    cell_size = 5000,
    crs = "EPSG:3857"
  )

  rast_scale <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$scale_hat,
    cell_size = 5000,
    crs = "EPSG:3857"
  )

  rast_mean_age <- rasterize_data(
    predictor_data$web_x,
    predictor_data$web_y,
    pred_list$mean_age_pred,
    cell_size = 5000,
    crs = "EPSG:3857"
  )

  # Combine rasters into a stack
  raster_stack <- c(rast_shape, rast_scale, rast_mean_age)

  # Set names for visualization
  names(raster_stack) <- c(
    "Shape Parameter",
    "Scale Parameter",
    "Mean Age Prediction"
  )

  if (save_raster) {
    # Define output path
    output_file <- file.path(
      output_dir,
      glue::glue("{country_code}_{file_name_suffix}.png")
    )

    # Create output directory if it doesn't exist
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save the plot
    png(output_file, width = width, height = height, res = png_resolution)

    # Adjust layout and spacing
    par(
      mfrow = c(2, 2), # Arrange plots in 2 rows and 2 columns
      mar = c(5, 5, 4, 2) + 0.1, # Margins for axis labels
      oma = c(0, 0, 2, 0) # Outer margins for titles
    )

    # Plot each raster with clear labeling
    terra::plot(rast_scale,
                main = "Scale Parameter", cex.main = 1.2,
                cex.axis = 0.8, cex.lab = 1.2
    )
    terra::plot(rast_shape,
                main = "Shape Parameter", cex.main = 1.2,
                cex.axis = 0.8, cex.lab = 1.2
    )
    # Skip the third position (top right) by plotting in fourth position
    par(mfg = c(2, 1)) # Move to bottom left position
    terra::plot(rast_mean_age,
                main = "Mean Age Prediction", cex.main = 1.2,
                cex.axis = 0.8, cex.lab = 1.2
    )

    dev.off()

    cli::cli_alert_success("Raster plot saved to {output_file}")
  }
}

#' Generate and Save Age Pyramid Plot
#'
#' This function processes an input dataset to compute age distribution,
#' generates age pyramid plots by region showing both proportions and counts,
#' and saves the plots to a specified directory.
#'
#' @param dataset A list containing two data frames:
#'   - prop_df: Population proportions data frame
#'   - pop_df: Population counts data frame
#'   Each with columns for `country`, `region`, `district`, and columns ending
#'   with "mean"
#' @param country_code A string representing the country code (e.g., "ken").
#' @param output_dir A string specifying the directory where plots should be saved.
#' @param line_color A string specifying the color of the plot's lines. Default
#'   is `"#67000d"`.
#' @param fill_high A string specifying the fill color for high values. Default
#'   is `"#fee0d2"`.
#' @param fill_low A string specifying the fill color for low values. Default
#'   is `"#a50f15"`
#' @param break_axis_by break axis to show less cluttered age groups. Default
#'   is 10
#' @return A list containing both proportion and count plots.
#'
#' @examples
#' # generate_age_pyramid_plot(
#' #  dataset = list(prop_df = prop_results, pop_df = pop_results),
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

  # Validate inputs ------------------------------------------------------------
  if (!all(c("prop_df", "pop_df") %in% names(dataset))) {
    stop("Dataset must be a list containing 'prop_df' and 'pop_df'")
  }

  required_cols <- c("country", "region", "popsize")
  for (df in list(dataset$prop_df, dataset$pop_df)) {
    if (!all(required_cols %in% names(df))) {
      stop("Each dataset must contain: ",
           paste(required_cols, collapse = ", "))
    }
  }

  # Process datasets for age structure
  process_age_data <- function(df, type = "count") {
    df |>
      dplyr::select(
        country, region, district,
        dplyr::contains("mean")
      ) |>
      dplyr::rename_with(
        ~ stringr::str_replace_all(
          ., c("_mean" = "", "_pop" = "", "_prop" = "",
               "plus" = "+y")
        ),
        dplyr::contains("mean")
      ) |>
      tidyr::pivot_longer(
        cols = -c(country, region, district),
        names_to = "age_group",
        values_to = ifelse(type == "count", "population", "proportion")
      ) |>
      dplyr::mutate(
        age_group = stringr::str_remove(age_group, "_mean"),
        age_group = stringr::str_replace_all(
          age_group, c("_" = "-", "to" = "-", " " = "")),
        region = stringr::str_to_title(region),
        region = stringr::str_remove(region, " County"))


  }

  age_struct_pop <- process_age_data(dataset$pop_df, "count")
  age_struct_prop <- process_age_data(dataset$prop_df, "prop")

  # Get common age group levels
  levels_age <- unique(age_struct_pop$age_group)

  # Aggregate data by region
  summarize_data <- function(data, value_col) {
    data |>
      dplyr::group_by(country, region, age_group) |>
      dplyr::summarise(
        total = sum(.data[[value_col]], na.rm = TRUE),
        .groups = 'drop'
      ) |>
      dplyr::filter(!is.na(age_group)) |>
      dplyr::mutate(
        age_group = factor(age_group, levels = levels_age)
      )
  }

  pop_by_region <- summarize_data(age_struct_pop, "population")
  prop_by_region <- summarize_data(age_struct_prop, "proportion")

  # Generate plots
  create_pyramid <- function(data, value_type, total_label) {

    x_break_labels <- levels(data$age_group)[
      seq(1, length(levels(data$age_group)), by = break_axis_by)]

    data |>
      dplyr::filter(age_group != "99+y") |>
      ggplot2::ggplot(
        ggplot2::aes(x = age_group, y = total, fill = as.numeric(age_group))
      ) +
      ggplot2::geom_bar(
        stat = "identity",
        color = line_color,
        linewidth = 0.4,
        position = "identity",
        width = 1
      ) +
      ggplot2::scale_y_continuous(
        labels = \(x) {
          if (value_type == "count") {
            format(x, scientific = FALSE, big.mark = ",")
          } else {
            scales::percent(x/100, accuracy = .1)
          }
        }
      ) +
      ggplot2::scale_x_discrete(
        breaks = x_break_labels,
        labels = x_break_labels
      ) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~region) +
      ggplot2::labs(
        title = glue::glue(
          "Age Pyramid by Region ({value_type})",
          "\n{stringr::str_to_title(data$country[1])} {total_label}"
        ),
        x = "Age Group \n",
        y = ifelse(value_type == "count",
                  "\n Population",
                  "\n Proportion of Population"),
        fill = "Region",
        caption =
          "Note: Total population includes ages 99+, pyramid shows ages 0-99"
      ) +
      ggplot2::scale_fill_gradient(high = fill_high, low = fill_low) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold"),
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        plot.caption = ggplot2::element_text(
          hjust = 1,
          vjust = 1,
          face = "italic",
          size = 10,
          margin = ggplot2::margin(t = 10)
        ),
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
      )

  }

  # Calculate totals for labels
  total_pop <- sum(pop_by_region$total, na.rm = TRUE) |>
    round() |> format(big.mark = ",")

  # Create both plots
  pop_plot <- create_pyramid(pop_by_region, "count",
                             glue::glue("(N = {total_pop})"))
  prop_plot <- create_pyramid(prop_by_region, "proportion", "")

  # Save plots
  for (plot_type in c("count", "prop")) {
    output_file <- file.path(
      output_dir,
      glue::glue("{country_code}_age_pyramid_{plot_type}.png")
    )

    plot_to_save <- if(plot_type == "count") pop_plot else prop_plot

    ggplot2::ggsave(
      output_file,
      plot = plot_to_save,
      width = 14,
      height = 14,
      dpi = 500,
      scale = 1,
      device = "png"
    )

    cli::cli_alert_success("Age pyramid {plot_type} plot saved to {output_file}")
  }

  return(list(
    count_plot = pop_plot,
    prop_plot = prop_plot
  ))
}
