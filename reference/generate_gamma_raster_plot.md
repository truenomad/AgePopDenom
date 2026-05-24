# Generate and Save Raster Plot for Gamma Predictions

This function creates rasters from prediction data, combines them into a
stack, and saves the faceted plot to a specified file.

## Usage

``` r
generate_gamma_raster_plot(
  predictor_data,
  pred_list,
  country_code,
  output_dir,
  save_raster = TRUE,
  file_name_suffix = "gamma_prediction_rasters",
  width = 2500,
  height = 2000,
  png_resolution = 300
)
```

## Arguments

- predictor_data:

  A data frame containing \`web_x\` and \`web_y\` coordinates and
  associated prediction values.

- pred_list:

  A list containing predictions (\`shape_hat\`, \`scale_hat\`,
  \`mean_age_pred\`) for creating rasters.

- country_code:

  A string representing the lowercase country code, used for naming the
  output file.

- output_dir:

  A string specifying the directory where the plot should be saved.

- save_raster:

  A logical input specifying whether to save output or not. Default is
  TRUE.

- file_name_suffix:

  A string specifying the suffix for the file name (default is
  "gamma_prediction_rasters").

- width:

  Numeric. Width of output plot in pixels (default: 2500).

- height:

  Numeric. Height of output plot in pixels (default: 2000).

- png_resolution:

  An integer specifying the resolution of the plot in DPI (default:
  300).

## Value

The path to the saved raster plot.

## Examples

``` r

# \donttest{
predictor_data <- data.frame(
  web_x = runif(100, -100, 100),
  web_y = runif(100, -50, 50)
)

pred_list <- list(
  shape_hat = rnorm(100, mean = 2, sd = 0.5),
  scale_hat = rnorm(100, mean = 10, sd = 2),
  mean_age_pred = rnorm(100, mean = 30, sd = 5)
)

generate_gamma_raster_plot(predictor_data,
                           pred_list,
                           country_code = "COD",
                           output_dir = file.path(tempdir()))
#> ✔ Raster plot saved to /tmp/RtmpTS1jsq/COD_gamma_prediction_rasters.png
# }
```
