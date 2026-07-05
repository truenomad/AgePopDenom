# Generate Variogram Plot

Creates a variogram plot showing the spatial dependence structure of the
data. The plot includes both the empirical variogram points and the
fitted theoretical variogram line. The empirical variogram points show
the actual semivariance values at different distances, while the red
line shows the fitted exponential variogram model.

## Usage

``` r
generate_variogram_plot(
  age_param_data,
  fit_vario,
  country_code,
  scale_outcome = "log_scale",
  output_dir,
  width = 12,
  height = 9,
  png_resolution = 300
)
```

## Arguments

- age_param_data:

  Data frame containing the age parameter data. Must include columns
  'web_x' and 'web_y' for spatial coordinates and the response variable
  specified in scale_outcome.

- fit_vario:

  Fitted variogram object from automap package. Should contain
  components \$psill (partial sill) and \$range (range parameter) for
  the exponential variogram model.

- country_code:

  Character string of the country code (e.g. "TZA") used for plot title
  and output filename.

- scale_outcome:

  Character string specifying the column name for the scale parameter
  response variable (default: "log_scale"). This is the variable for
  which the variogram is computed.

- output_dir:

  Character string specifying the directory path where the plot will be
  saved as a PNG file.

- width:

  Plot width in pixels (default: 2000). Controls the output image width.

- height:

  Plot height in pixels (default: 1500). Controls the output image
  height.

- png_resolution:

  PNG resolution in DPI (dots per inch, default: 300). Higher values
  create larger, higher quality images.

## Value

Invisibly returns the ggplot object containing the variogram plot. The
plot is also saved as a PNG file in the specified output directory.

## Details

The function creates a variogram plot with the following elements: -
Points showing empirical semivariance values at different distances - A
red line showing the fitted exponential variogram model - Clear axis
labels and title - Comma-formatted distance values on x-axis - Clean
theme with black and white style

The output filename is constructed as lowercase country code +
"\_variogram.png"

## Examples

``` r
# \donttest{
set.seed(123)  # For reproducibility
age_param_data <- data.frame(
  country = rep("TZA", 100),
  web_x = runif(100, 0, 100),
  web_y = runif(100, 0, 100),
  log_scale = rnorm(100, mean = 5, sd = 2)
)

# Create a dummy fitted variogram object
fit_vario <- list(
  psill = c(0.1, 0.5),
  range = c(0, 50)
)

vario_plot <- generate_variogram_plot(
   age_param_data = age_param_data,
   fit_vario = fit_vario,
   country_code = "TZA",
   output_dir = file.path(tempdir()))
#> ✔ Variogram saved to /tmp/RtmpNUOBYi/tza_variogram.png
# }
```
