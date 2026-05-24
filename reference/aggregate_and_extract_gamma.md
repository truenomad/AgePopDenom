# Aggregate Individual Survey Data and Extract Gamma Parameters by Location

This script aggregates the individual Survey data to location level and
extracts the gamma parameters for the locations.

## Usage

``` r
aggregate_and_extract_gamma(
  data,
  lat_column = "lat",
  long_column = "long",
  age_column = "ageyrs",
  urban_column = "urban"
)
```

## Arguments

- data:

  Data frame containing individual-level age data with coordinates and
  urban/rural classification.

- lat_column:

  Column name for latitude coordinates (default: "lat")

- long_column:

  Column name for longitude coordinates (default: "long")

- age_column:

  Column name for age values (default: "ageyrs")

- urban_column:

  Column name for urban/rural classification (default: "urban")

## Value

List containing:

- outlier_free_data: Original data with added spatial coordinates,
  outliers removed, and clusters with less than 10 samples removed

- age_param_data: Location-level gamma parameters with columns: lon,
  lat, web_x, web_y, log_scale, log_shape, urban, b1, c, b2, nsampled
