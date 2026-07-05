# Generate Age Population Raster

Creates age-stratified population raster layers from predictor data and
gamma distribution parameters. Supports parallel processing and caching
of results. The output is a multi-layer raster stack with each layer
representing the population proportion for a specific age interval.

## Usage

``` r
generate_age_pop_raster(
  predictor_data,
  scale_pred,
  shape_pred,
  age_range = c(0, 10),
  age_interval = 1,
  country_code,
  ignore_cache = FALSE,
  output_dir,
  n_cores = max(1, parallel::detectCores() - 2, na.rm = TRUE)
)
```

## Arguments

- predictor_data:

  Data frame containing population and spatial data with columns:
  country, region, district, pop, web_x, web_y

- scale_pred:

  Matrix of scale parameters for gamma distribution predictions

- shape_pred:

  Matrix of shape parameters for gamma distribution predictions

- age_range:

  Numeric vector of length 2 specifying min and max ages, default
  c(0,99)

- age_interval:

  Numeric interval size between age groups in years, default 1

- country_code:

  Character ISO3 country code

- ignore_cache:

  Logical whether to ignore cached results, default FALSE

- output_dir:

  Character path to output directory

- n_cores:

  Integer number of cores for parallel processing, default max(1,
  detectCores()-2)

## Value

SpatRaster object (terra package) containing multiple layers, where each
layer represents the population proportion for an age interval. Layer
names indicate the age range (e.g., "Age 0 to 1 years"). The raster uses
EPSG:3857 projection with 5000m resolution.

## Details

The function processes age intervals sequentially, computing population
proportions using parallel processing. Results are cached as a GeoTIFF
file for future use. The output raster maintains spatial properties of
the input data and is suitable for GIS analysis and visualization.

## Examples

``` r
# \donttest{
predictor_data <- data.frame(
 country = rep("CountryX", 100),
 region = rep("RegionA", 100),
 district = rep("District1", 100),
 pop = sample(100:1000, 100, replace = TRUE),
 web_x = runif(100, -100, 100),
 web_y = runif(100, -50, 50)
)

scale_pred <- matrix(runif(100 * 10, 1, 5), nrow = 100, ncol = 10)
shape_pred <- matrix(runif(100 * 10, 1, 5), nrow = 100, ncol = 10)

res <- generate_age_pop_raster(predictor_data,
                       scale_pred,
                       shape_pred,
                       country_code = "COD",
                       output_dir = file.path(tempdir()),
                       n_cores = 1)
#> ℹ Processing interval 1/11...
#> ✔ Completed interval 1/11.
#> 
#> ℹ Processing interval 2/11...
#> ✔ Completed interval 2/11.
#> 
#> ℹ Processing interval 3/11...
#> ✔ Completed interval 3/11.
#> 
#> ℹ Processing interval 4/11...
#> ✔ Completed interval 4/11.
#> 
#> ℹ Processing interval 5/11...
#> ✔ Completed interval 5/11.
#> 
#> ℹ Processing interval 6/11...
#> ✔ Completed interval 6/11.
#> 
#> ℹ Processing interval 7/11...
#> ✔ Completed interval 7/11.
#> 
#> ℹ Processing interval 8/11...
#> ✔ Completed interval 8/11.
#> 
#> ℹ Processing interval 9/11...
#> ✔ Completed interval 9/11.
#> 
#> ℹ Processing interval 10/11...
#> ✔ Completed interval 10/11.
#> 
#> ℹ Processing interval 11/11...
#> ✔ Completed interval 11/11.
#> 
#> ✔ Raster stack saved to /tmp/Rtmperi88o/cod_age_pop_grid_0_10_yrs_by_1yrs.tif
# }
```
