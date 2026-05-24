# Rasterize Spatial Data

This function converts spatial data with x, y coordinates and a value
field into a raster using a specified resolution and CRS.

## Usage

``` r
rasterize_data(x_coords, y_coords, values, cell_size = 5000, crs, fun = mean)
```

## Arguments

- x_coords:

  Numeric vector of x-coordinates (e.g., longitude).

- y_coords:

  Numeric vector of y-coordinates (e.g., latitude).

- values:

  Numeric vector of values associated with each point.

- cell_size:

  Numeric. Grid cell size in meters (default: 5000).

- crs:

  Character, the coordinate reference system in EPSG format (e.g.,
  "EPSG:3857").

- fun:

  Function to aggregate values in cells (default is \`mean\`).

## Value

A \`terra::SpatRaster\` object.

## Examples

``` r

# \donttest{
x_coords <- runif(100, -100, 100)
y_coords <- runif(100, -50, 50)
values <- rnorm(100, mean = 10, sd = 5)

rasterize_data(x_coords, y_coords, values,
               cell_size = 5000, crs = "EPSG:3857", fun = mean)
#> class       : SpatRaster
#> size        : 1, 1, 1  (nrow, ncol, nlyr)
#> resolution  : 5000, 5000  (x, y)
#> extent      : -97.90658, 4902.093, -49.36992, 4950.63  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857)
#> source(s)   : memory
#> name        :      mean
#> min value   : 10.075716
#> max value   : 10.075716
# }
```
