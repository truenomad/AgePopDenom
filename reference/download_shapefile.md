# Download WHO ADM2 Boundaries with Partial Update

This function ensures the specified shapefile (\`dest_file\`) contains
boundaries for all requested ISO3 codes (\`country_codes\`). It
dynamically Downloades only the missing codes and appends them to the
file, ensuring no duplication.

## Usage

``` r
download_shapefile(
  country_codes,
  dest_file = here::here("01_data", "1c_shapefiles", "district_shape.gpkg")
)
```

## Arguments

- country_codes:

  Character vector of ISO3 country codes (e.g. c("KEN","UGA")).

- dest_file:

  File path where data is saved (default:
  "01_data/1c_shapefiles/district_shape.gpkg").

## Value

An \`sf\` object of combined boundaries for all requested country codes.

## Examples

``` r

# \donttest{

tf <- file.path(tempdir(), "test_env")

# Download population rasters from worldpop
download_shapefile(
  country_codes = "COM",
  dest_file = here::here(tf, "district_shape.gpkg")
)
#> ℹ Downloading missing WHO ADM2 data for: COM
#> ✔ Created new shapefile with country codes: COM
# }
```
