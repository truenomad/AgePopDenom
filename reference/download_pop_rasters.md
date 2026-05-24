# Download population rasters for given country codes.

This function attempts to download population rasters from WorldPop for
the specified country codes. If \`dest_dir\` is not provided, file paths
will be generated based on the given country codes and saved into the
current project directory (using \`here::here()\`). It first checks if a
local file already exists, and if so, it will skip downloading.

## Usage

``` r
download_pop_rasters(
  country_codes,
  dest_dir = here::here("01_data", "1b_rasters", "pop_raster"),
  quiet = FALSE
)
```

## Arguments

- country_codes:

  A character vector of ISO3 country codes.

- dest_dir:

  A character vector of file paths for saving rasters. If NULL, defaults
  to "\<cc\>\_ppp_2020_constrained2.tif" in the project dir.

- quiet:

  Logical; if TRUE, suppress status messages.

## Value

Invisibly returns a vector of downloaded file paths (or NA if not
found).

## Details

The function tries a baseline URL (BSGM) for each country code. If the
file is not found there, it then tries a secondary URL (maxar_v1). If
neither location provides the file, it returns NA and prompts you to
check the WorldPop website directly.

## Examples

``` r
# \donttest{
download_pop_rasters(country_codes = "COM", dest_dir = tempdir())
#> ✔ Population raster files successfully processed.
# }
```
