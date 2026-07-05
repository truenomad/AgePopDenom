# Extract Urban/Rural Extent Raster

Extracts the \`afurextent.asc\` raster file from the package's
\`inst/extdata\` directory to a specified destination.

## Usage

``` r
extract_afurextent(
  dest_dir = here::here("01_data", "1b_rasters", "urban_extent"),
  overwrite = FALSE
)
```

## Arguments

- dest_dir:

  A character string specifying the directory to save the extracted
  raster file.

- overwrite:

  Logical. Whether to overwrite an existing file in the destination
  directory. Default is FALSE.

## Value

A character string representing the full path to the extracted raster
file.

## Details

This function extracts the \`afurextent.asc\` file from the package's
\`extdata\` directory, where it is stored as a compressed \`.zip\` file.
It requires the \`raster\` package to load the raster file.

## Examples

``` r
# \donttest{
 extract_afurextent(tempdir(), overwrite = TRUE)
#> ℹ Extracting raster file to /tmp/Rtmperi88o...
#> Warning: cannot remove file '/tmp/Rtmperi88o/__MACOSX', reason 'Directory not empty'
#> Warning: cannot remove file '/tmp/Rtmperi88o/bslib-71d7f13118c36706c39339f77436fb7b', reason 'Directory not empty'
#> Warning: cannot remove file '/tmp/Rtmperi88o/downlit', reason 'Directory not empty'
#> Warning: cannot remove file '/tmp/Rtmperi88o/test_env', reason 'Directory not empty'
#> ✔ Raster file successfully extracted to: /tmp/Rtmperi88o/afurextent.asc
#> [1] "/tmp/Rtmperi88o/afurextent.asc"
# }
```
