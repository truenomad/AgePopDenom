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
#> ℹ Extracting raster file to /tmp/Rtmp5XisFS...
#> Warning: cannot remove file '/tmp/Rtmp5XisFS/__MACOSX', reason 'Directory not empty'
#> Warning: cannot remove file '/tmp/Rtmp5XisFS/bslib-e9b2b13fa612f50d23e4850d93d60d01', reason 'Directory not empty'
#> Warning: cannot remove file '/tmp/Rtmp5XisFS/downlit', reason 'Directory not empty'
#> Warning: cannot remove file '/tmp/Rtmp5XisFS/test_env', reason 'Directory not empty'
#> ✔ Raster file successfully extracted to: /tmp/Rtmp5XisFS/afurextent.asc
#> [1] "/tmp/Rtmp5XisFS/afurextent.asc"
# }
```
