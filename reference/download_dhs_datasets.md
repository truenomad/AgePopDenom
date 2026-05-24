# Download WHO ADM2 Boundaries with Partial Updateads the latest PR (household) and GE (geographic) DHS datasets for specified countries.

Download WHO ADM2 Boundaries with Partial Updateads the latest PR
(household) and GE (geographic) DHS datasets for specified countries.

## Usage

``` r
download_dhs_datasets(
  country_codes,
  cache_path = here::here("01_data", "1a_survey_data", "raw"),
  output_dir_root = here::here("01_data", "1a_survey_data", "raw"),
  survey_id = NULL,
  email,
  project,
  verbose = TRUE,
  clear_cache = TRUE
)
```

## Arguments

- country_codes:

  A character vector of ISO3 country codes.

- cache_path:

  A character string specifying the cache path for RDHS.

- output_dir_root:

  A character string specifying the root directory for output.

- survey_id:

  A character vector of survey IDs. If NULL, uses latest survey.

- email:

  A character string. Email registered with DHS.

- project:

  A character string. Project name as registered with DHS.

- verbose:

  Logical for rdhs setup and messages to be printed.

- clear_cache:

  Logical whether to clear cache before downloading.

## Value

Invisibly returns a list of downloaded dataset filenames.
