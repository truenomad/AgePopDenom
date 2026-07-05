# Process DHS Data: Merge RDS Files with Shapefiles and Extract Gamma Parameters

This function processes DHS (Demographic and Health Survey) data by: 1.
Reading RDS files and shapefiles for each country. 2. Merging
demographic data with geographic information. 3. Cleaning and
aggregating the data. 4. Extracting gamma parameters for age-related
analysis.

## Usage

``` r
process_dhs_data(
  rds_dir = here::here("01_data", "1a_survey_data", "raw", "pr_records"),
  shp_dir = here::here("01_data", "1a_survey_data", "raw", "shapefiles"),
  output_path = here::here("01_data", "1a_survey_data", "processed",
    "dhs_pr_records_combined.rds")
)
```

## Arguments

- rds_dir:

  Character. Path to the directory containing raw RDS files.

- shp_dir:

  Character. Path to the directory containing shapefiles.

- output_path:

  Character. Path to save the final processed dataset as an RDS file.

## Value

None. Saves the final combined dataset to the specified output path.

## Details

The function loops through RDS files, processes each country's data by
merging demographic information with shapefile data, and computes gamma
parameters for age-related analysis. The progress is tracked and
displayed for each country.

The function also filters out incomplete data (e.g., age values of
\`98\`) and handles labelled data using the \`haven::zap_labels\`
function.

The final output includes two datasets: 1. Outlier-free data. 2.
Aggregated age parameter data.

## Examples

``` r
# \donttest{
tf <- file.path(tempdir(), "test_env")
dir.create(tf, recursive = TRUE, showWarnings = FALSE)
tmp_rds_dir <- file.path(tf, "rds")
tmp_shp_dir <- file.path(tf, "shp")
tmp_output <- file.path(tf, "output.rds")

dir.create(tmp_rds_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_shp_dir, recursive = TRUE, showWarnings = FALSE)

# Create fake DHS data
create_fake_dhs_data <- function(country_code) {
  set.seed(123) # For reproducibility
  n <- 100

  # Create labelled vectors
  hv007 <- haven::labelled(
    sample(c(2015, 2016), n, replace = TRUE),
    labels = c("2015" = 2015, "2016" = 2016)
  )

  hv001 <- haven::labelled(
    sample(1:20, n, replace = TRUE),
    labels = setNames(1:20, paste("Cluster", 1:20))
  )

  hv105 <- haven::labelled(
    sample(c(1:97, 98), n, replace = TRUE),
    labels = c(setNames(1:97, paste("Age", 1:97)), "Don't know" = 98)
  )

  # Combine into data frame
  data.frame(
    hv007 = hv007,
    hv001 = hv001,
    hv105 = hv105
  )
}

# Create fake shapefile data
# Create fake shapefile data with explicit CRS
create_fake_shapefile <- function(country_code) {
  set.seed(123)
  n_clusters <- 20

  # Create spatial data frame with explicit CRS
  sf_data <- sf::st_as_sf(
    dplyr::tibble(
      DHSCLUST = 1:n_clusters,
      URBAN_RURA = sample(c("R", "U"), n_clusters, replace = TRUE),
      LATNUM = runif(n_clusters, -10, 10),
    LONGNUM = runif(n_clusters, -10, 10)
  ),
    coords = c("LONGNUM", "LATNUM"),
    crs = 4326 # WGS84
  ) |>
    dplyr::mutate(
      LATNUM = runif(n_clusters, -10, 10),
      LONGNUM = runif(n_clusters, -10, 10)
    )
}

# Save test data for two countries
countries <- c("KE", "TZ")
for (country in countries) {
  saveRDS(
    create_fake_dhs_data(country),
    file = file.path(tmp_rds_dir, paste0(country, "HR71FL.rds"))
  )
  saveRDS(
    create_fake_shapefile(country),
    file = file.path(tmp_shp_dir, paste0(country, "HR7SHP.rds"))
  )
}

# Run the function
process_dhs_data(
  rds_dir = tmp_rds_dir,
  shp_dir = tmp_shp_dir,
  output_path = tmp_output
)
#> 
#> ── Processing DHS data and joining with shapefile ──────────────────────────────
#> ℹ Processing country: KE
#> ✔ Processed country: KE (1 of 2)
#> 
#> ℹ Processing country: TZ
#> ✔ Processed country: TZ (2 of 2)
#> 
#> 
#> ── Process gamma parameters ────────────────────────────────────────────────────
#> ℹ Aggregating and extracting gamma for: KEN
#> ✔ Aggregated and extracted gamma for: KEN (1 of 2)
#> 
#> ℹ Aggregating and extracting gamma for: TZA
#> ✔ Aggregated and extracted gamma for: TZA (2 of 2)
#> 
#> ✔ All countries processed. Combined data saved to /tmp/Rtmpkz8QDq/test_env/output.rds
# }
```
