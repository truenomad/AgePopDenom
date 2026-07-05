# Generate Age Population Tables

Creates age-stratified population tables from predictor data and gamma
distribution parameters. Supports parallel processing and caching of
results.

## Usage

``` r
generate_age_pop_table(
  predictor_data,
  scale_pred,
  shape_pred,
  age_range = c(0, 99),
  age_interval = 1,
  country_code,
  ignore_cache = FALSE,
  output_dir,
  n_cores = max(1, parallel::detectCores() - 2, na.rm = TRUE)
)
```

## Arguments

- predictor_data:

  Data frame containing population data with columns: country, region,
  district, pop

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

List containing two data frames: - prop_df: Age-stratified population
proportions with uncertainty intervals - pop_df: Age-stratified
population counts with uncertainty intervals

## Examples

``` r

# \donttest{
predictor_data <- data.frame(
 country = rep("ABC", 1100),
  region = rep("Region1", 1100),
  district = rep("District1", 1100),
  pop = rep(1000, 1100)
)
scale_pred <- matrix(rep(1:10, 1100), nrow = 1100, ncol = 10)
shape_pred <- matrix(rep(1:10, 1100), nrow = 1100, ncol = 10)
output <- generate_age_pop_table(
  predictor_data, scale_pred, shape_pred, age_range = c(0, 99),
  age_interval = 10, country_code = "ABC", ignore_cache = TRUE,
  output_dir = tempdir(), n_cores = 1
)
#> ℹ Processing interval 1/10...
#> ✔ Completed interval 1/10.
#> 
#> ℹ Processing interval 2/10...
#> ✔ Completed interval 2/10.
#> 
#> ℹ Processing interval 3/10...
#> ✔ Completed interval 3/10.
#> 
#> ℹ Processing interval 4/10...
#> ✔ Completed interval 4/10.
#> 
#> ℹ Processing interval 5/10...
#> ✔ Completed interval 5/10.
#> 
#> ℹ Processing interval 6/10...
#> ✔ Completed interval 6/10.
#> 
#> ℹ Processing interval 7/10...
#> ✔ Completed interval 7/10.
#> 
#> ℹ Processing interval 8/10...
#> ✔ Completed interval 8/10.
#> 
#> ℹ Processing interval 9/10...
#> ✔ Completed interval 9/10.
#> 
#> ℹ Processing interval 10/10...
#> ✔ Completed interval 10/10.
#> 
#> ✔ Final age population data saved to /tmp/RtmpNUOBYi/ABC_age_tables_pop_0_99plus_yrs_by_10yrs.rds
# }
```
