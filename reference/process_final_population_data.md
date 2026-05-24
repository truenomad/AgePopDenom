# Process Final Population Data

Reads population and proportion data from RDS files, processes the data
to generate age-group-based population summaries and proportions at
different administrative levels (Country, Region, District), and writes
the results to an Excel file with separate sheets for each level and
metric.

## Usage

``` r
process_final_population_data(
  input_dir = here::here("03_outputs", "3c_table_outputs"),
  excel_output_file = here::here("03_outputs", "3d_compiled_results",
    "age_pop_denom_compiled.xlsx")
)
```

## Arguments

- input_dir:

  A character string specifying the directory containing RDS files.
  Default is "03_outputs/3c_table_outputs" in the project directory.

- excel_output_file:

  A character string specifying the output Excel file path. Default is
  "03_outputs/3d_compiled_results/age_pop_denom_compiled.xlsx" in the
  project directory.

## Value

None. The function writes an Excel file to the specified output location
with six sheets containing population counts and proportions at
different administrative levels.
