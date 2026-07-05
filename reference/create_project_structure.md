# Create a Standardized Project Folder Structure

This function creates a standardized folder structure for organizing
data, scripts, and outputs within a project directory. It ensures
consistency and reproducibility for data-related workflows.

## Usage

``` r
create_project_structure(base_path = here::here())
```

## Arguments

- base_path:

  A character string specifying the root directory where the folder
  structure will be created. Defaults to \`here::here()\` to use the
  current project directory.

## Value

Creates directories under the specified \`base_path\`. Returns invisible
\`NULL\` and prints messages about folder creation status.

## Details

The function generates the following folder structure:


    # 01_data/
    # +-- 1a_survey_data/
    # |    +-- processed/
    # |    \-- raw/
    # +-- 1b_rasters/
    # |    +-- urban_extent/
    # |    \-- pop_raster/
    # +-- 1c_shapefiles/
    # 02_scripts/
    # 03_outputs/
    # +-- 3a_model_outputs/
    # +-- 3b_visualizations/
    # +-- 3c_table_outputs/
    # \-- 3d_compiled_results/

## Examples

``` r
# \donttest{
# Create temp directory with normalized path
tf <- file.path(tempdir(), "test_env")
dir.create(tf, recursive = TRUE, showWarnings = FALSE)

#  Initialize with normalized path
cpp_path <- file.path(tf, "02_scripts", "model")
dir.create(cpp_path, recursive = TRUE, showWarnings = FALSE)
cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = FALSE)

create_project_structure(base_path = tf)
#> ℹ Created: /tmp/RtmpqFflLL/test_env/01_data/1a_survey_data/processed
#> ℹ Created: /tmp/RtmpqFflLL/test_env/01_data/1a_survey_data/raw
#> ℹ Created: /tmp/RtmpqFflLL/test_env/01_data/1b_rasters/urban_extent
#> ℹ Created: /tmp/RtmpqFflLL/test_env/01_data/1b_rasters/pop_raster
#> ℹ Created: /tmp/RtmpqFflLL/test_env/01_data/1c_shapefiles
#> ! Exists: /tmp/RtmpqFflLL/test_env/02_scripts
#> ℹ Created: /tmp/RtmpqFflLL/test_env/03_outputs/3a_model_outputs
#> ℹ Created: /tmp/RtmpqFflLL/test_env/03_outputs/3b_visualizations
#> ℹ Created: /tmp/RtmpqFflLL/test_env/03_outputs/3c_table_outputs
#> ℹ Created: /tmp/RtmpqFflLL/test_env/03_outputs/3d_compiled_results
#> ✔ Folder structure created successfully.
# }
```
