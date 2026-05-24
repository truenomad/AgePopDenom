# Initialize Full Pipeline Script and Model Script

Creates a full pipeline R script and a model C++ script, saving them to
the appropriate folders within the project directory structure. The
folder structure is created using
\`AgePopDenom::create_project_structure()\`. The scripts contain example
code for downloading and processing DHS data, shapefiles, and running
models.

## Usage

``` r
init(
  r_script_name = "full_pipeline.R",
  cpp_script_name = "model.cpp",
  path = here::here(),
  open_r_script = TRUE,
  setup_rscript = TRUE
)
```

## Arguments

- r_script_name:

  Character. The name of the R script file to be created. Defaults to
  \`"full_pipeline.R"\`.

- cpp_script_name:

  Character. The name of the C++ script file to be created. Defaults to
  \`"model.cpp"\`.

- path:

  Character. The directory in which to create the scripts. Defaults to
  \`here::here()\`.

- open_r_script:

  Logical. Whether to open the R script automatically in RStudio (if
  available). Defaults to \`TRUE\`.

- setup_rscript:

  Logical. Whether to setup the R script with example code. Defaults to
  \`TRUE\`.

## Value

Invisibly returns a list containing:

- r_script_path: Path to the created R script

- cpp_script_path: Path to the created C++ script

The function also prints success messages upon script creation.

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

init(
r_script_name = "full_pipeline.R",
cpp_script_name = "model.cpp",
path = tf,
open_r_script = FALSE
)
#> ✔ All suggested packages are already installed.
#> ! Exists: /tmp/RtmpTS1jsq/test_env/01_data/1a_survey_data/processed
#> ! Exists: /tmp/RtmpTS1jsq/test_env/01_data/1a_survey_data/raw
#> ! Exists: /tmp/RtmpTS1jsq/test_env/01_data/1b_rasters/urban_extent
#> ! Exists: /tmp/RtmpTS1jsq/test_env/01_data/1b_rasters/pop_raster
#> ! Exists: /tmp/RtmpTS1jsq/test_env/01_data/1c_shapefiles
#> ! Exists: /tmp/RtmpTS1jsq/test_env/02_scripts
#> ! Exists: /tmp/RtmpTS1jsq/test_env/03_outputs/3a_model_outputs
#> ! Exists: /tmp/RtmpTS1jsq/test_env/03_outputs/3b_visualizations
#> ! Exists: /tmp/RtmpTS1jsq/test_env/03_outputs/3c_table_outputs
#> ! Exists: /tmp/RtmpTS1jsq/test_env/03_outputs/3d_compiled_results
#> ✔ Folder structure created successfully.
#> ℹ R script created but could not open automatically: RStudio not available.
#> ✔ C++ script '/tmp/RtmpTS1jsq/test_env/02_scripts/model.cpp' successfully created.
# }
```
