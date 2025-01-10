# AgePopDenom: High-Resolution Age-Structured Population Denominators

## What is the AgePopDenom Package?

**`AgePopDenom`** is an R package designed to facilitate the generation of fine-scale, age-structured population denominators for public health decision-making and service delivery. By combining census and household survey data with a novel parameter-based geostatistical modeling approach, the package produces high-resolution (5km x 5km) population estimates disaggregated by age.

------------------------------------------------------------------------

## Installation

To install the package from GitHub, use the following commands:

``` r
# Ensure you have the devtools package installed:
install.packages("devtools")

# Install AgePopDenom from GitHub
devtools::install_github("truenomad/AgePopDenom")

 # Additional packages installed
Run `install_suggests()` to install additional dependencies
```

Once the package is installed, it is suggested that users should install additional dependencies by running the following code:

``` r
# To additional packages 
AgePopDenom::install_suggests()
```

------------------------------------------------------------------------

## Setting Up Your Project

Before starting, ensure you have an **RStudio project** set up. This will help organize your analysis and outputs into a single, self-contained directory. An RStudio project is essential for maintaining reproducibility and keeping your workflow organized.

Once the RStudio project is created, initialize the project folder structure and create the key scripts by running:

``` r
AgePopDenom::init()
```

This function: - Creates a standardized directory structure for managing data, scripts, and outputs systematically. - Generates two essential scripts in the `02_scripts/` directory:

1.  **`full_pipeline.R`**: Contains the complete analytical workflow.

2.  **`model.cpp`**: Provides the C++ code used to optimize the geostatistical models.

### Folder Structure

The following directories will be created automatically:

``` plaintext
01_data/
├── 1a_survey_data/
│   ├── processed/
│   └── raw/
├── 1b_rasters/
│   ├── urban_extent/
│   └── pop_raster/
├── 1c_shapefiles/
02_scripts/
03_outputs/
├── 3a_model_outputs/
├── 3b_visualizations/
├── 3c_table_outputs/
└── 3d_compiled_results/
```

------------------------------------------------------------------------

## Running the Pipeline

The **`full_pipeline.R`** script automates the workflow. It performs the following steps:

### Data Downloads

-   **DHS Datasets**: Retrieves demographic and health data for the specified countries.
-   **WorldPop Rasters**: Downloads 100m unconstrained population rasters.
-   **WHO Adm2 Shapefiles**: Fetches WHO administrative boundary shapefiles.
-   **Urban-rural extent**: Extracts Urban-rural raster file included in the package (this is the predictor data).

If you prefer to use your own shapefiles, replace the file at `01_data/1c_shapefiles/district_shape.gpkg` with your custom file, ensuring it has the same name.

### Model Execution

-   Runs geostatistical models for each country.
-   Produces high-resolution prediction raster images and diagnostic outputs.
-   Generates regional age-pyramids for visualizing population distributions.
-   Computes population denominators disaggregated by age groups for each district.

------------------------------------------------------------------------

## Outputs

The results are saved in the `03_outputs/` directory, structured as follows:

-   **`3a_model_outputs/`**: Model parameters and diagnostics.
-   **`3b_visualizations/`**: Age-pyramid visualizations & prediction raster plots.
-   **`3c_table_outputs/`**: Grided age-population tables with confidence interval.
-   **`3d_compiled_results/`**: Final aggregated age-population tables with model parameters.

------------------------------------------------------------------------

## Example Workflow

Below is an example workflow using the **`full_pipeline.R`** script:

``` r
# Set up countries of interest
cntry_codes <- c("KEN", "UGA")

# Gather and process datasets --------------------------------------------------

# Download DHS datasets
AgePopDenom::download_dhs_datasets(
  country_codes = cntry_codes,
  email = "your_email@example.com",
  project = "My DHS Project"
)

# Process DHS datasets
AgePopDenom::process_dhs_data()

# Download shapefiles
AgePopDenom::download_shapefile(cntry_codes)

# Download population rasters from worldpop
AgePopDenom::download_pop_rasters(cntry_codes)

# Extract urban extent raster
AgePopDenom::extract_afurextent()

# Run models and get outputs ---------------------------------------------------

# Run the model
AgePopDenom::run_full_workflow(cntry_codes) 
```

------------------------------------------------------------------------

## Support and Contributions

For support, bug reports, or feature requests, please contact:

-   **Mo Yusuf** (Package Developer)\
    **Email**: [moyusuf\@who.int](mailto:moyusuf@who.int){.email}\
    **Affiliation**: World Health Organization Regional Office for Africa, P.O. Box 06, Cite du Djoue, Brazzaville, Congo

Alternatively, open an issue on the [GitHub repository](https://github.com/trunomad/AgePopDenom).

We welcome contributions from the community to improve `AgePopDenom`.
