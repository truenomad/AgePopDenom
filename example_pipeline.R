# Working directory ------------------------------------------------------------

# set working directory based on script location if not using an R project
# this ensures relative paths work as expected outside .Rproj environments

# get script path
current_file_path <- AgePopDenom::get_current_script_path()
script_dir <- dirname(current_file_path)

# set working directory to script directory if not already in a project
if (!any(grepl("\\.Rproj$", list.files(script_dir)))) {
  setwd(script_dir)
  message(
    "no .Rproj file found. using script directory as working directory: ",
    script_dir
  )
} else {
  message(".Rproj file detected. assuming project is correctly set up.")
}

# Create directory and file Structure ------------------------------------------

AgePopDenom::init(
  r_script_name = "full_pipeline.R",
  cpp_script_name = "model.cpp",
  open_r_script = FALSE
)

# Gather and process datasets --------------------------------------------------

# set up country code
cntry_code = "GMB"
country = "Gambia"
country_code_dhs = "GM"

# Simulateand and save processed survey dataset for Gambia
AgePopDenom::simulate_dummy_dhs_pr(
  country = country,
  country_code_iso3 = cntry_code,
  country_code_dhs = country_code_dhs,
  year_of_survey = 2024,
  output_path = here::here(
    "01_data",
    "1a_survey_data",
    "processed",
    "dhs_pr_records_combined.rds"
  )
)

# download shapefiles
AgePopDenom::download_shapefile(cntry_code)

# download population rasters from worldpop
AgePopDenom::download_pop_rasters(cntry_code)

# wxtract urban extent raster
AgePopDenom::extract_afurextent()

# Run models and get outputs ---------------------------------------------------

# run the full model workflow
AgePopDenom::run_full_workflow(cntry_code)
