# AgePopDenom: Geostatistical Modeling for Fine-Scale Population Age-Structures

<!-- [![JOSS Status](https://joss.theoj.org/papers/10.21105/joss.xxxxx/status.svg)](https://joss.theoj.org/papers/10.21105/joss.xxxxx) -->

This repository contains the JOSS paper submission for the **AgePopDenom** R package, which generates spatial resolution (5 km × 5 km) estimates of population by single year of age across space using publicly available datasets.

## About AgePopDenom

`AgePopDenom` is the first R package to operationalize a parameter-based geostatistical approach for modeling full age structures at fine spatial resolution. Unlike existing tools that focus on demographic simulation or limited health indicators, AgePopDenom integrates:

- Survey microdata (DHS, MICS, or custom household surveys)
- WorldPop gridded population rasters
- Administrative boundaries
- User-defined covariates (urban/rural status, etc.)

The package models age distributions using a Gamma distribution framework with bivariate Gaussian processes to capture spatial autocorrelation, delivering high-resolution estimates with built-in uncertainty quantification.

## Repository Contents

- `paper.md` - Main JOSS manuscript file
- `references.bib` - Bibliography
- `paper.html` - Rendered HTML version
- `schematic_figure_final.png/.html` - Workflow diagram
- **`example_pipeline.R`** - Complete example demonstrating package functionality
- **`renv.lock`** - renv lockfile with package dependencies
- **`AgePopDenom_Paper_JOSS.Rproj`** - RStudio Project file

## Getting Started

### Prerequisites

Before running the example, ensure your system is properly configured:

1. **System Requirements**: Follow the system setup instructions at: https://github.com/truenomad/AgePopDenom

2. **R Environment**: This repository uses `renv` for dependency management. You must work within the provided RStudio project. run `renv::restore(confirm = FALSE)` once environment is activated.

### Installation and Setup

1. **Clone and open the repository**:
   ```bash
   git clone https://github.com/truenomad/AgePopDenom_Paper_JOSS.git
   ```
