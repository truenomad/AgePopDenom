# Automatically Fit a Variogram Model

This function was originally sourced from the \`automap\` package
(version 1.1-16), which is no longer available on CRAN. It is provided
here with minimal modification for internal use in this package.

\`autofitVariogram()\` automates the fitting of a variogram model to
spatial input data by testing a range of model types and kappa values
(for Matern and Stein models), and selecting the best-fit model based on
the sum of squared errors.

Dependencies from \`gstat\`, \`sp\`, and \`sf\` are required for this
function to operate correctly.

## Usage

``` r
autofitVariogram(
  formula,
  input_data,
  model = c("Sph", "Exp", "Gau", "Ste"),
  kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
  fix.values = c(NA, NA, NA),
  verbose = FALSE,
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  ...
)
```

## Arguments

- formula:

  model formula for the variogram (e.g., \`z ~ 1\`)

- input_data:

  an \`sf\` or \`Spatial\*\` object containing the spatial data

- model:

  a character vector of variogram model names (e.g., \`"Sph"\`,
  \`"Exp"\`)

- kappa:

  vector of kappa values for Matern/Stein models

- fix.values:

  optional vector of fixed values for nugget, range, sill

- verbose:

  logical; if \`TRUE\`, prints additional output

- GLS.model:

  optional variogram model for Generalized Least Squares

- start_vals:

  optional vector of starting values for nugget, range, sill

- miscFitOptions:

  named list of additional control options

- ...:

  additional arguments passed to \`gstat::variogram()\`

## Value

a list with class \`"autofitVariogram"\` containing:

- exp_var:

  the empirical variogram

- var_model:

  the best-fit variogram model object

- sserr:

  sum of squared errors for the best model

## Note

Original author: J. H. Hiemstra (2013), \`automap\` package. License:
GPL-2 \| GPL-3.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- autofitVariogram(z ~ 1, input_data)
plot(result$exp_var)
plot(result$var_model)
} # }
```
