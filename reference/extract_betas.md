# Extract Beta Parameters from Model Output

This function extracts beta coefficients from a model parameter object,
separating them into beta1 and beta2 components.

## Usage

``` r
extract_betas(
  params_result,
  params = c("gamma", "log_sigma2", "log_phi", "log_tau1")
)
```

## Arguments

- params_result:

  A model parameter object containing parameter estimates

- params:

  A character vector specifying parameter names, defaults to c("gamma",
  "log_sigma2", "log_phi", "log_tau1")

## Value

A list with two components:

- beta1: First set of beta coefficients

- beta2: Second set of beta coefficients

## Details

The function assumes the parameter vector contains beta coefficients
followed by other model parameters. It splits the betas into two equal
groups after removing the last 4 parameters.
