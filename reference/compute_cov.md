# Compute Covariance Matrix for Spatial Model

This function computes a block covariance matrix for a bivariate spatial
model with age-structured parameters.

## Usage

``` r
compute_cov(
  gamma,
  sigma2,
  phi,
  u_dist,
  n_x,
  tau2_1 = 1,
  tau2_2 = 1,
  age_param_data
)
```

## Arguments

- gamma:

  Correlation parameter between the two spatial processes

- sigma2:

  Variance parameter for the spatial processes

- phi:

  Range parameter for the spatial correlation

- u_dist:

  Distance matrix between locations

- n_x:

  Number of spatial locations

- tau2_1:

  Variance parameter for first process (default = 1)

- tau2_2:

  Variance parameter for second process (default = 1)

- age_param_data:

  List containing age-structured parameters:

  - b1: Vector of age parameters for first process

  - b2: Vector of age parameters for second process

  - c: Vector of cross-process age parameters

## Value

A sparse symmetric matrix of dimension 2n_x × 2n_x
