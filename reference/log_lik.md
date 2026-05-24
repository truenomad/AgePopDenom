# Log-Likelihood Function for Spatial Model

Computes the log-likelihood for a spatial statistical model with a
covariance structure determined by parameters including spatial decay
and variance.

## Usage

``` r
log_lik(
  par,
  p1,
  p2,
  d1,
  d2,
  y,
  u_dist,
  n_x,
  tau2_1 = 1,
  tau2_2 = 1,
  age_param_data
)
```

## Arguments

- par:

  A numeric vector of parameters to estimate. The vector contains:

  - `par[1:p1]`: Coefficients for fixed effects in dataset 1
    (\\\beta_1\\).

  - `par[(p1 + 1):(p1 + p2)]`: Coefficients for fixed effects in dataset
    2 (\\\beta_2\\).

  - `par[p1 + p2 + 1]`: Spatial decay parameter (\\\gamma\\).

  - `par[p1 + p2 + 2]`: Log of the variance parameter (\\\sigma^2\\).

  - `par[p1 + p2 + 3]`: Log of the range parameter (\\\phi\\).

- p1:

  An integer. The number of fixed-effect parameters in dataset 1.

- p2:

  An integer. The number of fixed-effect parameters in dataset 2.

- d1:

  A numeric matrix. Design matrix for dataset 1 used to model the mean
  structure.

- d2:

  A numeric matrix. Design matrix for dataset 2 used to model the mean
  structure.

- y:

  A numeric vector. Observed response variable, including both datasets.

- u_dist:

  A numeric matrix. Distance matrix for spatial locations.

- n_x:

  An integer. The number of unique spatial locations.

- tau2_1:

  Variance parameter for first process (default = 1)

- tau2_2:

  Variance parameter for second process (default = 1)

- age_param_data:

  A numeric matrix or vector. Additional parameters specific to
  age-based modeling.

## Value

A numeric scalar. The computed log-likelihood value.

## Details

The log-likelihood is computed as: \$\$ -0.5 \left\[ \log(\det(M)) +
(y - \mu)^T M^{-1} (y - \mu) \right\] \$\$ where:

- \\M\\ is the covariance matrix, computed using `compute_cov`.

- \\\mu\\ is the mean structure, determined by the design matrices `d1`,
  `d2` and coefficients \\\beta_1, \beta_2\\.

The covariance matrix \\M\\ is computed using spatial parameters
(\\\gamma, \sigma^2, \phi\\) and the distance matrix `u_dist`.

## Note

This function requires a helper function, `compute_cov`, to compute the
covariance matrix based on spatial parameters.
