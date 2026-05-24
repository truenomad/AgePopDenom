# Process Gamma Prediction Results

This function processes gamma prediction results to calculate the mean
age predictions, scale, and shape parameters efficiently.

## Usage

``` r
process_gamma_predictions(gamma_prediction)
```

## Arguments

- gamma_prediction:

  A list containing \`scale_pred\` and \`shape_pred\` matrices from the
  gamma prediction model.

## Value

A list containing the following elements: - \`mean_age_pred\`: A vector
of mean age predictions. - \`scale_hat\`: A vector of mean scale
parameters. - \`shape_hat\`: A vector of mean shape parameters.
