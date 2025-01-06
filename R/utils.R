#' Install Suggested Packages
#'
#' This function installs suggested packages required for extended functionality.
#' It checks for and installs missing CRAN packages from the suggested list,
#' and also installs the 'esri2sf' package from GitHub if not already present.
#'
#' @details
#' The function installs the following CRAN packages if missing:
#' cli, countrycode, crayon, scales, glue, haven, here, matrixStats,
#' rstudioapi, geodata, pbmcapply, future, future.apply, testthat
#'
#' Additionally, it installs the esri2sf package from GitHub if not present.
#'
#' @return None (called for side effects)
#'
#' @examples
#' \dontrun{
#' install_suggests()
#' }
#' @export
install_suggests <- function() {
  suggested_pkgs <- c("cli", "countrycode", "crayon", "scales", "glue",
                      "haven", "here", "matrixStats", "rstudioapi", "geodata",
                      "pbmcapply", "remotes", "future", "future.apply",
                      "testthat")

  # Install CRAN packages
  cran_pkgs <- suggested_pkgs[!vapply(suggested_pkgs, requireNamespace,
                                      logical(1),
                                      quietly = TRUE
  )]
  if (length(cran_pkgs) > 0) {
    message("Installing missing CRAN packages...")
    utils::install.packages(cran_pkgs)
  }

  # Install GitHub package
  if (!requireNamespace("esri2sf", quietly = TRUE)) {
    message("Installing 'esri2sf' from GitHub...")
    if (!requireNamespace("remotes", quietly = TRUE)) {
      utils::install.packages("remotes")
    }
    remotes::install_github("yonghah/esri2sf")
  }

  message("Installation of suggested packages complete.")
}
