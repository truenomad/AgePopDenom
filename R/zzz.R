.onAttach <- function(libname, pkgname) {
  # Suggested packages
  suggested_pkgs <- c(
    "cli", "countrycode", "crayon", "scales", "glue",
    "haven", "here", "matrixStats", "rstudioapi", "geodata",
    "pbmcapply", "remotes", "future", "future.apply",
    "testthat", "purrr", "rdhs", "rlang"
  )

  # Check for missing packages
  missing_pkgs <- suggested_pkgs[!vapply(suggested_pkgs, requireNamespace,
                                         logical(1),
                                         quietly = TRUE
  )]

  if (!requireNamespace("pak", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "pak")
  }
  if (!requireNamespace("remotes", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "esri2sf (GitHub)")
  }

  if (length(missing_pkgs) > 0) {
    msg <- sprintf(
      "The following suggested packages are not installed:\n%s\n\n%s",
      paste0("- ", missing_pkgs, collapse = "\n"),
      paste("Install them using 'install.packages()' or with the helper",
            "function `install_suggests()`.")
    )
    packageStartupMessage(msg)
  }
}
