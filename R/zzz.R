.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Run `install_suggests()` to install additional dependencies.")
}
