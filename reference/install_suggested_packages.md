# Package Initialization and Dependency Check

This function checks for suggested packages and prompts the user to
install any missing ones that are needed for full functionality.

## Usage

``` r
install_suggested_packages(libname = NULL, pkgname = NULL)
```

## Arguments

- libname:

  The library name where the package is installed (not used)

- pkgname:

  The name of the package being loaded (not used)

## Value

Returns NULL invisibly. The function's main effects are:

- Checking for missing suggested packages

- Displaying missing packages to user

- Installing packages if user agrees

- Providing feedback on installation success/failure

## Details

The function maintains a predefined list of suggested packages and
checks if they are installed. For missing packages, it prompts the user
for installation in interactive sessions.

The function uses 'cli' for user communication and handles errors
gracefully during installation attempts. In non-interactive sessions, it
skips installation and returns with a warning.

## Note

\- Function requires an interactive session for package installation -
Some functionality may be limited if suggested packages are not
installed - Installation errors are caught and reported but don't stop
execution
