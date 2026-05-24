# Get the path to the currently running script

This function attempts to determine the file path of the currently
executing R script. It works across multiple contexts: - In RStudio:
returns the path of the active source editor tab. - In Rscript: extracts
the \`–file\` argument used in script execution. - When sourced: uses
the internal \`ofile\` value.

## Usage

``` r
get_current_script_path()
```

## Value

A character string with the full path to the current script, or \`NULL\`
if it cannot be determined.

## Examples

``` r
get_current_script_path()
#> [1] "/home/runner/work/_temp/841119c0-6573-4b03-8cc5-71985d617a0c"
```
