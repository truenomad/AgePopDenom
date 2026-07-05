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
#> [1] "/home/runner/work/_temp/54463012-9418-4453-90ed-626dc34f3ad3"
```
