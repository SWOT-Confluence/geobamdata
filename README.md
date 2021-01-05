
# geobamdata

The goal of geobamdata is to take the netCDF file data from a user-specified input directory and run geoBAMr on each file. The results of the geoBAMr computation are placed in a user-specified output directory as a single netCDF file. Mean and sigma variables are captured for each reach file and grouped by Reach ID.

## Installation

You can install the GitHub version of geobamdata from [SWOT-Confluence](https://github.com/SWOT-Confluence/geobamdata) with:

``` r
devtools::install_github("SWOT-Confluence/geobamdata", force=TRUE)
```

### Execution

1. After installing geobamdata, create references to a directory that contains the netCDF input data and to a directory where you would like output data to be written. See example below.
2. Run `process_data(input_dir, output_dir)`
3. If successful, output will be in the directory you specified.

## Example

Running process_data on specified input and output directories (note example is taken from Linux file system):

``` r
library(geobam)
input_dir <- file.path("/home", "username", "Documents", "geobam", "input")
output_dir <- file.path("/home", "username", "Documents", "geobam", "output")
process_data(input_dir, output_dir)
```
