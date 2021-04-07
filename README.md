
# geobamdata

The goal of geobamdata is to take the netCDF file data from a user-specified input directory and run geoBAMr on the file. The results of the geoBAMr computation are placed in a user-specified output directory as a single netCDF file. Mean and sigma variables are captured for each reach file and grouped by Reach ID.

Orchestration of geobamdata execution on multiple files needs to be handled outside of geobamdata.

## Installation

You can install the GitHub version of geobamdata from [SWOT-Confluence](https://github.com/SWOT-Confluence/geobamdata) with:

```
devtools::install_github("SWOT-Confluence/geobamdata", force=TRUE)
```

### Execution

1. After installing geobamdata, create references to the reachid of the input data, to a directory that contains the netCDF input data, and to a directory where you would like output data to be written. See example below.
2. Run `run_geobam(reachid, input_dir, output_dir)`
3. If successful, output will be in the directory you specified.

## Example

Running `run_geobam` on specified input and output directories (note example is taken from Linux file system):

```
library(geobamdata)
data_dir <- file.path("/home", "username", "Documents", "geobam", "input")
output_dir <- file.path("/home", "username", "Documents", "geobam", "output")
reachid <- "002_01"
run_geobam(reachid, data_dir, output_dir)
```
