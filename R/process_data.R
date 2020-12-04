#' Execute geobam on data
#'
#' Execute geobam on data in user-provided input directory and outputs a single
#' netCDF file of results organized as variables grouped by reachid to
#' user-provided output directory. Directories need to be passed in as absolute
#' paths as character values.
#'
#' @param input_dir character
#' @param output_dir character
#'
#' @return None
#'
#' @importFrom foreach %dopar%
#'
#' @export
process_data <- function(input_dir, output_dir) {

  # Run geobam on each file in the input directory
  file_list <- list.files(path = input_dir, pattern = "*.nc", full.names = TRUE)

  # Setup cluster and register do parallel operator
  doParallel::registerDoParallel(parallel::makeCluster(parallel::detectCores()))

  # Get a dataframe of geobam computation results
  result_df <- foreach::foreach(file = file_list, .combine = c_data,
                                .packages = c("geobamsevern", "ncdf4", "geoBAMr")) %dopar% run_geobam(file)
  doParallel::stopImplicitCluster()

  # Write netcdf of results grouped by reachid
  write_netcdf(result_df, output_dir)

  print("netCDF file written to output directory.")
}

#
#' Combine results of run_geobam
#'
#' Combine function for foreach statement.
#'
#' @param result1 data.frame
#' @param result2 data.frame
#'
#' @return data.frame
c_data <- function(result1, result2) {
  names(result1)[1] <- "reachid"
  names(result2)[1] <- "reachid"
  rbind(result1, result2)
}
