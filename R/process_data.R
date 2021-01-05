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

  # Get a list of lists which contains data for each reach
  reach_list <- foreach::foreach(file = file_list, .combine = 'c',
                                 .packages = c("ncdf4")) %dopar% get_input(file)

  # Get a dataframe of geobam computation results
  result_df <- foreach::foreach(reach = reach_list, .combine = df_data,
                                .packages = c("ncdf4", "geoBAMr")) %dopar% run_geobam(reach)

  doParallel::stopImplicitCluster()

  # Write netcdf of results grouped by reachid
  write_netcdf(result_df, output_dir)

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
df_data <- function(df1, df2) {
  rbind(df1, df2)
}
