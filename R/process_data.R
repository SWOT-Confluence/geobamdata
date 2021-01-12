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
#' @importFrom foreach %:%
#'
#' @export
process_data <- function(input_dir, output_dir) {

  # Obtain a list of reach input data from swot and sword files
  reach_list <- get_input_data(input_dir)

  # Setup cluster and register do parallel operator
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)

  # Get a dataframe of geobam computation results
  reach <- NULL
  result_df <- foreach::foreach(reach = reach_list, .combine = df_data, .export = c("run_geobam"),
                                .packages = c("ncdf4", "geoBAMr")) %dopar% run_geobam(reach)

  # Write netcdf of results grouped by reachid
  write_netcdf(result_df, output_dir)

  # Close cluster connections
  parallel::stopCluster(cl)

}

#
#' Combine results of run_geobam
#'
#' Combine function for foreach statement.
#'
#' @param df1 data.frame
#' @param df2 data.frame
#'
#' @return data.frame
df_data <- function(df1, df2) {
  rbind(df1, df2)
}
