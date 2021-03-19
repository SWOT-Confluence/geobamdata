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

  # Obtain a list of reach identifiers from input data
  swot_dir = file.path(input_dir, "swot")
  reaches <- lapply(list.files(swot_dir), get_reach_id)

  # Setup future backend for foreach
  doFuture::registerDoFuture()
  #future::plan(future::multisession)
  future::plan(future.batchtools::batchtools_slurm,
               template = "slurm-simple.tmpl",
               resources = list(ncpus = 48L, memory = "4G", walltime = 3600L, queue = "cee_water_cjgleason"))

  # Run geobam on each reach
  reachid <- NULL
  `%dorng%` <- doRNG::`%dorng%`
  foreach::foreach(reachid = reaches, .packages = c("ncdf4", "geoBAMr")) %dorng% { run_geobam(reachid = reachid, data_dir = input_dir, output_dir = output_dir) }

}

#' Retrieve reach id from SWOT file name.
#'
#' @param swot_file Path to SWOT file
#'
#' @return String reach identifier
get_reach_id <- function(swot_file) {
  file_vec <- unlist(strsplit(swot_file, split = "/"))
  file_name <- strsplit(file_vec[length(file_vec)], split = ".nc")
  name <- strsplit(as.character(file_name[1]), split = "_")
  return(paste0(name[[1]][1], "_", name[[1]][2]))
}
