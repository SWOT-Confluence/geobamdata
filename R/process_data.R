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

  # Run geobam on each reach in parallel
  rslurm::slurm_map(reaches, run_geobam, data_dir = input_dir, output_dir = output_dir, nodes = 1, cpus_per_node = 48, pkgs = c("ncdf4", "geoBAMr"), submit = FALSE)

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
