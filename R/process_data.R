#' Execute geobam on data
#'
#' Execute geobam on data in user-provided input directory and outputs a single
#' netCDF file of results organized as variables grouped by reachid to
#' user-provided output directory. Directories need to be passed in as absolute
#' paths as character values.
#'
#' @param input_dir character vecto
#' @param output_dir character
#' @param env character name of environment to execute in (i.e. "workstation" or "slurm")
#' @param cores number of cores to run on (for workstation run only)
#' @param ncpus number of cpus required per job (for HPC SLURM run only)
#'
#' @return None
#'
#' @export
process_data <- function(input_dir, output_dir, env, cores = NA, ncpus = NA) {

  # Obtain a list of reach identifiers from input data
  swot_dir <- file.path(input_dir, "swot")
  reaches <- lapply(list.files(swot_dir), get_reach_id)

  # Run geobam on each reach in parallel single workstation
  if (env == "workstation") { run_workstation(reaches, input_dir, output_dir, cores) }
  # Run geobam on each reach in parallel HPC
  else if (env == "slurm") { run_slurm(reaches, input_dir, output_dir, ncpus) }
  # Message error
  else { message("Invalid environment selected.") }

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

#' Run geoBAM in parallel on multiple processes on single workstation.
#'
#' @param reaches list of string reach identifiers
#' @param input_dir string input directory
#' @param output_dir string output directory
#' @param cores integer number of cores in system (default is determed by detectCores)
run_workstation <- function(reaches, input_dir, output_dir, cores = parallel::detectCores()) {
  # Setup cluster and register do parallel operator
  cl <- parallel::makeCluster(cores, outfile = "debug_process.txt")
  parallel::clusterExport(cl, c("run_geobam", "get_input_data",
                                "check_observations", "get_invalid",
                                "create_posterior_list", "get_posteriors",
                                "extract_geobam_posteriors", "update_posteriors",
                                "get_mean", "get_sd",
                                "write_netcdf", "create_dimensions",
                                "create_vars", "create_nc_file",
                                "concatenate_invalid", "write_vars"))
  doParallel::registerDoParallel(cl)
  reachid <- NULL
  `%dopar%` <- foreach::`%dopar%`
  foreach::foreach(reachid = reaches, .packages = c("ncdf4", "geoBAMr")) %dopar%
    run_geobam(reachid = reachid, data_dir = input_dir, output_dir = output_dir)

  # Close cluster connections
  parallel::stopCluster(cl)
}

#' Run geoBAM in parallel using batchtools where each reach is submitted
#' as a job.
#'
#' @param reaches list of string reach identifiers
#' @param input_dir string input directory
#' @param output_dir string output directory
#' @param ncpus integer number of cpus per job.
run_slurm <- function(reaches, input_dir, output_dir, ncpus) {
  # Create a registry
  reg_dir <- "/home/ntebaldi_umass_edu/geobamdata_run/batchtools_data/batch_10_rpof"
  conf_file <- "/home/ntebaldi_umass_edu/geobamdata_run/batchtools_data/.batchtools.conf.R"
  reg <- batchtools::makeRegistry(file.dir = reg_dir, conf.file = conf_file)

  # Map jobs
  ids <- batchtools::batchMap(fun = run_geobam, reachid = reaches,
                              more.args = list(data_dir = input_dir,
                                               output_dir = output_dir), reg = reg)

  # Submit jobs to the cluster
  done <- batchtools::submitJobs(ids, reg = reg,
                                 resources = list(walltime = 3600, memory = 4096,
                                                  ncpus = 3, foreach.backend = "parallel", ntasks = 1,
                                                  partition = "cee_water_cjgleason"))
  batchtools::waitForJobs()
}
