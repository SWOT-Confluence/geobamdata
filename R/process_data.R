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
#' @param partition string partition name to run jobs on (slurm run only)
#' @param max_jobs integer max number of jobs to be run concurrently (slurm run only)
#' @param as_job_array boolean indication to run execute as job array (slurm run only)
#'
#' @return None
#'
#' @export
process_data <- function(input_dir, output_dir, env, cores = NA, partition = NA,
                         max_jobs = NA, as_job_array = FALSE) {

  # Obtain a list of reach identifiers from input data
  swot_dir <- file.path(input_dir, "swot")
  reaches <- lapply(list.files(swot_dir), get_reach_id)

  # Run geobam on each reach in parallel single workstation
  if (env == "workstation") { run_workstation(reaches, input_dir, output_dir, cores) }
  # Run geobam on each reach in parallel HPC
  else if (env == "slurm") { run_slurm(reaches, input_dir, output_dir, partition, max_jobs, as_job_array) }
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
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  reachid <- NULL
  `%dopar%` <- foreach::`%dopar%`
  foreach::foreach(reachid = reaches, .packages = c("ncdf4", "geoBAMr"),
                   .export =  c("run_geobam", "get_input_data",
                               "check_observations", "get_invalid",
                               "create_posterior_list", "get_posteriors",
                               "extract_geobam_posteriors", "update_posteriors",
                               "get_mean", "get_sd",
                               "write_netcdf", "create_dimensions",
                               "create_vars", "create_nc_file",
                               "concatenate_invalid", "write_vars")) %dopar%
    run_geobam(reachid = reachid, data_dir = input_dir, output_dir = output_dir)

  # Close cluster connections
  parallel::stopCluster(cl)
}

#' Run geoBAM in parallel using batchtools where each reach is submitted
#' as a job.
#'
#' NOTE: To AVOID submitting too many jobs to the cluster you need to consider
#' the number of reaches (jobs) that you will need to process in parallel and
#' the max number of cores you have available to you. Each execution of geobamdata
#' takes 4 cores per reach. So if you divide the max number of cores by 4 you
#' should get the max_jobs you can run concurrently and that is what you need
#' to pass to the max_jobs parameter.
#'
#' @param reaches list of string reach identifiers
#' @param input_dir string input directory
#' @param output_dir string output directory
#' @param partition string name of partition to run on
#' @param max_jobs maximum number of jobs to be run concurrently; enter -1 to disable chunking
#' @param as_job_array boolean indicates if job should be run as a job array
#' @importFrom data.table :=
run_slurm <- function(reaches, input_dir, output_dir, partition, max_jobs, as_job_array = FALSE) {
  # Create a registry
  reg_dir <- file.path("/home", Sys.getenv("USER"), "gb_reg")
  conf_file <- file.path("/app", "batchtools_data", ".batchtools.conf.R")
  reg <- batchtools::makeRegistry(file.dir = reg_dir, conf.file = conf_file)

  # Map jobs
  ids <- batchtools::batchMap(fun = run_geobam, reachid = reaches,
                              more.args = list(data_dir = input_dir,
                                               output_dir = output_dir),
                              reg = reg)

  # Name jobs
  names <- lapply(ids[[1]], function(id) { paste("geobam_", id, sep = '') })
  batchtools::setJobNames(ids = ids, names = unlist(names))

  # Chunk jobs where input size is divided by max_jobs
  if (max_jobs != -1){
    chunk <- NULL
    job.id <- NULL
    ids[, chunk := batchtools::chunk(job.id, chunk.size = (length(reaches) / max_jobs))]
  }

  # Submit jobs to the cluster
  done <- batchtools::submitJobs(ids, reg = reg,
                                 resources = list(walltime = 10080, memory = 1000,
                                                  ncpus = 3,
                                                  chunks.as.arrayjobs = as_job_array,
                                                  foreach.backend = "parallel",
                                                  ntasks = 1,
                                                  partition = partition))
  # Wait for jobs to complete then clear registry
  batchtools::waitForJobs()
  batchtools::clearRegistry()
  batchtools::removeRegistry(wait = 0, reg = reg)
}
