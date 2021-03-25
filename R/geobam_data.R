#' Run geobam
#'
#' Write output from geoBAM execution on each reach data input.
#'
#' @param reachid string reach identifier
#' @param data_dir directory of SWOT and SoS input files
#' @param output_dir directory where posterior output will be written
#'
#' @export
run_geobam <- function(reachid, data_dir, output_dir) {

  # Get SWOT and SoS input data
  reach_data <- get_input_data(reachid, data_dir)

  # Create list to hold posterior data that contains NA values
  if (reach_data$valid == TRUE) nx <- nrow(reach_data$width) else nx <- length(reach_data$nx)
  posterior_list <- create_posterior_list(nx, length(reach_data$nt))

  # Run geobam on valid input reach data
  if (reach_data$valid == TRUE) {
    posteriors <- get_posteriors(reach_data)
    posterior_list <- lapply(list(1, 2, 3), update_posteriors, posteriors = posteriors, posterior_list = posterior_list)
  }

  # Write posteriors to netCDF
  write_netcdf(reach_data, posterior_list, output_dir)

}

#' Creates data.frame of NA values for each geoBAM posterior.
#'
#' @param nx integer number of nodes
#' @param nt integer number of time steps
#'
#' @return list of posteriors set to NA
create_posterior_list <- function(nx, nt){

  # Define list length for nx and nt vectors
  nx_vector <- rep(NA, nx)
  nt_vector <- rep(NA, nt)

  base_list <- list(logQ_mean = nt_vector,
                    logn_man_mean = nx_vector,
                    logn_amhg_mean = nx_vector,
                    A0_mean = nx_vector,
                    logWc_mean = NA,
                    logQc_mean = NA,
                    b_mean = nx_vector,
                    logr_mean = nx_vector,
                    logWb_mean = nx_vector,
                    logDb_mean = nx_vector,
                    logQ_sd = nt_vector,
                    logn_man_sd = nx_vector,
                    logn_amhg_sd = nx_vector,
                    A0_sd = nx_vector,
                    logWc_sd = NA,
                    logQc_sd = NA,
                    b_sd = nx_vector,
                    logr_sd = nx_vector,
                    logWb_sd = nx_vector,
                    logDb_sd = nx_vector)

  # Create a list of NA values for each chain
  return(list(base_list, base_list, base_list))
}

#' Generate and extract geobam posteriors from reach data.
#'
#' @param reach_data list of reach data (input observations)
#'
#' @return list of 3 chains with mean and sd posterior values
get_posteriors <- function(reach_data) {

  # Run geobam
  geobam <- geoBAMr::bam_data(w = reach_data$width, s = reach_data$slope2,
                              dA = reach_data$d_x_area, Qhat = reach_data$qhat,
                              variant = 'manning_amhg', max_xs = nrow(reach_data$width))
  priors <- geoBAMr::bam_priors(bamdata = geobam)

  # Create and store three chains from bam_estimate run
  cl <- parallel::makeCluster(3)
  #parallel::clusterExport(cl, c("extract_geobam_posteriors", "get_mean", "get_sd"))
  doParallel::registerDoParallel(cl)
  `%dopar%` <- foreach::`%dopar%`
  posteriors_list <- foreach::foreach(indexes = 1:3, .combine = 'c',
                                      .packages = c("geoBAMr"),
                                      .export = c("extract_geobam_posteriors", "get_mean", "get_sd")) %dopar%
    extract_geobam_posteriors(geobam_data = geobam,
                              priors = priors)
  parallel::stopCluster(cl)

  # Return list of posteriors for each chain
  return(posteriors_list)

}

#' Title
#'
#' @param geobam_data object that contains geobam observation data
#' @param priors object that contains prior data
#'
#' @return list of mean and sd values for the chain
extract_geobam_posteriors <- function(geobam_data, priors) {
  # Get stanfit object data
  estimate <- geoBAMr::bam_estimate(bamdata = geobam_data,
                                    bampriors = priors,
                                    variant = 'manning_amhg',
                                    chains = 1, cores = 1)
  # Extract posteriors
  posteriors <- rstan::extract(estimate, permuted = TRUE)[1:10]
  mean_posteriors <- lapply(posteriors, get_mean)
  sd_posteriors <- lapply(posteriors, get_sd)

  return(list(list(mean = mean_posteriors, sd = sd_posteriors)))

}

#' Column-wise mean
#'
#' Get the column-wise mean.
#'
#' @param posterior list
#'
#' @return list of means
get_mean <- function(posterior) {
  as.vector(colMeans(posterior))
}

#' Column-wise standard deviation
#'
#'Get the column-wise standard deviation
#'
#' @param posterior list
#'
#' @return list of standard deviations
get_sd <- function(posterior) {

  # An array with 3 dimensions (nchain x 1 x nx)
  if (ncol(posterior) == 1 && !identical(dim(posterior), as.integer(c(500, 1)))) {
    posterior <- posterior[,-2,]
  }

  apply(posterior, 2, stats::sd)

}

#' Update posterior list with mean and sd posteriors in posteriors parameter
#'
#' @param index integer number index (chain number) for both parameter lists
#' @param posteriors list of posterior mean and sd values
#' @param posterior_list list of posterior data
#'
#' @return list of updated posterior data for chain
update_posteriors <- function(index, posteriors, posterior_list) {

  # Update posterior list with posterior data
  mean_posteriors <- posteriors[[index]]$mean
  sd_posteriors <- posteriors[[index]]$sd

  posterior_list[[index]]$logQ_mean = mean_posteriors$logQ
  posterior_list[[index]]$logn_man_mean = mean_posteriors$logn_man
  posterior_list[[index]]$logn_amhg_mean = mean_posteriors$logn_amhg
  posterior_list[[index]]$A0_mean = mean_posteriors$A0
  posterior_list[[index]]$logWc_mean = mean_posteriors$logWc
  posterior_list[[index]]$logQc_mean = mean_posteriors$logQc
  posterior_list[[index]]$b_mean = mean_posteriors$b
  posterior_list[[index]]$logr_mean = mean_posteriors$logr
  posterior_list[[index]]$logWb_mean = mean_posteriors$logWb
  posterior_list[[index]]$logDb_mean = mean_posteriors$logDb
  posterior_list[[index]]$logQ_sd = sd_posteriors$logQ
  posterior_list[[index]]$logn_man_sd = sd_posteriors$logn_man
  posterior_list[[index]]$logn_amhg_sd = sd_posteriors$logn_amhg
  posterior_list[[index]]$A0_sd = sd_posteriors$A0
  posterior_list[[index]]$logWc_sd = sd_posteriors$logWc
  posterior_list[[index]]$logQc_sd = sd_posteriors$logQc
  posterior_list[[index]]$b_sd = sd_posteriors$b
  posterior_list[[index]]$logr_sd = sd_posteriors$logr
  posterior_list[[index]]$logWb_sd = sd_posteriors$logWb
  posterior_list[[index]]$logDb_sd = sd_posteriors$logDb

  return(posterior_list[[index]])
}
