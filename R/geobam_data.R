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

  # Create data.frame to hold posterior data contains NA values
  if (reach_data$valid == TRUE) nx <- nrow(reach_data$width) else nx <- length(reach_data$nx)
  posterior_list <- create_posterior_list(reachid, nx, length(reach_data$nt))

  # Run geobam on valid input reach data
  if (reach_data$valid == TRUE) {
    posteriors = get_posteriors(reach_data)
    posterior_list <- update_posteriors(posterior_list, posteriors)
  }

  # Write posteriors to netCDF
  write_netcdf(reach_data, posterior_list, output_dir)

}

#' Creates data.frame of NA values for each geoBAM posterior.
#'
#' @param reachid string reach identifier
#' @param nx integer number of nodes
#' @param nt integer number of time steps
#'
#' @return list of posteriors set to NA
create_posterior_list <- function(reachid, nx, nt){

  # Define list length for nx and nt vectors
  nx_vector <- rep(NA, nx)
  nt_vector <- rep(NA, nt)

  # Create a list of NA values
  return(list(reachid = reachid,
             logQ_mean = nt_vector,
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
             logDb_sd = nx_vector))
}

#' Generate and extract geobam posteriors from reach data.
#'
#' @param reach_data list of reach data (input observations)
#'
#' @return list of mean and sd posteriors
get_posteriors <- function(reach_data) {

  # Run geobam
  geobam <- geoBAMr::bam_data(w = reach_data$width, s = reach_data$slope2, dA = reach_data$d_x_area,
                              Qhat = reach_data$qhat, variant = 'manning_amhg', max_xs = nrow(reach_data$width))
  priors <- geoBAMr::bam_priors(bamdata = geobam)
  estimate <- geoBAMr::bam_estimate(bamdata = geobam, bampriors = priors, variant = 'manning_amhg')

  # Extract posteriors
  posteriors <- rstan::extract(estimate, permuted = TRUE)[1:10]
  mean_posteriors <- lapply(posteriors, get_mean)
  sd_posteriors <- lapply(posteriors, get_sd)

  return(list(mean_posteriors = mean_posteriors, sd_posteriors = sd_posteriors))

}

#' Update posterior list with mean and sd posteriors in posteriors parameter
#'
#' @param posterior_list list of posterior data
#' @param posteriors list of posterior mean and sd values
#'
#' @return data.frame of updated posterior data
update_posteriors <- function(posterior_list, posteriors) {

  # Update data.frame with posterior data
  mean_posteriors <- posteriors$mean_posteriors
  sd_posteriors <- posteriors$sd_posteriors

  # Update posterior data.frame
  posterior_list$logQ_mean = mean_posteriors$logQ
  posterior_list$logn_man_mean = mean_posteriors$logn_man
  posterior_list$logn_amhg_mean = mean_posteriors$logn_amhg
  posterior_list$A0_mean = mean_posteriors$A0
  posterior_list$logWc_mean = mean_posteriors$logWc
  posterior_list$logQc_mean = mean_posteriors$logQc
  posterior_list$b_mean = mean_posteriors$b
  posterior_list$logr_mean = mean_posteriors$logr
  posterior_list$logWb_mean = mean_posteriors$logWb
  posterior_list$logDb_mean = mean_posteriors$logDb
  posterior_list$logQ_sd = sd_posteriors$logQ
  posterior_list$logn_man_sd = sd_posteriors$logn_man
  posterior_list$logn_amhg_sd = sd_posteriors$logn_amhg
  posterior_list$A0_sd = sd_posteriors$A0
  posterior_list$logWc_sd = sd_posteriors$logWc
  posterior_list$logQc_sd = sd_posteriors$logQc
  posterior_list$b_sd = sd_posteriors$b
  posterior_list$logr_sd = sd_posteriors$logr
  posterior_list$logWb_sd = sd_posteriors$logWb
  posterior_list$logDb_sd = sd_posteriors$logDb

  return(posterior_list)
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
  if (ncol(posterior) == 1 && !identical(dim(posterior), as.integer(c(1500, 1)))) {
    posterior <- posterior[,-2,]
  }

  apply(posterior, 2, stats::sd)

}
