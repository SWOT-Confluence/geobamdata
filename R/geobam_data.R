#' Run geobam
#'
#' Get output from geoBAMr execution on each netCDF input file.
#'
#' @param file netCDF file
#'
#' @return data.frame
#'
#' @export
run_geobam <- function(reach) {

  # Run geobam on input reach data
  geobam <- geoBAMr::bam_data(w = reach$width, s = reach$slope2, dA = reach$d_x_area,
                              Qhat = reach$qhat, variant = 'manning_amhg', max_xs = nrow(reach$width))
  priors <- geoBAMr::bam_priors(bamdata = geobam)
  estimate <- geoBAMr::bam_estimate(bamdata = geobam, bampriors = priors,variant = 'manning_amhg')

  # Obtain output
  posteriors <- rstan::extract(estimate, permuted = TRUE)[1:10]
  mean_posteriors <- lapply(posteriors, get_mean)
  sd_posteriors <- lapply(posteriors, get_sd)

  # Create dataframe to hold output data
  n <- length(mean_posteriors$logQ)
  return(data.frame(reachid = reach$reachid,
                    logQ_mean = mean_posteriors$logQ[1:n],
                    logn_man_mean = mean_posteriors$logn_man[1:n],
                    logn_amhg_mean = mean_posteriors$logn_amhg[1:n],
                    A0_mean = mean_posteriors$A0[1:n],
                    logWc_mean = mean_posteriors$logWc[1:n],
                    logQc_mean = mean_posteriors$logQc[1:n],
                    b_mean = mean_posteriors$b[1:n],
                    logr_mean = mean_posteriors$logr[1:n],
                    logWb_mean = mean_posteriors$logWb[1:n],
                    logDb_mean = mean_posteriors$logDb[1:n],
                    logQ_sd = sd_posteriors$logQ[1:n],
                    logn_man_sd = sd_posteriors$logn_man[1:n],
                    logn_amhg_sd = sd_posteriors$logn_amhg[1:n],
                    A0_sd = sd_posteriors$A0[1:n],
                    logWc_sd = sd_posteriors$logWc[1:n],
                    logQc_sd = sd_posteriors$logQc[1:n],
                    b_sd = sd_posteriors$b[1:n],
                    logr_sd = sd_posteriors$logr[1:n],
                    logWb_sd = sd_posteriors$logWb[1:n],
                    logDb_sd = sd_posteriors$logDb[1:n]))

  # ?? Easier data.frame creation but does not include NA's for filtering different sized columns
  #return(data.frame(reachid = id, mean = mean_posteriors, sigma = sd_posteriors, stringsAsFactors = FALSE))

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
