#' Run geobam
#'
#' Get output from geoBAMr execution on each netCDF input file.
#'
#' @param file netCDF file
#'
#' @return data.frame
#'
#' @export
run_geobam <- function(file) {

  # Open file for reading and get data
  input <- ncdf4::nc_open(file)
  data <- get_data(input)

  # Run geobam on input data
  geobam <- geoBAMr::bam_data(w = data$width, s = data$slope2, dA = data$d_x_area,
                              Qhat = data$qhat, variant = 'manning_amhg', max_xs = nrow(data$width))
  priors <- geoBAMr::bam_priors(bamdata = geobam)
  estimate <- geoBAMr::bam_estimate(bamdata = geobam, bampriors = priors,variant = 'manning_amhg')

  # Obtain output
  posteriors <- rstan::extract(estimate, permuted = TRUE)[1:10]
  mean_posteriors <- lapply(posteriors, get_mean)
  sd_posteriors <- lapply(posteriors, get_sd)

  # Create dataframe to hold output data
  id <- get_reachid(file)
  n <- length(mean_posteriors$logQ)
  return(data.frame(reachid = id,
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

#' Get netcdf data from file
#'
#' Retrieve geoBAM input variables and replace fill values with NA.
#'
#' @param file netcdf
#'
#' @return list matrix
get_data <- function(file) {

  # width ?? set width NAs to 1
  width <- ncdf4::ncvar_get(file, "swot_node/width")
  width_fill <- ncdf4::ncatt_get(file, "swot_node/width", "_FillValue")
  width[width == width_fill$value] <- NA
  width[width == 0] <- NA
  width[is.na(width)] <- 1
  width <- width[1:5, 1:10]

  #d_x_area
  d_x_area <- ncdf4::ncvar_get(file, "swot_node/d_x_area")
  da_fill <- ncdf4::ncatt_get(file, "swot_node/d_x_area", "_FillValue")
  d_x_area[d_x_area == da_fill$value] <- NA
  d_x_area[d_x_area == 0] <- NA
  d_x_area <- d_x_area[1:5, 1:10]

  # slope2
  slope2_array <- ncdf4::ncvar_get(file, "swot_reach/slope2")
  slope_fill <- ncdf4::ncatt_get(file, "swot_reach/slope2", "_FillValue")
  slope2_array[slope2_array == slope_fill$value] <- NA
  slope2_array <- slope2_array[1:10]
  slope2 <- matrix(slope2_array, nrow = nrow(d_x_area), ncol = ncol(d_x_area), byrow = TRUE)
  slope2[slope2 == 0] <- 0.00001

  # Qhat ?? check that Qhat is not negative
  qhat <- ncdf4::ncvar_get(file, "sword/Qhat")
  qhat_fill <- ncdf4::ncatt_get(file, "sword/Qhat", "_FillValue")
  qhat[qhat == qhat_fill$value] <- NA
  qhat[qhat < 0] <- qhat * -1

  return(list(width = width, slope2 = slope2, d_x_area = d_x_area, qhat = qhat))

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

#' Get Reach ID
#'
#' Extract reach id from file name.
#'
#' @param file netcdf file
#'
#' @return character vector
get_reachid <- function(file) {

  file_vec <- unlist(strsplit(file, split = "/"))
  file_name <- file_vec[length(file_vec)]
  return(strsplit(substring(file_name, first = 19), split = ".nc"))
}
