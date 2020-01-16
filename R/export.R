#' Predict sample distribution of SF-6D dimensions
#'
#' These functions provide estimates of the predicted sample distribution of each SF-6D dimension from the sample means
#'   and standard deviations of SF-12 PCS and MCS scores (and optionally their correlation).
#'
#' @param means A vector: \code{(mean(PCS), mean(MCS))}
#' @param SDs A vector: \code{(sd(PCS), sd(MCS))}
#' @param corr \strong{(optional)} A matrix: \code{(Cor(PCS, MCS))}. Default is to use the correlation matrix obtained
#'   from the estimation sample.
#' @param version \strong{(optional)} An integer: 1 = PCS/MCS scores are from SF-12-v1; 2 (default) = PCS/MCS scores are
#'   from SF-12-v2. Other values give an error.
#'
#' @seealso \code{\link{SF12_to_SF6D}} for calculating the resulting utility values.
#'
#' @name SF6D_distributions
NULL

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length three: \code{(Pr(PF==1), Pr(PF==2), Pr(PF==3))}
#' @export
SF12_to_PF <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("PF", means, SDs, corr, version)

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length two: \code{(Pr(RL==1), Pr(PF>1))}
#' @export
SF12_to_RL <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("RL", means, SDs, corr, version)

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length five: \code{(Pr(SF==1), ..., Pr(SF==5))}
#' @export
SF12_to_SF <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("SF", means, SDs, corr, version)

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length five: \code{(Pr(PAIN==1), ..., Pr(PAIN==5))}
#' @export
SF12_to_PAIN <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("PAIN", means, SDs, corr, version)

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length five: \code{(Pr(MH==1), ..., Pr(MH==5))}
#' @export
SF12_to_MH <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("MH", means, SDs, corr, version)

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length five: \code{(Pr(VIT==1), ..., Pr(VIT==5))}
#' @export
SF12_to_VIT <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("VIT", means, SDs, corr, version)

#' @rdname SF6D_distributions
#' @return \code{SF12_to_PF}: A vector of length two: \code{(Pr(MOST==0), Pr(MOST==1))}
#' @export
SF12_to_MOST <- function(means, SDs, corr = NULL, version = 2) SF12_to_dim("MOST", means, SDs, corr, version)

#' Predict sample mean SF-6D utility
#'
#' \code{SF12_to_SF6D} provides an estimate of the predicted sample mean SF-6D utility value from the sample means and
#'   standard deviations of SF-12 PCS and MCS scores (and optionally their correlation).
#'
#' @inheritParams SF6D_distributions
#' @param dimensions \strong{(optional)} A character vector: one or more of "utility", "PF", "RL", "SF", "PAIN", "MH",
#'   "VIT", "MOST", indicating which dimensions should be predicted ("utility" (the default) gives the overall SF-6D
#'   utility score); or "all", indicating all of the above.
#' @param values \strong{(optional)} A list with items \code{PF}, \code{RL}, .... SF-6D utility values (additive
#'   detriments from perfect health). The default is to use the UK general population valuations provided by Brazier &
#'   Roberts (2004).
#' @param version \strong{(optional)} An integer: 1 = PCS/MCS scores are from SF-12-v1; 2 (default) = PCS/MCS scores are
#'   from SF-12-v2. Other values give an error.
#'
#' @return A numeric vector of length \code{length(dimensions)} (or length 8 for \code{dimensions == "all"}): predicted
#'   sample mean utility value and/or contributions to mean utility of each dimension.
#'
#' @seealso \code{\link{SF6D_distributions}} for the dimension-specific functions, and \code{\link{SF6D_uncertainty}}
#' for uncertainty intervals around the predicted mean utility values.
#'
#' @section References:
#' Brazier, John E and Roberts, Jennifer. 2004. The estimation of a preference-based measure of health from the SF-12.
#'   \emph{Med Care}, 42(9):851-859.
#'
#' @export
SF12_to_SF6D <- function(means, SDs, corr = NULL, dimensions = NULL, values = NULL, version = 2) {
	if (is.null(dimensions)) dimensions <- "utility"
	if ("all" %in% dimensions) dimensions <- c("utility", "PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST")
	if (!all(dimensions %in% c("utility", "PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST"))){
		warning("Unknown dimension names in 'dimensions'", call. = FALSE)
		dimensions <- intersect(dimensions, c("utility", "PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST"))
	}
	if (is.null(values)) values <- list(PF = c(0, 0, 0.045),
																			RL = c(0, 0.063),
																			SF = c(0, 0.063, 0.066, 0.081, 0.093),
																			PAIN = c(0, 0, 0.042, 0.077, 0.137),
																			MH = c(0, 0.059, 0.059, 0.113, 0.134),
																			VIT = c(0, 0.078, 0.078, 0.078, 0.106),
																			MOST = c(0, 0.077))

	if ("utility" %in% dimensions) {
		dimensions_est <- c("PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST")
	} else dimensions_est <- dimensions
	if (!(version %in% c(1, 2))) warning("Version must be either '1' (SF-12-v1) or '2' (SF-12-v2)", call. = FALSE)

	pr_distr <- utility <- vector("list", length = length(dimensions_est))
	names(pr_distr) <- names(utility) <- dimensions_est
	for (d in 1:length(dimensions_est)) {
		pr_distr[[d]] <- SF12_to_dim(dimensions_est[[d]], means, SDs, corr, version)
		utility[[d]] <- -crossprod(pr_distr[[d]], values[[dimensions_est[[d]]]])
	}

	if ("utility" %in% dimensions) utility[["utility"]] <- 1 + sum(unlist(utility))

	unlist(utility[dimensions])
}

#' Estimate prediction uncertainty in mapped mean SF-6D utility values
#'
#' \code{SF6D_uncertainty} provides an estimate of the prediction uncertainty associated with the mean utility value
#'   predicted by the mapping algorithm.
#'
#' @param N A positive integer: sample size.
#' @param dimensions \strong{(optional)} A character vector: one or more of "utility", "PF", "RL", "SF", "PAIN", "MH",
#'   "VIT", "MOST", indicating which dimensions should be predicted ("utility" (the default) gives the overall SF-6D
#'   utility score); or "all", indicating all of the above.
#' @param level \strong{(optional)} A numeric scalar in (0, 1): significance level for uncertainty intervals. Default is
#'   0.95, for 95 percent uncertainty intervals. Can also be specified as \code{"se"} to give standard errors
#'   (equivalent to \code{level=1-2*(1-pnorm(1))}).
#' @param version \strong{(optional)} An integer: 1 = PCS/MCS scores are from SF-12-v1; 2 (default) = PCS/MCS scores are
#'   from SF-12-v2. Other values give an error.
#'
#' @return A numeric vector of length \code{length(dimensions)} (or length 8 for \code{dimensions == "all"}):
#'   (half-)width of the estimated uncertainty intervals for sample mean utility and/or contributions to mean utility of
#'   each dimension (i.e. (level * 100)\% uncertainty interval = point estimate ± return value)
#'
#' @seealso \code{\link{SF6D_distributions}} for the dimension-specific functions, and \code{\link{SF12_to_SF6D}} for
#' the estimated sample mean utility values.
#'
#' @export
SF6D_uncertainty <- function(N, dimensions = NULL, level = 0.95, version = 2) {
	if (length(N) != 1L || N <= 0 || abs(N - round(N)) > .Machine$double.eps^0.5)
		stop("'N' must be a positive scalar integer")
	if (is.null(dimensions)) dimensions <- "utility"
	if ("all" %in% dimensions) dimensions <- c("utility", "PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST")
	if (!all(dimensions %in% c("utility", "PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST"))){
		warning("Unknown dimension names in 'dimensions'", call. = FALSE)
		dimensions <- intersect(dimensions, c("utility", "PF", "RL", "SF", "PAIN", "MH", "VIT", "MOST"))
	}
	if (identical(level, "se")) level <- 1 - 2 * (1 - stats::pnorm(1))
	if (length(level) != 1L || !is.numeric(level) || level <= 0 || level >= 1)
		stop("'level' must be a scalar between 0 and 1, or \"se\" for standard errors instead of uncertainty interval")
	if (!(version %in% c(1, 2))) warning("Version must be either '1' (SF-12-v1) or '2' (SF-12-v2)", call. = FALSE)

	se <- vector("list", length = length(dimensions))
	names(se) <- dimensions
	if (version == 1) SE_coef_table <- SE_coefs_v1 else SE_coef_table <- SE_coefs
	for (d in 1:length(dimensions)) {
		coefs <- SE_coef_table[[dimensions[[d]]]]
		se[[d]] <- coefs[["alpha"]] + coefs[["beta"]] * N ^ -coefs[["gamma"]]
	}

	unlist(se) * stats::qnorm(1 - (1 - level) / 2)
}
