#' SF12toSF6D: Predict mean SF-6D utilities from SF-12 scores in osteoarthritis patients.
#'
#' This package provides calculators to estimate SF-6D distributions, mean SF-6D utility values, and uncertainty
#' intervals around the predicted mean values from group summary SF-12 PCS and MCS scores in people with osteoarthritis,
#' using the algorithm described in the article 'An Algorithm to Predict Mean SF-6D Utility Values from Mean SF-12 MCS
#' and PCS  Scores in Osteoarthritis Patients' [manuscript under review].
#'
#' @section Functions:
#' \code{\link{SF12_to_SF6D}}: Predicted sample mean SF-6D utility values and
#'   contribution of SF-6D dimensions to mean utility values, based on sample
#'   PCS and MCS scores.
#'
#' \code{\link{SF6D_uncertainty}}: Estimated uncertainty intervals for the
#'   predicted mean utility values given by \code{\link{SF12_to_SF6D}}.
#'
#' \code{\link[=SF6D_distributions]{SF12_to_xx}}: Predicted sample
#'   distributions of SF-6D dimensions, based on sample PCS and MCS scores.
#'
#' @docType package
#' @name SF12toSF6D
NULL
