SF12_to_dim <- function(dim, means, SDs, corr = NULL, version = 2) {
	if (length(means) != 2) stop("'means'must be a length 2 vector (mean(PCS), mean(MCS))")
	if (length(SDs) != 2) stop("'SDs'must be a length 2 vector (SD(PCS), SD(MCS))")
	if (is.null(corr)) {
		if (version == 1) { corr <- corr_OAI_v1
		} else corr <- corr_OAI
	}
	if (!is.matrix(corr) || dim(corr) != c(2, 2) ||
			corr[[1, 1]] != 1 || corr[[2, 2]] != 1 || corr[[1, 2]] != corr[[2, 1]])
		stop("'corr'must be a 2*2 symmetric correlation matrix")

	if (version == 1) { dim_coefs <- coefs_v1[[dim]]
	} else dim_coefs <- coefs[[dim]]

	beta <- dim_coefs[(length(dim_coefs) - length(means) + 1):length(dim_coefs)]
	intercepts <- dim_coefs[1:(length(dim_coefs) - length(means))]
	mu_y <- crossprod(beta, means)
	Cov <- tcrossprod(SDs) * corr
	sd_y <- sqrt(1 + crossprod(beta, crossprod(Cov, beta)))
	p_gt_k <- 1 - stats::pnorm(-intercepts, mean = mu_y, sd = sd_y)

	unname(c(1, p_gt_k) - c(p_gt_k, 0))
}
