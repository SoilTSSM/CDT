
SPEI_function <- function(data.mat, tscale = 1, frequency = 12, distribution = 'Gamma'){
	nl <- nrow(data.mat)
	don.tmp0 <- data.mat*NA
	icol <- which(colSums(!is.na(data.mat)) > 4)
	if(length(icol) == 0) return(don.tmp0)
	data.mat <- data.mat[, icol, drop = FALSE]

	if(tscale > 1){
		don.tmp <- don.tmp0
		for(k in 1:(nl-tscale+1))
			don.tmp[k+tscale-1, ] <- colSums(data.mat[k:(k+tscale-1), , drop = FALSE], na.rm = TRUE)
	}else don.tmp <- data.mat

	estime.pars.fun <- switch(distribution,
						"Gamma" = list(pargam, cdfgam),
						"Pearson Type III" = list(parpe3, cdfpe3),
						"log-Logistic" = list(parglo, cdfglo))

	estime.pars <- function(x){
		x <- x[!is.na(x)]
		if(length(x) < 5) return(NULL)
		lmr <- lmoms(x)
		if(!are.lmom.valid(lmr)) return(NULL)
		estime.pars.fun[[1]](lmr)
	}

	spi <- lapply(1:frequency, function(k){
		iseq <- seq(k+tscale-1, nl, frequency)
		don.mon <- don.tmp[iseq, , drop = FALSE]
		spi.out <- don.mon*NA

		no.na <- colSums(!is.na(don.mon))
		don.sd <- matrixStats::colSds(don.mon, na.rm = TRUE)
		icol1 <- no.na > 4 & !is.na(don.sd) & don.sd != 0
		if(!any(icol1)) return(spi.out)
		don.mon <- don.mon[, icol1, drop = FALSE]

		if(distribution %in% c("Gamma", "Pearson Type III", "log-Logistic")){
			don.mon1 <- don.mon
			SPI <- don.mon*NA

			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- colSums(!is.na(don.mon) & don.mon == 0)/colSums(!is.na(don.mon))
				don.mon[don.mon <= 0] <- NA
			}

			PARS <- lapply(seq(ncol(don.mon)), function(j) estime.pars(don.mon[, j]))
			inull <- sapply(PARS, is.null)
			if(all(inull)) return(spi.out)

			PARS <- PARS[!inull]
			don.mon1 <- don.mon1[, !inull, drop = FALSE]

			spi <- lapply(seq(ncol(don.mon1)), function(j) qnorm(estime.pars.fun[[2]](don.mon1[, j], PARS[[j]])))
			spi <- do.call(cbind, spi)

			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- matrix(pzero[!inull], nrow(spi), ncol(spi), byrow = TRUE)
				SPI[, !inull] <- qnorm(pzero + (1-pzero)*pnorm(spi))
			}else SPI[, !inull] <- spi
		}

		if(distribution == 'Z-Score'){
			don.sd <- don.sd[icol1]
			don.mean <- colMeans(don.mon, na.rm = TRUE)
			SPI <- sweep(sweep(don.mon, 2, don.mean, FUN = "-"), 2, don.sd, FUN = "/")
		}

		spi.out[, icol1] <- SPI
		return(spi.out)
	})

	for(k in 1:frequency) don.tmp[seq(k+tscale-1, nl, frequency), ] <- spi[[k]]
	don.tmp0[, icol] <- don.tmp
	rm(spi, don.tmp)
	return(don.tmp0)
}

#########################################################################

SPEI_Aggregate_data <- function(data.mat, tscale = 1){
	nl <- nrow(data.mat)
	if(tscale > 1){
		don.tmp <- data.mat*NA
		for(k in 1:(nl-tscale+1))
			don.tmp[k+tscale-1, ] <- colSums(data.mat[k:(k+tscale-1), , drop = FALSE], na.rm = TRUE)
	}else don.tmp <- data.mat
	return(don.tmp)
}

#########################################################################

SPEI_Compute_params <- function(data.mat, tscale = 1, frequency = 12, distribution = 'Gamma'){
	nl <- nrow(data.mat)
	dist.params <- matrix(list(NA), nrow = frequency, ncol = ncol(data.mat))
	icol <- which(colSums(!is.na(data.mat)) > 4)
	if(length(icol) == 0) return(dist.params)
	data.mat <- data.mat[, icol, drop = FALSE]

	estime.pars.fun <- switch(distribution,
							"Gamma" = pargam,
							"Pearson Type III" = parpe3,
							"log-Logistic" = parglo)

	estime.pars <- function(x){
		x <- x[!is.na(x)]
		if(length(x) < 5) return(NULL)
		lmr <- lmoms(x)
		if(!are.lmom.valid(lmr)) return(NULL)
		estime.pars.fun(lmr)
	}

	params <- lapply(1:frequency, function(k){
		iseq <- seq(k+tscale-1, nl, frequency)
		don.mon <- data.mat[iseq, , drop = FALSE]
		pars.out <- matrix(list(NA), nrow = 1, ncol = ncol(don.mon))

		no.na <- colSums(!is.na(don.mon))
		don.sd <- matrixStats::colSds(don.mon, na.rm = TRUE)
		icol1 <- no.na > 4 & !is.na(don.sd) & don.sd != 0
		if(!any(icol1)) return(pars.out)
		don.mon <- don.mon[, icol1, drop = FALSE]

		if(distribution %in% c("Gamma", "Pearson Type III", "log-Logistic")){
			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- colSums(!is.na(don.mon) & don.mon == 0)/colSums(!is.na(don.mon))
				don.mon[don.mon <= 0] <- NA
			}

			PARS <- lapply(seq(ncol(don.mon)), function(j){
				prs <- estime.pars(don.mon[, j])
				if(is.null(prs)) return(list(NA))
				if(distribution == "log-Logistic") prs else c(prs, list(pzero = pzero[j]))
			})
		}

		if(distribution == 'Z-Score'){
			don.sd <- don.sd[icol1]
			don.mean <- colMeans(don.mon, na.rm = TRUE)
			PARS <- lapply(seq_along(don.mean), function(j) list(mean = don.mean[j], sd = don.sd[j]))
		}

		pars.out[, icol1] <- PARS
		return(pars.out)
	})

	dist.params[, icol] <- do.call(rbind, params)
	return(dist.params)
}

#########################################################################

SPEI_computation <- function(data.mat, params, tscale = 1, frequency = 12, distribution = 'Gamma'){
	nl <- nrow(data.mat)
	nc <- ncol(data.mat)
	don.tmp0 <- data.mat*NA

	cdf.fun <- switch(distribution,
					"Gamma" = cdfgam,
					"Pearson Type III" = cdfpe3,
					"log-Logistic" = cdfglo)

	spi.out <- lapply(1:frequency, function(k){
		iseq <- seq(k+tscale-1, nl, frequency)
		don.tmp <- data.mat[iseq, , drop = FALSE]
		if(distribution %in% c("Gamma", "Pearson Type III", "log-Logistic")){
			spi <- lapply(seq(nc), function(j){
				pars <- params[k, j][[1]]
				if(is.na(pars[[1]])) return(don.tmp[, j]*NA)
				qnorm(cdf.fun(don.tmp[, j], pars))
			})
			spi <- do.call(cbind, spi)

			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- sapply(params[k, ], function(x){
					pz <- if(is.list(x)) x$pzero else NULL
					if(is.null(pz)) pz <- NA
					pz
				})
				pzero <- matrix(pzero, nrow(spi), ncol(spi), byrow = TRUE)
				spi <- qnorm(pzero + (1-pzero)*pnorm(spi))
			}
		}

		if(distribution == 'Z-Score'){
			don.sd <- sapply(params[k, ], '[[', 'sd')
			don.mean <- sapply(params[k, ], '[[', 'mean')
			spi <- sweep(sweep(don.tmp, 2, don.mean, FUN = "-"), 2, don.sd, FUN = "/")
		}

		return(spi)
	})

	for(k in 1:frequency) don.tmp0[seq(k+tscale-1, nl, frequency), ] <- spi.out[[k]]
	rm(spi.out)
	return(don.tmp0)
}



