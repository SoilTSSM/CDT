
SPI_monthly_function <- function(precip.mat, tscale, distribution = 'Gamma'){
	nl <- nrow(precip.mat)
	don.tmp <- precip.mat
	don.tmp[] <- NA
	don.tmp0 <- don.tmp
	icol <- which(colSums(!is.na(precip.mat)) > 5)
	if(length(icol) == 0) return(don.tmp0)
	don.tmp <- don.tmp[, icol, drop = FALSE]
	precip.mat <- precip.mat[, icol, drop = FALSE]

	for(k in 1:(nl-tscale+1))
		don.tmp[k+tscale-1, ] <- colSums(precip.mat[k:(k+tscale-1), , drop = FALSE], na.rm = TRUE)

	spi <- lapply(1:12, function(k){
		don.mon <- don.tmp[seq(k+tscale-1, nl, 12), , drop = FALSE]
		if(distribution == 'Gamma'){
			pzero <- colSums(!is.na(don.mon) & don.mon == 0)/colSums(!is.na(don.mon))
			don.mon[don.mon == 0] <- NA
			don.mean <- colMeans(don.mon, na.rm = TRUE)

			beta <- don.mean
			alpha <- rep(1, length(beta))
			nonzero <- colSums(!is.na(don.mon))
			iz <- nonzero > 0
			if(any(iz)){
				A <- log(don.mean[iz]) - colSums(log(don.mon[, iz, drop = FALSE]), na.rm = TRUE)/nonzero[iz]
				alpha[iz] <- (1 + sqrt(1 + 4 * A / 3)) / (4 * A)
				beta[iz] <- don.mean[iz]/alpha
			}

			G <- pgamma(don.tmp[seq(k+tscale-1, nl, 12), , drop = FALSE], shape = alpha, scale = beta)
			pzero <- matrix(pzero, nrow = nrow(G), ncol = ncol(G), byrow = TRUE)
			H <- pzero + (1-pzero) * G
			H[is.nan(H)] <- NA
			tt <- H
			ix <- !is.na(H) & H > 0 & H <= 0.5
			tt[ix] <- sqrt(log(1/(H[ix])^2))
			tt[!ix] <- sqrt(log(1/(1-H[!ix])^2))

			spi <- tt-((2.515517+0.802853*tt+0.010328*(tt^2))/(1+1.432788*tt+0.189269*(tt^2)+0.001308*(tt^3)))
			spi[ix] <- -spi[ix]
		}

		if(distribution == 'Normal'){
			don.mean <- colMeans(don.mon, na.rm = TRUE)
			don.sd <- matrixStats::colSds(don.mon, na.rm = TRUE)
			spi <- sweep(sweep(don.mon, 2, don.mean, FUN = "-"), 2, don.sd, FUN = "/")
		}
		return(spi)
	})

	for(k in 1:12) don.tmp[seq(k+tscale-1, nl, 12), ] <- spi[[k]]
	don.tmp0[, icol] <- don.tmp
	rm(spi, don.tmp)
	return(don.tmp0)
}
