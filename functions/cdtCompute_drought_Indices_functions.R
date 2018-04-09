
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
		don.sd <- don.sd[icol1]

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
