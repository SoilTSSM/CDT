
bias.RR.times.fun <- function(df, min.len) by(df, df$times, bias.RR.calc.fun, min.len)

bias.RR.calc.fun <- function(df, min.len){
	bs <- 1
	ix <- which(!is.na(df$stn) & !is.na(df$rfe))
	df <- df[ix, , drop = FALSE]
	if(nrow(df) >= min.len){
		sum.stn <- sum(df$stn)
		sum.rfe <- sum(df$rfe)
	 	if(sum.rfe > 0){
	 		bs <- sum.stn/sum.rfe
	 		if(bs > 3) bs <- 3
	 	} 
	}
	return(bs)
}

### fit distribution Bernoulli-Gamma 
fit.berngamma <- function(x, min.len){
	x <- x[!is.na(x)]
	if(length(x) > min.len){
		if(length(x[x > 0]) > 2){
			if(var(x[x > 0]) == 0) x[x > 0] <- x[x > 0]+runif(length(x[x > 0]))
		}else return(NULL)
		ts <- startberngamma(x)
		fit.mod <- try(fitdist(x, "berngamma", method = 'mle',
					start = list(prob = ts$prob, scale = ts$scale, shape = ts$shape), 
					lower = c(0, 1e-10, 1e-10), upper = rep(Inf, 3)), silent = TRUE)
		if(!inherits(fit.mod, "try-error")) return(fit.mod)
		else return(NULL)
	}else return(NULL)
}

### Extract parameters
extract.BG.parameters <- function(months, fitted.model){
	pars.Ret <- vector(mode = 'list', length = 12)
	pars.Ret[months] <- lapply(months, function(m){
		pars <- sapply(fitted.model[[m]], function(x){
				y <- x$estimate
				if(is.null(y)) y <- rep(NA, 3)
				y
			})
		list(prob = pars[1, ], scale = pars[2, ], shape = pars[3, ])
	})
	pars.Ret[months] <- rapply(pars.Ret[months], f = function(x) ifelse(is.nan(x) | is.infinite(x), NA, x), how = "replace")
	return(pars.Ret)
}


### Quantile mapping
quantile.mapping.BGamma <- function(x, pars.stn, pars.rfe, rfe.zero){
	res <- x
	p.rfe <- 1-pars.rfe$prob
	ix <- !is.na(x) & (x > 0)
	p.rfe[ix] <- 1-pars.rfe$prob[ix] + pars.rfe$prob[ix]*pgamma(x[ix],
	 			scale = pars.rfe$scale[ix], shape = pars.rfe$shape[ix])
	p.rfe[p.rfe == 1] <- 1-1e-15
	ip <- p.rfe > (1-pars.stn$prob)
	res[ip] <- qgamma((pars.stn$prob[ip]+p.rfe[ip]-1)/pars.stn$prob[ip],
                     scale = pars.stn$scale[ip], shape = pars.stn$shape[ip])
	res[is.na(x)] <- NA
	if(rfe.zero) res[x == 0] <- 0
    return(res)
}

## Fit linear model
fitLM.fun.RR <- function(df, min.len){
	df <- na.omit(df)
	if(nrow(df) >= min.len) lm(stn~rfe, data = df)
	else return(NULL)
}

fitLM.month.RR <- function(df, min.len) by(df, df$month, fitLM.fun.RR, min.len)

#################################################################################################

bias.TT.times.fun <- function(df, min.len) by(df, df$times, bias.TT.calc.fun, min.len)

bias.TT.calc.fun <- function(df, min.len){
	bs <- 1
	ix <- which(!is.na(df$stn) & !is.na(df$tt))
	df <- df[ix, , drop = FALSE]
	if(nrow(df) >= min.len){
		bs <- sum(df$stn)/sum(df$tt)
		if(is.nan(bs)) bs <- 1    # 0/0
		if(is.infinite(bs)) bs <- 1.5  # n/0
		if(bs == 0) bs <- 0.5  # 0/n
		if(bs > 3) bs <- 3
		if(bs < 0) bs <- 1
	}
	return(bs)
}

### fit normal distribution for temp
fit.norm.temp <- function(x, min.len){
	x <- x[!is.na(x)]
	if(length(x) > min.len){
		xmoy <- mean(x)
		xsd <- sd(x)
		fit.mod <- try(fitdist(x, "norm", method = 'mle',
					start = list(mean = xmoy, sd = xsd), 
					lower = c(-40, 0), upper = c(60, Inf)), silent = TRUE)
		if(!inherits(fit.mod, "try-error")) return(fit.mod)
		else return(NULL)
	}else return(NULL)
}

### Extract parameters
extract.Gau.parameters <- function(months, fitted.model){
	pars.Ret <- vector(mode = 'list', length = 12)
	pars.Ret[months] <- lapply(months, function(m){
		pars <- sapply(fitted.model[[m]], function(x){
				y <- x$estimate
				if(is.null(y)) y <- rep(NA, 2)
				y
			})
		list(mean = pars[1, ], sd = pars[2, ])
	})
	pars.Ret[months] <- rapply(pars.Ret[months], f = function(x) ifelse(is.nan(x) | is.infinite(x), NA, x), how = "replace")
	return(pars.Ret)
}

### Quantile mapping
quantile.mapping.Gau <- function(x, pars.stn, pars.reanal){
	p.reanal <- x
	ix <- !is.na(x)
	p.reanal[ix] <- pnorm(x[ix], mean = pars.reanal$mean[ix], sd = pars.reanal$sd[ix])
	res <- qnorm(p.reanal, mean = pars.stn$mean, sd = pars.stn$sd)
	res[is.na(x)] <- NA
    return(res)
}

## Fit linear model
fitLM.fun.TT <- function(df, min.len){
	df <- na.omit(df)
	if(nrow(df) >= min.len) lm(stn~tt, data = df)
	else return(NULL)
}

fitLM.month.TT <- function(df, min.len) by(df, df$month, fitLM.fun.TT, min.len)


