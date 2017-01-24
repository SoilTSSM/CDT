
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

##############################
## Fit Mixture distribution
fit.mixture.distr <- function(x, min.len = 7, alpha = 0.05,
						distr.fun = c("berngamma", "bernlnorm", "bernweibull", "bernexp"),
						method = 'mle', lower = c(0, 1e-10, 1e-10), upper = c(1, Inf, Inf),
						keepdata = FALSE, keepdata.nb = 3, ...){
	require(ADGofTest)
	x <- x[!is.na(x)]
	ret <- NULL
	if(length(x) > min.len){
		if(length(x[x > 0]) > 2){
			if(var(x[x > 0]) == 0) x[x > 0] <- x[x > 0]+runif(length(x[x > 0]))
			if(length(which(x == 0)) == 0) x <- c(x, 0)
		}else return(NULL)

		ret <- lapply(distr.fun, function(distrf){
			start.fun <- match.fun(paste("start", distrf, sep = ""))
			start.pars <- start.fun(x)
			fit.mod <- try(fitdist(x, distrf, method = method, start = start.pars, lower = lower, upper = upper,
									keepdata = keepdata, keepdata.nb = keepdata.nb, ...), silent = TRUE)
			if(!inherits(fit.mod, "try-error")){
				pdistrf <- match.fun(paste('p', distrf, sep = ''))
				goftest <- do.call("ad.test", c(list(x), pdistrf, as.list(fit.mod$estimate)))	
				goftest$data.name <- paste(deparse(substitute(x)), 'and', distrf) 
				test <- if(goftest$p.value > alpha) 'yes' else 'no'
				res <- list(fitted.distr = fit.mod, ADgoftest = goftest, h0 = test)
			}else res <- list(fitted.distr = NULL, ADgoftest = NULL, h0 = 'null')
			return(res)
		})
		names(ret) <- distr.fun
	}
	return(ret)
}

##############################

outputADTest <- function(X, months = 1:12, distr = "berngamma"){
	H0.test <- vector(mode = 'list', length = 12)
	H0.test[months] <- lapply(X[months], function(mon){
		sapply(mon, function(stn) if(!is.null(stn)) stn[[distr]]$h0 else 'null')
	})
	return(H0.test)
}

##############################
### Extract parameters
extractDistrParams <- function(X, months = 1:12, distr = "berngamma"){
	pars <- vector(mode = 'list', length = 12)
	pars[months] <- lapply(X[months], function(mon){
		parstn <- lapply(mon, function(stn){
			if(!is.null(stn)){
				fitdist <- stn[[distr]]$fitted.distr
				if(!is.null(fitdist)) fitdist$estimate else NA
			}else NA
		})
		nom <- na.omit(do.call('rbind', lapply(parstn, function(x) if(length(x) > 1) names(x) else NA)))[1, ]
		parstn <- do.call('rbind', parstn)
		dimnames(parstn)[[2]] <- nom
		parstn
	})
	pars[months] <- rapply(pars[months], f = function(x) ifelse(is.nan(x) | is.infinite(x), NA, x), how = "replace")
	return(pars)
}

##############################
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
	res[is.nan(res)] <- x[is.nan(res)]
	res[is.infinite(res)] <- 3*x[is.infinite(res)]
	res[!is.na(res) & (res > 3*x)] <- 3*x[!is.na(res) & (res > 3*x)]
	if(rfe.zero) res[x == 0] <- 0
    return(res)
}

##############################
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
		if(bs == 0) bs <- 0.6  # 0/n
		if(bs > 1.5) bs <- 1.5
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
	res[is.nan(res)] <- x[is.nan(res)]
	res[is.infinite(res)] <- 3*x[is.infinite(res)]
	res[!is.na(res) & (res > x*3)] <- 3*x[!is.na(res) & (res > x*3)]
	res[!is.na(res) & (res < x/3)] <- 0.5*x[!is.na(res) & (res < x/3)]
    return(res)
}

## Fit linear model
fitLM.fun.TT <- function(df, min.len){
	df <- na.omit(df)
	if(nrow(df) >= min.len) lm(stn~tt, data = df)
	else return(NULL)
}

fitLM.month.TT <- function(df, min.len) by(df, df$month, fitLM.fun.TT, min.len)


