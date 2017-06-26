
startnorm <- function(x){
	m <- mean(x)
	s <- sd(x)
	list(mean = m, sd = s)
}

startsnorm <- function(x){
	m <- mean(x)
	s <- sd(x)
	# xi <- min(0.99, abs(mean(((x-m)/s)^3)))
	xi <- 1
	list(mean = m, sd = s, xi = xi)
}

startlnorm <- function(x){
	m <- mean(x)
	v <- var(x)
	slog2 <- log((v+m^2)/m^2)
	mlog <- log(m)-slog2/2
	slog <- sqrt(slog2)
	list(meanlog = mlog, sdlog = slog)
}

startgamma <- function(x){
	m <- mean(x)
	v <- var(x)
	shape <- m^2/v
	scale <- v/m
	list(shape = shape, scale = scale)
}

startexp <- function(x){
	m <- mean(x)
	rate <- 1/m
	list(rate = rate)
}

startweibull <- function(x){
	m <- mean(x)
	s <- sd(x)
	# Ramírez and Carta (2005)
	# shape <- (m/s)^1.086
	shape <- (0.9874/(s/m))^1.0983
	scale <- m/gamma(1+1/shape)
	list(shape = shape, scale = scale)
}

## cauchy
startcauchy <- function(x) return(NULL)


# library(qmap)
# c("berngamma", "bernlnorm", "bernweibull", "bernexp")

##############################
## PICSA plot theoretical distribution

fit.distributions <- function(x, distr = c("norm", "snorm", "lnorm", "gamma", "weibull"),
							  method = 'mle', ...)
{
	fit.distr <- lapply(distr, function(dm){
		start.pars <- do.call(paste0("start", dm), list(x))
		fit.mod <- try(fitdist(x, dm, method = method, start = start.pars, ...), silent = TRUE)
		fit.mod
	})
	idist <- sapply(fit.distr, function(d) !inherits(d, "try-error"))
	fit.distr <- if(any(idist)) fit.distr[idist] else NULL
	return(fit.distr)
}

##############################
## Merging RR - Quantile matching Bias
## Fit Mixture distribution
fit.mixture.distr <- function(x, min.len = 7, alpha = 0.05,
						distr.fun = c("berngamma", "bernlnorm", "bernweibull", "bernexp"),
						method = 'mle', lower = c(0, 1e-10, 1e-10), upper = c(1, Inf, Inf),
						keepdata = FALSE, keepdata.nb = 3, ...)
{
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
				# Anderson-Darling Test
				pdistrf <- match.fun(paste0('p', distrf))
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
## Merging TT - Quantile matching Bias
### fit normal distribution for temp
fit.norm.temp <- function(x, min.len, alpha = 0.05, method = 'mle',
						lower = c(-20, 0), upper = c(60, 10),
						keepdata = FALSE, keepdata.nb = 3, ...)
{
	x <- x[!is.na(x)]
	ret <- NULL
	if(length(x) > min.len){
		xmoy <- mean(x)
		xsd <- sd(x)
		fit.mod <- try(fitdist(x, "norm", method = method,
					start = list(mean = xmoy, sd = xsd), lower = lower, upper = upper,
					keepdata = keepdata, keepdata.nb = keepdata.nb, ...), silent = TRUE)
		if(!inherits(fit.mod, "try-error")){
			# Shapiro-Wilk normality test
			swnt <- shapiro.test(x)
			test <- if(swnt$p.value > alpha) 'yes' else 'no'
			ret <- list(fitted.distr = fit.mod, SWNtest = swnt, h0 = test)
		}else ret <- list(fitted.distr = NULL, SWNtest = NULL, h0 = 'null')
	}
	return(ret)
}
