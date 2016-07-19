geodist.km <- function(lon0, lat0, lon1, lat1) 6371*acos(sin(lat0*pi/180)*sin(lat1*pi/180)+cos(lat0*pi/180)*cos(lat1*pi/180)*cos((lon1-lon0)*pi/180))

####################################################

choose.neignbors <- function(mvar, xpos, lon, lat, elv, max.dist, min.stn){
	x0 <- mvar[,xpos]
	Y <- mvar[,-xpos]

	idNA <- apply(Y, 2, function(x) (length(x[!is.na(x)])/length(x)) > 0.8)
	Y <- Y[,idNA]
	if(ncol(Y) < min.stn){
		msg <- paste('Neighbor stations have too much missing values', 'with min.stn >= ',min.stn)
		ret <- list(x = x0, Y = NULL, rho = NULL, dist = NULL, cov = NULL, delv = NULL, msg = msg)
	}else{
		xl0 <- lon[xpos]
		yl0 <- lat[xpos]
		xl1 <- lon[-xpos]
		xl1 <- xl1[idNA]
		yl1 <- lat[-xpos]
		yl1 <- yl1[idNA]
		if(!is.null(elv)){
			elv0 <- elv[xpos]
			elv1 <- elv[-xpos]
			elv <- elv1[idNA]
		}else{
			elv0 <- NULL
			elv <- NULL
		}

		##Distance
		dist <- geodist.km(xl0, yl0, xl1, yl1)
		idR <- which(dist <= max.dist)
		if(!is.null(Y) & (length(idR) >= min.stn)){
			Y <- Y[,order(dist[idR])]
			dist0 <- dist[idR]
			dist0 <- dist0[order(dist0)]
			rho <- cor(x0, Y, use = "pairwise.complete.obs")
			vcov <- cov(x0, Y, use = "pairwise.complete.obs")
			if(!is.null(elv) & !is.null(elv0)) delv <- abs(elv0-elv[order(dist[idR])])
			else delv <- NULL
			ret <- list(x = x0, Y = Y, rho = rho, dist = dist0, cov = t(vcov), delv = delv, msg = NULL)
		}else{
			msg <- paste('No stations within a radius of', max.dist, 'km', 'with min.stn >= ',min.stn)
			ret <- list(x = x0, Y = NULL, rho = NULL, dist = NULL, cov = NULL, delv = NULL, msg = msg)
		}
	}
	return(ret)
}


####################################################
use.elevation <- function(ret, uselv, elv.diff, min.stn){
	if(!is.null(ret$Y)){
		if(uselv == '1' & !is.null(ret$delv)){
			idELV <- which(ret$delv <= elv.diff)
			if((length(idELV) >= min.stn)) res <- list(x = ret$x, Y = ret$Y[,idELV], rho = ret$rho[idELV], dist = ret$dist[idELV], cov = ret$cov[idELV], msg = ret$msg)
			else res <- list(x = ret$x, Y = NULL, rho = NULL, dist = NULL, cov = NULL, msg = paste('No stations between +/-',elv.diff, 'm', 'with min.stn >= ',min.stn))
		}else res <- list(x = ret$x, Y = ret$Y, rho = ret$rho, dist = ret$dist, cov = ret$cov, msg = paste(ret$msg,'. Reference series has been created without consideration elevation'))
	}else res <- list(x = ret$x, Y = NULL, rho = NULL, dist = NULL, cov = NULL, msg = ret$msg)
	return(res)
}

##############################

weighting.factors <- function(ret, weight.fac, min.stn, min.rho, max.stn){
	Y <- ret$Y
	rho <- ret$rho
	dist <- ret$dist
	vcov <- ret$cov
	if(weight.fac == '1'){ #correlation
		if(!is.null(Y)){
			idRHO <- which(rho >= min.rho)
			if((length(idRHO) >= min.stn)){
				Y <- Y[,order(rho[idRHO], decreasing = TRUE)]
				rhor <- rho[idRHO]
				rho <- rhor[order(rhor, decreasing = TRUE)]
				if(ncol(Y) >= max.stn){
					Y <- Y[,1:max.stn]
					rho <- rho[1:max.stn]
				}
				lambda <- as.vector((rho*rho)/sum(rho*rho))
				res <- list(x = ret$x, Y = Y, lambda = lambda, msg = ret$msg)
			}else res <- list(x = ret$x, Y = NULL, lambda = NULL, msg = paste('No stations with correlation coef >= ',min.rho, 'with min.stn >= ',min.stn))
		}else res <- list(x = ret$x, Y = NULL, lambda = NULL, msg = ret$msg)
	}else if(weight.fac == '2'){ #distance
		if(!is.null(Y)){
			if(ncol(Y) >= max.stn){
				Y <- Y[,1:max.stn]
				dist <- dist[1:max.stn]
			}
			lambda <- as.vector((1/(dist*dist))/sum(1/(dist*dist)))
			res <- list(x = ret$x, Y = Y, lambda = lambda, msg = ret$msg)
		}else res <- list(x = ret$x, Y = NULL, lambda = NULL, msg = ret$msg)
	}else if(weight.fac == '3'){ #optimal
		if(!is.null(Y)){
			if(ncol(Y) >= max.stn){
				Y <- Y[,1:max.stn]
				vcov <- as.matrix(vcov[1:max.stn, 1])
			}
			ones <- matrix(1, ncol = 1, nrow = ncol(Y))
			covmat <- cov(Y, use = "pairwise.complete.obs")
			if(round(det(covmat), 10) == 0) invcov <- solve(cov(Y*10, use = "pairwise.complete.obs"))*10^2
			else invcov <- solve(covmat)
			lambda <- as.vector(invcov%*%(vcov+ones%*%((1-(t(ones)%*%(invcov%*%vcov)))/(t(ones)%*%(invcov%*%vcov)))))
			res <- list(x = ret$x, Y = Y, lambda = lambda, msg = ret$msg)
		}else res <- list(x = ret$x, Y = NULL, lambda = NULL, msg = ret$msg)
	}
	return(res)
}

###############
weighting.factors.usr <- function(xpos, ypos, mvar, lon, lat, weight.fac){
	x0 <- mvar[,xpos]
	Y <- mvar[,ypos]
	xl0 <- lon[xpos]
	yl0 <- lat[xpos]
	xl1 <- lon[ypos]
	yl1 <- lat[ypos]

	dist <- geodist.km(xl0, yl0, xl1, yl1)
	rho <- cor(x0, Y, use = "pairwise.complete.obs")
	vcov <- t(cov(x0, Y, use = "pairwise.complete.obs"))

	if(weight.fac == '1'){ #correlation
		lambda <- as.vector((rho*rho)/sum(rho*rho))
		res <- list(x = x0, Y = Y, lambda = lambda, msg = NULL)
	}else if(weight.fac == '2'){ #distance
		lambda <- as.vector((1/(dist*dist))/sum(1/(dist*dist)))
		res <- list(x = x0, Y = Y, lambda = lambda, msg = NULL)
	}else if(weight.fac == '3'){ #optimal
		ones <- matrix(1, ncol = 1, nrow = ncol(Y))
		covmat <- cov(Y, use = "pairwise.complete.obs")
		if(round(det(covmat), 10) == 0) invcov <- solve(cov(Y*10, use = "pairwise.complete.obs"))*10^2
		else invcov <- solve(covmat)
		lambda <- as.vector(invcov%*%(vcov+ones%*%((1-(t(ones)%*%(invcov%*%vcov)))/(t(ones)%*%(invcov%*%vcov)))))
		res <- list(x = x0, Y = Y, lambda = lambda, msg = NULL)
	}
	return(res)
}


###################################################

refSeries.creation <- function(ret, diff.ratio, use.climato = FALSE, scale = c('daily', 'dekadal', 'monthly'), dates){
	Y <- ret$Y
	x <- ret$x
	lambda <- ret$lambda
	scale <- match.arg(scale)
	if(!is.null(Y)){
		if(diff.ratio == '1'){ #difference
			if(use.climato){
				fun <- paste('anomaly', scale, sep = '.')
				fun <- match.fun(fun)
				Y1 <- apply(Y, 2, fun, dates)
				x1 <- fun(x, dates)
			}else{
				Y1 <- apply(Y, 2, function(x) x-mean(x, na.rm = T))
				x1 <- x-mean(x, na.rm = T)
			}
			ref <- x1-apply(t(lambda*t(Y1)), 1, sum, na.rm = T)
			msg <- NULL
		}else if(diff.ratio == '2'){ #ratio
			if(use.climato){
				fun <- paste('ratio', scale, sep = '.')
				fun <- match.fun(fun)
				Y1 <- apply(Y, 2, fun, dates)
				x1 <- fun(x, dates)
			}else{
				Y1 <- apply(Y, 2, function(x) x/mean(x, na.rm = T))
				x1 <- x/mean(x, na.rm = T)
			}
			ref <- x1/apply(t(lambda*t(Y1)), 1, sum, na.rm = T)
			ref[is.nan(ref)] <- NA
			ref[is.infinite(ref)] <- NA
			msg <- NULL
		}else{ #logratio
			if(use.climato){
				fun <- paste('ratio', scale, sep = '.')
				fun <- match.fun(fun)
				Y1 <- apply(Y, 2, fun, dates)
				x1 <- fun(x, dates)
			}else{
				Y1 <- apply(Y, 2, function(x) x/mean(x, na.rm = T))
				x1 <- x/mean(x, na.rm = T)
			}
			ref <- log(x1/apply(t(lambda*t(Y1)), 1, sum, na.rm = T))
			ref[is.nan(ref)] <- NA
			ref[is.infinite(ref)] <- NA
			msg <- NULL
		}
	}else{
		if(use.climato){
			fun <- paste('standard', scale, sep = '.')
			fun <- match.fun(fun)
			ref <- fun(x, dates)
		}else ref<-(x-mean(x, na.rm = T))/sd(x, na.rm = T)
		msg <- list(msg = ret$msg, msg1 = 'The breakpoint detection is carried without reference series')
	}
	return(list(refs = ref, msg = msg))
}

#####################################
refSeries.creation1S <- function(cand, refs, diff.ratio, use.climato = FALSE, scale = c('daily', 'dekadal', 'monthly'), dates){
	if(diff.ratio == '1'){ #difference
		if(use.climato){
			fun <- paste('anomaly', scale, sep = '.')
			fun <- match.fun(fun)
			Y1 <- fun(refs, dates)
			x1 <- fun(cand, dates)
		}else{
			Y1 <- refs-mean(refs, na.rm = T)
			x1 <- cand-mean(cand, na.rm = T)
		}
		ref <- x1-Y1
	}else if(diff.ratio == '2'){ #ratio
		if(use.climato){
			fun <- paste('ratio', scale, sep = '.')
			fun <- match.fun(fun)
			Y1 <- fun(refs, dates)
			x1 <- fun(cand, dates)
		}else{
			Y1 <- refs/mean(refs, na.rm = T)
			x1 <- cand/mean(cand, na.rm = T)
		}
		ref <- x1/Y1
		ref[is.nan(ref)] <- NA
		ref[is.infinite(ref)] <- NA
	}else{ #logratio
		if(use.climato){
			fun <- paste('ratio', scale, sep = '.')
			fun <- match.fun(fun)
			Y1 <- fun(refs, dates)
			x1 <- fun(cand, dates)
		}else{
			Y1 <- refs/mean(refs, na.rm = T)
			x1 <- cand/mean(cand, na.rm = T)
		}
		ref <- log(x1/Y1)
		ref[is.nan(ref)] <- NA
		ref[is.infinite(ref)] <- NA
	}
	return(ref)
}

#####################################
standardize <- function(x, use.climato = FALSE, scale = c('daily', 'dekadal', 'monthly'), dates){
	scale <- match.arg(scale)
	if(use.climato){
		fun <- paste('standard', scale, sep = '.')
		fun <- match.fun(fun)
		ref <- fun(x, dates)
	}else{
		ref<-(x-mean(x, na.rm = T))/sd(x, na.rm = T)
	}
	return(ref)
}

