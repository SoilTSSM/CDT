##detection of breakpoints
MultiBreaks.detection <- function(x, min.int, fun,...){
	ret <- list()
	foo <- match.fun(fun)
	n <- length(x)
	rang <- 1:n
	change.res <- list()
	change.count <- 1

	change.detectionF <- function(x, s, e, rang, change.res){
		len <- e-s+1
		len1 <- length(x[!is.na(x)])

		if((len >= min.int) & (len1 >= min.int)) is.change <- foo(x,...)
		else is.change <- NULL
		if(!is.null(is.change)){
			kk <- is.change$index.change
			val <- list(c(rang[1], rang[len], kk+rang[1]-1), is.change)
			s1 <- 1
			e1 <- kk-sample(0:3,1)
			s2 <- kk+1+sample(0:3,1)
			e2 <- len
			change.detectionF(x[s1:e1], s1, e1, rang[s1:e1], change.res)
			change.detectionF(x[s2:e2], s2, e2, rang[s2:e2], change.res)
			change.res[[change.count]] <<- val
		}
		change.count <<- change.count+1
	}
	change.detectionF(x, 1, n, rang, change.res)
	change.bounds <- do.call(rbind, lapply(change.res, function(v) v[[1]]))
	change.stats <- do.call(rbind, lapply(change.res, function(v) unlist(v[[2]])))
	if(is.null(change.stats)){
		ret$res <- NULL
	}else{
		res <- data.frame(change.bounds, change.stats)
		res <- res[,-4]
		names(res) <- c('start', 'end', 'cpt.index', 'statistics', 'max.conf.lev')
		ret$res <- res
	}
	class(ret) <- "breakpoints"
	return(ret)
}

#########################
##Extract potential change-points based on information criteria (modified BIC)

changepoints <- function(x, obj, cpt.max = 10, conf.lev = NULL){
	mean.cpt <- function(x, cpt){
		ints <- c(1, sort(cpt), length(x))
		nt <- length(ints)
		mns <- numeric(length(x))
		for(i in 1:(nt-1)) 	mns[ints[i]:ints[i+1]] <- rep(mean(x[ints[i]:ints[i+1]], na.rm = T), ints[i+1]-ints[i]+1)
		mns[is.na(x)] <- NA
		return(mns)
	}
	changepoints <- list()
	if(is.null(obj$res) | class(obj) != "breakpoints"){
	changepoints$res <- NULL
	changepoints$cpt$mbic.curve <- NULL
	changepoints$cpt$index <- NULL
	changepoints$cpt$number <- 0
	}else{
		n <- length(x)
		tobj <- obj$res[order(obj$res$statistics, decreasing = TRUE),]
		if(!is.null(conf.lev)) tobj <- tobj[tobj$max.conf.lev >= conf.lev,]
		kmax <- min(cpt.max, nrow(tobj))
		tobj <- tobj[1:kmax,]
		cpt <- tobj$cpt.index
		ncpt <- length(cpt)
		mbic.curve <- numeric(ncpt+1)
		mbic.curve[1] <- n/2 * log(var(x, na.rm = T))
		for(i in ncpt:1) mbic.curve[i+1]<- (n/2)*log(sum((x-mean.cpt(x, cpt[1:i]))^2, na.rm = TRUE)/n)+
		ifelse(length(cpt[1:i])>0,1.5*length(cpt[1:i])*log(n)+0.5*sum(log(diff(c(0, sort(cpt[1:i]), n))/n)), 0.5*sum(log(diff(c(0, n))/n)))
		pmin <- as.integer(median(which.min(mbic.curve)))
		if(pmin > 1){
			changepoints$res <- tobj
			changepoints$cpt$mbic.curve <- mbic.curve
			changepoints$cpt$index <- cpt[1:(pmin-1)]
			changepoints$cpt$number <- pmin-1
		}else{
			changepoints$res <- obj$res
			changepoints$cpt$mbic.curve <- NULL
			changepoints$cpt$index <- NULL
			changepoints$cpt$number <- 0
		}
	}
	class(changepoints) <- "changepoints"
	return(changepoints)
}

###############################################################################################3
getMean.cptSeg <- function(obj, x){
	n <- length(x)
#	if(!is.null(obj$res)){
#		if(class(obj) == "breakpoints") cpt <- sort(obj$res$cpt.index)
#		if(class(obj) == "changepoints") cpt <- sort(obj$cpt$index)
	if(!is.null(obj$cpt$index)){
		cpt <- sort(obj$cpt$index)
		ints <- c(1, cpt, n+1)
		nt <- length(ints)
		st <- ints[1:(nt-1)]
		ed <- ints[2:nt]-1
		mns <- numeric(n)
		for(i in 1:(nt-1)) 	mns[st[i]:ed[i]] <- rep(mean(x[st[i]:ed[i]], na.rm = T), ed[i]-st[i]+1)
		# mns[is.na(x)] <- NA
	}else{
		mns <- rep(mean(x, na.rm = T), n)
		# mns[is.na(x)] <- NA
	}
	return(mns)
}

######################
getMean.cptSeg1 <- function(obj, x){
	n <- length(x)
	if(length(obj) > 0){
		cpt <- sort(obj)
		ints <- c(1, cpt, n+1)
		nt <- length(ints)
		st <- ints[1:(nt-1)]
		ed <- ints[2:nt]-1
		mns <- numeric(n)
		for(i in 1:(nt-1)) 	mns[st[i]:ed[i]] <- rep(mean(x[st[i]:ed[i]], na.rm = T), ed[i]-st[i]+1)
		# mns[is.na(x)] <- NA
	}else{
		mns <- rep(mean(x, na.rm = T), n)
		# mns[is.na(x)] <- NA
	}
	return(mns)
}

####################
getTrend.cptSeg <- function(obj, x){
	n <- length(x)
	beta <- 12*sum(x*((1:n)-((n+1)/2)), na.rm = T)/(n*(n+1)*(n-1))
#	if(!is.null(obj$res)){
#		if(class(obj) == "breakpoints") cpt <- sort(obj$res$cpt.index)
#		if(class(obj) == "changepoints") cpt <- sort(obj$cpt$index)
	if(!is.null(obj$cpt$index)){
		cpt <- sort(obj$cpt$index)
		ints <- c(1, cpt, n+1)
		nt <- length(ints)
		st <- ints[1:(nt-1)]
		ed <- ints[2:nt]-1
		mns <- numeric(n)
		for(i in 1:(nt-1)) mns[st[i]:ed[i]] <- mean(x[st[i]:ed[i]], na.rm = T)-beta*(((ed[i]-st[i]+1)+1)/2)+beta*(1:(ed[i]-st[i]+1))
		# mns[is.na(x)] <- NA
	}else{
		mns <- mean(x, na.rm = T)-(beta*((n+1)/2))+beta*(1:n)
		# mns[is.na(x)] <- NA
	}
	return(mns)
}

#################
getTrend.cptSeg1 <- function(obj, x){
	n <- length(x)
	beta <- 12*sum(x*((1:n)-((n+1)/2)), na.rm = T)/(n*(n+1)*(n-1))
	if(length(obj) > 0){
		cpt <- sort(obj)
		ints <- c(1, cpt, n+1)
		nt <- length(ints)
		st <- ints[1:(nt-1)]
		ed <- ints[2:nt]-1
		mns <- numeric(n)
		for(i in 1:(nt-1)) mns[st[i]:ed[i]] <- mean(x[st[i]:ed[i]], na.rm = T)-beta*(((ed[i]-st[i]+1)+1)/2)+beta*(1:(ed[i]-st[i]+1))
		# mns[is.na(x)] <- NA
	}else{
		mns <- mean(x, na.rm = T)-(beta*((n+1)/2))+beta*(1:n)
		# mns[is.na(x)] <- NA
	}
	return(mns)
}

#########################################################################

AdjustM.byMean <- function(x, cpt, SegAdj = 0, min.adj){
	n <- length(x)
	ints <- c(1, cpt, n+1)
	nt <- length(ints)
	st <- ints[1:(nt-1)]
	ed <- ints[2:nt]-1
	mus <- numeric(nt-1)
	res <- numeric(n)
	for(i in 1:(nt-1)) 	mus[i] <- mean(x[st[i]:ed[i]], na.rm = T)
	if(SegAdj == 0){
		if(length(na.omit(x[st[nt-1]:ed[nt-1]])) >= min.adj){
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- x[st[i]:ed[i]]-(mus[i]-mus[nt-1])
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else if(SegAdj > 0 & SegAdj <= (nt-1)){
		if(length(na.omit(x[st[SegAdj]:ed[SegAdj]])) >= min.adj){
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- x[st[i]:ed[i]]-(mus[i]-mus[SegAdj])
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else{
		res <- NULL
		msg <- 'Check the segment you have selected, it does not exist'
	}
	return(list(res = res, msg = msg))
}

###############
AdjustT.byMean <- function(x, cpt, SegAdj = 0, min.adj){
	n <- length(x)
	beta <- 12*sum(x*((1:n)-((n+1)/2)), na.rm = T)/(n*(n+1)*(n-1))
	ints <- c(1, cpt, n+1)
	nt <- length(ints)
	st <- ints[1:(nt-1)]
	ed <- ints[2:nt]-1
	mus <- numeric(nt-1)
	res <- numeric(n)
	for(i in 1:(nt-1)) 	mus[i] <- mean(x[st[i]:ed[i]], na.rm = T)-beta*(((ed[i]-st[i]+1)+1)/2)
	if(SegAdj == 0){
		if(length(na.omit(x[st[nt-1]:ed[nt-1]])) >= min.adj){
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- x[st[i]:ed[i]]-((mus[i]-mus[nt-1])-beta*(st[i]-st[nt-1]))
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else if(SegAdj > 0 & SegAdj <= (nt-1)){
		if(length(na.omit(x[st[SegAdj]:ed[SegAdj]])) >= min.adj){
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- x[st[i]:ed[i]]-((mus[i]-mus[SegAdj])-beta*(st[i]-st[SegAdj]))
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else{
		res <- NULL
		msg <- 'Check the segment you have selected, it does not exist'
	}
	return(list(res = res, msg = msg))
}


##############################

AdjustM.byQM <- function(x, cpt, SegAdj = 0, min.adj){
	n <- length(x)
	ints <- c(1, cpt, n+1)
	nt <- length(ints)
	st <- ints[1:(nt-1)]
	ed <- ints[2:nt]-1
	res <- numeric(n)
	for(i in 1:(nt-1)) 	assign(paste('F', i, sep = ''), ecdf(x[st[i]:ed[i]]))
	if(SegAdj == 0){
		if(length(na.omit(x[st[nt-1]:ed[nt-1]])) >= min.adj){
			fy <- function(t) quantile(get(paste('F', nt-1, sep = '')), t)
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- fy(get(paste('F', i, sep = ''))(x[st[i]:ed[i]]))
			res[st[nt-1]:ed[nt-1]] <- x[st[nt-1]:ed[nt-1]]
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else if(SegAdj > 0 & SegAdj <= (nt-1)){
		if(length(na.omit(x[st[SegAdj]:ed[SegAdj]])) >= min.adj){
			fy <- function(t) quantile(get(paste('F', SegAdj, sep = '')), t)
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- fy(get(paste('F', i, sep = ''))(x[st[i]:ed[i]]))
			res[st[SegAdj]:ed[SegAdj]] <- x[st[SegAdj]:ed[SegAdj]]
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else{
		res <- NULL
		msg <- 'Check the segment you have selected, it does not exist'
	}
	return(list(res = res, msg = msg))
}

##############################

AdjustT.byQM <- function(x, cpt, SegAdj = 0, min.adj){
	n <- length(x)
	beta <- 12*sum(x*((1:n)-((n+1)/2)), na.rm = T)/(n*(n+1)*(n-1))
	trend<-(mean(x, na.rm = T)-(beta*((n+1)/2))+beta*(1:n))
	xt = x-trend
	ints <- c(1, cpt, n+1)
	nt <- length(ints)
	st <- ints[1:(nt-1)]
	ed <- ints[2:nt]-1
	res <- numeric(n)
	for(i in 1:(nt-1)) 	assign(paste('F', i, sep = ''), ecdf(xt[st[i]:ed[i]]))
	if(SegAdj == 0){
		if(length(na.omit(x[st[nt-1]:ed[nt-1]])) >= min.adj){
			fy <- function(t) quantile(get(paste('F', nt-1, sep = '')), t)
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- fy(get(paste('F', i, sep = ''))(xt[st[i]:ed[i]]))
			res <- res+trend
			res[st[nt-1]:ed[nt-1]] <- x[st[nt-1]:ed[nt-1]]
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else if(SegAdj > 0 & SegAdj <= (nt-1)){
		if(length(na.omit(x[st[SegAdj]:ed[SegAdj]])) >= min.adj){
			fy <- function(t) quantile(get(paste('F', SegAdj, sep = '')), t)
			for(i in 1:(nt-1)) res[st[i]:ed[i]] <- fy(get(paste('F', i, sep = ''))(xt[st[i]:ed[i]]))
			res <- res+trend
			res[st[SegAdj]:ed[SegAdj]] <- x[st[SegAdj]:ed[SegAdj]]
			msg <- NULL
		}else{
			res <- NULL
			msg <- 'The length of segment is too short to adjust the series'
		}
	}else{
		res <- NULL
		msg <- 'Check the segment you have selected, it does not exist'
	}
	return(list(res = res, msg = msg))
}

