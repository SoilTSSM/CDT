getRHtestsData <- function(GeneralParameters){
	freqdata <- GeneralParameters$period
	single.series <- GeneralParameters$single.series
	useref <- GeneralParameters$use.ref.series
	filedatefrmt <- as.character(GeneralParameters$file.date.format$Values)
	filefrmt <- filedatefrmt[1]
	datefrmt <- filedatefrmt[2]
	varcat <- filedatefrmt[3]
	comppars <- as.character(GeneralParameters$compute.var$Values)
	comp.fun <- comppars[1]
	miss.frac <- as.numeric(comppars[2])
	file.pars <- as.character(GeneralParameters$file.io$Values)

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
	origdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Data', fsep = .Platform$file.sep)
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	flmondata <- file.path(origdir, paste('MONTHLY', file.pars[1], sep = '_'), fsep = .Platform$file.sep)
	if(freqdata != 'monthly'){
		fldekdata <- file.path(origdir, paste('DEKADAL', file.pars[1], sep = '_'), fsep = .Platform$file.sep)
		if(freqdata == 'daily') fldlydata <- file.path(origdir, paste('DAILY', file.pars[1], sep = '_'), fsep = .Platform$file.sep)
	}

	#####
	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]
	MissingValue <- AllOpenFilesData[[jfile]][[4]]$miss.val

	##CDT data format
	retval <- NULL
	if(single.series == "0"){
		#######get data
		donne <- splitCDTData(donne, freqdata)
		lon <- donne$lon
		lat <- donne$lat
		donne.id <- donne$id
		elv <- donne$elv
		dates <- donne$dates
		donne <- donne$data

		#if(nrow(donne$duplicated.coords) > 0)  #diplay table
		#if(nrow(dat$missing.coords) > 0)

		if(!is.null(donne)){
			flstninfo <- file.path(origdir, paste('Infos', file.pars[1], sep = '_'), fsep = .Platform$file.sep)
			dydonne <- dkdonne <- dmdonne <- NULL
			dydaty <- dkdaty <- dmdaty <- NULL

			if(freqdata == 'daily'){
				###monthly data
				mdonne <- apply(donne, 2, daily2monthly, dates = dates, fun = comp.fun, frac = miss.frac)
				##dekadal data
				ddonne <- apply(donne, 2, daily2dekadal, dates = dates, fun = comp.fun, frac = miss.frac)

				dydonne <- donne
				dkdonne <- sapply(mdonne, function(x) x$Values)
				dmdonne <- sapply(mdonne, function(x) x$Values)
				dydaty <- dates
				dkdaty <- as(ddonne[[1]]$Date, 'character')
				dmdaty <- as(mdonne[[1]]$Date, 'character')
			}else if(freqdata == 'dekadal'){
				###monthly data
				mdonne <- apply(donne, 2, dekadal2monthly, dates = dates, fun = comp.fun)

				dkdonne <- donne
				dmdonne <- sapply(mdonne, function(x) x$Values)
				dkdaty <- dates
				dmdaty <- as(mdonne[[1]]$Date, 'character')
			}else if(freqdata == 'monthly'){
				dmdonne <- donne
				dmdaty <- dates
			}

			mondata <- data.frame(dmdaty, round(dmdonne, 1))
			write.table(mondata, flmondata, row.names = FALSE, col.names = FALSE)
			if(freqdata != 'monthly'){
				dekdata <- data.frame(dkdaty, round(dkdonne, 1))
				write.table(dekdata, fldekdata, row.names = FALSE, col.names = FALSE)
				if(freqdata == 'daily'){
					dlydata <- data.frame(dydaty, round(dydonne, 1))
					write.table(dlydata, fldlydata, row.names = FALSE, col.names = FALSE)
				}
			}
			StnInfo <- data.frame(IDs = donne.id, Lon = lon, Lat = lat, Elv = if(is.null(elv)) NA else elv)
			write.table(StnInfo, flstninfo, row.names = FALSE, col.names = TRUE)

			retval <- list(IDs = donne.id, Lon = lon, Lat = lat, Elv = elv, datdly = dydonne, dydates = dydaty, datdek = dkdonne, dkdates = dkdaty, datmon = dmdonne, modates = dmdaty, MissingValue = MissingValue)
		}
	}else{ #1series
		if(useref == 1){ #1series with ref
			if(file.pars[2] == ""){
				InsertMessagesTxt(main.txt.out, 'There is no reference series provided', format = TRUE)
				return(NULL)
			}else{
				jfile1 <- which(all.open.file == file.pars[2])
				donne1 <- AllOpenFilesData[[jfile1]][[2]]
			}
		}

		###DATE
		if(datefrmt == "1"){ #1date
			if(freqdata == 'daily'){
				dates <- as.Date(as.character(donne[,1]), format='%Y%m%d')
				if(useref == 1){
					dates1 <- as.Date(as.character(donne1[,1]), format='%Y%m%d')
				}
			}
			if(freqdata == 'dekadal'){
				dates <- as.Date(as.character(donne[,1]), format='%Y%m%d')
				dates[as.numeric(format(dates,'%d'))>3] <- NA
				if(useref == 1){
					dates1 <- as.Date(as.character(donne1[,1]), format='%Y%m%d')
					dates1[as.numeric(format(dates1,'%d'))>3] <- NA
				}
			}
			if(freqdata == 'monthly'){
				dates <- as.Date(paste(as.character(donne[,1]),'01', sep = ''), format='%Y%m%d')
				if(useref == 1){
					dates1 <- as.Date(paste(as.character(donne1[,1]),'01', sep = ''), format='%Y%m%d')
				}
			}
		}else{ #3dates
			if(freqdata == 'daily'){
				dates <- as.Date(paste(as.character(donne[,1]), as.character(donne[,2]), as.character(donne[,3]), sep = '-'))
				if(useref == 1){
					dates1 <- as.Date(paste(as.character(donne1[,1]), as.character(donne1[,2]), as.character(donne1[,3]), sep = '-'))
				}
			}
			if(freqdata == 'dekadal'){
				dates <- as.Date(paste(as.character(donne[,1]), as.character(donne[,2]), as.character(donne[,3]), sep = '-'))
				dates[as.numeric(format(dates,'%d'))>3] <- NA
				if(useref == 1){
					dates1 <- as.Date(paste(as.character(donne1[,1]), as.character(donne1[,2]), as.character(donne1[,3]), sep = '-'))
					dates1[as.numeric(format(dates1,'%d'))>3] <- NA
				}
			}
			if(freqdata == 'monthly'){
				dates <- as.Date(paste(as.character(donne[,1]), as.character(donne[,2]),'01', sep = '-'))
				if(useref == 1){
					dates1 <- as.Date(paste(as.character(donne1[,1]), as.character(donne1[,2]),'01', sep = '-'))
				}
			}
		}

		#####VARS
		if(filefrmt == "1"){ #1var
			if(datefrmt == "1"){ #1date
				var <- as.numeric(donne[,2])
				if(useref == 1){
					var1 <- as.numeric(donne1[,2])
				}
			}else{ #3dates
				if(freqdata == 'monthly'){
					if(ncol(donne) == 3) var <- as.numeric(donne[,3])
					if(ncol(donne) == 4) var <- as.numeric(donne[,4])
					if(useref == 1){
						if(ncol(donne1) == 3) var1 <- as.numeric(donne1[,3])
						if(ncol(donne1) == 4) var1 <- as.numeric(donne1[,4])
					}
				}else{
					var <- as.numeric(donne[,4])
					if(useref == 1){
						var1 <- as.numeric(donne1[,4])
					}
				}
			}
		}else{ #3var
			if(datefrmt == "1"){ #1date
				var <- if(varcat == "1") as.numeric(donne[,2]) else if(varcat == "2") as.numeric(donne[,3]) else as.numeric(donne[,4])
				if(useref == 1){
					var1 <- if(varcat == "1") as.numeric(donne1[,2]) else if(varcat == "2") as.numeric(donne1[,3]) else as.numeric(donne1[,4])
				}
			}else{ #3dates
				if(freqdata == 'monthly'){
					if(ncol(donne) == 5) var <- if(varcat == "1") as.numeric(donne[,3]) else if(varcat == "2") as.numeric(donne[,4]) else as.numeric(donne[,5])
					if(ncol(donne) == 6) var <- if(varcat == "1") as.numeric(donne[,4]) else if(varcat == "2") as.numeric(donne[,5]) else as.numeric(donne[,6])
					if(useref == 1){
						if(ncol(donne1) == 5) var1 <- if(varcat == "1") as.numeric(donne1[,3]) else if(varcat == "2") as.numeric(donne1[,4]) else as.numeric(donne1[,5])
						if(ncol(donne1) == 6) var1 <- if(varcat == "1") as.numeric(donne1[,4]) else if(varcat == "2") as.numeric(donne1[,5]) else as.numeric(donne1[,6])
					}
				}else{
					var <- if(varcat == "1") as.numeric(donne[,4]) else if(varcat == "2") as.numeric(donne[,5]) else as.numeric(donne[,6])
					if(useref == 1){
						var1 <- if(varcat == "1") as.numeric(donne1[,4]) else if(varcat == "2") as.numeric(donne1[,5]) else as.numeric(donne1[,6])
					}
				}
			}
		}

		#remove invalid date and reordering
		var <- var[!is.na(dates)]
		dates <- dates[!is.na(dates)]
		var <- var[order(dates)]
		dates <- dates[order(dates)]
		if(freqdata == 'daily') dates <- format(dates,'%Y%m%d')
		if(freqdata == 'dekadal') dates <- paste(format(dates,'%Y%m'), as.numeric(format(dates,'%d')), sep = '')
		if(freqdata == 'monthly') dates <- format(dates,'%Y%m')
		if(useref == 1){
			var1 <- var1[!is.na(dates1)]
			dates1 <- dates1[!is.na(dates1)]
			var1 <- var1[order(dates1)]
			dates1 <- dates1[order(dates1)]
			if(freqdata == 'daily') dates1 <- format(dates1,'%Y%m%d')
			if(freqdata == 'dekadal')  dates1 <- paste(format(dates1,'%Y%m'), as.numeric(format(dates1,'%d')), sep = '')
			if(freqdata == 'monthly') dates1 <- format(dates1,'%Y%m')
		}

		########################
		if(useref == 1){
			dydonne <- dkdonne <- dmdonne <- NULL
			rdydonne <- rdkdonne <- rdmdonne <- NULL
			dmdonne1mm <- rdmdonne1mm <- NULL
			dydaty <- dkdaty <- dmdaty <- NULL

			if(freqdata == 'daily'){
				comdt <- dates%in%dates1
				dates <- dates[comdt]
				comdt1 <- dates1%in%dates
				cand_var <- var[comdt]
				ref_var <- var1[comdt1]
				mcand <- daily2monthly(cand_var, dates = dates, fun = comp.fun, frac = miss.frac)
				mrefs <- daily2monthly(ref_var, dates = dates, fun = comp.fun, frac = miss.frac)
				dcand <- daily2dekadal(cand_var, dates = dates, fun = comp.fun, frac = miss.frac)
				drefs <- daily2dekadal(ref_var, dates = dates, fun = comp.fun, frac = miss.frac)
				dydonne <- cand_var
				rdydonne <- ref_var
				dydaty <- dates
				dkdonne <- dcand$Values
				rdkdonne <- drefs$Values
				dkdaty <- as.character(dcand$Date)
				dmdonne <- mcand$Values
				rdmdonne <- mrefs$Values
				dmdaty <- as.character(mcand$Date)
#				if(comppars[1] == 'sum'){
#					cvar1mm <- cand_var
#					cvar1mm[cvar1mm < 1] <- 0
#					mcand <- daily2monthly(cvar1mm, dates = dates, fun = comp.fun, frac = miss.frac)
#					dmdonne1mm <- mcand$Values
#					rvar1mm <- ref_var
#					rvar1mm[rvar1mm < 1] <- 0
#					mrefs <- daily2monthly(rvar1mm, dates = dates, fun = comp.fun, frac = miss.frac)
#					rdmdonne1mm <- mcand$Values
#				}
			}else if(freqdata == 'dekadal'){
				comdt <- dates%in%dates1
				dates <- dates[comdt]
				comdt1 <- dates1%in%dates
				cand_var <- var[comdt]
				ref_var <- var1[comdt1]
				mcand <- dekadal2monthly(cand_var, dates = dates, fun = comp.fun)
				mrefs <- dekadal2monthly(ref_var, dates = dates, fun = comp.fun)
				dkdonne <- cand_var
				rdkdonne <- ref_var
				dkdaty <- dates
				dmdonne <- mcand$Values
				rdmdonne <- mrefs$Values
				dmdaty <- as.character(mcand$Date)
			}else if(freqdata == 'monthly'){
				comdt <- dates%in%dates1
				dates <- dates[comdt]
				comdt1 <- dates1%in%dates
				cand_var <- var[comdt]
				ref_var <- var1[comdt1]
				dmdonne <- cand_var
				rdmdonne <- ref_var
				dmdaty <- dates
			}

			####
			mondata <- data.frame(dmdaty, round(dmdonne, 1), round(rdmdonne, 1))
			if(freqdata != 'monthly'){
				dekdata <- data.frame(dkdaty, round(dkdonne, 1), round(rdkdonne, 1))
				if(freqdata == 'daily') dlydata <- data.frame(dydaty, round(dydonne, 1), round(rdydonne, 1))
			}

			retval <- list(datdly = dydonne, refdly = rdydonne, dydates = dydaty, datdek = dkdonne, refdek = rdkdonne, dkdates = dkdaty, datmon = dmdonne, refmon = rdmdonne, modates = dmdaty, MissingValue = MissingValue)
		}else{
			dydonne <- dkdonne <- dmdonne <- NULL
			dmdonne1mm <- NULL
			dydaty <- dkdaty <- dmdaty <- NULL

			if(freqdata == 'daily'){
				mcand <- daily2monthly(var, dates = dates, fun = comp.fun, frac = miss.frac)
				dcand <- daily2dekadal(var, dates = dates, fun = comp.fun, frac = miss.frac)
				dydonne <- var
				dkdonne <- dcand$Values
				dmdonne <- mcand$Values
				dydaty <- dates
				dkdaty <- as.character(dcand$Date)
				dmdaty <- as.character(mcand$Date)
#				if(comppars[1] == 'sum'){
#					xvar1mm <- var
#					xvar1mm[xvar1mm < 1] <- 0
#					mcand <- daily2monthly(xvar1mm, dates = dates, fun = comp.fun, frac = miss.frac)
#					dmdonne1mm <- mcand$Values
#				}
			}else if(freqdata == 'dekadal'){
				mcand <- dekadal2monthly(var, dates = dates, fun = comp.fun)
				dkdonne <- var
				dmdonne <- mcand$Values
				dkdaty <- dates
				dmdaty <- as.character(mcand$Date)
			}else if(freqdata == 'monthly'){
				dmdonne <- var
				dmdaty <- dates
			}

			######
			mondata <- data.frame(dmdaty, round(dmdonne, 1))
			if(freqdata != 'monthly'){
				dekdata <- data.frame(dkdaty, round(dkdonne, 1))
				if(freqdata == 'daily') dlydata <- data.frame(dydaty, round(dydonne, 1))
			}

			retval <- list(datdly = dydonne, dydates = dates, datdek = dkdonne, dkdates = dkdaty, datmon = dmdonne, modates = dmdaty, MissingValue = MissingValue)
		}

		###
		write.table(mondata, flmondata, row.names = FALSE, col.names = FALSE)
		if(freqdata != 'monthly'){
			write.table(dekdata, fldekdata, row.names = FALSE, col.names = FALSE)
			if(freqdata == 'daily') write.table(dlydata, fldlydata, row.names = FALSE, col.names = FALSE)
		}
	}
	return(retval)
}



######################################################################

getRHtestsDEM <- function(donnees, GeneralParameters){
	single.series <- GeneralParameters$single.series
	useref <- as.character(GeneralParameters$use.ref.series)
	uselv <- as.character(GeneralParameters$ref.series.choix$Values[2])
	interp.dem <- as.character(GeneralParameters$ref.series.choix$Values[3])
	file.dem <- as.character(GeneralParameters$file.io$Values[3])
	if(single.series == "0"){
		if(useref == "1" & uselv == "1"){
			if(is.null(donnees)){
				InsertMessagesTxt(main.txt.out, 'Data not yet loaded.',format = TRUE)
				return(NULL)
			}else{
				if(interp.dem == "0"){
					if(file.dem == ""){
						InsertMessagesTxt(main.txt.out, 'There is no elevation data in format NetCDF', format = TRUE)
						return(NULL)
					}else{
						all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
						jncdf <- which(all.open.file == file.dem)
						fdem <- AllOpenFilesData[[jncdf]][[2]]
						dem <- fdem$value
						dem[dem < 0] <- NA
						dem <- data.frame(expand.grid(x = fdem$x, y = fdem$y), z = c(dem))
						xy0 <- data.frame(x = donnees$Lon, y = donnees$Lat)
						elv <- idw(formula = z ~ 1, locations = ~x+y, data = na.omit(dem), newdata = xy0, nmax = 4, debug.level = 0)[,3]
					}
				}else{
					if(is.null(donnees$Elv)){
						InsertMessagesTxt(main.txt.out, 'There is no elevation data in your file', format = TRUE)
						return(NULL)
					}else  elv <- donnees$Elv
				}
			}
		}
	}
	return(elv)
}


##############################################################################


getRHtestsRefSeries <- function(xpos, donnees, dem_data, GeneralParameters, freqDON = 'MON'){

	if(freqDON == 'MON') don <- donnees$datmon
	if(freqDON == 'DEK') don <- donnees$datdek
	if(freqDON == 'DLY') don <- donnees$datdly

	parsRef <- as.character(GeneralParameters$ref.series.choix$Values)
	weight.fac <- parsRef[1]
	uselv <- parsRef[2]
	min.stn <- as.numeric(parsRef[4])
	max.stn <- as.numeric(parsRef[5])
	max.dist <- as.numeric(parsRef[6])
	elv.diff <- as.numeric(parsRef[7])
	min.rho <- as.numeric(parsRef[8])

	if(GeneralParameters$use.ref.series == '1'){
		if(GeneralParameters$ref.series.user == '1'){
			ypos <- GeneralParameters$stn.user.choice
			if(is.null(ypos)){
				ref <- NULL
				msg <- 'There is no station selected'
			}else{
				if(length(ypos) > 1){
					x0 <- don[,xpos]
					Y <- don[,ypos]
					xl0 <- donnees$Lon[xpos]
					yl0 <- donnees$Lat[xpos]
					xl1 <- donnees$Lon[ypos]
					yl1 <- donnees$Lat[ypos]
					dist <- geodist.km(xl0, yl0, xl1, yl1)
					rho <- cor(x0, Y, use = "pairwise.complete.obs")
					vcov <- t(cov(x0, Y, use = "pairwise.complete.obs"))
					if(weight.fac == 'Correlation'){
						lambda <- as.vector((rho*rho)/sum(rho*rho))
					}else if(weight.fac == 'Distance'){
						lambda <- as.vector((1/(dist*dist))/sum(1/(dist*dist)))
					}else if(weight.fac == 'Optimal'){
						ones <- matrix(1, ncol = 1, nrow = ncol(Y))
						covmat <- cov(Y, use = "pairwise.complete.obs")
						if(round(det(covmat), 10) == 0) invcov <- solve(cov(Y*10, use = "pairwise.complete.obs"))*10^2
						else invcov <- solve(covmat)
						lambda <- as.vector(invcov%*%(vcov+ones%*%((1-(t(ones)%*%(invcov%*%vcov)))/(t(ones)%*%(invcov%*%vcov)))))
					}else if(weight.fac == 'Average'){
						lambda <- rep(1/ncol(Y), ncol(Y))
					}
					ref <- unname(apply(t(lambda*t(Y)), 1, sum))
					msg <- NULL
				}else if(length(ypos) == 1){
					ref <- don[,ypos]
					msg <- NULL
				}else{
					ref <- NULL
					msg <- 'There is no station selected'
				}
			}
		}else{
			ret <- choose.neignbors(mvar = don, xpos = xpos, lon = donnees$Lon, lat = donnees$Lat, elv = dem_data, max.dist = max.dist, min.stn = min.stn)
			ret <- use.elevation(ret = ret, uselv = uselv, elv.diff = elv.diff, min.stn = min.stn)

			if(weight.fac == 'Average'){
				Y <- ret$Y
				lambda <- rep(1/ncol(Y), ncol(Y))
				ref <- unname(apply(t(lambda*t(Y)), 1, sum))
				msg <- NULL
			}else{
				if(weight.fac == 'Correlation') facw <- '1'
				if(weight.fac == 'Distance') facw <- '2'
				if(weight.fac == 'Optimal') facw <- '3'
				ret <- weighting.factors(ret = ret, weight.fac = facw, min.stn = min.stn, min.rho = min.rho, max.stn = max.stn)
				Y <- ret$Y
				lambda <- ret$lambda
				if(is.null(Y)){
					ref <- NULL
					msg <- ret$msg
				}else{
					ref <- unname(apply(t(lambda*t(Y)), 1, sum))
					msg <- NULL
				}
			}
		}
	}else{
		ref <- NULL
		msg <- "Check (Use reference series) and Choose a method"
	}
	return(list(ref = ref, msg = msg))
}

