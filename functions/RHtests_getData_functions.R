getRHtestsData <- function(GeneralParameters){
	freqdata <- GeneralParameters$period
	single.series <- GeneralParameters$stn.type$single.series
	useref <- GeneralParameters$use.refSeries
	filefrmt <- GeneralParameters$stn.type$file.format
	datefrmt <- GeneralParameters$stn.type$date.format
	varcat <- GeneralParameters$stn.type$vars
	comp.fun <- GeneralParameters$aggr.var$fonction
	miss.frac <- GeneralParameters$aggr.var$miss.frac

	rhtestout.dir <- file.path(GeneralParameters$IO.files$dir2save, paste('RHtests_Output',
								getf.no.ext(GeneralParameters$IO.files$Cand.file), sep = '_'))
	outdir <- file.path(rhtestout.dir, 'Outputs')
	dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
	origdir <- file.path(rhtestout.dir, 'Data')
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	flmondata <- file.path(origdir, paste('MONTHLY', GeneralParameters$IO.files$Cand.file, sep = '_'))
	if(freqdata != 'monthly'){
		fldekdata <- file.path(origdir, paste('DEKADAL', GeneralParameters$IO.files$Cand.file, sep = '_'))
		if(freqdata == 'daily') fldlydata <- file.path(origdir, paste('DAILY', GeneralParameters$IO.files$Cand.file, sep = '_'))
	}

	#####

	donne <- getStnOpenData(GeneralParameters$IO.files$Cand.file)
	if(is.null(donne)) return(NULL)
	infoDonne <- getStnOpenDataInfo(GeneralParameters$IO.files$Cand.file)[2:3]
	MissingValue <- infoDonne[[2]]$miss.val

	## a changer complement
	## /Users/rijaf/Desktop/ECHANGE/github/CDT/functions/cdtClimato0_functions.R
	retval <- NULL
	if(!single.series){
		donne <- getCDTdataAndDisplayMsg(donne, freqdata)
		if(is.null(donne)) return(NULL)
		lon <- donne$lon
		lat <- donne$lat
		donne.id <- donne$id
		elv <- donne$elv
		dates <- donne$dates
		donne <- donne$data

		flstninfo <- file.path(origdir, paste('Infos', GeneralParameters$IO.files$Cand.file, sep = '_'))
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

		retval <- list(IDs = donne.id, Lon = lon, Lat = lat, Elv = elv,
							datdly = dydonne, dydates = dydaty,
							datdek = dkdonne, dkdates = dkdaty,
							datmon = dmdonne, modates = dmdaty,
							MissingValue = MissingValue)
	}else{
		donne <- getCDTTSdataAndDisplayMsg(donne, freqdata, filefrmt, datefrmt)
		if(is.null(donne)) return(NULL)
		dates <- donne$dates
		var <- if(filefrmt == "0") switch(varcat, "1" = donne$var$rr, "2" = donne$var$tx, "3" = donne$var$tn) else donne$var$var

		if(useref){
			if(GeneralParameters$IO.files$Ref.file == ""){
				InsertMessagesTxt(main.txt.out, 'There is no reference series provided', format = TRUE)
				return(NULL)
			}else{
				donne1 <- getStnOpenData(GeneralParameters$IO.files$Ref.file)
				if(is.null(donne1)) return(NULL)
				donne1 <- getCDTTSdataAndDisplayMsg(donne1, freqdata, filefrmt, datefrmt)
				if(is.null(donne1)) return(NULL)
				dates1 <- donne1$dates
				var1 <- if(filefrmt == "0") switch(varcat, "1" = donne1$var$rr, "2" = donne1$var$tx, "3" = donne1$var$tn) else donne1$var$var
			}
		}

		########################

		if(useref){
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

			retval <- list(datdly = dydonne, refdly = rdydonne, dydates = dydaty,
							datdek = dkdonne, refdek = rdkdonne, dkdates = dkdaty,
							datmon = dmdonne, refmon = rdmdonne, modates = dmdaty,
							MissingValue = MissingValue)
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

			retval <- list(datdly = dydonne, dydates = dates, datdek = dkdonne,
							dkdates = dkdaty, datmon = dmdonne, modates = dmdaty,
							MissingValue = MissingValue)
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
	if(!GeneralParameters$stn.type$single.series){
		if(GeneralParameters$use.refSeries & GeneralParameters$refSeries.choix$use.elv){
			if(is.null(donnees)){
				InsertMessagesTxt(main.txt.out, 'Data not yet loaded.',format = TRUE)
				return(NULL)
			}else{
				if(GeneralParameters$refSeries.choix$interp.dem == "0"){
					if(GeneralParameters$IO.files$DEM.file == ""){
						InsertMessagesTxt(main.txt.out, 'There is no elevation data in format NetCDF', format = TRUE)
						return(NULL)
					}else{
						demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
						ijGrd <- grid2pointINDEX(list(lon = donnees$Lon, lat = donnees$Lat),
												list(lon = demData$lon, lat = demData$lat))
						elv <- demData$demGrd@data[ijGrd, 1]
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

	weight.fac <- GeneralParameters$refSeries.choix$weight.mean
	uselv <- GeneralParameters$refSeries.choix$use.elv
	min.stn <- GeneralParameters$refSeries.choix$min.stn
	max.stn <- GeneralParameters$refSeries.choix$max.stn
	max.dist <- GeneralParameters$refSeries.choix$max.dist
	elv.diff <- GeneralParameters$refSeries.choix$elv.diff
	min.rho <- GeneralParameters$refSeries.choix$min.rho

	if(GeneralParameters$use.refSeries){
		if(GeneralParameters$refSeries.by.user){
			ypos <- GeneralParameters$stn.user.choice
			if(is.null(ypos)){
				ref <- NULL
				msg <- 'There is no station selected'
			}else{
				if(length(ypos) > 1){
					x0 <- don[, xpos]
					Y <- don[, ypos]
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
					ref <- don[, ypos]
					msg <- NULL
				}else{
					ref <- NULL
					msg <- 'There is no station selected'
				}
			}
		}else{
			ret <- choose.neignbors(mvar = don, xpos = xpos, lon = donnees$Lon, lat = donnees$Lat,
									elv = dem_data, max.dist = max.dist, min.stn = min.stn)
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

