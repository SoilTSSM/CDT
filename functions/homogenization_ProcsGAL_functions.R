
getrefSeries <- function(xpos){
	freqdata <- GeneralParameters$period
	refpars <- as.character(GeneralParameters$ref.series.choix$Values)
	diff.ratio <- refpars[1]
	weight.fac <- refpars[2]
	uselv <- refpars[3]
	interp.dem <- refpars[4]
	min.stn <- as.numeric(refpars[5])
	max.stn <- as.numeric(refpars[6])
	max.dist <- as.numeric(refpars[7])
	elv.diff <- as.numeric(refpars[8])
	min.rho <- as.numeric(refpars[9])

	datdly <- EnvHomogzData$dly_data[[1]]$data
	dydates <- EnvHomogzData$dly_data[[1]]$date
	datdek <- EnvHomogzData$dek_data[[1]]$data
	dkdates <- EnvHomogzData$dek_data[[1]]$date
	datmon <- EnvHomogzData$mon_data[[1]]$data
	modates <- EnvHomogzData$mon_data[[1]]$date
	lon <- EnvHomogzData$donnees1$lon
	lat <- EnvHomogzData$donnees1$lat

	##get elevation data
	if(uselv == '1'){
		if(is.null(EnvHomogzData$r_elv)){
			r_elv <- getElevationData2()
			assign('r_elv',r_elv, envir = EnvHomogzData)
		}else{
			if(interp.dem == "0" & (is.null(EnvHomogzData$r_elv[[2]]) | EnvHomogzData$r_elv[[4]] != as.character(GeneralParameters$file.io$Values[3]))){
				r_elv <- getElevationData2()
				EnvHomogzData$r_elv[2:4] <- r_elv[2:4]
			}else if(interp.dem == "1" & is.null(EnvHomogzData$r_elv[[1]])){
				elv_stn <- EnvHomogzData$donnees1$elv
				if(!is.null(elv_stn)){
					EnvHomogzData$r_elv[[1]] <- r_elv[[1]] <- elv_stn
					EnvHomogzData$r_elv[[3]] <- r_elv[[3]] <- NULL
				}else{
					EnvHomogzData$r_elv[[3]] <- r_elv[[3]] <- 'There is no elevation data in your file'
				}
			}else r_elv <- EnvHomogzData$r_elv
		}

		if(!is.null(r_elv[[3]])){
			msg <- r_elv[[3]]
			status <- 'no'
			elv <- NULL
			return(NULL)
		}else{
			if(interp.dem == "0") elv <- r_elv[[2]]
			else elv <- r_elv[[1]]
		}
	}else elv <- NULL

	dyret <- dkret <- moret <- NULL

	#monthly
	ret <- choose.neignbors(mvar = datmon, xpos = xpos, lon = lon, lat = lat, elv = elv, max.dist = max.dist, min.stn = min.stn)
	ret <- use.elevation(ret = ret, uselv = uselv, elv.diff = elv.diff, min.stn = min.stn)
	ret <- weighting.factors(ret = ret, weight.fac = weight.fac, min.stn = min.stn, min.rho = min.rho, max.stn = max.stn)
	moret <- refSeries.creation(ret = ret, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'monthly', dates = modates)
	rm(ret)
	if(freqdata != 'monthly'){
		#dekadal
		ret <- choose.neignbors(mvar = datdek, xpos = xpos, lon = lon, lat = lat, elv = elv, max.dist = max.dist, min.stn = min.stn)
		ret <- use.elevation(ret = ret, uselv = uselv, elv.diff = elv.diff, min.stn = min.stn)
		ret <- weighting.factors(ret = ret, weight.fac = weight.fac, min.stn = min.stn, min.rho = min.rho, max.stn = max.stn)
		dkret <- refSeries.creation(ret = ret, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'dekadal', dates = dkdates)
		rm(ret)
		if(freqdata == 'daily'){
			#daily
			ret <- choose.neignbors(mvar = datdly, xpos = xpos, lon = lon, lat = lat, elv = elv, max.dist = max.dist, min.stn = min.stn)
			ret <- use.elevation(ret = ret, uselv = uselv, elv.diff = elv.diff, min.stn = min.stn)
			ret <- weighting.factors(ret = ret, weight.fac = weight.fac, min.stn = min.stn, min.rho = min.rho, max.stn = max.stn)
			dyret <- refSeries.creation(ret = ret, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'daily', dates = dydates)
			rm(ret)
		}
	}
	return(list(retdly = dyret, retdek = dkret, retmon = moret))
}


#######################################################################################

getrefSeriesUser <- function(xpos){
	freqdata <- GeneralParameters$period
	refpars <- as.character(GeneralParameters$ref.series.choix$Values)
	diff.ratio <- refpars[1]
	weight.fac <- refpars[2]
	ypos <- as.numeric(GeneralParameters$stn.user.choice)

	datdly <- EnvHomogzData$dly_data[[1]]$data
	dydates <- EnvHomogzData$dly_data[[1]]$date
	datdek <- EnvHomogzData$dek_data[[1]]$data
	dkdates <- EnvHomogzData$dek_data[[1]]$date
	datmon <- EnvHomogzData$mon_data[[1]]$data
	modates <- EnvHomogzData$mon_data[[1]]$date
	lon <- EnvHomogzData$donnees1$lon
	lat <- EnvHomogzData$donnees1$lat

	dyret <- dkret <- moret <- NULL

	#monthly
	ret <- weighting.factors.usr(xpos = xpos, ypos = ypos, mvar = datmon, lon = lon, lat = lat, weight.fac = weight.fac)
	moret <- refSeries.creation(ret = ret, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'monthly', dates = modates)
	rm(ret)
	if(freqdata != 'monthly'){
		#dekadal
		ret <- weighting.factors.usr(xpos = xpos, ypos = ypos, mvar = datdek, lon = lon, lat = lat, weight.fac = weight.fac)
		dkret <- refSeries.creation(ret = ret, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'dekadal', dates = dkdates)
		rm(ret)
		if(freqdata == 'daily'){
			#daily
			ret <- weighting.factors.usr(xpos = xpos, ypos = ypos, mvar = datdly, lon = lon, lat = lat, weight.fac = weight.fac)
			dyret <- refSeries.creation(ret = ret, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'daily', dates = dydates)
			rm(ret)
		}
	}
	return(list(retdly = dyret, retdek = dkret, retmon = moret))
}

#######################################################################################

getrefSeries1S <- function(xpos, ypos){
	freqdata <- GeneralParameters$period
	diff.ratio <- as.character(GeneralParameters$ref.series.choix$Values[1])

	if(!is.null(xpos)){
		datdly <- EnvHomogzData$dly_data[[1]]$data[,xpos]
		refdly <- EnvHomogzData$dly_data[[1]]$data[,ypos]
		datdek <- EnvHomogzData$dek_data[[1]]$data[,xpos]
		refdek <- EnvHomogzData$dek_data[[1]]$data[,ypos]
		datmon <- EnvHomogzData$mon_data[[1]]$data[,xpos]
		refmon <- EnvHomogzData$mon_data[[1]]$data[,ypos]

		dydates <- EnvHomogzData$dly_data[[1]]$date
		dkdates <- EnvHomogzData$dek_data[[1]]$date
		modates <- EnvHomogzData$mon_data[[1]]$date
	}else{
		datdly <- EnvHomogzData$dly_data[[1]]$data
		dlydt <- EnvHomogzData$dly_data[[1]]$date
		refdly <- EnvHomogzData$dly_data[[2]]$data
		rdlydt <- EnvHomogzData$dly_data[[2]]$date
		comdt <- dlydt%in%rdlydt
		comdt1 <- rdlydt%in%dlydt
		datdly <- datdly[comdt]
		refdly <- refdly[comdt1]
		dydates <- dlydt[comdt]

		datdek <- EnvHomogzData$dek_data[[1]]$data
		dekdt <- EnvHomogzData$dek_data[[1]]$date
		refdek <- EnvHomogzData$dek_data[[2]]$data
		rdekdt <- EnvHomogzData$dek_data[[2]]$date
		comdt <- dekdt%in%rdekdt
		comdt1 <- rdekdt%in%dekdt
		datdek <- datdek[comdt]
		refdek <- refdek[comdt1]
		dkdates <- dekdt[comdt]

		datmon <- EnvHomogzData$mon_data[[1]]$data
		mondt <- EnvHomogzData$mon_data[[1]]$date
		refmon <- EnvHomogzData$mon_data[[2]]$data
		rmondt <- EnvHomogzData$mon_data[[2]]$date
		comdt <- mondt%in%rmondt
		comdt1 <- rmondt%in%mondt
		datmon <- datmon[comdt]
		refmon <- refmon[comdt1]
		modates <- mondt[comdt]
	}

	dyret <- dkret <- moret <- NULL

	#monthly
	moret <- refSeries.creation1S(cand = datmon, refs = refmon, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'monthly', dates = modates)
	if(freqdata != 'monthly'){
		#dekadal
		dkret <- refSeries.creation1S(cand = datdek, refs = refdek, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'dekadal', dates = dkdates)
		#daily
		if(freqdata == 'daily') dyret <- refSeries.creation1S(cand = datdly, refs = refdly, diff.ratio = diff.ratio, use.climato = TRUE, scale = 'daily', dates = dydates)
	}
	return(list(retdly = dyret, retdek = dkret, retmon = moret))
}

#######################################################################################

gettestSeries <- function(xpos){
	freqdata <- GeneralParameters$period

	if(!is.null(xpos)){
		datdly <- EnvHomogzData$dly_data[[1]]$data[,xpos]
		datdek <- EnvHomogzData$dek_data[[1]]$data[,xpos]
		datmon <- EnvHomogzData$mon_data[[1]]$data[,xpos]
	}else{
		datdly <- EnvHomogzData$dly_data[[1]]$data
		datdek <- EnvHomogzData$dek_data[[1]]$data
		datmon <- EnvHomogzData$mon_data[[1]]$data
	}
	dydates <- EnvHomogzData$dly_data[[1]]$date
	dkdates <- EnvHomogzData$dek_data[[1]]$date
	modates <- EnvHomogzData$mon_data[[1]]$date

	dyret <- dkret <- moret <- NULL

	#monthly
	moret <- standardize(datmon, use.climato = TRUE, scale = 'monthly', dates = modates)
	if(freqdata != 'monthly'){
		#dekadal
		dkret <- standardize(datdek, use.climato = TRUE, scale = 'dekadal', dates = dkdates)
		#daily
		if(freqdata == 'daily') dyret <- standardize(datdly, use.climato = TRUE, scale = 'daily', dates = dydates)
	}
	return(list(retdly = dyret, retdek = dkret, retmon = moret))
}


############################################################################################################


getBreakpoints <- function(dyref, dkref, moref){
	freqdata <- GeneralParameters$period
	hompars <- as.character(GeneralParameters$hom.opts$Values)
	cropb <- hompars[1]
	h <- as.numeric(hompars[2])
	conf.lev <- as.numeric(hompars[3])
	cpt.max <- as.numeric(hompars[4])
	min.int <- as.numeric(hompars[5])
	stat.fun <- as.character(GeneralParameters$use.method$Values[1])

	homog.mthd <- matrix(c(c("Pettitt Test", "SNHT(Alexandersson & Moberg, 1997)","CUSUM-type (Gallagher et al.,2013)",
	"CUSUM-type with Trend(Gallagher et al.,2013)"), c('PettittRank.s', 'SNHT.s', 'cusum.s', 'cusum.t'),
	c('PettittRank.sh', 'SNHT.sh', 'cusum.sh', 'cusum.th')), ncol = 3)

	res1 <- res2 <- res3 <- NULL

	if(cropb == '1'){
		fun <- homog.mthd[as.character(homog.mthd[,1])%in%stat.fun, 3]

		#monthly
		res3 <- MultiBreaks.detection(moref, min.int, fun, h)
		if(freqdata != 'monthly'){
			#dekadal
			res2 <- MultiBreaks.detection(dkref, min.int = min.int*3, fun, h)
			#daily
			if(freqdata == 'daily') res1 <- MultiBreaks.detection(dyref, min.int = min.int*30, fun, h)
		}
	}else{
		fun <- homog.mthd[as.character(homog.mthd[,1])%in%stat.fun, 2]

		#monthly
		res3 <- MultiBreaks.detection(moref, min.int, fun)
		if(freqdata != 'monthly'){
			#dekadal
			res2 <- MultiBreaks.detection(dkref, min.int = min.int*3, fun)
			#daily
			if(freqdata == 'daily') res1 <- MultiBreaks.detection(dyref, min.int = min.int*30, fun)
		}
	}

	dyobj <- dkobj <- moobj <- NULL

	#monthly
	moobj <- changepoints(moref, res3, cpt.max, conf.lev)
	if(freqdata != 'monthly'){
		#dekadal
		dkobj <- changepoints(dkref, res2, cpt.max, conf.lev)
		#daily
		if(freqdata == 'daily') dyobj <- changepoints(dyref, res1, cpt.max, conf.lev)
	}

	return(list(dyobj = list('daily', dyobj), dkobj = list('dekadal', dkobj), moobj = list('monthly', moobj)))
}


#######################################################################################################################################3

#convert homog results to data.frame

getBreakpointsData <- function(homRetRes){
	if(homRetRes$action == 'homog'){
		dydates <- EnvHomogzData$dly_data[[1]]$date
		dkdates <- EnvHomogzData$dek_data[[1]]$date
		modates <- EnvHomogzData$mon_data[[1]]$date
		dyobj <- homRetRes$res$dyobj[[2]]
		dkobj <- homRetRes$res$dkobj[[2]]
		moobj <- homRetRes$res$moobj[[2]]

		dlybreakpts <- dekbreakpts <- monbreakpts <- NULL

		if(moobj$cpt$number > 0){
			cptID <- moobj$cpt$index
			cptDates <- modates[cptID]
			maxConfLev <- moobj$res$max.conf.lev[moobj$res$cpt.index%in%cptID]
			breakpts <- data.frame(Breakpoints.Index = cptID, Breakpoints.Date = cptDates, Max.Conf.Lev = maxConfLev)
			monbreakpts <- breakpts[order(breakpts$Breakpoints.Index),]
		}else monbreakpts <- data.frame(Breakpoints.Index = NA, Breakpoints.Date = NA, Max.Conf.Lev = NA)

		if(homRetRes$period != 'monthly'){
			if(dkobj$cpt$number > 0){
				cptID <- dkobj$cpt$index
				cptDates <- dkdates[cptID]
				maxConfLev <- dkobj$res$max.conf.lev[dkobj$res$cpt.index%in%cptID]
				breakpts <- data.frame(Breakpoints.Index = cptID, Breakpoints.Date = cptDates, Max.Conf.Lev = maxConfLev)
				dekbreakpts <- breakpts[order(breakpts$Breakpoints.Index),]
			}else dekbreakpts <- data.frame(Breakpoints.Index = NA, Breakpoints.Date = NA, Max.Conf.Lev = NA)
			if(homRetRes$period == 'daily'){
				if(dyobj$cpt$number > 0){
					cptID <- dyobj$cpt$index
					cptDates <- dydates[cptID]
					maxConfLev <- dyobj$res$max.conf.lev[dyobj$res$cpt.index%in%cptID]
					breakpts <- data.frame(Breakpoints.Index = cptID, Breakpoints.Date = cptDates, Max.Conf.Lev = maxConfLev)
					dlybreakpts <- breakpts[order(breakpts$Breakpoints.Index),]
				}else dlybreakpts <- data.frame(Breakpoints.Index = NA, Breakpoints.Date = NA, Max.Conf.Lev = NA)
			}
		}

		return(list(dlybreakpts = dlybreakpts, dekbreakpts = dekbreakpts, monbreakpts = monbreakpts))
	}else{
		InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
		return(NULL)
	}
}


#############################################################
###Format

HomOutFormat <- function(){
	if(ReturnExecResults$action == 'homog'){

		monfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_MON.txt', sep = ''), fsep = .Platform$file.sep)
		modat <- read.table(monfileout, header = TRUE, colClasses = 'character')
		datarr <- as.matrix(rbind(c('Monthly', c(rep(NA, ncol(modat)))), cbind(c(rep(NA, nrow(modat))), modat)))
		datarr <- data.frame(datarr, row.names = 1:nrow(datarr))
		names(datarr) <- c('Period', names(modat))
		retdata <- list(datarr, monfileout)

		if(ReturnExecResults$period != 'monthly'){
			dekfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DEK.txt', sep = ''), fsep = .Platform$file.sep)
			dkdat <- read.table(dekfileout, header = TRUE, colClasses = 'character')
			datarr <- rbind(as.matrix(rbind(c('Dekadal', c(rep(NA, ncol(dkdat)))), cbind(c(rep(NA, nrow(dkdat))), dkdat), NA)), as.matrix(rbind(c('Monthly', c(rep(NA, ncol(modat)))), cbind(c(rep(NA, nrow(modat))), modat))))
			datarr <- data.frame(datarr, row.names = 1:nrow(datarr))
			names(datarr) <- c('Period', names(dkdat))
			retdata <- list(datarr, dekfileout, monfileout)
			if(ReturnExecResults$period == 'daily'){
				dlyfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DLY.txt', sep = ''), fsep = .Platform$file.sep)
				dydat <- read.table(dlyfileout, header = TRUE, colClasses = 'character')
				datarr <- rbind(as.matrix(rbind(c('Daily', c(rep(NA, ncol(dydat)))), cbind(c(rep(NA, nrow(dydat))), dydat), NA)), as.matrix(rbind(c('Dekadal', c(rep(NA, ncol(dkdat)))), cbind(c(rep(NA, nrow(dkdat))), dkdat), NA)), as.matrix(rbind(c('Monthly', c(rep(NA, ncol(modat)))), cbind(c(rep(NA, nrow(modat))), modat))))
				datarr <- data.frame(datarr, row.names = 1:nrow(datarr))
				names(datarr) <- c('Period', names(dydat))
				retdata <- list(datarr, dlyfileout, dekfileout, monfileout)
			}
		}
	}else{
		InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
		retdata <- NULL
	}
	return(retdata)
}


##############################################

reHomOutFormat <- function(dat2format){
	if(ReturnExecResults$action == 'homog'){
		iper <- as.character(dat2format$Period)
		lper <- length(iper)
		ldates <- as.character(dat2format$Breakpoints.Date)

		imo <- grep('Monthly', as.character(iper))
		iimo <- ldates[imo:lper]
		dmo <- dat2format[imo:lper,-1]
		dmo <- dmo[!is.na(iimo),]
		if(length(iimo[!is.na(iimo)])>0){
			datys <- as.character(dmo$Breakpoints.Date)
			datys1 <- gsub("\\s","", datys)
			ndatys <- nchar(datys1)
			datys1 <- as.Date(paste(datys1, '01', sep = ''), format='%Y%m%d')
			msgdt <- which(is.na(datys1) | ndatys != 6)
			if(length(msgdt) > 0){
				InsertMessagesTxt(main.txt.out, paste('Wrong date format [monthly]: ',paste('[',datys[msgdt],']',sep = '',collapse='|')), format = TRUE)
				#return(NULL)
			}else{
				idna <- which(is.na(as.character(dmo$Breakpoints.Index)))
				val1 <- as.character(dmo[,1])
				val1[idna] <- match(as.character(dmo[idna, 2]), EnvHomogzData$mon_data[[1]]$date)
				dmo[,1] <- val1
			}
		}else dmo[1,] <- NA
		retdata <- list(dmo)

		if(ReturnExecResults$period != 'monthly'){
			idk <- grep('Dekadal', as.character(iper))
			iidk <- ldates[idk:(imo-1)]
			ddk <- dat2format[idk:(imo-1),-1]
			ddk <- ddk[!is.na(iidk),]
			if(length(iidk[!is.na(iidk)])>0){
				datys <- as.character(ddk$Breakpoints.Date)
				datys1 <- gsub("\\s","", datys)
				ndatys <- nchar(datys1)
				datys1 <- as.Date(datys1, format='%Y%m%d')
				msgdt <- which(is.na(datys1) | ndatys != 7)
				if(length(msgdt) > 0){
					InsertMessagesTxt(main.txt.out, paste('Wrong date format [dekadal]: ',paste('[',datys[msgdt],']',sep = '',collapse='|')), format = TRUE)
					#return(NULL)
				}else{
					idna <- which(is.na(as.character(ddk$Breakpoints.Index)))
					val1 <- as.character(ddk[,1])
					val1[idna] <- match(as.character(ddk[idna, 2]), EnvHomogzData$dek_data[[1]]$date)
					ddk[,1] <- val1
				}
			}else ddk[1,] <- NA
			retdata <- list(ddk, dmo)

			if(ReturnExecResults$period == 'daily'){
				idy <- grep('Daily', as.character(iper))
				iidy <- ldates[idy:(idk-1)]
				ddy <- dat2format[idy:(idk-1),-1]
				ddy <- ddy[!is.na(iidy),]
				if(length(iidy[!is.na(iidy)])>0){
					datys <- as.character(ddy$Breakpoints.Date)
					datys1 <- gsub("\\s","", datys)
					ndatys <- nchar(datys1)
					datys1 <- as.Date(datys1, format='%Y%m%d')
					msgdt <- which(is.na(datys1) | ndatys != 8)
					if(length(msgdt) > 0){
						InsertMessagesTxt(main.txt.out, paste('Wrong date format [daily]: ',paste('[',datys[msgdt],']',sep = '',collapse='|')), format = TRUE)
						#return(NULL)
					}else{
						idna <- which(is.na(as.character(ddy$Breakpoints.Index)))
						val1 <- as.character(ddy[,1])
						val1[idna] <- match(as.character(ddy[idna, 2]), EnvHomogzData$dly_data[[1]]$date)
						ddy[,1] <- val1
					}
				}else ddy[1,] <- NA
				retdata <- list(ddy, ddk, dmo)
			}
		}
	}else{
		InsertMessagesTxt(main.txt.out, 'Reinitialize the operation. Parameters or Outputs are not a homogenization results', format = TRUE)
		retdata <- NULL
	}
	return(retdata)
}

#####################################################################################3

undoBreaksChange <- function(){
	if(ReturnExecResults$action == 'homog'){
		monfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_MON.txt', sep = ''), fsep = .Platform$file.sep)
		monfileout0 <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_MON0.txt', sep = ''), fsep = .Platform$file.sep)
		file.copy(monfileout0, monfileout, overwrite = TRUE)
		if(ReturnExecResults$period != 'monthly'){
			dekfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DEK.txt', sep = ''), fsep = .Platform$file.sep)
			dekfileout0 <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DEK0.txt', sep = ''), fsep = .Platform$file.sep)
			file.copy(dekfileout0, dekfileout, overwrite = TRUE)
			if(ReturnExecResults$period == 'daily'){
				dlyfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DLY.txt', sep = ''), fsep = .Platform$file.sep)
				dlyfileout0 <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DLY0.txt', sep = ''), fsep = .Platform$file.sep)
				file.copy(dlyfileout0, dlyfileout, overwrite = TRUE)
			}
		}
	}else{
		InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
		return(NULL)
	}
}

#####################################################################################3

AdjustInHomog <- function(choix){
	if(ReturnExecResults$action == 'homog' & GeneralParameters$action == 'homog'){

		adjustdir <- file.path(paste(head(unlist(strsplit(ReturnExecResults$outputdir,.Platform$file.sep)), n=-2), sep ='',collapse = .Platform$file.sep),'AdjustedData', fsep = .Platform$file.sep)
		if(!file.exists(adjustdir)) dir.create(adjustdir, showWarnings = FALSE, recursive = TRUE)
		adjustdirstn <- file.path(adjustdir, ReturnExecResults$station, fsep = .Platform$file.sep)
		if(!file.exists(adjustdirstn)) dir.create(adjustdirstn, showWarnings = FALSE, recursive = TRUE)

		infofileadj <- file.path(adjustdirstn, paste(ReturnExecResults$station, '_INFO.txt', sep = ''), fsep = .Platform$file.sep)
		choixfileadj <- file.path(adjustdirstn, paste(ReturnExecResults$station, '_CHOICE.txt', sep = ''), fsep = .Platform$file.sep)

		dlyfileadj <- file.path(adjustdirstn, paste(ReturnExecResults$station, '_DLY.txt', sep = ''), fsep = .Platform$file.sep)
		dekfileadj <- file.path(adjustdirstn, paste(ReturnExecResults$station, '_DEK.txt', sep = ''), fsep = .Platform$file.sep)
		monfileadj <- file.path(adjustdirstn, paste(ReturnExecResults$station, '_MON.txt', sep = ''), fsep = .Platform$file.sep)

		dlyfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DLY.txt', sep = ''), fsep = .Platform$file.sep)
		dekfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DEK.txt', sep = ''), fsep = .Platform$file.sep)
		monfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_MON.txt', sep = ''), fsep = .Platform$file.sep)

		stat.fun <- as.character(GeneralParameters$use.method$Values[1])
		minadjmo <- as.numeric(as.character(GeneralParameters$Adjust.method$Values[1]))
		minadjdk <- as.numeric(as.character(GeneralParameters$Adjust.method$Values[2]))
		minadjdy <- as.numeric(as.character(GeneralParameters$Adjust.method$Values[3]))
		SegAdjmo <- as.numeric(as.character(GeneralParameters$Adjust.method$Values[4]))
		SegAdjdk <- as.numeric(as.character(GeneralParameters$Adjust.method$Values[5]))
		SegAdjdy <- as.numeric(as.character(GeneralParameters$Adjust.method$Values[6]))

		dydates <- EnvHomogzData$dly_data[[1]]$date
		dkdates <- EnvHomogzData$dek_data[[1]]$date
		modates <- EnvHomogzData$mon_data[[1]]$date

		if(as.character(GeneralParameters$use.method$Values[2]) == "0"){
			get.stn <- ReturnExecResults$station
			xpos <- which(as.character(EnvHomogzData$donnees1$id) == get.stn)
			xdly <- EnvHomogzData$dly_data[[1]]$data[,xpos]
			xdek <- EnvHomogzData$dek_data[[1]]$data[,xpos]
			xmon <- EnvHomogzData$mon_data[[1]]$data[,xpos]
		}else{
			xdly <- EnvHomogzData$dly_data[[1]]$data
			xdek <- EnvHomogzData$dek_data[[1]]$data
			xmon <- EnvHomogzData$mon_data[[1]]$data
		}

		#######
		modat <- read.table(monfileout, header = TRUE, colClasses = 'character')
		cptmo <- sort(na.omit(as.numeric(as.character(modat$Breakpoints.Index))))
		if(stat.fun == "CUSUM-type with Trend(Gallagher et al.,2013)"){
			retmon <- AdjustT.byMean(xmon, cptmo, SegAdj = SegAdjmo, minadjmo)
			vmon <- retmon$res
			msgmon <- retmon$msg
			adjmth <- "By mean"
			###
			retmon1 <- AdjustT.byQM(xmon, cptmo, SegAdj = SegAdjmo, minadjmo)
			vmon1 <- retmon1$res
			msgmon1 <- retmon1$msg
			adjmth1 <- 'By quantile matching'
		}else{
			retmon <- AdjustM.byMean(xmon, cptmo, SegAdj = SegAdjmo, minadjmo)
			vmon <- retmon$res
			msgmon <- retmon$msg
			adjmth <- "By mean"
			###
			retmon1 <- AdjustM.byQM(xmon, cptmo, SegAdj = SegAdjmo, minadjmo)
			vmon1 <- retmon1$res
			msgmon1 <- retmon1$msg
			adjmth1 <- 'By quantile matching'
		}
		###
		if(!is.null(vmon)){
			if(as.character(GeneralParameters$ref.series.choix$Values[1]) != '1') vmon[xmon == 0 | vmon < 0] <- 0
			smon <- vmon
			adjvmon <- if(length(cptmo) > 0) c('Monthly Data', 'Adjusted', adjmth, 'Adjusted Value') else c('Monthlyly Data', 'No adjustment needed', 'No breakpoints', 'Original Data')
		}else{
			InsertMessagesTxt(main.txt.out, paste('Monthly data:: No adjustment by mean was made for', ReturnExecResults$station,' :: ',msgmon), format = TRUE)
			smon <- xmon
			adjvmon <- c('Monthly Data', 'No adjustment performed', 'Options not match', 'Original Data')
		}
		###
		if(!is.null(vmon1)){
			if(as.character(GeneralParameters$ref.series.choix$Values[1]) != '1') vmon1[xmon == 0 | vmon1 < 0] <- 0
			smon1 <- vmon1
			adjvmon1 <- if(length(cptmo) > 0) c('Monthly Data', 'Adjusted', adjmth1, 'Adjusted Value') else c('Monthlyly Data', 'No adjustment needed', 'No breakpoints', 'Original Data')
		}else{
			InsertMessagesTxt(main.txt.out, paste('Monthly data:: No adjustment by quantile matching was made for', ReturnExecResults$station,' :: ',msgmon1), format = TRUE)
			smon1 <- xmon
			adjvmon1 <- c('Monthly Data', 'No adjustment performed', 'Options not match', 'Original Data')
		}

		sdon3 <- data.frame(modates, round(xmon, 1), round(smon, 1), round(smon1, 1))
		write.table(sdon3, monfileadj, col.names = FALSE, row.names = FALSE)
		infotxt <- t(cbind(adjvmon, adjvmon1))
		retdata <- list(Info = infotxt, DataMon = xmon, DataAdjMon = smon, DataAdjMon1 = smon1, DatesAdjmon = modates)

		if(ReturnExecResults$period != 'monthly'){
			dkdat <- read.table(dekfileout, header = TRUE, colClasses = 'character')
			cptdk <- sort(na.omit(as.numeric(as.character(dkdat$Breakpoints.Index))))
			if(stat.fun == "CUSUM-type with Trend(Gallagher et al.,2013)"){
				retdek <- AdjustT.byMean(xdek, cptdk, SegAdj = SegAdjdk, minadjdk*3)
				vdek <- retdek$res
				msgdek <- retdek$msg
				adjmth <- "By mean"
				###
				retdek1 <- AdjustT.byQM(xdek, cptdk, SegAdj = SegAdjdk, minadjdk*3)
				vdek1 <- retdek1$res
				msgdek1 <- retdek1$msg
				adjmth1 <- 'By quantile matching'
			}else{
				retdek <- AdjustM.byMean(xdek, cptdk, SegAdj = SegAdjdk, minadjdk*3)
				vdek <- retdek$res
				msgdek <- retdek$msg
				adjmth <- "By mean"
				###
				retdek1 <- AdjustM.byQM(xdek, cptdk, SegAdj = SegAdjdk, minadjdk*3)
				vdek1 <- retdek1$res
				msgdek1 <- retdek1$msg
				adjmth1 <- 'By quantile matching'
			}
			###
			if(!is.null(vdek)){
				if(as.character(GeneralParameters$ref.series.choix$Values[1]) != '1') vdek[xdek == 0 | vdek < 0] <- 0
				sdek <- vdek
				adjvdek <- if(length(cptdk) > 0) c('Dekadal Data', 'Adjusted', adjmth, 'Adjusted Value') else c('Dekadal Data', 'No adjustment needed', 'No breakpoints', 'Original Data')
			}else{
				InsertMessagesTxt(main.txt.out, paste('Dekadal data:: No adjustment by mean was made for', ReturnExecResults$station,' :: ',msgdek), format = TRUE)
				sdek <- xdek
				adjvdek <- c('Dekadal Data', 'No adjustment performed', 'Options not match', 'Original Data')
			}
			####
			if(!is.null(vdek1)){
				if(as.character(GeneralParameters$ref.series.choix$Values[1]) != '1') vdek1[xdek == 0 | vdek1 < 0] <- 0
				sdek1 <- vdek1
				adjvdek1 <- if(length(cptdk) > 0) c('Dekadal Data', 'Adjusted', adjmth1, 'Adjusted Value') else c('Dekadal Data', 'No adjustment needed', 'No breakpoints', 'Original Data')
			}else{
				InsertMessagesTxt(main.txt.out, paste('Dekadal data:: No adjustment by quantile matching was made for', ReturnExecResults$station,' :: ',msgdek1), format = TRUE)
				sdek1 <- xdek
				adjvdek1 <- c('Dekadal Data', 'No adjustment performed', 'Options not match', 'Original Data')
			}

			sdon2 <- data.frame(dkdates, round(xdek, 1), round(sdek, 1), round(sdek1, 1))
			write.table(sdon2, dekfileadj, col.names = FALSE, row.names = FALSE)
			infotxt <- t(cbind(adjvdek, adjvdek1, adjvmon, adjvmon1))
			retdata <- list(Info = infotxt, DataDek = xdek, DataAdjDek = sdek, DataAdjDek1 = sdek1, DatesAdjDek = dkdates, DataMon = xmon, DataAdjMon = smon, DataAdjMon1 = smon1, DatesAdjmon = modates)

			if(ReturnExecResults$period == 'daily'){
				dydat <- read.table(dlyfileout, header = TRUE, colClasses = 'character')
				cptdy <- sort(na.omit(as.numeric(as.character(dydat$Breakpoints.Index))))
				if(stat.fun == "CUSUM-type with Trend(Gallagher et al.,2013)"){
					retdly <- AdjustT.byMean(xdly, cptdy, SegAdj = SegAdjdy, minadjdy*30)
					vdly <- retdly$res
					msgdly <- retdly$msg
					adjmth <- "By mean"
					###
					retdly1 <- AdjustT.byQM(xdly, cptdy, SegAdj = SegAdjdy, minadjdy*30)
					vdly1 <- retdly1$res
					msgdly1 <- retdly1$msg
					adjmth1 <- 'By quantile matching'
				}else{
					retdly <- AdjustM.byMean(xdly, cptdy, SegAdj = SegAdjdy, minadjdy*30)
					vdly <- retdly$res
					msgdly <- retdly$msg
					adjmth <- "By mean"
					###
					retdly1 <- AdjustM.byQM(xdly, cptdy, SegAdj = SegAdjdy, minadjdy*30)
					vdly1 <- retdly1$res
					msgdly1 <- retdly1$msg
					adjmth1 <- 'By quantile matching'
				}
				###
				if(!is.null(vdly)){
					if(as.character(GeneralParameters$ref.series.choix$Values[1]) != '1') vdly[xdly == 0 | vdly < 0] <- 0
					sdly <- vdly
					adjvdly <- if(length(cptdy) > 0) c('Daily Data', 'Adjusted', adjmth, 'Adjusted Value') else c('Daily Data', 'No adjustment needed', 'No breakpoints', 'Original Data')
				}else{
					InsertMessagesTxt(main.txt.out, paste('Daily data:: No adjustment by mean was made for', ReturnExecResults$station,' :: ',msgdly), format = TRUE)
					sdly <- xdly
					adjvdly <- c('Daily Data', 'No adjustment performed', 'Options not match', 'Original Data')
				}
				#####
				if(!is.null(vdly1)){
					if(as.character(GeneralParameters$ref.series.choix$Values[1]) != '1') vdly1[xdly == 0 | vdly1 < 0] <- 0
					sdly1 <- vdly1
					adjvdly1 <- if(length(cptdy) > 0) c('Daily Data', 'Adjusted', adjmth1, 'Adjusted Value') else c('Daily Data', 'No adjustment needed', 'No breakpoints', 'Original Data')
				}else{
					InsertMessagesTxt(main.txt.out, paste('Daily data:: No adjustment by quantile matching was made for', ReturnExecResults$station,' :: ',msgdly1), format = TRUE)
					sdly1 <- xdly
					adjvdly1 <- c('Daily Data', 'No adjustment performed', 'Options not match', 'Original Data')
				}

				sdon1 <- data.frame(dydates, round(xdly, 1), round(sdly, 1), round(sdly1, 1))
				write.table(sdon1, dlyfileadj, col.names = FALSE, row.names = FALSE)
				infotxt <- t(cbind(adjvdly, adjvdly1, adjvdek, adjvdek1, adjvmon, adjvmon1))
				retdata <- list(Info = infotxt, DataDly = xdly, DataAdjDly = sdly, DataAdjDly1 = sdly1, DatesAdjDly = dydates, DataDek = xdek, DataAdjDek = sdek, DataAdjDek1 = sdek1, DatesAdjDek = dkdates, DataMon = xmon, DataAdjMon = smon, DataAdjMon1 = smon1, DatesAdjmon = modates)
			}
		}

		write.table(infotxt, infofileadj, col.names = FALSE, row.names = FALSE)
		write.table(choix, choixfileadj, col.names = FALSE, row.names = FALSE)
		return(retdata)
	}else{
		InsertMessagesTxt(main.txt.out, 'Reinitialize the operation. Parameters or Outputs are not a homogenization results', format = TRUE)
		return(NULL)
	}
}


