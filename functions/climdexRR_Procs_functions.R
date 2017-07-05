
climdexCalc.RR <- function(GeneralParameters){
	if(GeneralParameters$IO.files$In.dir.file == "" | GeneralParameters$IO.files$In.dir.file == "NA"){
		InsertMessagesTxt(main.txt.out, 'No input data found', format = TRUE)
		return(NULL)
	}

	if(!file.exists(GeneralParameters$IO.files$Out.dir.file)){
		InsertMessagesTxt(main.txt.out, 'Directory to save computed indices not found', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste('The indices will be put in', getwd()))
		GeneralParameters$IO.files$Out.dir.file <- getwd()
	}

	is.Rx1day <- GeneralParameters$Indices$Rx1day
	is.Rx5day <- GeneralParameters$Indices$Rx5day
	is.SDII <- GeneralParameters$Indices$SDII
	is.R10mm <- GeneralParameters$Indices$R10mm
	is.R20mm <- GeneralParameters$Indices$R20mm
	is.Rnnmm <- GeneralParameters$Indices$Rnnmm
	is.CDD <- GeneralParameters$Indices$CDD
	is.CWD <- GeneralParameters$Indices$CWD
	is.R95pTOT <- GeneralParameters$Indices$R95pTOT
	is.R99pTOT <- GeneralParameters$Indices$R99pTOT
	is.PRCPTOT <- GeneralParameters$Indices$PRCPTOT
	
	indxlst <- c("Rx1day", "Rx5day", "SDII", "R10mm", "R20mm", "Rnnmm",
					"CDD", "CWD", "R95pTOT", "R99pTOT", "PRCPTOT")

	if(!any(unlist(GeneralParameters$Indices[indxlst]))){
		InsertMessagesTxt(main.txt.out, 'No indices selected.', format = TRUE)
		return(0)
	}

	#############
	indexDIR <- file.path(GeneralParameters$IO.files$Out.dir.file, "ClimdexRR")
	if(file.exists(indexDIR)){
		msg <- paste0("The directory\n", indexDIR, "\nalready exists. Do you want to overwrite it?")
		retval <- tkmessageBox(message = msg, icon = "question", type = "yesno", default = "yes")
		retval <- substr(tolower(tclvalue(retval)), 1, 1)
		if(retval == 'n'){
			InsertMessagesTxt(main.txt.out, 'Change the directory to save indices', format = TRUE)
			return(NULL)
		}
		tcl("update")
	}else dir.create(indexDIR, showWarnings = FALSE, recursive = TRUE)

	#############
	if(GeneralParameters$data.type == 'cdt'){
		cdtdata <- getStnOpenData(GeneralParameters$IO.files$In.dir.file)
		if(is.null(cdtdata)) return(NULL)
		cdtdata <- getCDTdataAndDisplayMsg(cdtdata, "daily")
		if(is.null(cdtdata)) return(NULL)
		cdtdataInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$In.dir.file)
		miss.val <- cdtdataInfo[[3]]$miss.val
		cdtdata <- cdtdata[c('id', 'lon', 'lat', 'dates', 'data')]
	}

	if(GeneralParameters$data.type == 'series'){
		cdtdata <- getStnOpenData(GeneralParameters$IO.files$In.dir.file)
		if(is.null(cdtdata)) return(NULL)
		cdtdataInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$In.dir.file)
		if(is.null(cdtdataInfo)) return(NULL)
		cdtdata <- getCDTTSdataAndDisplayMsg(cdtdata, "daily",
											GeneralParameters$One.series$file.format,
											GeneralParameters$One.series$date.format)
		if(is.null(cdtdata)) return(NULL)
		miss.val <- cdtdataInfo[[3]]$miss.val

		cdtdata <- list(
			id = GeneralParameters$One.series$id,
			lon = GeneralParameters$One.series$lon,
			lat = GeneralParameters$One.series$lat,
			dates = cdtdata$dates,
			data = {
				if(cdtdata$nbvar == 3) matrix(cdtdata$var$rr, ncol = 1)
				else matrix(cdtdata$var$var, ncol = 1)
			}
		)
	}

	if(GeneralParameters$data.type == 'netcdf'){
		dstart <- try(as.Date(paste(GeneralParameters$ncdf.file$start.year,
									GeneralParameters$ncdf.file$start.mon,
									GeneralParameters$ncdf.file$start.day, sep = '-')),
						silent = TRUE)
		dend <- try(as.Date(paste(GeneralParameters$ncdf.file$end.year,
								GeneralParameters$ncdf.file$end.mon,
								GeneralParameters$ncdf.file$end.day, sep = '-')),
						silent = TRUE)
		if(inherits(dstart, "try-error")){
			InsertMessagesTxt(main.txt.out, "Wrong start date for netcdf data", format = TRUE)
			return(NULL)
		}
		if(inherits(dend, "try-error")){
			InsertMessagesTxt(main.txt.out, "Wrong end date for netcdf data", format = TRUE)
			return(NULL)
		}

		dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
		filein0 <- file.path(GeneralParameters$IO.files$In.dir.file,
							sprintf(GeneralParameters$ncdf.file$format,
							as.character(substr(dates, 1, 4)),
							as.character(substr(dates, 5, 6)),
							as.character(substr(dates, 7, 8))))

		existFl <- unlist(lapply(filein0, file.exists))
		if(!any(existFl)){
			InsertMessagesTxt(main.txt.out, "Unable to locate NetCDF files", format = TRUE)
			return(NULL)
		}

		ncInfo <- getRFESampleData(GeneralParameters$ncdf.file$sample)
		if(is.null(ncInfo)){
			InsertMessagesTxt(main.txt.out, "No daily rainfall sample file found", format = TRUE)
			return(NULL)
		}

		nc <- nc_open(filein0[which(existFl)[1]])
		lon <- nc$dim[[ncInfo$rfeILon]]$vals
		lat <- nc$dim[[ncInfo$rfeILat]]$vals
		nc_close(nc)

		InsertMessagesTxt(main.txt.out, 'Read daily rainfall data ...')

		is.parallel <- doparallel(length(which(existFl)) >= 180)
		`%parLoop%` <- is.parallel$dofun

		xo <- order(lon)
		lon <- lon[xo]
		yo <- order(lat)
		lat <- lat[yo]

		ncData <- foreach(jj = seq_along(filein0), .packages = "ncdf4",
						.export = c("existFl", "filein0", "ncInfo")) %parLoop% {
			if(existFl[jj]){
				nc <- nc_open(filein0[jj])
				vars <- ncvar_get(nc, varid = ncInfo$rfeVarid)
				nc_close(nc)
				vars <- vars[xo, yo]
				if(ncInfo$rfeILat < ncInfo$rfeILon){
					vars <- matrix(c(vars), nrow = length(lon), ncol = length(lat), byrow = TRUE)
				}
			}else vars <- NULL
			vars
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		InsertMessagesTxt(main.txt.out, 'Reading daily rainfall data finished')

		ncData[sapply(ncData, is.null)] <- list(matrix(NA, nrow = length(lon), ncol = length(lat)))
		xycrd <- expand.grid(lon, lat)

		####
		nx <- length(lon)
		ny <- length(lat)
		dx <- ncdim_def("Lon", "degreeE", lon)
		dy <- ncdim_def("Lat", "degreeN", lat)
		xy.dim <- list(dx, dy)
		grd.slp <- ncvar_def("slope", "", xy.dim, NA, longname = "Slope estimate", prec = "float")
		grd.std.slp <- ncvar_def("std.slope", "", xy.dim, NA, longname = "Slope error", prec = "float")
		grd.pvalue <- ncvar_def("pvalue", "", xy.dim, NA, longname = "P-value", prec = "float")
		grd.r2 <- ncvar_def("r2", "", xy.dim, NA, longname = "Coefficient of determination R2", prec = "float")
		trend.vars <- list(grd.slp, grd.std.slp, grd.pvalue, grd.r2)

		####
		cdtdata <- list(
			id = paste("p", seq(nrow(xycrd)), sep = ""),
			lon = xycrd[, 1],
			lat = xycrd[, 2],
			dates = dates,
			data = t(sapply(ncData, c))
		)
		rm(ncData)
	}

	#############

	cdtdata <- climdex.completeYear(cdtdata)

	separe <- FALSE
	nacount <- climdex.NACount.MonDay(cdtdata, separe = separe)

	#############
	# Rx1day: Monthly maximum 1-day precipitation
	Rx1day <- NULL
	if(is.Rx1day){
		Rx1day <- climdex.RX1day(cdtdata, nacount, separe = separe)

		ncparsRx1day <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsRx1day <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
							nc.unit = "mm", nc.prec = "short",
							longname.yrs = "Annual maximum 1-day precipitation",
							longname.mon = "Monthly maximum 1-day precipitation")
		}else{
			ncparsRx1day <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(Rx1day, "Rx1day", GeneralParameters$data.type, indexDIR, nc.pars = ncparsRx1day)
	}

	# Rx5day: Monthly maximum consecutive 5-day precipitation
	Rx5day <- NULL
	if(is.Rx5day){
		Rx5day <- climdex.RX5day(cdtdata, nacount, winsize = 5, separe = separe)

		ncparsRx5day <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsRx5day <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
							nc.unit = "mm", nc.prec = "short",
							longname.yrs = "Annual maximum consecutive 5-day precipitation",
							longname.mon = "Monthly maximum consecutive 5-day precipitation")
		}else{
			ncparsRx5day <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(Rx5day, "Rx5day", GeneralParameters$data.type, indexDIR, nc.pars = ncparsRx5day)
	}

	# SDII: Simple pricipitation intensity index
	SDII <- NULL
	if(is.SDII){
		SDII <- climdex.SDII(cdtdata, nacount, separe = separe)

		ncparsSDII <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsSDII <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "mm", nc.prec = "short",
								longname.yrs = "Simple precipitation intensity index")
		}else{
			ncparsSDII <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(SDII, "SDII", GeneralParameters$data.type, indexDIR, nc.pars = ncparsSDII)
	}

	# Rnnmm: Annual count of days when PRCP≥ nnmm
	R10mm <- NULL
	if(is.R10mm){
		R10mm <- climdex.RRsupmm(cdtdata, nacount, xup = 10, separe = separe)

		ncparsR10mm <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsR10mm <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "day", nc.prec = "short",
								longname.yrs = "Annual count of days when PRCP ≥ 10mm")
		}else{
			ncparsR10mm <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(R10mm, "R10mm", GeneralParameters$data.type, indexDIR, nc.pars = ncparsR10mm)
	}

	R20mm <- NULL
	if(is.R20mm){
		R20mm <- climdex.RRsupmm(cdtdata, nacount, xup = 20, separe = separe)

		ncparsR20mm <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsR20mm <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "day", nc.prec = "short",
								longname.yrs = "Annual count of days when PRCP ≥ 20mm")
		}else{
			ncparsR20mm <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(R20mm, "R20mm", GeneralParameters$data.type, indexDIR, nc.pars = ncparsR20mm)
	}

	Rnnmm <- NULL
	if(is.Rnnmm){
		xup <- GeneralParameters$Indices$thres.Rnnmm
		if(!is.na(xup)) Rnnmm <- climdex.RRsupmm(cdtdata, nacount, xup = xup, separe = separe)
		else InsertMessagesTxt(main.txt.out, 'User defined threshold is missing, Rnnmm will not be calculated', format = TRUE)

		ncparsRnnmm <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsRnnmm <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "day", nc.prec = "short",
								longname.yrs = paste("Annual count of days when PRCP ≥", xup, "mm"))
		}else{
			ncparsRnnmm <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(Rnnmm, "Rnnmm", GeneralParameters$data.type, indexDIR, nc.pars = ncparsRnnmm)
	}

	# CDD: Maximum length of dry spell, maximum number of consecutive days with RR < 1mm
	CDD <- NULL
	if(is.CDD){
		CDD <- climdex.consecDaySpell(cdtdata, nacount, index = 'CDD', separe = separe)

		ncparsCDD <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsCDD <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "count", nc.prec = "short",
								longname.yrs = "Maximum length of dry spell, maximum number of consecutive days with RR < 1mm")
		}else{
			ncparsCDD <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(CDD, "CDD", GeneralParameters$data.type, indexDIR, nc.pars = ncparsCDD)
	}

	# CWD: Maximum length of wet spell, maximum number of consecutive days with RR ≥ 1mm
	CWD <- NULL
	if(is.CWD){
		CWD <- climdex.consecDaySpell(cdtdata, nacount, index = 'CWD', separe = separe)

		ncparsCWD <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsCWD <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "count", nc.prec = "short",
								longname.yrs = "Maximum length of wet spell, maximum number of consecutive days with RR ≥ 1mm")
		}else{
			ncparsCWD <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(CWD, "CWD", GeneralParameters$data.type, indexDIR, nc.pars = ncparsCWD)
	}

	if(is.R95pTOT | is.R99pTOT){
		startyear <- GeneralParameters$baseYear$start.year
		endyear <- GeneralParameters$baseYear$end.year
		if(is.na(startyear) | is.na(endyear)){
			InsertMessagesTxt(main.txt.out, 'Base period is missing, all years will be used to compute climatology', format = TRUE)
			baseYear <- as.numeric(substr(c(cdtdata$dates[1], cdtdata$dates[length(cdtdata$dates)]), 1, 4))
			startyear <- baseYear[1]
			endyear <- baseYear[2]
		}
		RpTOT <- climdex.RRqqTOT(cdtdata, nacount, startyear, endyear, separe = separe)
	}

	# R95pTOT: Annual total PRCP when RR > 95p
	R95pTOT <- NULL
	if(is.R95pTOT){
		R95pTOT <- list(yIdx = list(date = RpTOT$date, data = RpTOT$R95pTOT), tIdx = RpTOT$tR95pTOT)

		ncparsR95pTOT <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsR95pTOT <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "mm", nc.prec = "short",
								longname.yrs = "Annual total PRCP when RR > 95p")
		}else{
			ncparsR95pTOT <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(R95pTOT, "R95pTOT", GeneralParameters$data.type, indexDIR, nc.pars = ncparsR95pTOT)
	}

	# R99pTOT: Annual total PRCP when RR > 99p
	R99pTOT <- NULL
	if(is.R99pTOT){
		R99pTOT <- list(yIdx = list(date = RpTOT$date, data = RpTOT$R99pTOT), tIdx = RpTOT$tR99pTOT) 

		ncparsR99pTOT <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsR99pTOT <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "mm", nc.prec = "short",
								longname.yrs = "Annual total PRCP when RR > 99p")
		}else{
			ncparsR99pTOT <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(R99pTOT, "R99pTOT", GeneralParameters$data.type, indexDIR, nc.pars = ncparsR99pTOT)
	}

	# PRCPTOT: Annual total precipitation in wet days
	PRCPTOT <- NULL
	if(is.PRCPTOT){
		PRCPTOT <- climdex.RRthresTOT(cdtdata, nacount, thres = 1, separe = separe)

		ncparsPRCPTOT <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			ncparsPRCPTOT <- list(trend.vars = trend.vars, xy.dim = xy.dim, nx = nx, ny = ny,
								nc.unit = "mm", nc.prec = "short",
								longname.yrs = "Annual total precipitation in wet days")
		}else{
			ncparsPRCPTOT <- list(head = cdtdata[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(PRCPTOT, "PRCPTOT", GeneralParameters$data.type, indexDIR, nc.pars = ncparsPRCPTOT)
	}

	return(0)
}
