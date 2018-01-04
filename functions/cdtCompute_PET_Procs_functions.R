
computePETProcs <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, paste("Compute", GeneralParameters$Tstep, "PET ......"))

	if(GeneralParameters$data.type == "cdtstation"){
		tmin <- getStnOpenData(GeneralParameters$cdtstation$tmin)
		if(is.null(tmin)) return(NULL)
		tmin <- getCDTdataAndDisplayMsg(tmin, GeneralParameters$Tstep)
		if(is.null(tmin)) return(NULL)

		tmax <- getStnOpenData(GeneralParameters$cdtstation$tmax)
		if(is.null(tmax)) return(NULL)
		tmax <- getCDTdataAndDisplayMsg(tmax, GeneralParameters$Tstep)
		if(is.null(tmax)) return(NULL)

		if(!any(tmin$id%in%tmax$id)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax stations do not match", format = TRUE)
			return(NULL)
		}
		if(!any(tmin$dates%in%tmax$dates)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax dates do not match", format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			prec <- getStnOpenData(GeneralParameters$cdtstation$prec)
			if(is.null(prec)) return(NULL)
			prec <- getCDTdataAndDisplayMsg(prec, GeneralParameters$Tstep)
			if(is.null(prec)) return(NULL)

			if(length(Reduce(intersect, list(tmin$id, tmax$id, prec$id))) == 0){
				InsertMessagesTxt(main.txt.out, "Tmin, Tmax & Precip stations do not match", format = TRUE)
				return(NULL)
			}
			if(length(Reduce(intersect, list(tmin$dates, tmax$dates, prec$dates))) == 0){
				InsertMessagesTxt(main.txt.out, "Tmin, Tmax & Precip dates do not match", format = TRUE)
				return(NULL)
			}
		}

		##################
		inx <- match(tmin$dates, tmax$dates)
		inx <- inx[!is.na(inx)]
		tmax$dates <- tmax$dates[inx]

		if(GeneralParameters$method == "HAR"){
			daty <- tmax$dates
			tmax$data <- tmax$data[inx, , drop = FALSE]
			tmin$data <- tmin$data[tmin$dates%in%tmax$dates, , drop = FALSE]
		}else{
			inp <- match(tmin$dates, prec$dates)
			inp <- inp[!is.na(inp)]
			prec$dates <- prec$dates[inp]

			ixp <- match(tmax$dates, prec$dates)
			ixp <- ixp[!is.na(ixp)]

			daty <- prec$dates[ixp]
			prec$data <- prec$data[ixp, , drop = FALSE]
			tmax$data <- tmax$data[tmax$dates%in%prec$dates, , drop = FALSE]
			tmin$data <- tmin$data[tmin$dates%in%prec$dates, , drop = FALSE]
		}

		##################
		jnx <- match(tmin$id, tmax$id)
		jnx <- jnx[!is.na(jnx)]
		tmax$id <- tmax$id[jnx]

		if(GeneralParameters$method == "HAR"){
			stn.id <- tmax$id
			stn.lon <- tmax$lon[jnx]
			stn.lat <- tmax$lat[jnx]
			tmax$data <- tmax$data[, jnx, drop = FALSE]
			tmin$data <- tmin$data[, tmin$id%in%tmax$id, drop = FALSE]
		}else{
			jnp <- match(tmin$id, prec$id)
			jnp <- jnp[!is.na(jnp)]
			prec$id <- prec$id[jnp]

			jxp <- match(tmax$id, prec$id)
			jxp <- jxp[!is.na(jxp)]

			stn.id <- prec$id[jxp]
			stn.lon <- prec$lon[jxp]
			stn.lat <- prec$lat[jxp]
			prec$data <- prec$data[, jxp, drop = FALSE]
			tmax$data <- tmax$data[, tmax$id%in%prec$id, drop = FALSE]
			tmin$data <- tmin$data[, tmin$id%in%prec$id, drop = FALSE]
		}

		##################

		Ra <- Extraterrestrial.Radiation(stn.lat, GeneralParameters$Tstep)

		year <- as.numeric(substr(daty, 1, 4))
		mon <- as.numeric(substr(daty, 5, 6))

		if(GeneralParameters$Tstep == "daily"){
			day <- as.numeric(strftime(as.Date(daty, "%Y%m%d"), format = "%j"))
			leap <- is.leapyears(year) & day >= 60
			day[leap] <- day[leap]-1
			Ra <- Ra[day, , drop = FALSE]
			multip <- 1
		}
		if(GeneralParameters$Tstep == "pentad"){
			pen <- as.numeric(substr(daty, 7, 7))
			vpen <- cbind(expand.grid(1:6, 1:12), 1:72)
			vpen <- vpen[match(paste0(pen, ".", mon), paste0(vpen[, 1], ".", vpen[, 2])), 3]
			Ra <- Ra[vpen, , drop = FALSE]
			multip <- ifelse(pen != 6, 5, as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))-25)
		}
		if(GeneralParameters$Tstep == "dekadal"){
			dek <- as.numeric(substr(daty, 7, 7))
			vdek <- cbind(expand.grid(1:3, 1:12), 1:36)
			vdek <- vdek[match(paste0(dek, ".", mon), paste0(vdek[, 1], ".", vdek[, 2])), 3]
			Ra <- Ra[vdek, , drop = FALSE]
			multip <- ifelse(dek != 3, 10, as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))-20)
		}
		if(GeneralParameters$Tstep == "monthly"){
			Ra <- Ra[mon, , drop = FALSE]
			multip <- as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))
		}

		##################
		prec <- if(GeneralParameters$method == "MHAR") prec else NULL
		etp <- Ref.ET.Hargreaves(tmax$data, tmin$data, Ra, GeneralParameters$Tstep, prec$data)
		etp <- multip * etp
		etp <- round(etp, 1)

		##################
		etp[is.na(etp)] <- -99
		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', paste0(toupper(GeneralParameters$Tstep), '/LAT'))
		infohead <- cbind(chead, xhead)

		etp <- rbind(infohead, cbind(daty, etp))
		writeFiles(etp, GeneralParameters$output)
		rm(tmax, tmin, prec, etp, Ra)
	}

	#######################################################
	if(GeneralParameters$data.type == "cdtnetcdf"){
		tnDataInfo <- getRFESampleData(GeneralParameters$cdtnetcdf$tmin$sample)
		if(is.null(tnDataInfo)){
			InsertMessagesTxt(main.txt.out, "No Tmin data sample found", format = TRUE)
			return(NULL)
		}
		txDataInfo <- getRFESampleData(GeneralParameters$cdtnetcdf$tmax$sample)
		if(is.null(txDataInfo)){
			InsertMessagesTxt(main.txt.out, "No Tmax data sample found", format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			rrDataInfo <- getRFESampleData(GeneralParameters$cdtnetcdf$prec$sample)
			if(is.null(rrDataInfo)){
				InsertMessagesTxt(main.txt.out, "No Precip data sample found", format = TRUE)
				return(NULL)
			}
		}

		##################
		SP1 <- defSpatialPixels(tnDataInfo[c('lon', 'lat')])
		SP2 <- defSpatialPixels(txDataInfo[c('lon', 'lat')])
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax have different resolution or bbox", format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$method == "MHAR"){
			SP3 <- defSpatialPixels(rrDataInfo[c('lon', 'lat')])
			if(is.diffSpatialPixelsObj(SP1, SP3, tol = 1e-04)){
				InsertMessagesTxt(main.txt.out, "Precip, Tmin & Tmax have different resolution or bbox", format = TRUE)
				return(NULL)
			}
		}
		rm(SP1, SP2, SP3)

		##################
		tstep <- GeneralParameters$Tstep
		months <- 1:12
		start.date <- as.Date("1900-1-1")
		end.date <- as.Date("2050-12-31")

		tmin.DIR <- GeneralParameters$cdtnetcdf$tmin$dir
		tmin.Format <- GeneralParameters$cdtnetcdf$tmin$format
		tmin.errmsg <- "Tmin data not found"
		tminInfo <- ncFilesInfo(tstep, start.date, end.date, months, tmin.DIR, tmin.Format, tmin.errmsg)
		if(is.null(tminInfo)) return(NULL)
		tminInfo$ncinfo <- list(xo = tnDataInfo$rfeILon, yo = tnDataInfo$rfeILat, varid = tnDataInfo$rfeVarid)

		tminInfo$dates <- tminInfo$dates[tminInfo$exist]
		tminInfo$nc.files <- tminInfo$nc.files[tminInfo$exist]
		tminInfo$exist <- tminInfo$exist[tminInfo$exist]

		tmax.DIR <- GeneralParameters$cdtnetcdf$tmax$dir
		tmax.Format <- GeneralParameters$cdtnetcdf$tmax$format
		tmax.errmsg <- "Tmax data not found"
		tmaxInfo <- ncFilesInfo(tstep, start.date, end.date, months, tmax.DIR, tmax.Format, tmax.errmsg)
		if(is.null(tmaxInfo)) return(NULL)
		tmaxInfo$ncinfo <- list(xo = txDataInfo$rfeILon, yo = txDataInfo$rfeILat, varid = txDataInfo$rfeVarid)

		tmaxInfo$dates <- tmaxInfo$dates[tmaxInfo$exist]
		tmaxInfo$nc.files <- tmaxInfo$nc.files[tmaxInfo$exist]
		tmaxInfo$exist <- tmaxInfo$exist[tmaxInfo$exist]

		if(GeneralParameters$method == "MHAR"){
			prec.DIR <- GeneralParameters$cdtnetcdf$prec$dir
			prec.Format <- GeneralParameters$cdtnetcdf$prec$format
			prec.errmsg <- "Precip data not found"
			precInfo <- ncFilesInfo(tstep, start.date, end.date, months, prec.DIR, prec.Format, prec.errmsg)
			if(is.null(tmaxInfo)) return(NULL)
			precInfo$ncinfo <- list(xo = rrDataInfo$rfeILon, yo = rrDataInfo$rfeILat, varid = rrDataInfo$rfeVarid)

			precInfo$dates <- precInfo$dates[precInfo$exist]
			precInfo$nc.files <- precInfo$nc.files[precInfo$exist]
			precInfo$exist <- precInfo$exist[precInfo$exist]
		}

		##################

		if(!any(tminInfo$dates%in%tmaxInfo$dates)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax dates do not match", format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			if(length(Reduce(intersect, list(tminInfo$dates, tmaxInfo$dates, precInfo$dates))) == 0){
				InsertMessagesTxt(main.txt.out, "Tmin, Tmax & Precip dates do not match", format = TRUE)
				return(NULL)
			}
		}

		##################
		inx <- match(tminInfo$dates, tmaxInfo$dates)
		inx <- inx[!is.na(inx)]
		tmaxInfo$dates <- tmaxInfo$dates[inx]

		if(GeneralParameters$method == "HAR"){
			## tmaxInfo$dates <- tmaxInfo$dates[inx]
			tmaxInfo$nc.files <- tmaxInfo$nc.files[inx]
			tmaxInfo$exist <- tmaxInfo$exist[inx]

			tndaty <- tminInfo$dates%in%tmaxInfo$dates
			tminInfo$dates <- tminInfo$dates[tndaty]
			tminInfo$nc.files <- tminInfo$nc.files[tndaty]
			tminInfo$exist <- tminInfo$exist[tndaty]
		}else{
			inp <- match(tminInfo$dates, precInfo$dates)
			inp <- inp[!is.na(inp)]
			precInfo$dates <- precInfo$dates[inp]

			ixp <- match(tmaxInfo$dates, precInfo$dates)
			ixp <- ixp[!is.na(ixp)]

			precInfo$dates <- precInfo$dates[ixp]
			precInfo$nc.files <- precInfo$nc.files[ixp]
			precInfo$exist <- precInfo$exist[ixp]

			txdaty <- tmaxInfo$dates%in%precInfo$dates
			tmaxInfo$dates <- tmaxInfo$dates[txdaty]
			tmaxInfo$nc.files <- tmaxInfo$nc.files[txdaty]
			tmaxInfo$exist <- tmaxInfo$exist[txdaty]

			tndaty <- tminInfo$dates%in%precInfo$dates
			tminInfo$dates <- tminInfo$dates[tndaty]
			tminInfo$nc.files <- tminInfo$nc.files[tndaty]
			tminInfo$exist <- tminInfo$exist[tndaty]
		}

		##################
		nc <- nc_open(tminInfo$nc.files[1])
		nc.lon <- nc$dim[[tminInfo$ncinfo$xo]]$vals
		nc.lat <- nc$dim[[tminInfo$ncinfo$yo]]$vals
		nc_close(nc)

		xo.tn <- order(nc.lon)
		nc.lon <- nc.lon[xo.tn]
		yo.tn <- order(nc.lat)
		nc.lat <- nc.lat[yo.tn]
		len.lon <- length(nc.lon)
		len.lat <- length(nc.lat)

		nc <- nc_open(tmaxInfo$nc.files[1])
		nc.lon1 <- nc$dim[[tmaxInfo$ncinfo$xo]]$vals
		nc.lat1 <- nc$dim[[tmaxInfo$ncinfo$yo]]$vals
		nc_close(nc)
		xo.tx <- order(nc.lon1)
		yo.tx <- order(nc.lat1)

		if(GeneralParameters$method == "MHAR"){
			nc <- nc_open(precInfo$nc.files[1])
			nc.lon2 <- nc$dim[[precInfo$ncinfo$xo]]$vals
			nc.lat2 <- nc$dim[[precInfo$ncinfo$yo]]$vals
			nc_close(nc)
			xo.rr <- order(nc.lon2)
			yo.rr <- order(nc.lat2)
		}

		##################

		Ra <- Extraterrestrial.Radiation(nc.lat, GeneralParameters$Tstep)

		daty <- tminInfo$dates
		year <- as.numeric(substr(daty, 1, 4))
		mon <- as.numeric(substr(daty, 5, 6))

		if(GeneralParameters$Tstep == "daily"){
			day <- as.numeric(strftime(as.Date(daty, "%Y%m%d"), format = "%j"))
			leap <- is.leapyears(year) & day >= 60
			day[leap] <- day[leap]-1
			idxRa <- day
			multip <- rep(1, length(day))
		}
		if(GeneralParameters$Tstep == "pentad"){
			pen <- as.numeric(substr(daty, 7, 7))
			vpen <- cbind(expand.grid(1:6, 1:12), 1:72)
			vpen <- vpen[match(paste0(pen, ".", mon), paste0(vpen[, 1], ".", vpen[, 2])), 3]
			idxRa <- vpen
			multip <- ifelse(pen != 6, 5, as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))-25)
		}
		if(GeneralParameters$Tstep == "dekadal"){
			dek <- as.numeric(substr(daty, 7, 7))
			vdek <- cbind(expand.grid(1:3, 1:12), 1:36)
			vdek <- vdek[match(paste0(dek, ".", mon), paste0(vdek[, 1], ".", vdek[, 2])), 3]
			idxRa <- vdek
			multip <- ifelse(dek != 3, 10, as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))-20)
		}
		if(GeneralParameters$Tstep == "monthly"){
			idxRa <- mon
			multip <- as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))
		}

		##################
		dx <- ncdim_def("Lon", "degreeE", nc.lon)
		dy <- ncdim_def("Lat", "degreeN", nc.lat)
		xy.dim <- list(dx, dy)

		if(GeneralParameters$method == "MHAR")
			longname <- paste("Modified Hargreaves", GeneralParameters$Tstep, "evapotranspiration")
		else longname <- paste("Hargreaves", GeneralParameters$Tstep, "evapotranspiration")
		grdNC <- ncvar_def("pet", 'mm', xy.dim, -99, longname = longname, prec = 'float', compression = 6)

		##################
		ncdir <- paste0("PET_", GeneralParameters$Tstep)
		outNCDIR <- file.path(GeneralParameters$output, ncdir)
		dir.create(outNCDIR, showWarnings = FALSE, recursive = TRUE)

		##################

		toExports <- c("Ref.ET.Hargreaves")

		is.parallel <- doparallel(length(tminInfo$nc.files) >= 180)
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(tminInfo$nc.files), .export = toExports, .packages = "ncdf4") %parLoop% {
			nc <- try(nc_open(tminInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			tmin <- ncvar_get(nc, varid = tminInfo$ncinfo$varid)
			nc_close(nc)
			tmin <- tmin[xo.tn, yo.tn]
			if(tminInfo$ncinfo$yo < tminInfo$ncinfo$xo)
				tmin <- matrix(c(tmin), nrow = len.lon, ncol = len.lat, byrow = TRUE)

			##################
			nc <- try(nc_open(tmaxInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			tmax <- ncvar_get(nc, varid = tmaxInfo$ncinfo$varid)
			nc_close(nc)
			tmax <- tmax[xo.tx, yo.tx]
			if(tmaxInfo$ncinfo$yo < tmaxInfo$ncinfo$xo)
				tmax <- matrix(c(tmax), nrow = len.lon, ncol = len.lat, byrow = TRUE)

			##################
			if(GeneralParameters$method == "MHAR"){
				nc <- try(nc_open(precInfo$nc.files[jj]), silent = TRUE)
				if(inherits(nc, "try-error")) return(NULL)
				prec <- ncvar_get(nc, varid = precInfo$ncinfo$varid)
				nc_close(nc)
				prec <- prec[xo.rr, yo.rr]
				if(precInfo$ncinfo$yo < precInfo$ncinfo$xo)
					prec <- matrix(c(prec), nrow = len.lon, ncol = len.lat, byrow = TRUE)
			}else prec <- NULL

			##################

			Ra.mat <- matrix(Ra[idxRa[jj], ], nrow = len.lon, ncol = len.lat, byrow = TRUE)
			etp <- Ref.ET.Hargreaves(tmax, tmin, Ra.mat, GeneralParameters$Tstep, prec)
			etp <- multip[jj] * etp
			etp[is.na(etp)] <- -99

			##################
			outfile <- file.path(outNCDIR, paste0("pet_", tminInfo$dates[jj], ".nc"))
			nc <- nc_create(outfile, grdNC)
			ncvar_put(nc, grdNC, etp)
			nc_close(nc)

			rm(tmin, tmax, etp, Ra.mat); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		rm(tnDataInfo, txDataInfo, rrDataInfo, tminInfo, tmaxInfo, precInfo, Ra)
	}

	#######################################################
	if(GeneralParameters$data.type == "cdtdataset"){
		tmin <- try(readRDS(GeneralParameters$cdtdataset$tmin), silent = TRUE)
		if(inherits(tmin, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$tmin), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$Tstep != tmin$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("Tmin dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		tmax <- try(readRDS(GeneralParameters$cdtdataset$tmax), silent = TRUE)
		if(inherits(tmax, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$tmax), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$Tstep != tmax$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("Tmax dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			prec <- try(readRDS(GeneralParameters$cdtdataset$prec), silent = TRUE)
			if(inherits(prec, "try-error")){
				InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$prec), format = TRUE)
				return(NULL)
			}
			if(GeneralParameters$Tstep != prec$TimeStep){
				InsertMessagesTxt(main.txt.out, paste("Precip dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
				return(NULL)
			}
		}

		##################
		SP1 <- defSpatialPixels(list(lon = tmin$coords$mat$x, lat = tmin$coords$mat$y))
		SP2 <- defSpatialPixels(list(lon = tmax$coords$mat$x, lat = tmax$coords$mat$y))
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax have different resolution or bbox", format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			SP3 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP3, tol = 1e-04)){
				InsertMessagesTxt(main.txt.out, "Precip, Tmin & Tmax have different resolution or bbox", format = TRUE)
				return(NULL)
			}
		}
		rm(SP1, SP2, SP3)

		##################
		if(tmin$chunksize != tmax$chunksize){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax have different chunk size", format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			if(tmin$chunksize != prec$chunksize){
				InsertMessagesTxt(main.txt.out, "Precip, Tmin & Tmax have different chunk size", format = TRUE)
				return(NULL)
			}
		}

		##################
		if(!any(tmin$dateInfo$date%in%tmax$dateInfo$date)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax dates do not match", format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == "MHAR"){
			if(length(Reduce(intersect, list(tmin$dateInfo$date, tmax$dateInfo$date, prec$dateInfo$date))) == 0){
				InsertMessagesTxt(main.txt.out, "Tmin, Tmax & Precip dates do not match", format = TRUE)
				return(NULL)
			}
		}

		##################
		inx <- match(tmin$dateInfo$date, tmax$dateInfo$date)
		inx <- inx[!is.na(inx)]
		tmax$dateInfo$date <- tmax$dateInfo$date[inx]

		if(GeneralParameters$method == "HAR"){
			## tmax$dateInfo$date <- tmax$dateInfo$date[inx]
			tmax$dateInfo$index <- tmax$dateInfo$index[inx]

			tndaty <- tmin$dateInfo$date%in%tmax$dateInfo$date
			tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
			tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]
		}else{
			inp <- match(tmin$dateInfo$date, prec$dateInfo$date)
			inp <- inp[!is.na(inp)]
			prec$dateInfo$date <- prec$dateInfo$date[inp]

			ixp <- match(tmax$dateInfo$date, prec$dateInfo$date)
			ixp <- ixp[!is.na(ixp)]

			prec$dateInfo$date <- prec$dateInfo$date[ixp]
			prec$dateInfo$index <- prec$dateInfo$index[ixp]

			txdaty <- tmax$dateInfo$date%in%prec$dateInfo$date
			tmax$dateInfo$date <- tmax$dateInfo$date[txdaty]
			tmax$dateInfo$index <- tmax$dateInfo$index[txdaty]

			tndaty <- tmin$dateInfo$date%in%prec$dateInfo$date
			tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
			tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]
		}

		##################

		daty <- tmin$dateInfo$date
		year <- as.numeric(substr(daty, 1, 4))
		mon <- as.numeric(substr(daty, 5, 6))

		if(GeneralParameters$Tstep == "daily"){
			day <- as.numeric(strftime(as.Date(daty, "%Y%m%d"), format = "%j"))
			leap <- is.leapyears(year) & day >= 60
			day[leap] <- day[leap]-1
			idxRa <- day
			multip <- 1
		}
		if(GeneralParameters$Tstep == "pentad"){
			pen <- as.numeric(substr(daty, 7, 7))
			vpen <- cbind(expand.grid(1:6, 1:12), 1:72)
			vpen <- vpen[match(paste0(pen, ".", mon), paste0(vpen[, 1], ".", vpen[, 2])), 3]
			idxRa <- vpen
			multip <- ifelse(pen != 6, 5, as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))-25)
		}
		if(GeneralParameters$Tstep == "dekadal"){
			dek <- as.numeric(substr(daty, 7, 7))
			vdek <- cbind(expand.grid(1:3, 1:12), 1:36)
			vdek <- vdek[match(paste0(dek, ".", mon), paste0(vdek[, 1], ".", vdek[, 2])), 3]
			idxRa <- vdek
			multip <- ifelse(dek != 3, 10, as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))-20)
		}
		if(GeneralParameters$Tstep == "monthly"){
			idxRa <- mon
			multip <- as.numeric(format(as.Date(paste(year, (mon%%12)+1, 1, sep = '-'))-1, "%d"))
		}

		##################

		if(GeneralParameters$method == "MHAR")
			longname <- paste("Modified Hargreaves", GeneralParameters$Tstep, "evapotranspiration")
		else longname <- paste("Hargreaves", GeneralParameters$Tstep, "evapotranspiration")

		index.out <- tmin
		index.out$varInfo$name <- "pet"
		index.out$varInfo$units <- "mm"
		index.out$varInfo$longname <- longname

		dataset <- paste0("PET_", GeneralParameters$Tstep)
		outDIR <- file.path(GeneralParameters$output, dataset)
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(outDIR, paste0(dataset, '.rds'))

		##################

		chunkfile <- sort(unique(tmin$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/tmin$chunkfac))

		do.parChunk <- if(tmin$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence",
						"Extraterrestrial.Radiation", "Ref.ET.Hargreaves", "doparallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(j = seq_along(chunkcalc), .export = toExports, .packages = "doParallel") %parLoop% {
			tn <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$tmin, do.par = do.parChunk)
			tn <- tn[tmin$dateInfo$index, , drop = FALSE]

			tx <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$tmax, do.par = do.parChunk)
			tx <- tx[tmax$dateInfo$index, , drop = FALSE]

			if(GeneralParameters$method == "MHAR"){
				rr <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$prec, do.par = do.parChunk)
				rr <- rr[prec$dateInfo$index, , drop = FALSE]
			}else rr <- NULL

			lat <- tmin$coords$df$y[tmin$colInfo$index%in%chunkcalc[[j]]]
			Ra <- Extraterrestrial.Radiation(lat, GeneralParameters$Tstep)
			Ra <- Ra[idxRa, , drop = FALSE]

			etp <- Ref.ET.Hargreaves(tx, tn, Ra, GeneralParameters$Tstep, rr)
			etp <- multip * etp

			writeCdtDatasetChunk.sequence(etp, chunkcalc[[j]], index.out, datadir, do.par = do.parChunk)

			rm(tn, tx, rr, Ra, etp); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		##################

		index.out$dateInfo$date <- daty
		index.out$dateInfo$index <- seq(length(daty))

		con <- gzfile(datafileIdx, compression = 6)
		open(con, "wb")
		saveRDS(index.out, con)
		close(con)

		rm(tmin, tmax, prec, index.out)
	}

	return(0)
}
