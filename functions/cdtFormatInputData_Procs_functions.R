
formatCDTDataMultiple.Files <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, 'Convert data to CDT data format ...')
	tcl("update")

	istart.yrs <- GeneralParameters$Date.Range$start.year
	istart.mon <- GeneralParameters$Date.Range$start.mon
	istart.day <- GeneralParameters$Date.Range$start.day
	iend.yrs <- GeneralParameters$Date.Range$end.year
	iend.mon <- GeneralParameters$Date.Range$end.mon
	iend.day <- GeneralParameters$Date.Range$end.day
	min.perc <- GeneralParameters$min.perc/100

	period <- GeneralParameters$period
	filefrmt <- GeneralParameters$Multiple.File$file.format
	datefrmt <- GeneralParameters$Multiple.File$date.format
	include.elev <- GeneralParameters$Multiple.File$include.elev

	STN.dir <- GeneralParameters$IO.files$STN.dir
	STN.sample <- GeneralParameters$IO.files$STN.sample.file
	STN.info <- GeneralParameters$IO.files$STN.coords.file
	File2Save <- GeneralParameters$IO.files$File2Save

	xinfo <- getStnOpenData(STN.info)
	if(is.null(xinfo)) return(NULL)
	xinfo <- apply(xinfo, 2, str_trim)
	xinfo[xinfo == ""] <- NA
	stn.id <- xinfo[, 1]
	stn.lon <- as.numeric(xinfo[, 3])
	stn.lat <- as.numeric(xinfo[, 4])
	stn.elv <- as.numeric(xinfo[, 5])
	xinfo <- cbind(stn.id, stn.lon, stn.lat, stn.elv)

	miss.stn <- list()
	miss.stn$dup.STN.ID <- xinfo[duplicated(xinfo[, 1]) | duplicated(xinfo[, 1], fromLast = TRUE), , drop = FALSE]
	xinfo <- xinfo[!duplicated(xinfo[, 1]), ]
	miss.stn$dup.coords <- xinfo[duplicated(xinfo[, 2:3]) | duplicated(xinfo[, 2:3], fromLast = TRUE), , drop = FALSE]
	miss.stn$miss.coords <- xinfo[is.na(xinfo[, 2]) | is.na(xinfo[, 3]), , drop = FALSE]

	infoheadI <- xinfo[, 1:3]
	capition <- c('Stations', 'LON', paste(toupper(period), 'LAT', sep = '/'))
	if(include.elev){
		infoheadI <- xinfo
		capition <- c('Stations', 'LON', 'LAT', paste(toupper(period), 'ELV', sep = '/'))
	}

	STN.ext <- file_ext(STN.sample)
	if(STN.ext == "") STN.files <- list.files(STN.dir)
	else STN.files <- file_path_sans_ext(list_files_with_exts(STN.dir, STN.ext, full.names = FALSE))
	istn <- stn.id%in%STN.files
	if(!any(istn)){
		InsertMessagesTxt(main.txt.out, 'No stations found', format = TRUE)
		return(NULL)
	}
	miss.stn$no.coords <- STN.files[!STN.files%in%stn.id]

	infoheadI1 <- infoheadI[istn, ]
	stn.id <- stn.id[istn]
	miss.stn$no.data <- infoheadI[!istn, , drop = FALSE]

	donneInfo <- getStnOpenDataInfo(STN.sample)
	if(is.null(donneInfo)) return(NULL)
	donne <- lapply(stn.id, function(x){
		filext <- if(STN.ext == "") x else paste(x, '.', STN.ext, sep = '')
		filein <- file.path(STN.dir, filext)
		donne <- try(read.table(filein, header = donneInfo[[3]]$header, sep = donneInfo[[3]]$sepr, skip = donneInfo[[3]]$skip-1, 
								na.strings = donneInfo[[3]]$miss.val, quote = "\"'", strip.white = TRUE, stringsAsFactors = FALSE,
								colClasses = "character", comment.char = ""), silent = TRUE)
		if(inherits(donne, "try-error")) return(NULL)
		donne <- getCDTTSdataAndDisplayMsg(donne, period, filefrmt, datefrmt)
		return(donne)
	})

	donne.null <- sapply(donne, is.null)
	donne <- donne[!donne.null]
	miss.stn$not.read <- if(any(donne.null)) infoheadI1[donne.null, , drop = FALSE] else NULL
	infoheadI1 <- infoheadI1[!donne.null, ]

	if(period == 'daily'){
		istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
		iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
		odates <- format(seq(istart, iend, 'day'), '%Y%m%d')
	}else if(period == 'dekadal'){
		istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
		iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
		odates <- seq(istart, iend, 'day')
		odates <- paste(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%d')), sep = '')
	}else if(period == 'monthly'){
		istart <- as.Date(paste(istart.yrs, istart.mon, 1, sep = '-'))
		iend <- as.Date(paste(iend.yrs, iend.mon, 1, sep = '-'))
		odates <- format(seq(istart, iend, 'month'), '%Y%m')
	}

	if(filefrmt == "1"){
		donne <- lapply(donne, function(x){
			xvar <- x$var$var
			xdaty <- x$dates
			xvar[match(odates, xdaty)]
		})
		donne <- do.call("cbind", donne)
		per.var <- 1-(apply(apply(donne, 2, is.na), 2, sum)/length(odates)) >= min.perc
		donne <- donne[, per.var]
		miss.stn$less.data <- infoheadI1[!per.var, , drop = FALSE]
		infoheadI1 <- infoheadI1[per.var, ]
		donne <- rbind(t(rbind(capition, infoheadI1)), cbind(odates, donne))
		donne[is.na(donne)] <- donneInfo[[3]]$miss.val
		writeFiles(donne, File2Save)
	}else{
		donne <- lapply(donne, function(x){
			xrr <- x$var$rr
			xtx <- x$var$tx
			xtn <- x$var$tn
			xdaty <- x$dates
			ix <- match(odates, xdaty)
			xrr <- xrr[ix]
			xtx <- xtx[ix]
			xtn <- xtn[ix]
			list(xrr, xtx, xtn)
		})
		xrr <- sapply(donne, '[[', 1)
		xtx <- sapply(donne, '[[', 2)
		xtn <- sapply(donne, '[[', 3)

		per.rr <- 1-(apply(apply(xrr, 2, is.na), 2, sum)/length(odates)) >= min.perc
		per.tx <- 1-(apply(apply(xtx, 2, is.na), 2, sum)/length(odates)) >= min.perc
		per.tn <- 1-(apply(apply(xtn, 2, is.na), 2, sum)/length(odates)) >= min.perc

		xrr <- xrr[, per.rr]
		xtx <- xtx[, per.tx]
		xtn <- xtn[, per.tn]

		miss.stn$less.data.rr <- infoheadI1[!per.rr, , drop = FALSE]
		miss.stn$less.data.tx <- infoheadI1[!per.tx, , drop = FALSE]
		miss.stn$less.data.tn <- infoheadI1[!per.tn, , drop = FALSE]

		infoheadI1.rr <- infoheadI1[per.rr, ]
		infoheadI1.tx <- infoheadI1[per.tx, ]
		infoheadI1.tn <- infoheadI1[per.tn, ]

		xrr <- rbind(t(rbind(capition, infoheadI1.rr)), cbind(odates, xrr))
		xtx <- rbind(t(rbind(capition, infoheadI1.tx)), cbind(odates, xtx))
		xtn <- rbind(t(rbind(capition, infoheadI1.tn)), cbind(odates, xtn))

		xrr[is.na(xrr)] <- donneInfo[[3]]$miss.val
		xtx[is.na(xtx)] <- donneInfo[[3]]$miss.val
		xtn[is.na(xtn)] <- donneInfo[[3]]$miss.val

		File2Save.rr <- file.path(dirname(File2Save), paste('PRECIP_', basename(File2Save), sep = ''))
		File2Save.tx <- file.path(dirname(File2Save), paste('TMAX_', basename(File2Save), sep = ''))
		File2Save.tn <- file.path(dirname(File2Save), paste('TMIN_', basename(File2Save), sep = ''))

		writeFiles(xrr, File2Save.rr)
		writeFiles(xtx, File2Save.tx)
		writeFiles(xtn, File2Save.tn)
	}

	outlist <- list()
	if(nrow(miss.stn$dup.STN.ID) > 0){
		outlist <- c(outlist, list('Duplicated station ID', miss.stn$dup.STN.ID))
	}
	if(nrow(miss.stn$dup.coords) > 0){
		outlist <- c(outlist, list('Duplicated coordinates', miss.stn$dup.coords))
	}
	if(nrow(miss.stn$miss.coords) > 0){
		outlist <- c(outlist, list('Missing coordinates', miss.stn$miss.coords))
	}
	if(length(miss.stn$no.coords) > 0){
		outlist <- c(outlist, list('Stations without information in the coordinates file', miss.stn$no.coords))
	}
	if(nrow(miss.stn$no.data) > 0){
		outlist <- c(outlist, list('Stations without data but have coordinates', miss.stn$no.data))
	}
	if(!is.null(miss.stn$not.read)){
		outlist <- c(outlist, list('Unable to read files', miss.stn$not.read))
	}
	if(filefrmt == "1"){
		if(nrow(miss.stn$less.data) > 0){
			outlist <- c(outlist, list('Stations excluded not enough values', miss.stn$less.data))
		}
	}else{
		if(nrow(miss.stn$less.data.rr) > 0){
			outlist <- c(outlist, list('Precip: Stations excluded not enough values', miss.stn$less.data.rr))
		}
		if(nrow(miss.stn$less.data.tx) > 0){
			outlist <- c(outlist, list('Tmax: Stations excluded not enough values', miss.stn$less.data.tx))
		}
		if(nrow(miss.stn$less.data.tn) > 0){
			outlist <- c(outlist, list('Tmin: Stations excluded not enough values', miss.stn$less.data.tn))
		}
	}

	if(length(outlist) > 0){
		containertab <- displayConsOutputTabs(tknotes, outlist, title = 'Failed Stations')
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'ctxt'
		AllOpenTabData[[ntab+1]] <<- containertab
		tkselect(tknotes, ntab)
	}
	return(0)
}

###################################################################

formatCDTDataSingle.File <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, 'Convert data to CDT data format ...')
	tcl("update")

	istart.yrs <- GeneralParameters$Date.Range$start.year
	istart.mon <- GeneralParameters$Date.Range$start.mon
	istart.day <- GeneralParameters$Date.Range$start.day
	iend.yrs <- GeneralParameters$Date.Range$end.year
	iend.mon <- GeneralParameters$Date.Range$end.mon
	iend.day <- GeneralParameters$Date.Range$end.day
	min.perc <- GeneralParameters$min.perc/100

	period <- GeneralParameters$period
	include.elev <- GeneralParameters$Single.File$include.elev
	coords.included <- GeneralParameters$Single.File$coords.included

	col.id <- GeneralParameters$Single.File$col.stn.id
	col.lon <- GeneralParameters$Single.File$col.stn.lon
	col.lat <- GeneralParameters$Single.File$col.stn.lat
	col.elv <- GeneralParameters$Single.File$col.stn.elv
	col.yr <- GeneralParameters$Single.File$col.year
	col.mo <- GeneralParameters$Single.File$col.month
	col.dy <- GeneralParameters$Single.File$col.day.dek
	col.dat <- GeneralParameters$Single.File$col.start.data
	nb.column <- GeneralParameters$Single.File$nb.column

	STN.file <- GeneralParameters$IO.files$STN.single.file
	File2Save <- GeneralParameters$IO.files$File2Save

	donne <- getStnOpenData(STN.file)
	if(is.null(donne)) return(NULL)
	# donneInfo <- getStnOpenDataInfo(STN.file)
	# if(is.null(donneInfo)) return(NULL)
	donne <- apply(donne, 2, str_trim)
	donne[donne == ""] <- NA
	donne <- donne[apply(!is.na(donne), 1, any), ]

	if(period == 'daily'){
		if(nb.column == 1) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo]) | is.na(donne[, col.dy])
		if(nb.column == 31) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo])
	}
	if(period == 'dekadal'){
		if(nb.column == 1) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo]) | is.na(donne[, col.dy])
		if(nb.column == 3) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo])
		if(nb.column == 36) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr])
	}
	if(period == 'monthly'){
		if(nb.column == 1) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo])
		if(nb.column == 12) ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr])
	}

	miss.stn <- list()
	miss.stn$vague.info <- donne[ina, , drop = FALSE]
	donne <- donne[!ina, ]

	if(coords.included){
		stn.id <- donne[, col.id]
		stn.lon <- as.numeric(donne[, col.lon])
		stn.lat <- as.numeric(donne[, col.lat])
		stn.elv <- if(include.elev) as.numeric(donne[, col.elv]) else rep(NA, length(stn.lon))
		xinfo <- cbind(stn.id, stn.lon, stn.lat, stn.elv)
		xinfo <- xinfo[!duplicated(xinfo[, 1:3]), , drop = FALSE]

		dupstn <- duplicated(xinfo[, 1]) | duplicated(xinfo[, 1], fromLast = TRUE)
		if(any(dupstn)){
			stn1 <- lapply(unique(xinfo[dupstn, 1]), function(x){
				xx <- xinfo[xinfo[, 1] == x, , drop = FALSE]
				if(any(is.na(xx[, 2:3]))) xx <- xx[!apply(is.na(xx[, 2:3]), 1, any), , drop = FALSE]
				xx
			})
			xinfo <- rbind(xinfo[!dupstn, , drop = FALSE], do.call(rbind, stn1))
		}
	}else{
		STN.info <- GeneralParameters$IO.files$STN.coords.file
		xinfo <- getStnOpenData(STN.info)
		if(is.null(xinfo)) return(NULL)
		xinfo <- apply(xinfo, 2, str_trim)
		if(is.null(dim(xinfo))) xinfo <- matrix(xinfo, nrow = 1)
		xinfo[xinfo == ""] <- NA
		stn.id <- xinfo[, 1]
		stn.lon <- as.numeric(xinfo[, 3])
		stn.lat <- as.numeric(xinfo[, 4])
		stn.elv <- as.numeric(xinfo[, 5])
		xinfo <- cbind(stn.id, stn.lon, stn.lat, stn.elv)
	}

	miss.stn$dup.STN.ID <- xinfo[duplicated(xinfo[, 1]) | duplicated(xinfo[, 1], fromLast = TRUE), , drop = FALSE]
	xinfo <- xinfo[!duplicated(xinfo[, 1]), , drop = FALSE]
	miss.stn$dup.coords <- xinfo[duplicated(xinfo[, 2:3, drop = FALSE]) | duplicated(xinfo[, 2:3, drop = FALSE], fromLast = TRUE), , drop = FALSE]
	miss.stn$miss.coords <- xinfo[is.na(xinfo[, 2]) | is.na(xinfo[, 3]), , drop = FALSE]

	infoheadI <- xinfo[, 1:3, drop = FALSE]
	capition <- c('Stations', 'LON', paste(toupper(period), 'LAT', sep = '/'))
	if(include.elev){
		infoheadI <- xinfo
		capition <- c('Stations', 'LON', 'LAT', paste(toupper(period), 'ELV', sep = '/'))
	}

	if(period == 'daily'){
		istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
		iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
		odates <- format(seq(istart, iend, 'day'), '%Y%m%d')
		if(nb.column == 1){
			xdonne <- donne[, c(col.id, col.yr, col.mo, col.dy)]
			donne <- donne[, col.dat, drop = FALSE]
		}
		if(nb.column == 31){
			xdonne <- donne[, c(col.id, col.yr, col.mo)]
			donne <- donne[, col.dat+(0:30)]
		}
	}
	if(period == 'dekadal'){
		istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
		iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
		odates <- seq(istart, iend, 'day')
		odates <- paste(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%d')), sep = '')
		if(nb.column == 1){
			xdonne <- donne[, c(col.id, col.yr, col.mo, col.dy)]
			donne <- donne[, col.dat, drop = FALSE]
		}
		if(nb.column == 3){
			xdonne <- donne[, c(col.id, col.yr, col.mo)]
			donne <- donne[, col.dat+(0:2)]
		}
		if(nb.column == 36){
			xdonne <- donne[, c(col.id, col.yr)]
			donne <- donne[, col.dat+(0:35)]
		}
	}
	if(period == 'monthly'){
		istart <- as.Date(paste(istart.yrs, istart.mon, 1, sep = '-'))
		iend <- as.Date(paste(iend.yrs, iend.mon, 1, sep = '-'))
		odates <- format(seq(istart, iend, 'month'), '%Y%m')
		if(nb.column == 1){
			xdonne <- donne[, c(col.id, col.yr, col.mo)]
			donne <- donne[, col.dat, drop = FALSE]
		}
		if(nb.column == 12){
			xdonne <- donne[, c(col.id, col.yr)]
			donne <- donne[, col.dat+(0:11)]
		}
	}

	donne1 <- apply(donne, 2, as.numeric)
	miss.stn$non.numeric <- donne[is.na(apply(donne1, 2, as.character))!= is.na(donne)]
	if(length(miss.stn$non.numeric) > 0) miss.stn$non.numeric <- miss.stn$non.numeric[!duplicated(miss.stn$non.numeric)]

	miss.stn$with.data.no.coords <- NULL
	if(!coords.included){
		id.stn.data <- unique(xdonne[, 1])
		isdata <- !id.stn.data%in%infoheadI[, 1]
		if(any(isdata)){
			stn.with.data <- id.stn.data[isdata]
			miss.stn$with.data.no.coords <- stn.with.data
			insert.stn <- matrix(NA, nrow = length(stn.with.data), ncol = ncol(infoheadI))
			insert.stn[, 1] <- stn.with.data
			infoheadI <- rbind(infoheadI, insert.stn)
		}
	}

	donne <- lapply(infoheadI[, 1], function(id){
		ix <- xdonne[, 1] == id
		if(!any(ix)) return(NULL)
		don <- donne1[ix, , drop = FALSE]
		don <- c(t(don))
		xdaty <- xdonne[ix, , drop = FALSE]

		if(period == 'daily'){
			if(nb.column == 1) daty <- as.Date(paste(xdaty[, 2], xdaty[, 3], xdaty[, 4], sep = '-'))
			if(nb.column == 31) daty <- as.Date(paste(paste(rep(paste(xdaty[, 2], xdaty[, 3], sep = '-'), each = 31),
										rep(1:31, nrow(xdaty)), sep = '-')))
		}
		if(period == 'dekadal'){
			if(nb.column == 1) daty <- as.Date(paste(xdaty[, 2], xdaty[, 3], xdaty[, 4], sep = '-'))
			if(nb.column == 3) daty <- as.Date(paste(paste(rep(paste(xdaty[, 2], xdaty[, 3], sep = '-'), each = 3),
										rep(1:3, nrow(xdaty)), sep = '-')))
			if(nb.column == 36) daty <- as.Date(paste(rep(xdaty[, 2], each = 36), rep(paste(rep(1:12, each = 3),
									rep(1:3, 12), sep = '-'), nrow(xdaty)), sep = '-'))

			daty[as.numeric(format(daty, '%d')) > 3] <- NA
		}
		if(period == 'monthly'){
			if(nb.column == 1) daty <- as.Date(paste(xdaty[, 2], xdaty[, 3], 1, sep = '-'))
			if(nb.column == 12) daty <- as.Date(paste(rep(xdaty[, 2], each = 12), rep(1:12, nrow(xdaty)), 1, sep = '-'))
		}

		don <- don[!is.na(daty)]
		daty <- daty[!is.na(daty)]
		don <- don[order(daty)]
		daty <- daty[order(daty)]

		if(period == 'daily') daty <- format(daty, '%Y%m%d')
		if(period == 'dekadal') daty <- paste(format(daty, '%Y%m'), as.numeric(format(daty, '%d')), sep = '')
		if(period == 'monthly') daty <- format(daty, '%Y%m')

		don <- don[match(odates, daty)]
		don
	})

	donne.null <- sapply(donne, is.null)
	donne <- donne[!donne.null]
	miss.stn$no.data <- if(any(donne.null)) infoheadI[donne.null, , drop = FALSE] else NULL
	infoheadI <- infoheadI[!donne.null, , drop = FALSE]
	donne <- do.call(cbind, donne)

	na.donne <- apply(donne, 2, is.na)
	if(is.null(dim(na.donne))) na.donne <- matrix(na.donne, ncol = 1)

	per.donne <- 1-(apply(na.donne, 2, sum)/length(odates)) >= min.perc
	if(is.null(dim(per.donne))) per.donne <- matrix(per.donne, ncol = 1)

	donne <- donne[, per.donne, drop = FALSE]
	miss.stn$less.data <- infoheadI[!per.donne, , drop = FALSE]
	infoheadI <- infoheadI[per.donne, , drop = FALSE]

	infoheadI[, 1] <- substr(str_replace_all(infoheadI[, 1], "[^[:alnum:]]", ""), 1, 15)

	donne <- rbind(t(rbind(capition, infoheadI)), cbind(odates, donne))
	# donne[is.na(donne)] <- donneInfo[[3]]$miss.val
	donne[is.na(donne)] <- -99
	writeFiles(donne, File2Save)

	outlist <- list()
	if(nrow(miss.stn$vague.info) > 0){
		outlist <- c(outlist, list('Missing values on Station IDs, Year, Months or Days', miss.stn$vague.info))
	}
	if(nrow(miss.stn$dup.STN.ID) > 0){
		outlist <- c(outlist, list('Duplicated station IDs', miss.stn$dup.STN.ID))
	}
	if(nrow(miss.stn$dup.coords) > 0){
		outlist <- c(outlist, list('Duplicated coordinates', miss.stn$dup.coords))
	}
	if(nrow(miss.stn$miss.coords) > 0){
		outlist <- c(outlist, list('Missing coordinates', miss.stn$miss.coords))
	}
	if(!is.null(miss.stn$with.data.no.coords)){
		outlist <- c(outlist, list('Stations with data but no coordinates found', miss.stn$with.data.no.coords))
	}
	if(length(miss.stn$non.numeric) > 0){
		outlist <- c(outlist, list('Non numeric values inside the data', miss.stn$non.numeric))
	}
	if(!is.null(miss.stn$no.data)){
		outlist <- c(outlist, list('Stations without data but have coordinates', miss.stn$no.data))
	}
	if(nrow(miss.stn$less.data) > 0){
		outlist <- c(outlist, list('Stations excluded not enough values', miss.stn$less.data))
	}

	if(length(outlist) > 0){
		containertab <- displayConsOutputTabs(tknotes, outlist, title = 'Failed Stations')
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'ctxt'
		AllOpenTabData[[ntab+1]] <<- containertab
		tkselect(tknotes, ntab)
	}
	return(0)
}


