excludeOutStnFun <- function(GeneralParameters){
	stnf <- GeneralParameters$IO.files$STN.file
	shpf <- GeneralParameters$IO.files$SHP.file
	dir2save <- GeneralParameters$IO.files$dir2save
	buff <- GeneralParameters$buffer/111

	outdir <- file.path(dir2save, paste('ChkCoord', getf.no.ext(stnf), sep = '_'))
	if(!file.exists(outdir)) dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
	file.no.ext <- getf.no.ext(stnf)
	file.extens <- file_ext(stnf)

	shpd <- getShpOpenData(shpf)[[2]]
	if(is.null(shpd)){
		InsertMessagesTxt(main.txt.out, paste('Unable to open', shpf, 'or it is not an ESRI shapefile'), format = TRUE)
		return(NULL)
	}

	shpd <- as(shpd, "SpatialPolygons")
	shpd <- gUnaryUnion(shpd)
	shpd <- gSimplify(shpd, tol = 0.05, topologyPreserve = TRUE)
	shpd <- gBuffer(shpd, width = buff)

	infos <- getStnOpenDataInfo(stnf)
	missval <- infos[[3]]$miss.val
	donne <- getStnOpenData(stnf)
	if(is.null(donne)){
		InsertMessagesTxt(main.txt.out, paste('Unable to open', stnf), format = TRUE)
		return(NULL)
	}
	ndaty <- as.character(nchar(donne[nrow(donne), 1]))
	period <- switch(ndaty, '8' = 'daily', '7' = 'dekadal', '6' = 'monthly', NA)
	if(is.na(period)){
		InsertMessagesTxt(main.txt.out, "Unknown date", format = TRUE)
		return(NULL)
	}
	
	donne1 <- splitCDTData(donne, period)

	## missing & duplicated coords
	imiss <- donne1$miss.coords.idx
	idup <- donne1$dup.coords.idx

	#coordinates outside country boundaries
	infodon0 <- t(donne[1:3, -1])
	infodon <- infodon0 <- data.frame(infodon0, 1:nrow(infodon0))
	names(infodon) <- c('ID', 'lon', 'lat', 'idout')
	infodon <- infodon[!imiss, , drop = FALSE]
	infodon[, -1] <- apply(infodon[, -1], 2, as.numeric)
	coordinates(infodon) <- ~lon+lat
	ioutc <- is.na(over(infodon, geometry(shpd)))
	ioutc <- infodon$idout[ioutc]

	exclus <- NULL
	if(any(imiss)) exclus <- rbind(exclus, cbind('Missing Coordinates', as.matrix(infodon0[imiss, , drop = FALSE])))
	if(any(idup)) exclus <- rbind(exclus, cbind('Duplicate Coordinates', as.matrix(infodon0[idup, , drop = FALSE])))
	if(length(ioutc) > 0) exclus <- rbind(exclus, cbind('Coordinates Outside', as.matrix(infodon0[ioutc, , drop = FALSE])))

	if(!is.null(exclus)){
		exclus <- as.data.frame(exclus, stringsAsFactors = FALSE)
		exclus <- exclus[!duplicated(exclus[, c(2, 5)]), ]
		exclus <- exclus[order(paste(exclus[, 3], exclus[, 4], sep = '_')), ]
	}else{
		exclus <- data.frame(NA, NA, NA, NA, NA)
	}

	names(exclus) <- c('Info', 'ID.Station', 'Longitude', 'Latitude', 'ID.Col')
	fexcl <- file.path(outdir, paste(file.no.ext, '_2CORRECT_STATIONS.txt', sep = ''))
	write.table(exclus, fexcl, row.names = FALSE, col.names = TRUE)

	daty0 <- donne1$dates
	if(is.null(donne1$elv)){
		daty <- donne[-(1:3), 1]
		donne1$data <- donne[-(1:3), -1]
		HeadInfo <- donne[1:3, ]
	}else{
		daty <- donne[-(1:4), 1]
		donne1$data <- donne[-(1:4), -1]
		HeadInfo <- donne[1:4, ]
	}
	donne1 <- donne1$data[match(daty0, daty), ]

	donne <- cbind(daty0, donne1)
	names(donne) <- names(HeadInfo)
	donne <- rbind(HeadInfo, donne)
	donne[is.na(donne)] <- missval
	fsave <- file.path(outdir, paste(file.no.ext, '_CHECKED_STATIONS.', file.extens, sep = ''))
	writeFiles(donne, fsave)

	idx.col <- cbind(seq(ncol(donne1)), seq(ncol(donne1)))
	res <- list(action = 'chk.coords', period = period, Stndoute = exclus, data = donne1, dates = daty0,
				HeadInfo = HeadInfo, missval = missval, outdir = outdir, idx.col = idx.col)
	rm(daty, daty0, donne, donne1)
	return(res)
}

#########################

checkCDTcoords <- function(ReturnExecResults, GeneralParameters){
	file.no.ext <- getf.no.ext(GeneralParameters$IO.files$STN.file)
	file.extens <- file_ext(GeneralParameters$IO.files$STN.file)
	fexcl <- file.path(ReturnExecResults$outdir, paste(file.no.ext, '_2CORRECT_STATIONS.txt', sep = ''))
	fsave <- file.path(ReturnExecResults$outdir, paste(file.no.ext, '_CHECKED_STATIONS.', file.extens, sep = ''))

	exclus <- ReturnExecResults$Stndoute
	retenus <- read.table(fexcl, header = TRUE, colClasses = 'character', stringsAsFactors = FALSE)

	######
	idna <- !is.na(str_trim(retenus$ID.Station)) & !is.na(str_trim(retenus$ID.Col))
	retenus <- retenus[idna, , drop = FALSE]
	
	retenus$ID.Col <- ReturnExecResults$idx.col[match(as.numeric(retenus$ID.Col), ReturnExecResults$idx.col[, 2]), 1]
	exclus$ID.Col <- ReturnExecResults$idx.col[match(as.numeric(exclus$ID.Col), ReturnExecResults$idx.col[, 2]), 1]

	exc.idcl <- str_trim(exclus$ID.Col)
	ret.idcl <- str_trim(retenus$ID.Col)
	stn.omit <- as.numeric(exc.idcl[is.na(match(exc.idcl, ret.idcl))])

	if(length(ret.idcl) > 0){
		ret.col <- as.numeric(ret.idcl)
		ReturnExecResults$HeadInfo[2, ret.col+1] <- str_trim(retenus$Longitude)
		ReturnExecResults$HeadInfo[3, ret.col+1] <- str_trim(retenus$Latitude)
		ReturnExecResults$Stndoute[match(ret.idcl, exc.idcl), 3] <- str_trim(retenus$Longitude)
		ReturnExecResults$Stndoute[match(ret.idcl, exc.idcl), 4] <- str_trim(retenus$Latitude)
	}

	don <- cbind(ReturnExecResults$dates, ReturnExecResults$data)
	names(don) <- names(ReturnExecResults$HeadInfo)
	donne <- rbind(ReturnExecResults$HeadInfo, don)
	donne[is.na(donne)] <- ReturnExecResults$missval

	if(length(stn.omit) > 0){
		donne <- donne[, -(stn.omit+1), drop = FALSE]
		ReturnExecResults$HeadInfo <- ReturnExecResults$HeadInfo[, -(stn.omit+1), drop = FALSE]
		ReturnExecResults$data <- ReturnExecResults$data[, -stn.omit, drop = FALSE]
		ReturnExecResults$Stndoute <- ReturnExecResults$Stndoute[!as.numeric(exc.idcl)%in%stn.omit, , drop = FALSE]
		idx <- ReturnExecResults$idx.col[-stn.omit, 2, drop = FALSE]
		ReturnExecResults$idx.col <- cbind(seq(ncol(ReturnExecResults$data)), idx)
	}

	writeFiles(donne, fsave)
	rm(don, donne)
	return(ReturnExecResults)
}

