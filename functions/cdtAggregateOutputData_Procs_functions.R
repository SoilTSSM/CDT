
AggregateQcData <- function(){
	outdirs <- as.character(GeneralParameters$file.io)
	datfin <- file.path(outdirs, 'AggregateData')
	if(!file.exists(datfin)) dir.create(datfin, showWarnings = FALSE, recursive = TRUE)

	outputdir <- file.path(outdirs, 'Outputs')
	outptID <- list.files(outputdir)
	chkdir <- file.path(outdirs, 'CorrectedData')
	corrctID <- list.files(chkdir)

	load(file.path(outdirs, 'OriginalData', 'Parameters.RData'))
	if(paramsGAL$inputPars$action == "qc.rain"){
		infohead <- cbind(paramsGAL$data$id, paramsGAL$data$lon, paramsGAL$data$lat, paramsGAL$data$elv)
		stnID <- as.character(paramsGAL$data$id)
	}
	if(paramsGAL$inputPars$action == "qc.temp"){
		infohead <- cbind(paramsGAL$data[[1]]$id, paramsGAL$data[[1]]$lon, paramsGAL$data[[1]]$lat, paramsGAL$data[[1]]$elv)
		stnID <- as.character(paramsGAL$data[[1]]$id)
	}
	existStn <- stnID%in%corrctID
	noChck <- !stnID%in%outptID
	stnID <- stnID[existStn]
	noTraiteStn <- infohead[!existStn, ]
	noQcChkStn <- infohead[noChck, ]
	infohead <- infohead[existStn, ]
	if(length(stnID) == 0){
		InsertMessagesTxt(main.txt.out, 'No checked stations found or Wrong directory', format = TRUE)
		return(NULL)
	}

	if(nrow(noTraiteStn) > 0 | nrow(noQcChkStn) > 0){
		if(nrow(noQcChkStn) > 0) faileds0 <- list('Stations not Checked', noQcChkStn)
		else faileds0 <- NULL
		if(nrow(noTraiteStn) > 0) faileds1 <- list('Stations not aggregated', noTraiteStn)
		else faileds1 <- NULL
		faileds <- list(faileds0, faileds1)
		containertab <- displayConsOutputTabs(tknotes, faileds, title = 'Failed Stations')
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'ctxt'
		AllOpenTabData[[ntab+1]] <<- containertab
		tkselect(tknotes, ntab)
	}

	stn.don <- read.table(file.path(chkdir, stnID[1], paste(stnID[1], '.txt', sep = '')))
	aggData <- stn.don[, 1]

	for(jj in stnID){
		filein <- file.path(chkdir, jj, paste(jj, '.txt', sep = ''))
		stn.don <- read.table(filein)
		aggData <- cbind(aggData, stn.don[, 2])
	}

	period <- paramsGAL$inputPars$period
	if(period == 'daily') pdate <- 'DAILY'
	if(period == 'dekadal') pdate <- 'DEKADAL'
	if(period == 'Monthly') pdate <- 'MONTHLY'

	if(ncol(infohead) == 3) capition <- c('Stations', 'LON', paste(pdate, 'LAT', sep = '/'))
	else capition <- c('Stations', 'LON', 'LAT', paste(pdate, 'ELV', sep = '/'))
	infohead <- cbind(capition, t(infohead))
	aggData <- t(cbind(t(infohead), t(aggData)))
	if(paramsGAL$inputPars$action == "qc.rain"){
		aggData[is.na(aggData)] <- paramsGAL$dataPars[[2]]$miss.val
		stnfile <- paramsGAL$inputPars$IO.files$STN.file
	}
	if(paramsGAL$inputPars$action == "qc.temp"){
		aggData[is.na(aggData)] <- paramsGAL$dataPars[[1]][[2]]$miss.val
		stnfile <- paramsGAL$inputPars$IO.files$STN.file1
	}

	fileout <- file.path(datfin, paste('Checked', stnfile, sep = '_'))
	writeFiles(aggData, fileout)
	return(0)
}

###################################################

AggregateHomData0 <- function(){
	outdirs <- as.character(GeneralParameters$file.io)
	datfin <- file.path(outdirs, 'AggregateData')
	if(!file.exists(datfin)) dir.create(datfin, showWarnings = FALSE, recursive = TRUE)

	chkdir <- file.path(outdirs, 'AdjustedData')

	load(file.path(outdirs, 'OriginalData', 'Parameters.RData'))
	infohead0 <- cbind(paramsGAL$data[[1]]$id, paramsGAL$data[[1]]$lon, paramsGAL$data[[1]]$lat, paramsGAL$data[[1]]$elv)
	StnId <- as.character(paramsGAL$data[[1]]$id)
	ggid <- list.files(chkdir)
	if(length(ggid) == 0){
		InsertMessagesTxt(main.txt.out, 'No tested stations found or Wrong directory', format = TRUE)
		return(NULL)
	}

	suffix <- c('DLY', 'DEK', 'MON')
	prefix <- c('DAILY', 'DEKADAL', 'MONTHLY')

	for(xfl in 1:3){
		if(is.null(paramsGAL$data1[[xfl]])) next
		dates <- paramsGAL$data1[[xfl]][[1]]$date
		donne1 <- round(paramsGAL$data1[[xfl]][[1]]$data, 1)
		donne3 <- donne2 <- donne1
		for(j in 1:length(ggid)){
			xpos <- StnId%in%ggid[j]
			filein2 <- file.path(chkdir, ggid[j])
			fileChoix <- file.path(filein2, paste(ggid[j], '_CHOICE.txt', sep = ''))
			filein2 <- file.path(filein2, paste(ggid[j], '_', suffix[xfl], '.txt', sep = ''))
			dat <- read.table(filein2)
			chx <- read.table(fileChoix)
			idx <- chx[, 1]
			donne1[,xpos] <- dat[, 3]
			donne2[,xpos] <- dat[, 4]
			if(idx == 1){
				tschx <- dat[, 3]
			}else if(idx == 2){
				tschx <- dat[, 4]
			}else if(idx == 3){
				tschx <- dat[, 2]
			}else tschx <- dat[, 2]
			donne3[, xpos] <- tschx
		}

		if(ncol(infohead0) == 3) capition <- c('Stations', 'LON', paste(prefix[xfl], 'LAT', sep = '/'))
		if(ncol(infohead0) == 4) capition <- c('Stations', 'LON', 'LAT', paste(prefix[xfl], 'ELV', sep = '/'))
		infohead <- t(cbind(capition, t(infohead0)))

		donne1 <- t(cbind(infohead, t(cbind(dates, donne1))))
		donne2 <- t(cbind(infohead, t(cbind(dates, donne2))))
		donne3 <- t(cbind(infohead, t(cbind(dates, donne3))))
		donne1[is.na(donne1)] <- paramsGAL$dataPars[[1]][[2]]$miss.val
		donne2[is.na(donne2)] <- paramsGAL$dataPars[[1]][[2]]$miss.val
		donne3[is.na(donne3)] <- paramsGAL$dataPars[[1]][[2]]$miss.val
		writeFiles(donne1, file.path(datfin, paste('AdjMean_', suffix[xfl], '_', paramsGAL$inputPars$IO.files$Cand.file, sep = '')))
		writeFiles(donne2, file.path(datfin, paste('AdjQM_', suffix[xfl], '_', paramsGAL$inputPars$IO.files$Cand.file, sep = '')))
		writeFiles(donne3, file.path(datfin, paste('Combined-Adj_', suffix[xfl], '_', paramsGAL$inputPars$IO.files$Cand.file, sep = '')))
	}

	noTraiteStn <- StnId[!StnId%in%ggid]
	if(length(noTraiteStn) > 0){
		faileds <- list('Not Tested Stations', noTraiteStn)
		containertab <- displayConsOutputTabs(tknotes, faileds, title = 'Not Tested Stations')
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'ctxt'
		AllOpenTabData[[ntab+1]] <<- containertab
		tkselect(tknotes, ntab)
	}
	return(0)
}

###################################################

AggregateHomData <- function(){
	outdirs <- as.character(GeneralParameters$file.io)
	datfin <- file.path(outdirs, 'AggregateData')
	if(!file.exists(datfin)) dir.create(datfin, showWarnings = FALSE, recursive = TRUE)

	chkdir <- file.path(outdirs, 'AdjustedData')
	orgdir <- file.path(outdirs, 'Data')

	xfile <- list.files(orgdir)
	info <- grep('Infos_', xfile)
	pars <- grep('RefSeries_Data', xfile)

	filein <- file.path(orgdir, xfile[info])
	gginfo <- read.table(filein, header = TRUE)
	ggid <- as.character(gginfo$IDs)
	ggelv <- gginfo$Elv
	if(sum(!is.na(ggelv)) == 0){
		infohead <- gginfo[, 1:3]
		infohead[, 1] <- ggid
		infohead <- t(infohead)
	}else{
		infohead <- gginfo
		infohead[, 1] <- ggid
		infohead <- t(infohead)
	}

	fdonne <- xfile[-c(info, pars)]
	for(xfl in fdonne){
		period <- strsplit(xfl, '_')[[1]][1]
		filein1 <- file.path(orgdir, xfl)
		ggdates <- read.table(filein1, header = FALSE)
		ggdates3 <- ggdates2 <- ggdates1 <- ggdates[, 1]
		miss <- NULL
		for(j in 1:length(ggid)){
			filein2 <- file.path(chkdir, ggid[j])
			fileChoix <- file.path(filein2, paste(ggid[j], '_CHOICE.txt', sep = ''))
			if(period == 'DAILY') filein2 <- file.path(filein2, paste(ggid[j], '_DLY.txt', sep = ''))
			if(period == 'DEKADAL') filein2 <- file.path(filein2, paste(ggid[j], '_DEK.txt', sep = ''))
			if(period == 'MONTHLY') filein2 <- file.path(filein2, paste(ggid[j], '_MON.txt', sep = ''))
			if(file.exists(filein2)){
				dat <- read.table(filein2)
				chx <- read.table(fileChoix)
				idx <- chx[, 1]
				ggdates1 <- cbind(ggdates1, dat[, 3])
				ggdates2 <- cbind(ggdates2, dat[, 4])
				if(idx == 1){
					tschx <- dat[, 3]
				}else if(idx == 2){
					tschx <- dat[, 4]
				}else if(idx == 3){
					tschx <- dat[, 2]
				}else tschx <- dat[, 2]
				ggdates3 <- cbind(ggdates3, tschx)
			}else{
				miss <- c(miss, j)
			}
		}

		if(!is.null(miss)){
			infohead1 <- infohead[, -miss]
			faileds <- list('Not Tested Stations', infohead[1, miss])
			containertab <- displayConsOutputTabs(tknotes, faileds, title = 'Not Tested Stations')
			ntab <- length(AllOpenTabType)
			AllOpenTabType[[ntab+1]] <<- 'ctxt'
			AllOpenTabData[[ntab+1]] <<- containertab
			tkselect(tknotes, ntab)
		}else infohead1 <- infohead
		if(nrow(infohead) == 3) capition <- c('Stations', 'LON', paste(period, 'LAT', sep = '/'))
		if(nrow(infohead) == 4) capition <- c('Stations', 'LON', 'LAT', paste(period, 'ELV', sep = '/'))
		infohead2 <- cbind(capition, infohead1)
		donne1 <- rbind(infohead2, ggdates1)
		donne2 <- rbind(infohead2, ggdates2)
		donne3 <- rbind(infohead2, ggdates3)
		donne1[is.na(donne1)] <- -99
		donne2[is.na(donne2)] <- -99
		donne3[is.na(donne3)] <- -99
		writeFiles(donne1, file.path(datfin, paste('AdjMean', xfl, sep = '-')))
		writeFiles(donne2, file.path(datfin, paste('AdjQM', xfl, sep = '-')))
		writeFiles(donne3, file.path(datfin, paste('Combined-Adj', xfl, sep = '-')))
	}
	return(0)
}

###################################################################

AggregateZeroChkData <- function(){
	outdirs <- as.character(GeneralParameters$file.io)
	datfin <- file.path(outdirs, 'AggregateData')
	if(!file.exists(datfin)) dir.create(datfin, showWarnings = FALSE, recursive = TRUE)

	outputdir <- file.path(outdirs, 'Outputs')
	outptID <- list.files(outputdir)
	zchkdir <- file.path(outdirs, 'CorrectedData')
	corrctID <- list.files(zchkdir)

	load(file.path(outdirs, 'OriginalData', 'Parameters.RData'))
	infohead <- cbind(paramsGAL$data$id, paramsGAL$data$lon, paramsGAL$data$lat, paramsGAL$data$elv)
	stnID <- as.character(paramsGAL$data$id)

	existStn <- stnID%in%corrctID
	noChck <- !stnID%in%outptID
	stnID <- stnID[existStn]
	noTraiteStn <- infohead[!existStn, ]
	noZeroChkStn <- infohead[noChck, ]
	infohead <- infohead[existStn, ]
	if(length(stnID) == 0){
		InsertMessagesTxt(main.txt.out, 'No corrected stations found or Wrong directory', format = TRUE)
		return(NULL)
	}

	if(nrow(noTraiteStn) > 0 | nrow(noZeroChkStn) > 0){
		if(nrow(noZeroChkStn) > 0) faileds0 <- list('Stations not Zeros Checked', noZeroChkStn)
		else faileds0 <- NULL
		if(nrow(noTraiteStn) > 0) faileds1 <- list('Stations not aggregated', noTraiteStn)
		else faileds1 <- NULL
		faileds <- list(faileds0, faileds1)
		containertab <- displayConsOutputTabs(tknotes, faileds, title = 'Failed Stations')
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'ctxt'
		AllOpenTabData[[ntab+1]] <<- containertab
		tkselect(tknotes, ntab)
	}

	stn.don <- read.table(file.path(zchkdir, stnID[1], paste(stnID[1], '.txt', sep = '')))
	aggData <- stn.don[, 1]

	for(jj in stnID){
		filein <- file.path(zchkdir, jj, paste(jj, '.txt', sep = ''))
		stn.don <- read.table(filein)
		aggData <- cbind(aggData, stn.don[, 2])
	}

	if(is.null(paramsGAL$data$elv)) capition <- c('Stations', 'LON', paste('DAILY', 'LAT', sep = '/'))
	else capition <- c('Stations', 'LON', 'LAT', paste('DAILY', 'ELV', sep = '/'))
	infohead <- cbind(capition, t(infohead))
	aggData <- t(cbind(t(infohead), t(aggData)))
	aggData[is.na(aggData)] <- paramsGAL$dataPars[[2]]$miss.val

	fileout <- file.path(datfin, paste('ZeroChecked', paramsGAL$inputPars$IO.files$STN.file, sep = '_'))
	writeFiles(aggData, fileout)
	return(0)
}



