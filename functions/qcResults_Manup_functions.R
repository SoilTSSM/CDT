
QcOutFormat <- function(){
		if(GeneralParameters$AllOrOne == 'one'){
			IJoutputdir <- ReturnExecResults$outputdir
			IJstation <- ReturnExecResults$station
		}
		if(GeneralParameters$AllOrOne == 'all'){
			ijstn <- which(as.character(GeneralParameters$parameter[[2]][,1]) == tclvalue(lchoixStnFr$env$stn.choix.val))
			IJoutputdir <- ReturnExecResults$outputdir[[ijstn]]
			IJstation <- ReturnExecResults$station[[ijstn]]
		}
		fileout <- file.path(IJoutputdir, paste(IJstation,'.txt', sep = ''), fsep = .Platform$file.sep)
		if(file.exists(fileout)){
			xdat <- read.table(fileout, header = TRUE, colClasses = 'character')
			retdata <- list(xdat, fileout)
		}else{
			InsertMessagesTxt(main.txt.out, 'There is no qc-results outputs', format = TRUE)
			retdata <- NULL
		}
	return(retdata)
}


###################################################################

getOutlier.params <- function(IJstation, IJoutputdir){
	flqcout <- file.path(IJoutputdir, paste(IJstation,'.txt', sep = ''), fsep = .Platform$file.sep)
	if(ReturnExecResults$action == 'qc.rain'){
		dates <- EnvQcOutlierData$donnees$dates
		if(as.character(GeneralParameters$use.method$Values[1]) == "0"){
			pos <- which(as.character(EnvQcOutlierData$donnees$id) == IJstation)
			xdat <- EnvQcOutlierData$donnees$data[,pos]
		}else{
			if(EnvQcOutlierData$donnees$nbvar == 3) xdat <- EnvQcOutlierData$donnees$var$rr
			else xdat <- EnvQcOutlierData$donnees$var$var
		}

		if(file.exists(flqcout)) outqcf <- read.table(flqcout, header = TRUE, colClasses = 'character')
		else outqcf <- NULL
		retval <- list(dates = dates, value = xdat, qcout = outqcf)
	}else if(ReturnExecResults$action == 'qc.temp'){
		if(as.character(GeneralParameters$use.method$Values[1]) == "0"){#not 1series
			dates <- EnvQcOutlierData$donnees1$dates
			pos <- which(as.character(EnvQcOutlierData$donnees1$id) == IJstation)
			xdat <- EnvQcOutlierData$donnees1$data[,pos]
		}else{
			dates <- EnvQcOutlierData$donnees1$dates
			if(EnvQcOutlierData$donnees1$nbvar == 3){
				if(as.character(GeneralParameters$test.tx) == "1") xdat <- EnvQcOutlierData$donnees1$var$tx
				else xdat <- EnvQcOutlierData$donnees1$var$tn
			} else xdat <- EnvQcOutlierData$donnees1$var$var
		}

		if(file.exists(flqcout)) outqcf <- read.table(flqcout, header = TRUE, colClasses = 'character')
		else outqcf <- NULL
		retval <- list(dates = dates, value = xdat, qcout = outqcf)
	}else{
		InsertMessagesTxt(main.txt.out, 'Reinitialize the operation. Parameters or Outputs are not a QC results', format = TRUE)
		retval <- NULL
	}
	return(retval)
}

######################################
replaceOutlier <- function(IJstation, thresStat, isReplace){

	if(GeneralParameters$AllOrOne == 'one') IJoutputdir <- ReturnExecResults$outputdir
	if(GeneralParameters$AllOrOne == 'all'){ 
		ijstn <- which(as.character(GeneralParameters$parameter[[2]][,1]) == IJstation)
		IJoutputdir <- ReturnExecResults$outputdir[[ijstn]]
	}

	outlparams <- getOutlier.params(IJstation, IJoutputdir)

	corrdir <- file.path(paste(head(unlist(strsplit(IJoutputdir,.Platform$file.sep)), n=-2), sep ='',collapse = .Platform$file.sep),'CorrectedData', fsep = .Platform$file.sep)
	if(!file.exists(corrdir)) dir.create(corrdir, showWarnings = FALSE, recursive = TRUE)
	corrdirstn <- file.path(corrdir, IJstation, fsep = .Platform$file.sep)
	if(!file.exists(corrdirstn)) dir.create(corrdirstn, showWarnings = FALSE, recursive = TRUE)
	filecor <- file.path(corrdirstn, paste(IJstation,'.txt', sep = ''), fsep = .Platform$file.sep)

	outqcf <- outlparams$qcout
	dates <- outlparams$dates
	xdat <- outlparams$value
	if(!is.null(outqcf)){
		outdates <- as.character(outqcf$dates)
		if(isReplace){
			thstat <- as.numeric(thresStat)
			stats <- as.numeric(as.character(outqcf$statistic))
			outdates1 <- outdates[which(stats >= thstat)]
			xdat[match(outdates1, dates)] <- NA
		}else{
			origvalues <- as.numeric(as.character(outqcf$values))
			noreplace <- as.character(outqcf$not.replace)
			changeval <- as.numeric(as.character(outqcf$change.values))
			xdat[match(outdates, dates)] <- NA
			xdat[match(outdates[!is.na(noreplace) & noreplace == "1"],dates)] <- origvalues[!is.na(noreplace) & noreplace == "1"]
			if(ReturnExecResults$action == 'qc.temp'){
				estvalue <- as.numeric(as.character(outqcf$estimated.values))
				xdat[match(outdates[!is.na(noreplace) & noreplace == "2"],dates)] <- estvalue[!is.na(noreplace) & noreplace == "2"]
			}
			xdat[match(outdates[!is.na(changeval)], dates)] <- changeval[!is.na(changeval)]
		}
	}
	sdon <- data.frame(dates, xdat)
	write.table(sdon, filecor, col.names = FALSE, row.names = FALSE)
}

######################################


isSpatialCheckOk <- function(){
	if(GeneralParameters$AllOrOne == 'one'){
		IJoutputdir <- ReturnExecResults$outputdir
		IJstation <- ReturnExecResults$station
	}
	if(GeneralParameters$AllOrOne == 'all'){
		ijstn <- which(as.character(GeneralParameters$parameter[[2]][,1]) == tclvalue(lchoixStnFr$env$stn.choix.val))
		IJoutputdir <- ReturnExecResults$outputdir[[ijstn]]
		IJstation <- ReturnExecResults$station[[ijstn]]
	}

	qcout <- getOutlier.params(IJstation, IJoutputdir)$qcout
	if(ReturnExecResults$action == 'qc.rain'){
		xxout <- as.character(qcout$spatial.check)
		outdts <- qcout[!is.na(xxout), c('stn', 'dates', 'spatial.check')]
	}
	if(ReturnExecResults$action == 'qc.temp'){
		xxout <- as.character(qcout$spatial.reg.check)
		outdts <- qcout[!is.na(xxout), c('stn', 'dates', 'spatial.reg.check')]
	}
	return(outdts)
}

#########################

