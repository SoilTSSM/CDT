Execute_All_Functions <- function(get.stn){
	if(is.null(GeneralParameters$action)) return(NULL)

	#QC Rain
	if(GeneralParameters$action == 'qc.rain'){
		sortieqc <- ExecQcRain(get.stn)
		if(GeneralParameters$AllOrOne == 'all'){
			outpdir <- file.path(EnvQcOutlierData$baseDir, 'Outputs')
			ggdir <- list.files(outpdir)
			failedqc <- lapply(ggdir, function(lstn){
				ggfileout <- file.path(outpdir, lstn, paste(lstn, '.txt', sep = ''))
				file.exists(ggfileout)
			})
			failedqc <- ggdir[!unlist(failedqc)]
			if(length(failedqc) > 0){
				failedqc <- data.frame(failedqc)
				names(failedqc) <- 'QC failed: Stations not checked'
				containertab <- displayConsOutputTabs(tknotes, failedqc,
								title = paste(getf.no.ext(GeneralParameters$IO.files$STN.file), 'Failed'))
				ntab <- length(AllOpenTabType)
				AllOpenTabType[[ntab+1]] <<- 'ctxt'
				AllOpenTabData[[ntab+1]] <<- containertab
				tkselect(tknotes, ntab)
			}
			InsertMessagesTxt(main.txt.out, "Rainfall QC finished!")
		}
		return(sortieqc)
	}

	#############################################################
	##zero check
	if(GeneralParameters$action == 'zero.check'){
		retzeroChk <- execZeroCheck(get.stn)
		if(retzeroChk$AllOrOne == 'all'){
			stns <- sapply(retzeroChk$res, function(x) x$station)
			isnull <- sapply(retzeroChk$res, function(x) is.null(x$res))
			failedzc <- stns[isnull]
			if(length(failedzc) > 0){
				failedzc <- data.frame(failedzc)
				names(failedzc) <- 'Zeros check failed: Stations not checked'
				containertab <- displayConsOutputTabs(tknotes, failedzc,
								title = paste(getf.no.ext(GeneralParameters$IO.files$STN.file), 'Failed'))
				ntab <- length(AllOpenTabType)
				AllOpenTabType[[ntab+1]] <<- 'ctxt'
				AllOpenTabData[[ntab+1]] <<- containertab
				tkselect(tknotes, ntab)
			}
			InsertMessagesTxt(main.txt.out, "Zeros check finished!")
		}
		return(retzeroChk)
	}

	#############################################################
	#qc  temp
	if(GeneralParameters$action == 'qc.temp'){
		sortieqc <- ExecQcTemp(get.stn)
		if(GeneralParameters$AllOrOne == 'all'){
			outpdir <- file.path(EnvQcOutlierData$baseDir, 'Outputs')
			ggdir <- list.files(outpdir)
			failedqc <- lapply(ggdir, function(lstn){
				ggfileout <- file.path(outpdir, lstn, paste(lstn, '.txt', sep = ''))
				file.exists(ggfileout)
			})
			failedqc <- ggdir[!unlist(failedqc)]

			if(length(failedqc) > 0){
				failedqc <- data.frame(failedqc)
				names(failedqc) <- 'QC failed: Stations not checked'
				containertab <- displayConsOutputTabs(tknotes, failedqc,
								title = paste(getf.no.ext(GeneralParameters$IO.files$STN.file), 'Failed'))
				ntab <- length(AllOpenTabType)
				AllOpenTabType[[ntab+1]] <<- 'ctxt'
				AllOpenTabData[[ntab+1]] <<- containertab
				tkselect(tknotes, ntab)
			}
			InsertMessagesTxt(main.txt.out, "Temperature QC finished!")
		}
		return(sortieqc)
	}

	#############################################################
	#homogenization
	if(GeneralParameters$action == 'homog'){
		RetHom <- try(ExecHomData(get.stn), silent = TRUE)
		if(inherits(RetHom, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Homogeneity test failed for", get.stn), format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', RetHom[1]), format = TRUE)
			return(NULL)
		}else{
			freqdata <- GeneralParameters$period
			outsdir <- file.path(EnvHomogzData$baseDir, 'Outputs', get.stn)
			if(!file.exists(outsdir)) dir.create(outsdir, showWarnings = FALSE, recursive = TRUE)

			fileout <- file.path(outsdir, paste(get.stn, '.Rdata', sep = ''))
			ReturnExecResults <- list(action = GeneralParameters$action, period = GeneralParameters$period,
										station = get.stn, res = RetHom$breakpts, outputdir = outsdir,
										refSerie = list(dyref = RetHom$dyref, dkref = RetHom$dkref, moref = RetHom$moref))
			breakpts <- getBreakpointsData(ReturnExecResults)
			save(ReturnExecResults, file = fileout)

			monfileout <- file.path(outsdir, paste(get.stn, '_MON.txt', sep = ''))
			monfileout0 <- file.path(outsdir, paste(get.stn, '_MON0.txt', sep = ''))
			write.table(breakpts$monbreakpts, monfileout0, col.names = TRUE, row.names = FALSE)
			file.copy(monfileout0, monfileout, overwrite = TRUE)
			if(freqdata != 'monthly'){
				dekfileout <- file.path(outsdir, paste(get.stn, '_DEK.txt', sep = ''))
				dekfileout0 <- file.path(outsdir, paste(get.stn, '_DEK0.txt', sep = ''))
				write.table(breakpts$dekbreakpts, dekfileout0, col.names = TRUE, row.names = FALSE)
				file.copy(dekfileout0, dekfileout, overwrite = TRUE)
				if(freqdata == 'daily'){
					dlyfileout <- file.path(outsdir, paste(get.stn, '_DLY.txt', sep = ''))
					dlyfileout0 <- file.path(outsdir, paste(get.stn, '_DLY0.txt', sep = ''))
					write.table(breakpts$dlybreakpts, dlyfileout0, col.names = TRUE, row.names = FALSE)
					file.copy(dlyfileout0, dlyfileout, overwrite = TRUE)
				}
			}
			InsertMessagesTxt(main.txt.out, paste("Homogeneity test finished successfully for", get.stn))
			return(ReturnExecResults)
		}
	}

	################################################################################
	
	if(GeneralParameters$action == "chk.coords"){
		RetChkCrd <- try(excludeOutStnFun(GeneralParameters), silent = TRUE)
		if(!inherits(RetChkCrd, "try-error")){
			if(is.na(RetChkCrd$Stndoute[1, 1])) InsertMessagesTxt(main.txt.out, "All station's coordinates are OK!")
			GeneralParameters$period <<- RetChkCrd$period
			InsertMessagesTxt(main.txt.out, paste("Stations coordinates checked successfully for",
								getf.no.ext(GeneralParameters$IO.files$STN.file)))
			return(RetChkCrd)
		}else{
			InsertMessagesTxt(main.txt.out, paste("Stations coordinates checking failed",
								getf.no.ext(GeneralParameters$IO.files$STN.file)), format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', RetChkCrd[1]), format = TRUE)
			return(NULL)
		}
	}

	####################################################################

	##Output message
	Execute_end_msg <- function(outret, msgOK, msgFail){
		if(!inherits(outret, "try-error")){
			if(!is.null(outret)){
				if(outret == 0) InsertMessagesTxt(main.txt.out, msgOK)
				else InsertMessagesTxt(main.txt.out, msgFail, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msgFail, format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, msgFail, format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', outret[1]), format = TRUE)
		}
		return(NULL)
	}

	####################################################################
	##Merge Rainfall

	# Mering rainfall once
	if(GeneralParameters$action == 'merge.rain.one'){
		daty <- GeneralParameters$Merging.Date
		xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
		xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
		if(GeneralParameters$period == 'daily') daty <- seq(xdeb, xfin, 'day')
		if(GeneralParameters$period == 'monthly') daty <- seq(xdeb, xfin, 'month')
		if(GeneralParameters$period == 'pentad'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 6]
		}
		if(GeneralParameters$period == 'dekadal'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 3]
		}
		daty <- daty[as.numeric(format(daty, '%m'))%in%GeneralParameters$Merging.Date$Months]
		if(GeneralParameters$period == 'daily'){
			xdeb <- format(daty[1], '%Y%m%d')
			xfin <- format(daty[length(daty)], '%Y%m%d')
		}
		if(GeneralParameters$period%in%c('pentad', 'dekadal')){
			xdeb <- paste0(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')))
			xfin <- paste0(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')))
		}
		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}
		origdir <- file.path(GeneralParameters$output$dir, paste('Merging_Precip_Data', xdeb, xfin, sep = '_'))
		ret <- try(Precip_Merging_ALL(origdir), silent = TRUE)

		msg0 <- "Rainfall merging finished successfully"
		msg1 <- "Rainfall merging failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###############################
	##compute mean Gauge-RFE bias
	if(GeneralParameters$action == 'coefbias.rain'){
		origdir <- file.path(GeneralParameters$output$dir, paste0('BIAS_Data_',
							 getf.no.ext(GeneralParameters$STN.file)))
		ret <- try(execBiasRain(origdir), silent = TRUE)

		msg0 <- "Computing Gauge-RFE bias finished successfully"
		msg1 <- "Computing Gauge-RFE bias failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###############################
	##Adjust bias
	if(GeneralParameters$action == 'rmbias.rain'){
		daty <- GeneralParameters$Adjust.Date
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste0(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
			xfin <- paste0(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
		}else{
			xdeb <- paste0(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
			xfin <- paste0(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
		}
		origdir <- file.path(GeneralParameters$output$dir, paste('ADJUSTED_Precip_Data', xdeb, xfin, sep = '_'))
		ret <- try(execAdjBiasRain(origdir), silent = TRUE)

		msg0 <- "Adjusting Gauge-RFE bias finished successfully"
		msg1 <- "Adjusting Gauge-RFE bias failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###############################
	##compute spatio-temporal LM coeff
	if(GeneralParameters$action == 'coefLM.rain'){
		origdir <- file.path(GeneralParameters$output$dir, paste0('LMCOEF_Data_',
							getf.no.ext(GeneralParameters$STN.file)))
		ret <- try(execLMCoefRain(origdir), silent = TRUE)

		msg0 <- "Computing LM Coefficients finished successfully"
		msg1 <- "Computing LM Coefficients failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###############################
	##Merging
	if(GeneralParameters$action == 'merge.rain'){
		daty <- GeneralParameters$Merging.Date
		xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
		xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
		if(GeneralParameters$period == 'daily') daty <- seq(xdeb, xfin, 'day')
		if(GeneralParameters$period == 'pentad'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 6]
		}
		if(GeneralParameters$period == 'dekadal'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 3]
		}
		if(GeneralParameters$period == 'monthly') daty <- seq(xdeb, xfin, 'month')

		daty <- daty[as.numeric(format(daty, '%m'))%in%GeneralParameters$Merging.Date$Months]
		if(GeneralParameters$period == 'daily'){
			xdeb <- format(daty[1], '%Y%m%d')
			xfin <- format(daty[length(daty)], '%Y%m%d')
		}

		if(GeneralParameters$period%in%c('pentad', 'dekadal')){
			xdeb <- paste0(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')))
			xfin <- paste0(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')))
		}

		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}
		origdir <- file.path(GeneralParameters$output$dir, paste("Merged_Precip_Data", xdeb, xfin, sep = '_'))
		ret <- try(execMergeRain(origdir), silent = TRUE)

		msg0 <- "Rainfall merging finished successfully"
		msg1 <- "Rainfall merging failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###############################
	####Merging 1 dekad
	if(GeneralParameters$action == 'merge.dekrain'){
		ret <- try(mergeOneDekadRain(), silent = TRUE)

		msg0 <- "Rainfall merging finished successfully"
		msg1 <- "Rainfall merging failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#########################################################################
	#Merge  temperature using reanalysis

	##compute regression coef
	if(GeneralParameters$action == 'coefdown.temp'){
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('CoefDownTemp',
							getf.no.ext(GeneralParameters$IO.files$STN.file), sep = '_'))
		ret <- try(Temp_execCoefDown(origdir), silent = TRUE)

		msg0 <- "Computing regression parameters finished successfully"
		msg1 <- "Computing regression parameters failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	##############################
	#downscaling
	if(GeneralParameters$action == 'down.temp'){
		daty <- GeneralParameters$Down.Date.Range
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste0(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
			xfin <- paste0(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
		}else{
			xdeb <- paste0(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
			xfin <- paste0(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
		}
		origdir <- file.path(GeneralParameters$output$dir, paste('Downscaled_Reanalysis', xdeb, xfin, sep = '_'))
		ret <- try(Temp_execDownscaling(origdir), silent = TRUE)

		msg0 <- "Downscaling finished successfully"
		msg1 <- "Downscaling failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	##############################
	# Mering temperature once
	if(GeneralParameters$action == 'merge.temp.one'){
		daty <- GeneralParameters$Merging.Date
		xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
		xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
		if(GeneralParameters$period == 'daily') daty <- seq(xdeb, xfin, 'day')
		if(GeneralParameters$period == 'monthly') daty <- seq(xdeb, xfin, 'month')
		if(GeneralParameters$period == 'pentad'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 6]
		}
		if(GeneralParameters$period == 'dekadal'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 3]
		}
		daty <- daty[as.numeric(format(daty, '%m'))%in%GeneralParameters$Merging.Date$Months]
		if(GeneralParameters$period == 'daily'){
			xdeb <- format(daty[1], '%Y%m%d')
			xfin <- format(daty[length(daty)], '%Y%m%d')
		}
		if(GeneralParameters$period%in%c('pentad', 'dekadal')){
			xdeb <- paste0(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')))
			xfin <- paste0(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')))
		}
		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}

		origdir <- file.path(GeneralParameters$output$dir, paste('Merging_Temp_Data', xdeb, xfin, sep = '_'))
		ret <- try(Temp_Merging_ALL(origdir), silent = TRUE)

		msg0 <- "Temperature merging finished successfully"
		msg1 <- "Temperature merging failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	##############################
	##compute mean bias coef
	if(GeneralParameters$action == 'coefbias.temp'){
		origdir <- file.path(GeneralParameters$output$dir, paste0('BIAS_Data_',
							getf.no.ext(GeneralParameters$STN.file)))
		ret <- try(execBiasTemp(origdir), silent = TRUE)

		msg0 <- "Computing bias coefficients finished successfully"
		msg1 <- "Computing bias coefficients failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	##############################
	##bias correction
	if(GeneralParameters$action == 'adjust.temp'){
		daty <- GeneralParameters$Adjust.Date
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste0(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
			xfin <- paste0(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
		}else{
			xdeb <- paste0(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
			xfin <- paste0(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
		}
		origdir <- file.path(GeneralParameters$output$dir, paste('ADJUSTED_Temp_Data', xdeb, xfin, sep = '_'))
		ret <- try(execAjdBiasDownTemp(origdir), silent = TRUE)

		msg0 <- "Adjustment of downscaled data finished successfully"
		msg1 <- "Adjustment of downscaled data failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###############################
	##compute spatio-temporal LM coeff
	if(GeneralParameters$action == 'coefLM.temp'){
		origdir <- file.path(GeneralParameters$output$dir, paste0('LMCOEF_Data_',
							getf.no.ext(GeneralParameters$STN.file)))
		ret <- try(execLMCoefTemp(origdir), silent = TRUE)

		msg0 <- "Computing LM Coefficients finished successfully"
		msg1 <- "Computing LM Coefficients failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	##############################
	##Merging
	if(GeneralParameters$action == 'merge.temp'){
		daty <- GeneralParameters$Merging.Date
		xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
		xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
		if(GeneralParameters$period == 'daily') daty <- seq(xdeb, xfin, 'day')
		if(GeneralParameters$period == 'pentad'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 6]
		}
		if(GeneralParameters$period == 'dekadal'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 3]
		}
		if(GeneralParameters$period == 'monthly') daty <- seq(xdeb, xfin, 'month')

		daty <- daty[as.numeric(format(daty, '%m'))%in%GeneralParameters$Merging.Date$Months]
		if(GeneralParameters$period == 'daily'){
			xdeb <- format(daty[1], '%Y%m%d')
			xfin <- format(daty[length(daty)], '%Y%m%d')
		}

		if(GeneralParameters$period%in%c('pentad', 'dekadal')){
			xdeb <- paste0(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')))
			xfin <- paste0(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')))
		}

		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}
		origdir <- file.path(GeneralParameters$output$dir, paste("Merged_Temp_Data", xdeb, xfin, sep = '_'))
		ret <- try(execMergeTemp(origdir), silent = TRUE)

		msg0 <- "Temperature merging finished successfully"
		msg1 <- "Temperature merging failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	##############################
	##Scale merged data
	if(GeneralParameters$action == 'scale.merged'){
		origdir <- file.path(GeneralParameters$outdir, "Merged_ScaledData")
		ret <- try(exec_ScalingUpData(origdir), silent = TRUE)

		msg0 <- "Scaling up merged data finished successfully"
		msg1 <- "Scaling up merged data failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	################################################################################

	if(GeneralParameters$action == "agg.qc"){
		ret <- try(AggregateQcData(), silent = TRUE)

		msg0 <- "Aggregation finished successfully"
		msg1 <- "Aggregation failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "agg.zc"){
		ret <- try(AggregateZeroChkData(), silent = TRUE)

		msg0 <- "Aggregation finished successfully"
		msg1 <- "Aggregation failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "agg.hom"){
		orgdir <- file.path(as.character(GeneralParameters$file.io),'Data')
		agg.hom.Fun <- if(file.exists(orgdir)) AggregateHomData else AggregateHomData0
		ret <- try(agg.hom.Fun(), silent = TRUE)

		msg0 <- "Aggregation finished successfully"
		msg1 <- "Aggregation failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "cdtInput.stn"){		
		if(GeneralParameters$data.type == "Multiple Files") cdtInput.stn.Fun <- formatCDTDataMultiple.Files
		if(GeneralParameters$data.type == "Single File") cdtInput.stn.Fun <- formatCDTDataSingle.File
		ret <- try(cdtInput.stn.Fun(GeneralParameters), silent = TRUE)

		msg0 <- "Conversion to CDT data finished successfully"
		msg1 <- "Conversion to CDT data failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "aggregate.ts"){
		ret <- try(AggregateTS_Execute(GeneralParameters), silent = TRUE)

		convert2 <- paste("Conversion from", GeneralParameters$in.tstep, "to", GeneralParameters$out.tstep)
		msg0 <- paste(convert2, "data finished successfully")
		msg1 <- paste(convert2, "data failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "aggregate.nc"){
		ret <- try(AggregateSpNc_Execute(GeneralParameters), silent = TRUE)

		msg0 <- "NetCDF regridding finished successfully"
		msg1 <- "NetCDF regridding failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "fill.temp"){
		ret <- try(fillDekTempMissVal(GeneralParameters), silent = TRUE)

		msg0 <- "Filling missing dekadal temperature values finished successfully"
		msg1 <- "Filling missing dekadal temperature values failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "create.CdtDataset"){
		ret <- try(cdtDataset_readData(GeneralParameters), silent = TRUE)

		msg0 <- "CDT dataset creation finished successfully"
		msg1 <- "CDT dataset creation values failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###################

	if(GeneralParameters$action == "compute.dervTemp"){
		ret <- try(computeTvarsProcs(GeneralParameters), silent = TRUE)

		msg <- paste("Computing", GeneralParameters$Tstep, tolower(GeneralParameters$variable), "temperature")
		msg0 <- paste(msg, "finished successfully")
		msg1 <- paste(msg, "failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "compute.PET"){
		ret <- try(computePETProcs(GeneralParameters), silent = TRUE)

		msg <- paste("Computing", GeneralParameters$Tstep, "potential evapotranspiration")
		msg0 <- paste(msg, "finished successfully")
		msg1 <- paste(msg, "failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "compute.WB"){
		ret <- try(computeWBProcs(GeneralParameters), silent = TRUE)

		msg <- paste("Computing", GeneralParameters$Tstep, "water balance")
		msg0 <- paste(msg, "finished successfully")
		msg1 <- paste(msg, "failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	###################

	if(GeneralParameters$action == "convert.CPTdata"){
		ret <- try(CPT.convertProcs(GeneralParameters), silent = TRUE)

		msg0 <- "Converting data into CPT data format finished successfully"
		msg1 <- "Converting data into CPT data format failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "convert.nc.tif.bil"){
		ret <- try(rasterData.convert_Proc(GeneralParameters), silent = TRUE)

		msg0 <- "Converting data format finished successfully"
		msg1 <- "Converting  data format failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	#####
	if(GeneralParameters$action == "grads.ctl"){
		ret <- try(grads_create.ctl_Procs(GeneralParameters), silent = TRUE)

		msg0 <- "Creating GrADS data descriptor file finished successfully"
		msg1 <- "Creating GrADS data descriptor file failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###################

}

