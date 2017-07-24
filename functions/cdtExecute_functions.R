Execute_All_Functions <- function(get.stn){
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

	####################################################################

	##Output message
	merging_end_msg <- function(outret, outtxt, msgOK, msgFail){
		if(!inherits(outret, "try-error")){
			if(!is.null(outret)){
				if(outret == 0) InsertMessagesTxt(outtxt, msgOK)
				else{
					InsertMessagesTxt(outtxt, msgFail, format = TRUE)
					InsertMessagesTxt(outtxt, gsub('[\r\n]', '', outret[1]), format = TRUE)
				}
			}else{
				InsertMessagesTxt(outtxt, msgFail, format = TRUE)
				InsertMessagesTxt(outtxt, gsub('[\r\n]', '', outret[1]), format = TRUE)
			}
		}else{
			InsertMessagesTxt(outtxt, msgFail, format = TRUE)
			InsertMessagesTxt(outtxt, gsub('[\r\n]', '', outret[1]), format = TRUE)
		}
	}
	
	############################### 
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
			xdeb <- paste(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')), sep = '')
			xfin <- paste(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')), sep = '')
		}
		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}
		origdir <- file.path(GeneralParameters$output$dir, paste('Merging_Precip_Data', xdeb, xfin, sep = '_'))
		mrg2run <- try(Precip_Merging_ALL(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Rainfall merging finished successfully", "Rainfall merging failed")
	}

	###############################
	##compute mean Gauge-RFE bias
	if(GeneralParameters$action == 'coefbias.rain'){
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('STN_RFE_Bias',
								getf.no.ext(GeneralParameters$IO.files$STN.file), sep = '_'))
		mrg2run <- try(execBiasRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing Gauge-RFE bias finished successfully", "Computing Gauge-RFE bias failed")
	}

	###############################
	##Adjust bias
	if(GeneralParameters$action == 'rmbias.rain'){

		daty <- GeneralParameters$Adjust.Date.Range
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}else{
			xdeb <- paste(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('Adjusted_RFE_Data', xdeb, xfin, sep = '_'))
		mrg2run <- try(execAdjBiasRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Adjusting Gauge-RFE bias finished successfully", "Adjusting Gauge-RFE bias failed")
	}

	###############################
	##compute spatio-temporal LM coeff
	if(GeneralParameters$action == 'coefLM.rain'){
		origdir <- file.path(GeneralParameters$IO.files$dir2save,
							paste('LMCoef', getf.no.ext(GeneralParameters$IO.files$STN.file), sep = '_'))
		mrg2run <- try(execLMCoefRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing LM Coefficients finished successfully", "Computing LM Coefficients failed")
	}

	###############################
	##Merging
	if(GeneralParameters$action == 'merge.rain'){
		daty <- GeneralParameters$Mrg.Date.Range
		xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
		xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
		if(GeneralParameters$period == 'daily') daty <- seq(xdeb, xfin, 'day')
		if(GeneralParameters$period == 'monthly') daty <- seq(xdeb, xfin, 'month')
		if(GeneralParameters$period == 'dekadal'){
			daty <- seq(xdeb, xfin, 'day')
			daty <- daty[as.numeric(format(daty, '%d')) <= 3]
		}
		daty <- daty[as.numeric(format(daty, '%m'))%in%GeneralParameters$Mrg.Months]
		if(GeneralParameters$period == 'daily'){
			xdeb <- format(daty[1], '%Y%m%d')
			xfin <- format(daty[length(daty)], '%Y%m%d')
		}
		if(GeneralParameters$period == 'dekadal'){
			xdeb <- paste(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')), sep = '')
			xfin <- paste(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')), sep = '')
		}
		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('Merged_RR_Data', xdeb, xfin, sep = '_'))
		mrg2run <- try(execMergeRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Rainfall merging finished successfully", "Rainfall merging failed")
	}

	###############################
	####Merging 1 dekad
	if(GeneralParameters$action == 'merge.dekrain'){
		mrg2run <- try(mergeOneDekadRain(), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Rainfall merging finished successfully", "Rainfall merging failed")
	}

	#########################################################################
	#Merge  temperature using reanalysis

	##compute regression coef
	if(GeneralParameters$action == 'coefdown.temp'){
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('CoefDownTemp', getf.no.ext(GeneralParameters$IO.files$STN.file), sep = '_'))
		mrg2run <- try(Temp_execCoefDown(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing regression parameters finished successfully", "Computing regression parameters failed")
	}

	##############################
	#downscaling
	if(GeneralParameters$action == 'down.temp'){
		daty <- GeneralParameters$Down.Date.Range
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}else{
			xdeb <- paste(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}
		origdir <- file.path(GeneralParameters$output$dir, paste('Downscaled_Reanalysis', xdeb, xfin, sep = '_'))
		mrg2run <- try(Temp_execDownscaling(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Downscaling finished successfully", "Downscaling failed")
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
			xdeb <- paste(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')), sep = '')
			xfin <- paste(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')), sep = '')
		}
		if(GeneralParameters$period == 'monthly'){
			xdeb <- format(daty[1], '%Y%m')
			xfin <- format(daty[length(daty)], '%Y%m')
		}

		origdir <- file.path(GeneralParameters$output$dir, paste('Merging_Temp_Data', xdeb, xfin, sep = '_'))
		mrg2run <- try(Temp_Merging_ALL(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Temperature merging finished successfully", "Temperature merging failed")
	}

	##############################
	##compute mean bias coef
	if(GeneralParameters$action == 'coefbias.temp'){
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('STN_REANAL_Bias', getf.no.ext(GeneralParameters$IO.files$STN.file), sep = '_'))
		mrg2run <- try(execBiasTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing bias coefficients finished successfully", "Computing bias coefficients failed")
	}

	##############################
	##bias correction
	if(GeneralParameters$action == 'adjust.temp'){
		daty <- GeneralParameters$Adjust.Date.Range
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}else{
			xdeb <- paste(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('Adjusted_Temp_Data', xdeb, xfin, sep = '_'))
		mrg2run <- try(execAjdBiasDownTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Adjustment of downscaled data finished successfully", "Adjustment of downscaled data failed")
	}

	###############################
	##compute spatio-temporal LM coeff
	if(GeneralParameters$action == 'coefLM.temp'){
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('LMCoef', getf.no.ext(GeneralParameters$IO.files$STN.file), sep = '_'))
		mrg2run <- try(execLMCoefTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing LM Coefficients finished successfully", "Computing LM Coefficients failed")
	}

	##############################
	##Merging
	if(GeneralParameters$action == 'merge.temp'){
		daty <- GeneralParameters$Mrg.Date.Range
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}else{
			xdeb <- paste(daty$start.dek, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year, sep = '')
			xfin <- paste(daty$end.dek, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year, sep = '')
		}
		origdir <- file.path(GeneralParameters$IO.files$dir2save, paste('Merged_Temp_Data', xdeb, xfin, sep = '_'))
		mrg2run <- try(execMergeTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Temperature merging finished successfully", "Temperature merging failed")
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

	#####
	if(GeneralParameters$action == "agg.qc"){
		agg2run <- try(AggregateQcData(), silent = TRUE)
		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) InsertMessagesTxt(main.txt.out, "Aggregation finished successfully")
				else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			} else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "agg.zc"){
		agg2run <- try(AggregateZeroChkData(), silent = TRUE)
		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) InsertMessagesTxt(main.txt.out, "Aggregation finished successfully")
				else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			} else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "agg.hom"){
		orgdir <- file.path(as.character(GeneralParameters$file.io),'Data')
		if(file.exists(orgdir)) agg2run <- try(AggregateHomData(), silent = TRUE)
		else agg2run <- try(AggregateHomData0(), silent = TRUE)

		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) 	InsertMessagesTxt(main.txt.out, "Aggregation finished successfully")
				else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			} else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "cdtInput.stn"){		
		if(GeneralParameters$data.type == "Multiple Files") conversionFunc <- "formatCDTDataMultiple.Files"
		if(GeneralParameters$data.type == "Single File") conversionFunc <- "formatCDTDataSingle.File"
		conversionFunc <- match.fun(conversionFunc)

		agg2run <- try(conversionFunc(GeneralParameters), silent = TRUE)
		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) 	InsertMessagesTxt(main.txt.out, "Conversion to CDT data finished successfully")
				else InsertMessagesTxt(main.txt.out, "Conversion to CDT data failed", format = TRUE)
			}else InsertMessagesTxt(main.txt.out, "Conversion to CDT data failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Conversion to CDT data failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "agg.ts"){
		agg2run <- try(ExeAggTimeSeries(GeneralParameters), silent = TRUE)
		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) InsertMessagesTxt(main.txt.out, paste("Conversion from", GeneralParameters$period, "to", GeneralParameters$output.time, "data finished successfully"))
				else InsertMessagesTxt(main.txt.out, paste("Conversion from", GeneralParameters$period, "to", GeneralParameters$output.time, "data failed"), format = TRUE)
			}else InsertMessagesTxt(main.txt.out, paste("Conversion from", GeneralParameters$period, "to", GeneralParameters$output.time, "data failed"), format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, paste("Conversion from", GeneralParameters$period, "to", GeneralParameters$output.time, "data failed"), format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "fill.temp"){
		retfill <- try(fillDekTempMissVal(GeneralParameters), silent = TRUE)
		if(!inherits(retfill, "try-error")){
			if(!is.null(retfill)){
				if(retfill == 0) InsertMessagesTxt(main.txt.out, "Filling missing dekadal temperature values finished successfully")
				else InsertMessagesTxt(main.txt.out, "Filling missing dekadal temperature values failed", format = TRUE)
			}else InsertMessagesTxt(main.txt.out, "Filling missing dekadal temperature values failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Filling missing dekadal temperature values failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', retfill[1]), format = TRUE)
		}
		return(NULL)
	}
	###################

}

