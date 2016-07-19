Execute_All_Functions <- function(get.stn){
	#QC Rain
	if(GeneralParameters$action == 'qc.rain'){
		sortieqc <- ExecQcRain(get.stn)
		if(GeneralParameters$AllOrOne == 'all'){
			outpdir <- file.path(EnvQcOutlierData$baseDir, 'Outputs', fsep = .Platform$file.sep)
			ggdir <- list.files(outpdir)
			failedqc <- lapply(ggdir, function(lstn){
				ggfileout <- file.path(outpdir, lstn, paste(lstn,'.txt', sep = ''), fsep = .Platform$file.sep)
				file.exists(ggfileout)
			})
			failedqc <- ggdir[!unlist(failedqc)]
			if(length(failedqc) > 0){
				failedqc <- data.frame(failedqc)
				names(failedqc) <- 'QC failed: Stations not checked'
				containertab <- displayConsOutputTabs(tknotes, failedqc, title = paste(getf.no.ext(as.character(GeneralParameters$file.io$Values[1])),'Failed'))
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
				title = paste(getf.no.ext(as.character(GeneralParameters$file.io$Values[1])),'Failed'))
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
			outpdir <- file.path(EnvQcOutlierData$baseDir, 'Outputs', fsep = .Platform$file.sep)
			ggdir <- list.files(outpdir)
			failedqc <- lapply(ggdir, function(lstn){
				ggfileout <- file.path(outpdir, lstn, paste(lstn,'.txt', sep = ''), fsep = .Platform$file.sep)
				file.exists(ggfileout)
			})
			failedqc <- ggdir[!unlist(failedqc)]

			if(length(failedqc) > 0){
				failedqc <- data.frame(failedqc)
				names(failedqc) <- 'QC failed: Stations not checked'
				containertab <- displayConsOutputTabs(tknotes, failedqc, title = paste(getf.no.ext(as.character(GeneralParameters$file.io$Values[1])),'Failed'))
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
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',RetHom[1]), format = TRUE)
			return(NULL)
		}else{
			freqdata <- GeneralParameters$period
			outsdir <- file.path(EnvHomogzData$baseDir, 'Outputs', get.stn, fsep = .Platform$file.sep)
			if(!file.exists(outsdir)) dir.create(outsdir, showWarnings = FALSE, recursive = TRUE)

			fileout <- file.path(outsdir, paste(get.stn,'.Rdata', sep = ''), fsep = .Platform$file.sep)
			ReturnExecResults <- list(action = GeneralParameters$action, period = GeneralParameters$period, station = get.stn, res = RetHom$breakpts, outputdir = outsdir, refSerie = list(dyref = RetHom$dyref, dkref = RetHom$dkref, moref = RetHom$moref))
			breakpts <- getBreakpointsData(ReturnExecResults)
			save(ReturnExecResults, file = fileout)

			monfileout <- file.path(outsdir, paste(get.stn, '_MON.txt', sep = ''), fsep = .Platform$file.sep)
			monfileout0 <- file.path(outsdir, paste(get.stn, '_MON0.txt', sep = ''), fsep = .Platform$file.sep)
			write.table(breakpts$monbreakpts, monfileout0, col.names = TRUE, row.names = FALSE)
			file.copy(monfileout0, monfileout, overwrite = TRUE)
			if(freqdata != 'monthly'){
				dekfileout <- file.path(outsdir, paste(get.stn, '_DEK.txt', sep = ''), fsep = .Platform$file.sep)
				dekfileout0 <- file.path(outsdir, paste(get.stn, '_DEK0.txt', sep = ''), fsep = .Platform$file.sep)
				write.table(breakpts$dekbreakpts, dekfileout0, col.names = TRUE, row.names = FALSE)
				file.copy(dekfileout0, dekfileout, overwrite = TRUE)
				if(freqdata == 'daily'){
					dlyfileout <- file.path(outsdir, paste(get.stn, '_DLY.txt', sep = ''), fsep = .Platform$file.sep)
					dlyfileout0 <- file.path(outsdir, paste(get.stn, '_DLY0.txt', sep = ''), fsep = .Platform$file.sep)
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
					InsertMessagesTxt(outtxt, gsub('[\r\n]','',outret[1]), format = TRUE)
				}
			}else{
				InsertMessagesTxt(outtxt, msgFail, format = TRUE)
				InsertMessagesTxt(outtxt, gsub('[\r\n]','',outret[1]), format = TRUE)
			}
		}else{
			InsertMessagesTxt(outtxt, msgFail, format = TRUE)
			InsertMessagesTxt(outtxt, gsub('[\r\n]','',outret[1]), format = TRUE)
		}
	}
	
	############################### 
	##Merge Rainfall

	##compute mean Gauge-RFE bias
	if(GeneralParameters$action == 'coefbias.rain'){
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[5]), paste('MeanBiasGGRFE', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execBiasRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing mean Gauge-RFE bias finished successfully", "Computing mean Gauge-RFE bias failed")
	}

	###############################
	##Adjust bias
	if(GeneralParameters$action == 'rmbias.rain'){
		daty <- as.character(GeneralParameters$dates.adj$Values)
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}else{
			xdeb <- paste(daty[3], format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(daty[6], format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('Adjusted_RFE_Data', xdeb, xfin, sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execAdjBiasRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Adjusting mean Gauge-RFE bias finished successfully", "Adjusting mean Gauge-RFE bias failed")
	}

	###############################
	##Merging
	if(GeneralParameters$action == 'merge.rain'){
		daty <- as.character(GeneralParameters$dates.mrg$Values)
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}else{
			xdeb <- paste(daty[3], format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(daty[6], format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[5]), paste('Merged_RR_Data', xdeb, xfin, sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execMergeRain(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Rainfall merging finished successfully", "Rainfall merging failed")
	}

	###############################
	####Merging 1 dekad
	if(GeneralParameters$action == 'merge.dekrain'){
		mrg2run <- try(mergeOneDekadRain(), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Rainfall merging finished successfully", "Rainfall merging failed")
	}

	###############################
	####Merging Mali 1 dekad
	if(GeneralParameters$action == 'mali.dekrain'){
		daty <- str_trim(as.character(GeneralParameters$dates.mrg$Values))
		daty <- paste(daty[3], format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[3]), paste('MALI_DEK', daty, sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(update1DekProc_Mali(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Rainfall merging finished successfully", "Rainfall merging failed")
	}

	#########################################################################
	#Merge  temperature using reanalysis

	##compute regression coef
	if(GeneralParameters$action == 'coefdown.temp'){
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[3]), paste('CoefDownTemp', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execCoefDownTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing regression parameters finished successfully", "Computing regression parameters failed")
	}

	##############################
	#downscaling
	if(GeneralParameters$action == 'down.temp'){
		daty <- as.character(GeneralParameters$dates.down$Values)
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}else{
			xdeb <- paste(daty[3], format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(daty[6], format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[5]), paste('Downscaled_Reanalysis_Data', xdeb, xfin, sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execDownscalingTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Downscaling finished successfully", "Downscaling failed")
	}

	##############################
	##compute mean bias coef
	if(GeneralParameters$action == 'coefbias.temp'){
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('CoefBiasAdjTemp', as.character(GeneralParameters$bias.method),
		getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execCoefBiasCompute(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Computing bias coefficients finished successfully", "Computing bias coefficients failed")
	}

	##############################
	##bias correction
	if(GeneralParameters$action == 'adjust.temp'){
		daty <- as.character(GeneralParameters$dates.adj$Values)
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}else{
			xdeb <- paste(daty[3], format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(daty[6], format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[5]), paste('Adjusted_Temp_Data', xdeb, xfin, sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execAjdBiasDownTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Adjustment of downscaled data finished successfully", "Adjustment of downscaled data failed")
	}

	##############################
	##Merging
	if(GeneralParameters$action == 'merge.temp'){
		daty <- as.character(GeneralParameters$dates.mrg$Values)
		if(GeneralParameters$period == 'monthly'){
			xdeb <- paste(format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}else{
			xdeb <- paste(daty[3], format(ISOdate(2014, daty[2], 1), "%b"), daty[1], sep = '')
			xfin <- paste(daty[6], format(ISOdate(2014, daty[5], 1), "%b"), daty[4], sep = '')
		}
		origdir <- file.path(as.character(GeneralParameters$file.io$Values[5]), paste('Merged_Temp_Data', xdeb, xfin, sep = '_'), fsep = .Platform$file.sep)
		mrg2run <- try(execMergeTemp(origdir), silent = TRUE)
		merging_end_msg(mrg2run, main.txt.out, "Temperature merging finished successfully", "Temperature merging failed")
	}

	################################################################################
	
	if(GeneralParameters$action == "chk.coords"){
		chk2run <- try(RetChkCrd <- excludeOutStnFun(GeneralParameters), silent = TRUE)
		if(!inherits(chk2run, "try-error")){
			if(is.na(RetChkCrd$Stndoute[1,1])) InsertMessagesTxt(main.txt.out, "All station's coordinates are OK!")
			GeneralParameters$period <<- RetChkCrd$period
			InsertMessagesTxt(main.txt.out, paste("Stations coordinates checked successfully for", getf.no.ext(as.character(GeneralParameters$file.io$Values[1]))))
			return(RetChkCrd)
		}else{
			InsertMessagesTxt(main.txt.out, paste("Stations coordinates checking failed", getf.no.ext(as.character(GeneralParameters$file.io$Values[1]))), format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',chk2run[1]), format = TRUE)
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
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',agg2run[1]), format = TRUE)
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
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "agg.hom"){
		orgdir <- file.path(as.character(GeneralParameters$file.io),'Data', fsep = .Platform$file.sep)
		if(file.exists(orgdir)) agg2run <- try(AggregateHomData(), silent = TRUE)
		else agg2run <- try(AggregateHomData0(), silent = TRUE)

		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) 	InsertMessagesTxt(main.txt.out, "Aggregation finished successfully")
				else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			} else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "agg.stn"){
		agg2run <- try(AggregateDataCDT(GeneralParameters), silent = TRUE)
		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) 	InsertMessagesTxt(main.txt.out, "Aggregation finished successfully")
				else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			}else InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Aggregation failed", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	#####
	if(GeneralParameters$action == "agg.ts"){
		agg2run <- try(ExeAggTimeSeries(GeneralParameters), silent = TRUE)
		if(!inherits(agg2run, "try-error")){
			if(!is.null(agg2run)){
				if(agg2run == 0) InsertMessagesTxt(main.txt.out, paste("Conversion from", as.character(GeneralParameters$period), "to", as.character(GeneralParameters$period1), "data finished successfully"))
				else InsertMessagesTxt(main.txt.out, paste("Conversion from", as.character(GeneralParameters$period), "to", as.character(GeneralParameters$period1), "data failed"), format = TRUE)
			}else InsertMessagesTxt(main.txt.out, paste("Conversion from", as.character(GeneralParameters$period), "to", as.character(GeneralParameters$period1), "data failed"), format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, paste("Conversion from", as.character(GeneralParameters$period), "to", as.character(GeneralParameters$period1), "data failed"), format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',agg2run[1]), format = TRUE)
		}
		return(NULL)
	}

	###################

}

