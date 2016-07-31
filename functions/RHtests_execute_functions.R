#donnees <- testdon

executeOnFindU <- function(donnees, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datmon[,xpos]
	}else{
		xval <- donnees$datmon
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(min(xval, na.rm = T) > 0) Inxval <- log(xval)
	 	else Inxval <- log(xval+1)
	}else Inxval <- xval

	InSeries <- cbind(as.numeric(substr(donnees$modates, 1,4)), as.numeric(substr(donnees$modates, 5,6)), 0, Inxval)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	ret <- FindU(InSeries, fileout, sel.stn, MissingValueCode = donnees$MissingValue, p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'FindU failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'FindU', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}


#####
executeOnFindUD <- function(donnees, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datmon[,xpos]
	}else{
		xval <- donnees$datmon
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(min(xval, na.rm = T) > 0) Inxval <- log(xval)
	 	else Inxval <- log(xval+1)
	}else Inxval <- xval

	InSeries <- cbind(as.numeric(substr(donnees$modates, 1,4)), as.numeric(substr(donnees$modates, 5,6)), 0, Inxval)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	ret <- FindUD(InSeries, InCs, fileout, sel.stn, MissingValueCode = donnees$MissingValue, p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'FindUD failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'FindUD', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}

#####
executeOnStepSize <- function(donnees, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datmon[,xpos]
	}else{
		xval <- donnees$datmon
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(min(xval, na.rm = T) > 0) Inxval <- log(xval)
	 	else Inxval <- log(xval+1)
	}else Inxval <- xval

	InSeries <- cbind(as.numeric(substr(donnees$modates, 1,4)), as.numeric(substr(donnees$modates, 5,6)), 0, Inxval)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	ret <- StepSize(InSeries, InCs, fileout, sel.stn, MissingValueCode = donnees$MissingValue, p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'StepSize failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'StepSize', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}

############
executeOnQMadj <- function(donnees, GeneralParameters, choix){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	rootdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'), fsep = .Platform$file.sep)
	outdir <- file.path(rootdir, 'Outputs', fsep = .Platform$file.sep)
	adjdir <- file.path(rootdir, 'AdjustedData', fsep = .Platform$file.sep)


	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))
	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0")  xpos <- which(as.character(donnees$IDs) == sel.stn)


	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)

	dirAdjStn <- file.path(adjdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirAdjStn, showWarnings = FALSE, recursive = TRUE)
	fileadj <- file.path(dirAdjStn, sel.stn, fsep = .Platform$file.sep)

	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(GeneralParameters$single.series == "0") xval0 <- donnees$datmon[,xpos]
		else xval0 <- donnees$datmon
		InSeries0 <- cbind(as.numeric(substr(donnees$modates, 1,4)), as.numeric(substr(donnees$modates, 5,6)), 0, xval0)
		ret0 <- QMadj.GaussianDLY(InSeries0, InCs, fileout, sel.stn, fsuffix = 'MON', MissingValueCode = donnees$MissingValue, Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
		if(ret0 < 0){
			InsertMessagesTxt(main.txt.out, 'QMadj failed for Monthly data', format = TRUE)
			return(NULL)
		}else{
			fileout0 <- paste(fileout, "_QMadjMON.dat", sep = "")
			outadjQM0 <- read.table(fileout0)
			outadjQM0 <- round(outadjQM0[,4:6], 1)
			outadjQM0[outadjQM0 != donnees$MissingValue & outadjQM0 < 0] <- 0
			outAdjMON <- data.frame(donnees$modates, outadjQM0)
		}
	}else{
		if(ReturnExecResults$action == 'FindU') outdonnees <- read.table(paste(fileout, '_U.dat', sep = ''))
		if(ReturnExecResults$action == 'FindUD') outdonnees <- read.table(paste(fileout, '_UD.dat', sep = ''))
		if(ReturnExecResults$action == 'StepSize') outdonnees <- read.table(paste(fileout, '_F.dat', sep = ''))
		outAdjMON <- data.frame(donnees$modates, round(outdonnees[,c(3,5,9)], 1))
	}
	write.table(outAdjMON, paste(fileadj, '_MON.txt', sep = ''), col.names = F, row.names = F)

	if(GeneralParameters$period != 'monthly'){
		if(GeneralParameters$single.series == "0") xval1 <- donnees$datdek[,xpos]
		else xval1 <- donnees$datdek
		InSeries1 <- cbind(as.numeric(substr(donnees$dkdates, 1,4)), as.numeric(substr(donnees$dkdates, 5,6)), as.numeric(substr(donnees$dkdates, 7,7)), xval1)
		ret1 <- QMadj.GaussianDLY(InSeries1, InCs, fileout, sel.stn, fsuffix = 'DEK', MissingValueCode = donnees$MissingValue, Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
		if(ret1 < 0){
			InsertMessagesTxt(main.txt.out, 'QMadj failed for dekadal data', format = TRUE)
			return(NULL)
		}else{
			fileout1 <- paste(fileout, "_QMadjDEK.dat", sep = "")
			outadjQM1 <- read.table(fileout1)
			outadjQM1 <- round(outadjQM1[,4:6], 1)
			if(as.character(GeneralParameters$prcpdata$Values[1]) == '1') outadjQM1[outadjQM1 != donnees$MissingValue & outadjQM1 < 0] <- 0
			outAdjDEK <- data.frame(donnees$dkdates, outadjQM1)
			write.table(outAdjDEK, paste(fileadj, '_DEK.txt', sep = ''), col.names = F, row.names = F)
		}
		if(GeneralParameters$period == 'daily'){
			if(GeneralParameters$single.series == "0") xval2 <- donnees$datdly[,xpos]
			else xval2 <- donnees$datdly
			InSeries2 <- cbind(as.numeric(substr(donnees$dydates, 1,4)), as.numeric(substr(donnees$dydates, 5,6)), as.numeric(substr(donnees$dydates, 7,8)), xval2)
			ret2 <- QMadj.GaussianDLY(InSeries2, InCs, fileout, sel.stn, fsuffix = 'DLY', MissingValueCode = donnees$MissingValue, Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
			if(ret2 < 0){
				InsertMessagesTxt(main.txt.out, 'QMadj failed for daily data', format = TRUE)
				return(NULL)
			}else{
				fileout2 <- paste(fileout, "_QMadjDLY.dat", sep = "")
				outadjQM2 <- read.table(fileout2)
				outadjQM2 <- round(outadjQM2[,4:6], 1)
				if(as.character(GeneralParameters$prcpdata$Values[1]) == '1') outadjQM2[outadjQM2 != donnees$MissingValue & outadjQM2 < 0] <- 0
				outAdjDLY <- data.frame(donnees$dydates, outadjQM2)
				write.table(outAdjDLY, paste(fileadj, '_DLY.txt', sep = ''), col.names = F, row.names = F)
			}
		}
	}
	write.table(choix, paste(fileadj, '_CHOICE.txt', sep = ''), col.names = F, row.names = F)
	res <- list(action = 'QMadj', StnOutDir = dirSelectStn, stn = sel.stn)

	return(res)
}

#############################################################################

executeOnFindU.wRef <- function(donnees, dem_data, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	datadir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Data', fsep = .Platform$file.sep)

	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datmon[,xpos]
		refseries <- getRHtestsRefSeries(xpos, donnees, dem_data, GeneralParameters)
		xrefs <- refseries$ref
		station_base <- station_ref<-sel.stn
		if(as.character(GeneralParameters$ref.series.choix$Values[2]) == '1' & is.null(dem_data)) InsertMessagesTxt(main.txt.out, refseries$msg, format = TRUE)
		if(is.null(xrefs)){
			InsertMessagesTxt(main.txt.out, refseries$msg, format = TRUE)
			return(NULL)
		}
	}else{
		xval <- donnees$datmon
		xrefs <- donnees$refmon
		station_base <- sel.stn
		station_ref <- getf.no.ext(as.character(GeneralParameters$file.io$Values[2]))
		if(is.null(xrefs)){
			InsertMessagesTxt(main.txt.out, 'No reference series found', format = TRUE)
			return(NULL)
		}
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(min(xval, na.rm = T) > 0) Inxval <- log(xval)
	 	else Inxval <- log(xval+1)
		if(min(xrefs, na.rm = T) > 0) Inxrefs <- log(xrefs)
	 	else Inxrefs <- log(xrefs+1)

	}else{
		Inxval <- xval
		Inxrefs <- xrefs
	}

	daty <- donnees$modates
	InSeries <- cbind(as.numeric(substr(daty, 1,4)), as.numeric(substr(daty, 5,6)), 0, Inxval)
	RefSeries <- cbind(as.numeric(substr(daty, 1,4)), as.numeric(substr(daty, 5,6)), 0, Inxrefs)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)

	dirRefSeries <- file.path(datadir, 'RefSeries_Data', sel.stn, fsep = .Platform$file.sep)
	dir.create(dirRefSeries, showWarnings = FALSE, recursive = TRUE)
	###reference series
	wRefseries <- data.frame(daty, round(xrefs, 4))
	write.table(wRefseries, file.path(dirRefSeries, paste(sel.stn, paste('RefS-','MON','.txt', sep = ''), sep = '_'), fsep = .Platform$file.sep), col.names = F, row.names = F)

	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	ret <- FindU.wRef(InSeries, RefSeries, fileout, station_base, station_ref, MissingValueCode = donnees$MissingValue, p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'FindU.wRef failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'FindU.wRef', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}


#######
executeOnFindUD.wRef <- function(donnees, dem_data, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	datadir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Data', fsep = .Platform$file.sep)

	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datmon[,xpos]
		refseries <- getRHtestsRefSeries(xpos, donnees, dem_data, GeneralParameters)
		xrefs <- refseries$ref
		station_base <- station_ref<-sel.stn
		if(as.character(GeneralParameters$ref.series.choix$Values[2]) == '1' & is.null(dem_data)) InsertMessagesTxt(main.txt.out, refseries$msg, format = TRUE)
		if(is.null(xrefs)){
			InsertMessagesTxt(main.txt.out, refseries$msg, format = TRUE)
			return(NULL)
		}
	}else{
		xval <- donnees$datmon
		xrefs <- donnees$refmon
		station_base <- sel.stn
		station_ref <- getf.no.ext(as.character(GeneralParameters$file.io$Values[2]))
		if(is.null(xrefs)){
			InsertMessagesTxt(main.txt.out, 'No reference series found', format = TRUE)
			return(NULL)
		}
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(min(xval, na.rm = T) > 0) Inxval <- log(xval)
	 	else Inxval <- log(xval+1)
		if(min(xrefs, na.rm = T) > 0) Inxrefs <- log(xrefs)
	 	else Inxrefs <- log(xrefs+1)

	}else{
		Inxval <- xval
		Inxrefs <- xrefs
	}

	daty <- donnees$modates
	InSeries <- cbind(as.numeric(substr(daty, 1,4)), as.numeric(substr(daty, 5,6)), 0, Inxval)
	RefSeries <- cbind(as.numeric(substr(daty, 1,4)), as.numeric(substr(daty, 5,6)), 0, Inxrefs)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)

	dirRefSeries <- file.path(datadir, 'RefSeries_Data', sel.stn, fsep = .Platform$file.sep)
	dir.create(dirRefSeries, showWarnings = FALSE, recursive = TRUE)
	###reference series
	wRefseries <- data.frame(daty, round(xrefs, 4))
	write.table(wRefseries, file.path(dirRefSeries, paste(sel.stn, paste('RefS-','MON','.txt', sep = ''), sep = '_'), fsep = .Platform$file.sep), col.names = F, row.names = F)

	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	ret <- FindUD.wRef(InSeries, RefSeries, InCs, fileout, station_base, station_ref, MissingValueCode = donnees$MissingValue, p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'FindUD.wRef failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'FindUD.wRef', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)


}

##########
executeOnStepSize.wRef <- function(donnees, dem_data, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	datadir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Data', fsep = .Platform$file.sep)

	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datmon[,xpos]
		refseries <- getRHtestsRefSeries(xpos, donnees, dem_data, GeneralParameters)
		xrefs <- refseries$ref
		station_base <- station_ref<-sel.stn
		if(as.character(GeneralParameters$ref.series.choix$Values[2]) == '1' & is.null(dem_data)) InsertMessagesTxt(main.txt.out, refseries$msg, format = TRUE)
		if(is.null(xrefs)){
			InsertMessagesTxt(main.txt.out, refseries$msg, format = TRUE)
			return(NULL)
		}
	}else{
		xval <- donnees$datmon
		xrefs <- donnees$refmon
		station_base <- sel.stn
		station_ref <- getf.no.ext(as.character(GeneralParameters$file.io$Values[2]))
		if(is.null(xrefs)){
			InsertMessagesTxt(main.txt.out, 'No reference series found', format = TRUE)
			return(NULL)
		}
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(min(xval, na.rm = T) > 0) Inxval <- log(xval)
	 	else Inxval <- log(xval+1)
		if(min(xrefs, na.rm = T) > 0) Inxrefs <- log(xrefs)
	 	else Inxrefs <- log(xrefs+1)

	}else{
		Inxval <- xval
		Inxrefs <- xrefs
	}

	daty <- donnees$modates
	InSeries <- cbind(as.numeric(substr(daty, 1,4)), as.numeric(substr(daty, 5,6)), 0, Inxval)
	RefSeries <- cbind(as.numeric(substr(daty, 1,4)), as.numeric(substr(daty, 5,6)), 0, Inxrefs)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)

	dirRefSeries <- file.path(datadir, 'RefSeries_Data', sel.stn, fsep = .Platform$file.sep)
	dir.create(dirRefSeries, showWarnings = FALSE, recursive = TRUE)
	###reference series
	wRefseries <- data.frame(daty, round(xrefs, 4))
	write.table(wRefseries, file.path(dirRefSeries, paste(sel.stn, paste('RefS-','MON','.txt', sep = ''), sep = '_'), fsep = .Platform$file.sep), col.names = F, row.names = F)

	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	ret <- StepSize.wRef(InSeries, RefSeries, InCs, fileout, station_base, station_ref, MissingValueCode = donnees$MissingValue, p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'StepSize.wRef failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'StepSize.wRef', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}

##########
executeOnQMadj.wRef <- function(donnees, dem_data, GeneralParameters, choix){
	LSmultiple <<- match.fun("LSmultiple.RHtestsV4")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtestsV4")

	rootdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'), fsep = .Platform$file.sep)
	outdir <- file.path(rootdir, 'Outputs', fsep = .Platform$file.sep)
	datadir <- file.path(rootdir, 'Data', fsep = .Platform$file.sep)
	adjdir <- file.path(rootdir, 'AdjustedData', fsep = .Platform$file.sep)

	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))
	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0")  xpos <- which(as.character(donnees$IDs) == sel.stn)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)

	dirRefSeries <- file.path(datadir, 'RefSeries_Data', sel.stn, fsep = .Platform$file.sep)

	dirAdjStn <- file.path(adjdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirAdjStn, showWarnings = FALSE, recursive = TRUE)
	fileadj <- file.path(dirAdjStn, sel.stn, fsep = .Platform$file.sep)

	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	if(as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'){
		if(GeneralParameters$single.series == "0"){
			xval0 <- donnees$datmon[,xpos]
			refseries0 <- getRHtestsRefSeries(xpos, donnees, dem_data, GeneralParameters, 'MON')
			xrefs0 <- refseries0$ref
			station_base <- station_ref<-sel.stn
		}else{
			xval0 <- donnees$datmon
			xrefs0 <- donnees$refmon
			station_base <- sel.stn
			station_ref <- getf.no.ext(as.character(GeneralParameters$file.io$Values[2]))
		}
		InSeries0 <- cbind(as.numeric(substr(donnees$modates, 1,4)), as.numeric(substr(donnees$modates, 5,6)), 0, xval0)
		RefSeries0 <- cbind(as.numeric(substr(donnees$modates, 1,4)), as.numeric(substr(donnees$modates, 5,6)), 0, xrefs0)
		ret0 <- QMadj.GaussianDLY.wRef(InSeries0, RefSeries0, InCs, fileout, station_base, station_ref, fsuffix = 'MON', MissingValueCode = donnees$MissingValue, Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
		if(ret0 < 0){
			InsertMessagesTxt(main.txt.out, 'QMadj.wRef failed for Monthly data', format = TRUE)
			return(NULL)
		}else{
			fileout0 <- paste(fileout, "_QMadjMON.dat", sep = "")
			outadjQM0 <- read.table(fileout0)
			outadjQM0 <- round(outadjQM0[,4:6], 1)
			outadjQM0[outadjQM0 != donnees$MissingValue & outadjQM0 < 0] <- 0
			outAdjMON <- data.frame(donnees$modates, outadjQM0)
		}
	}else{
		if(ReturnExecResults$action == 'FindU.wRef') outdonnees <- read.table(paste(fileout, '_U.dat', sep = ''))
		if(ReturnExecResults$action == 'FindUD.wRef') outdonnees <- read.table(paste(fileout, '_UD.dat', sep = ''))
		if(ReturnExecResults$action == 'StepSize.wRef') outdonnees <- read.table(paste(fileout, '_F.dat', sep = ''))
		outAdjMON <- data.frame(donnees$modates, round(outdonnees[,c(3,5,11)], 1))
	}
	write.table(outAdjMON, paste(fileadj, '_MON.txt', sep = ''), col.names = F, row.names = F)

	if(GeneralParameters$period != 'monthly'){
		if(GeneralParameters$single.series == "0"){
			xval1 <- donnees$datdek[,xpos]
			refseries1 <- getRHtestsRefSeries(xpos, donnees, dem_data, GeneralParameters, 'DEK')
			xrefs1 <- refseries1$ref
			station_base <- station_ref<-sel.stn
		}else{
			xval1 <- donnees$datdek
			xrefs1 <- donnees$refdek
			station_base <- sel.stn
			station_ref <- getf.no.ext(as.character(GeneralParameters$file.io$Values[2]))
		}
		InSeries1 <- cbind(as.numeric(substr(donnees$dkdates, 1,4)), as.numeric(substr(donnees$dkdates, 5,6)), as.numeric(substr(donnees$dkdates, 7,7)), xval1)
		RefSeries1 <- cbind(as.numeric(substr(donnees$dkdates, 1,4)), as.numeric(substr(donnees$dkdates, 5,6)), as.numeric(substr(donnees$dkdates, 7,7)), xrefs1)

		wRefseries1 <- data.frame(donnees$dkdates, round(xrefs1, 4))
		write.table(wRefseries1, file.path(dirRefSeries, paste(sel.stn, paste('RefS-','DEK','.txt', sep = ''), sep = '_'), fsep = .Platform$file.sep), col.names = F, row.names = F)

		ret1 <- QMadj.GaussianDLY.wRef(InSeries1, RefSeries1, InCs, fileout, station_base, station_ref, fsuffix = 'DEK', MissingValueCode = donnees$MissingValue, Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
		if(ret1 < 0){
			InsertMessagesTxt(main.txt.out, 'QMadj.wRef failed for dekadal data', format = TRUE)
			return(NULL)
		}else{
			fileout1 <- paste(fileout, "_QMadjDEKwRef.dat", sep = "")
			outadjQM1 <- read.table(fileout1)
			outadjQM1 <- round(outadjQM1[,4:6], 1)
			if(as.character(GeneralParameters$prcpdata$Values[1]) == '1') outadjQM1[outadjQM1 != donnees$MissingValue & outadjQM1 < 0] <- 0
			outAdjDEK <- data.frame(donnees$dkdates, outadjQM1)
			write.table(outAdjDEK, paste(fileadj, '_DEK.txt', sep = ''), col.names = F, row.names = F)
		}
		if(GeneralParameters$period == 'daily'){

			if(GeneralParameters$single.series == "0"){
				xval2 <- donnees$datdly[,xpos]
				refseries2 <- getRHtestsRefSeries(xpos, donnees, dem_data, GeneralParameters, 'DLY')
				xrefs2 <- refseries2$ref
				station_base <- station_ref<-sel.stn
			}else{
				xval2 <- donnees$datdly
				xrefs2 <- donnees$refdly
				station_base <- sel.stn
				station_ref <- getf.no.ext(as.character(GeneralParameters$file.io$Values[2]))
			}
			InSeries2 <- cbind(as.numeric(substr(donnees$dydates, 1,4)), as.numeric(substr(donnees$dydates, 5,6)), as.numeric(substr(donnees$dydates, 7,8)), xval2)
			RefSeries2 <- cbind(as.numeric(substr(donnees$dydates, 1,4)), as.numeric(substr(donnees$dydates, 5,6)), as.numeric(substr(donnees$dydates, 7,8)), xrefs2)

			wRefseries2 <- data.frame(donnees$dydates, round(xrefs2, 4))
			write.table(wRefseries2, file.path(dirRefSeries, paste(sel.stn, paste('RefS-','DLY','.txt', sep = ''), sep = '_'), fsep = .Platform$file.sep), col.names = F, row.names = F)

			ret2 <- QMadj.GaussianDLY.wRef(InSeries2, RefSeries2, InCs, fileout, station_base, station_ref, fsuffix = 'DLY', MissingValueCode = donnees$MissingValue, Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
			if(ret2 < 0){
				InsertMessagesTxt(main.txt.out, 'QMadj.wRef failed for daily data', format = TRUE)
				return(NULL)
			}else{
				fileout2 <- paste(fileout, "_QMadjDLYwRef.dat", sep = "")
				outadjQM2 <- read.table(fileout2)
				outadjQM2 <- round(outadjQM2[,4:6], 1)
				if(as.character(GeneralParameters$prcpdata$Values[1]) == '1') outadjQM2[outadjQM2 != donnees$MissingValue & outadjQM2 < 0] <- 0
				outAdjDLY <- data.frame(donnees$dydates, outadjQM2)
				write.table(outAdjDLY, paste(fileadj, '_DLY.txt', sep = ''), col.names = F, row.names = F)
			}
		}
	}
	write.table(choix, paste(fileadj, '_CHOICE.txt', sep = ''), col.names = F, row.names = F)
	res <- list(action = 'QMadj.wRef', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}


#############################################################################
##dlyPrcp


executeOnFindU.dlyPrcp <- function(donnees, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtests_dlyPrcp")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtests_dlyPrcp")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datdly[,xpos]
	}else{
		xval <- donnees$datdly
	}
	InSeries <- cbind(as.numeric(substr(donnees$dydates, 1,4)), as.numeric(substr(donnees$dydates, 5,6)), as.numeric(substr(donnees$dydates, 7,8)), xval)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	ret <- FindU.dlyPrcp(InSeries, fileout, sel.stn, MissingValueCode = donnees$MissingValue, pthr = pars[5], p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'FindU.dlyPrcp failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'FindU.dlyPrcp', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}



#####
executeOnFindUD.dlyPrcp <- function(donnees, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtests_dlyPrcp")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtests_dlyPrcp")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datdly[,xpos]
	}else{
		xval <- donnees$datdly
	}
	InSeries <- cbind(as.numeric(substr(donnees$dydates, 1,4)), as.numeric(substr(donnees$dydates, 5,6)), as.numeric(substr(donnees$dydates, 7,8)), xval)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	ret <- FindUD.dlyPrcp(InSeries, InCs, fileout, sel.stn, MissingValueCode = donnees$MissingValue, pthr = pars[5], p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'FindUD.dlyPrcp failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'FindUD.dlyPrcp', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}


#####
executeOnStepSize.dlyPrcp <- function(donnees, GeneralParameters){
	LSmultiple <<- match.fun("LSmultiple.RHtests_dlyPrcp")
	LSmultipleRed <<- match.fun("LSmultipleRed.RHtests_dlyPrcp")

	outdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'),'Outputs', fsep = .Platform$file.sep)
	pars <- as.numeric(as.character(GeneralParameters$rhtests.pars$Values))

	sel.stn <- tclvalue(stn.choix.val)
	if(GeneralParameters$single.series == "0"){
		xpos <- which(as.character(donnees$IDs) == sel.stn)
		xval <- donnees$datdly[,xpos]
	}else{
		xval <- donnees$datdly
	}
	InSeries <- cbind(as.numeric(substr(donnees$dydates, 1,4)), as.numeric(substr(donnees$dydates, 5,6)), as.numeric(substr(donnees$dydates, 7,8)), xval)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirSelectStn, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)
	InCs <- paste(fileout, "_mCs.txt", sep = '')
	if(!file.exists(InCs)){
		InsertMessagesTxt(main.txt.out, paste(paste(sel.stn, "_mCs.txt", sep = ''), "doesn't exist"), format = TRUE)
		return(NULL)
	}

	ret <- StepSize.dlyPrcp(InSeries, InCs, fileout, sel.stn, MissingValueCode = donnees$MissingValue, pthr = pars[5], p.lev = pars[1], Iadj = pars[2], Mq = pars[3], Ny4a = pars[4])
	if(ret < 0){
		InsertMessagesTxt(main.txt.out, 'StepSize.dlyPrcp failed...',format = TRUE)
		res <- NULL
	}else res <- list(action = 'StepSize.dlyPrcp', StnOutDir = dirSelectStn, stn = sel.stn)
	return(res)
}


##########
executeOnadjDLY.dlyPrcp <- function(donnees, GeneralParameters, choix){

	rootdir <- file.path(as.character(GeneralParameters$file.io$Values[4]), paste('RHtests_Output', getf.no.ext(as.character(GeneralParameters$file.io$Values[1])), sep = '_'), fsep = .Platform$file.sep)
	outdir <- file.path(rootdir, 'Outputs', fsep = .Platform$file.sep)
	adjdir <- file.path(rootdir, 'AdjustedData', fsep = .Platform$file.sep)

	sel.stn <- tclvalue(stn.choix.val)

	dirSelectStn <- file.path(outdir, sel.stn, fsep = .Platform$file.sep)
	fileout <- file.path(dirSelectStn, sel.stn, fsep = .Platform$file.sep)

	dirAdjStn <- file.path(adjdir, sel.stn, fsep = .Platform$file.sep)
	dir.create(dirAdjStn, showWarnings = FALSE, recursive = TRUE)
	fileadj <- file.path(dirAdjStn, sel.stn, fsep = .Platform$file.sep)

	if(!is.null(ReturnExecResults)){
		if(length(grep('dlyPrcp', ReturnExecResults$action)) > 0){
			if(ReturnExecResults$action == 'FindU.dlyPrcp') outdonnees <- read.table(paste(fileout, '_U.dat', sep = ''))
			if(ReturnExecResults$action == 'FindUD.dlyPrcp') outdonnees <- read.table(paste(fileout, '_UD.dat', sep = ''))
			if(ReturnExecResults$action == 'StepSize.dlyPrcp') outdonnees <- read.table(paste(fileout, '_F.dat', sep = ''))
			outAdjp <- round(outdonnees[,c(3,5,6)], 1)
			gpthr <- outAdjp[,2] == as.numeric(donnees$MissingValue) & outAdjp[,1] != as.numeric(donnees$MissingValue)
			outAdjp[gpthr, 2] <- outAdjp[gpthr, 1]
			outAdjp[gpthr, 3] <- outAdjp[gpthr, 1]
			outAdjp[outAdjp != as.numeric(donnees$MissingValue) & outAdjp < 0] <- 0
			outAdjDLYp <- data.frame(donnees$dydates, outAdjp)
			write.table(outAdjDLYp, paste(fileadj, '_DLY.txt', sep = ''), col.names = F, row.names = F)
			write.table(choix, paste(fileadj, '_CHOICE.txt', sep = ''), col.names = F, row.names = F)
			InsertMessagesTxt(main.txt.out, 'Adjusted data extracted!')
		}else InsertMessagesTxt(main.txt.out, 'There is no  RHtests.dlyPrcp output', format = TRUE)
	}else InsertMessagesTxt(main.txt.out, 'There is no RHtests.dlyPrcp output', format = TRUE)
}




#############################################################################

######
RHtestsDisplayPdfPlot <- function(ret){
	if(!is.null(ret)){
		fileout <- file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep)
		if(ret$action == 'FindU') system(paste("open", paste(fileout, '_U.pdf', sep = '')))
		if(ret$action == 'FindUD') system(paste("open", paste(fileout, '_UD.pdf', sep = '')))
		if(ret$action == 'StepSize') system(paste("open", paste(fileout, '_F.pdf', sep = '')))
		if(ret$action == 'FindU.wRef') system(paste("open", paste(fileout, '_U.pdf', sep = '')))
		if(ret$action == 'FindUD.wRef') system(paste("open", paste(fileout, '_UD.pdf', sep = '')))
		if(ret$action == 'StepSize.wRef') system(paste("open", paste(fileout, '_F.pdf', sep = '')))
		if(ret$action == 'FindU.dlyPrcp') system(paste("open", paste(fileout, '_U.pdf', sep = '')))
		if(ret$action == 'FindUD.dlyPrcp') system(paste("open", paste(fileout, '_UD.pdf', sep = '')))
		if(ret$action == 'StepSize.dlyPrcp') system(paste("open", paste(fileout, '_F.pdf', sep = '')))
		if(ret$action == 'QMadj'){
			logRR <- as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'
			if(logRR) system(paste("open", paste(fileout, paste("_QMadj", 'MON',".pdf", sep = ''), sep = '')))
			if(GeneralParameters$period != 'monthly'){
				system(paste("open", paste(fileout, paste("_QMadj", 'DEK',".pdf", sep = ''), sep = '')))
				if(GeneralParameters$period == 'daily') system(paste("open", paste(fileout, paste("_QMadj", 'DLY',".pdf", sep = ''), sep = '')))
			}
		}

		if(ret$action == 'QMadj.wRef'){
			logRR <- as.character(GeneralParameters$prcpdata$Values[1]) == '1' & as.character(GeneralParameters$prcpdata$Values[2]) == '1'
			if(logRR) system(paste("open", paste(fileout, paste("_QMadj", 'MON', "wRef.pdf", sep = ''), sep = '')))
			if(GeneralParameters$period != 'monthly'){
				system(paste("open", paste(fileout, paste("_QMadj", 'DEK', "wRef.pdf", sep = ''), sep = '')))
				if(GeneralParameters$period == 'daily') system(paste("open", paste(fileout, paste("_QMadj", 'DLY', "wRef.pdf", sep = ''), sep = '')))
			}
		}


	}else InsertMessagesTxt(main.txt.out, 'Unable to open pdf file', format = TRUE)
}

######
formatRHtestsPreviewOutput <- function(ret, text1, text2){
	fileout <- file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep)
	file1 <- readLines(paste(fileout, text1, sep = ''))
	file2 <- readLines(paste(fileout, text2, sep = ''))
	todisplay <- c(paste('File:',paste(ret$stn, text1, sep = '')),'------------------------',' ',file1,' ',
	paste(rep('*',100), collapse=''),' ',
	paste('File:',paste(ret$stn, text2, sep = '')),'------------------------',' ',file2)
	retOut <- paste(todisplay, collapse='\n')
	return(retOut)
}

######
formatRHtestsPreviewOutput1 <- function(ret, text1, text2, text3 = NULL){
	fileout <- file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep)
	file1 <- readLines(paste(fileout, text1, sep = ''))
	file2 <- readLines(paste(fileout, text2, sep = ''))
	if(is.null(text3)){
		todisplay <- c(paste('File:',paste(ret$stn, text1, sep = '')),'------------------------',' ',file1,' ',
		paste(rep('*',100), collapse=''),' ',
		paste('File:',paste(ret$stn, text2, sep = '')),'------------------------',' ',file2)
	}else{
		file3 <- readLines(paste(fileout, text3, sep = ''))
		todisplay <- c(paste('File:',paste(ret$stn, text1, sep = '')),'------------------------',' ',file1,' ',
		paste(rep('*',100), collapse=''),' ',
		paste('File:',paste(ret$stn, text2, sep = '')),'------------------------',' ',file2,' ',
		paste('File:',paste(ret$stn, text3, sep = '')),'------------------------',' ',file3)
	}
	retOut <- paste(todisplay, collapse='\n')
	return(retOut)
}

######
RHtestsPreviewOutput <- function(ret){
	if(ret$action == 'FindU') retOut <- formatRHtestsPreviewOutput(ret, '_1Cs.txt', '_Ustat.txt')
	if(ret$action == 'FindUD') retOut <- formatRHtestsPreviewOutput(ret, '_pCs.txt', '_UDstat.txt')
	if(ret$action == 'StepSize') retOut <- formatRHtestsPreviewOutput(ret, '_fCs.txt', '_Fstat.txt')
	if(ret$action == 'FindU.wRef') retOut <- formatRHtestsPreviewOutput(ret, '_1Cs.txt', '_Ustat.txt')
	if(ret$action == 'FindUD.wRef') retOut <- formatRHtestsPreviewOutput(ret, '_pCs.txt', '_UDstat.txt')
	if(ret$action == 'StepSize.wRef') retOut <- formatRHtestsPreviewOutput(ret, '_fCs.txt', '_Fstat.txt')
	if(ret$action == 'FindU.dlyPrcp') retOut <- formatRHtestsPreviewOutput(ret, '_1Cs.txt', '_Ustat.txt')
	if(ret$action == 'FindUD.dlyPrcp') retOut <- formatRHtestsPreviewOutput(ret, '_pCs.txt', '_UDstat.txt')
	if(ret$action == 'StepSize.dlyPrcp') retOut <- formatRHtestsPreviewOutput(ret, '_fCs.txt', '_Fstat.txt')
	if(ret$action == 'QMadj'){
		if(GeneralParameters$period == 'daily') retOut <- formatRHtestsPreviewOutput1(ret, '_mCs.txt', paste("_QMadj", 'DEK', "stat.txt", sep = ''), paste("_QMadj", 'DLY', "stat.txt", sep = ''))
		if(GeneralParameters$period == 'dekadal') retOut <- formatRHtestsPreviewOutput1(ret, '_mCs.txt', paste("_QMadj", 'DEK', "stat.txt", sep = ''))
	}

	if(ret$action == 'QMadj.wRef'){
		if(GeneralParameters$period == 'daily') retOut <- formatRHtestsPreviewOutput1(ret, '_mCs.txt', paste("_QMadj", 'DEK', "wRefstat.txt", sep = ''), paste("_QMadj", 'DLY', "wRefstat.txt", sep = ''))
		if(GeneralParameters$period == 'dekadal') retOut <- formatRHtestsPreviewOutput1(ret, '_mCs.txt', paste("_QMadj", 'DEK', "wRefstat.txt", sep = ''))
	}


	return(retOut)
}

######
RHtestsUndoChange <- function(ret){
	fileout <- paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_mCs.txt', sep = '')
	if(ret$action == 'FindU') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_1Cs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'FindUD') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_pCs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'StepSize') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_fCs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'FindU.wRef') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_1Cs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'FindUD.wRef') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_pCs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'StepSize.wRef') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_fCs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'FindU.dlyPrcp') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_1Cs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'FindUD.dlyPrcp') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_pCs.txt', sep = ''), fileout, overwrite = TRUE)
	if(ret$action == 'StepSize.dlyPrcp') file.copy(paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_fCs.txt', sep = ''), fileout, overwrite = TRUE)

}


######

formatted_mCs2vec <- function(x){
	tmp <- c(substr(x, 1,1), substr(x, 3,6), substr(x, 7,16), substr(x, 19,24), substr(x, 26,31), substr(x, 33,38), substr(x, 39,48), substr(x, 51,60), substr(x, 62,71))
	tmp <- str_trim(tmp)
	#tmp[tmp == ""] <- NA
	return(tmp)
}

#######
RHtests_mCsFormat <- function(ret){
	fileInCs <- paste(file.path(ret$StnOutDir, ret$stn, fsep = .Platform$file.sep),'_mCs.txt', sep = '')
	Incs <- readLines(fileInCs)
	Incs <- Incs[sapply(Incs, nchar) > 0]
	#ou
	#Incs <- scan(fileInCs, what="", sep = "\n", blank.lines.skip = TRUE, quiet = TRUE)

	if(length(Incs) > 0){
		nbcpt <- as.numeric(strsplit(Incs[1], " ")[[1]][1])
		if(nbcpt > 0){
			#tmp <- t(sapply(strsplit(gsub("[()-]", " ", Incs[-1]), " "), function(x) x[x != ""]))
			tmp <- t(sapply(Incs[-1],formatted_mCs2vec))
			if(nrow(tmp) == 1) tmp <- data.frame(matrix(c(tmp[,1:3], paste('(',tmp[,4],'-',tmp[,5],')',sep = ''), tmp[,6:7], paste('(',tmp[,8],'-',tmp[,9],')',sep = '')), nrow = 1))
			else tmp <- data.frame(cbind(tmp[,1:3], paste('(',tmp[,4],'-',tmp[,5],')',sep = ''), tmp[,6:7], paste('(',tmp[,8],'-',tmp[,9],')',sep = '')), row.names = NULL)
		}else tmp <- data.frame(matrix(NA, ncol = 7))
	}else tmp <- data.frame(matrix(NA, ncol = 7))

	return(list(tmp, fileInCs))
}

