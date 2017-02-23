
AssessDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- as.integer(w.scale(24)/sfont0)
	largeur1 <- as.integer(w.scale(16)/sfont0)

	###################
	cmd.frame <- tkframe(panel.left)

	###################
	chkframe <- tkframe(cmd.frame, relief = 'groove', bd = 2)
	chkframe1 <- ttklabelframe(cmd.frame, text = "Missing Data Summary for each Station", labelanchor = "nw", relief = 'groove', borderwidth = 2)

	###################
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	file.period <- tclVar('Dekadal data')
	file.stnfl <- tclVar()
	stnSumNA.val <- tclVar()
	plotORtable <- tclVar('Chart')

	cbperiod <- ttkcombobox(chkframe, values = cb.periodVAL, textvariable = file.period)
	sep1 <- ttkseparator(chkframe)
	labStn1 <- tklabel(chkframe, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(chkframe, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	bt.stnfl <- tkbutton(chkframe, text = "...")
	sep2 <- ttkseparator(chkframe)
	cmd.DistCor <- ttkbutton(chkframe, text = "Plot Distance-Correlation")
	cmd.AllNA <- ttkbutton(chkframe, text = "Plot Non-Missing Data Summary")
	cmd.AllTab <- ttkbutton(chkframe, text = "Table of Non-Missing Data")

	PlotTab.cb <- ttkcombobox(chkframe1, values = c('Chart', 'Table'), textvariable = plotORtable, state = 'normal', width = largeur1)

	stnSumNA.cb <- ttkcombobox(chkframe1, values = '', textvariable = stnSumNA.val, state = 'normal', width = largeur1)
	stnSumNA.prev <- ttkbutton(chkframe1, text = "<<-", width = 5)
	stnSumNA.next <- ttkbutton(chkframe1, text = "->>", width = 5)

	###################

	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)

			stnSumNA <- as.character(dat.opfiles[[2]][1,-1])
			tclvalue(stnSumNA.val) <- stnSumNA[1]
			tkconfigure(stnSumNA.cb, values = stnSumNA, textvariable = stnSumNA.val)
		}else return(NULL)
	})

	###################

	tkgrid(cbperiod, row = 0, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 1, sticky = "we")
	tkgrid(sep1, row = 1, column = 0, rowspan = 1, columnspan = 6, pady = 5, sticky = 'we')
	tkgrid(labStn1, row = 2, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 1, sticky = "we")
	tkgrid(cb.stnfl, row = 3, column = 0, rowspan = 1, columnspan = 5, padx = 1, pady = 1, sticky = "we")
	tkgrid(bt.stnfl, row = 3, column = 5, rowspan = 1, columnspan = 1, padx = 1, pady = 1, sticky = "e")
	tkgrid(sep2, row = 4, column = 0, rowspan = 1, columnspan = 6, pady = 5, sticky = 'we')

	tkgrid(cmd.DistCor, row = 5, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 2, sticky = "we")
	tkgrid(cmd.AllNA, row = 6, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 2, sticky = "we")
	tkgrid(cmd.AllTab, row = 7, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 2, sticky = "we")

	tkgrid(PlotTab.cb, row = 0, column = 1, rowspan = 1, padx = 1, pady = 3, sticky = "we")

	tkgrid(stnSumNA.prev, row = 1, column = 0, padx = 1, pady = 3, sticky = "w")
	tkgrid(stnSumNA.cb, row = 1, column = 1, padx = 1, pady = 3, sticky = "we")
	tkgrid(stnSumNA.next, row = 1, column = 2, padx = 1, pady = 3, sticky = "e")

	#######################

	infobulle(cb.stnfl, 'Choose the station data in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the station data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')


	#######################

	tkgrid(chkframe, sticky = 'we', pady = 10)
	tkgrid(chkframe1, sticky = 'we', pady = 10)
	tkgrid.columnconfigure(chkframe, 0, weight = 1)
	tkgrid.columnconfigure(chkframe, 3, weight = 1)
	tkgrid.columnconfigure(chkframe1, 1, weight = 1)

	#######################

	get.period <- function(){
		period <- switch(str_trim(tclvalue(file.period)),
								'Daily data' = 'daily',
								'Dekadal data' =  'dekadal',
								'Monthly data' = 'monthly')
		return(period)		
	}

	#######################

	assessAva <- new.env()
	assign('DONNEES', NULL, envir = assessAva)
	assign('input.file', NULL, envir = assessAva)
	assign('non.miss', NULL, envir = assessAva)

	get.donnees <- function(period, file.stnfl){
		if(is.null(assessAva$DONNEES)){
			donne <- getStnOpenData(file.stnfl)
			donne <- getCDTdataAndDisplayMsg(donne, period)
			assessAva$DONNEES <- donne
			assessAva$input.file <- tclvalue(file.stnfl)
		}else{
			if(tclvalue(file.stnfl) != assessAva$input.file){
				donne <- getStnOpenData(file.stnfl)
				donne <- getCDTdataAndDisplayMsg(donne, period)
				assessAva$DONNEES <- donne
				assessAva$input.file <- tclvalue(file.stnfl)
			}else{
				donne <- assessAva$DONNEES
			}
		}
		return(donne)
	}

	nombre.non.missing <- function(donne){
		an <- substr(donne$dates, 1, 4)
		mo <- substr(donne$dates, 5, 6)
		res <- lapply(seq(ncol(donne$data)), function(i){
			tapply(donne$data[, i], list(paste(an, mo, sep = '')), function(j) length(which(!is.na(j))))
		})
		res <- do.call(cbind, res)
		daty <- dimnames(res)[[1]]
		return(list(date = daty, nb = res))
	}

	get.non.missing.wrap <- function(donne){
		if(is.null(donne)) return(NULL)
		if(is.null(assessAva$non.miss)){
			nb <- nombre.non.missing(donne)
			assessAva$non.miss <- nb
		}else{
			if(tclvalue(file.stnfl) != assessAva$input.file){
				nb <- nombre.non.missing(donne)
				assessAva$non.miss <- nb
			}else{
				nb <- assessAva$non.miss
			}
		}
		return(nb)
	}

	nombre.non.missing.all <- function(nb, donne){
		an <- substr(nb$date, 1, 4)
		mo <- substr(nb$date, 5, 6)
		res <- cbind(an, mo, nb$nb)
		res <- rbind(c('Year', 'Months', donne$id), res)
		res <- data.frame(res, stringsAsFactors = FALSE)
		names(res) <- NULL
		row.names(res) <- NULL
		return(res)
	}

	nombre.non.missing.stn <- function(nb, donne, jstn){
		an <- substr(nb$date, 1, 4)
		mo <- substr(nb$date, 5, 6)
		res <- reshapeXYZ2Matrix(cbind(an, mo, nb$nb[, jstn]))
		res <- rbind(c(paste('STN', donne$id[jstn], sep = ':'), res$y), cbind(res$x, res$z))
		res <- data.frame(res, stringsAsFactors = FALSE)
		names(res) <- NULL
		row.names(res) <- NULL
		return(res)
	}

	#######################
		
	tkbind(cb.stnfl, "<<ComboboxSelected>>", function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)

		if(!is.null(donne)){
			stnSumNA <- donne$id
			tclvalue(stnSumNA.val) <- stnSumNA[1]
			tkconfigure(stnSumNA.cb, values = stnSumNA, textvariable = stnSumNA.val)
		}
	})

	#######################

	notebookTab <- NULL
	MissOneStnTab <- NULL

	tkconfigure(stnSumNA.next, command = function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)
		nb <- get.non.missing.wrap(donne)

		if(!is.null(donne)){
			istn <- as.numeric(tclvalue(tcl(stnSumNA.cb, "current")))+1
			istn <- istn+1
			stnSumNA <- donne$id
			if(istn > length(stnSumNA)) istn <- 1
			tclvalue(stnSumNA.val) <- stnSumNA[istn]
			jstn <- tclvalue(stnSumNA.val)

			if(tclvalue(plotORtable) == 'Chart'){
				imgContainer <- DisplayStnNASum(tknotes, jstn, donne, period, notebookTab)
				if(!is.null(imgContainer)){
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
					notebookTab <<- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			}
			if(tclvalue(plotORtable) == 'Table'){
				nb1 <- nombre.non.missing.stn(nb, donne, istn)
				retNBTab <- tableAssessAvaitebookTab_unik(tknotes, nb1, paste('Non-Missing', jstn),
														MissOneStnTab, AllOpenTabType, AllOpenTabData)
				MissOneStnTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
				ReturnExecResults <<- 0
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################

	tkconfigure(stnSumNA.prev, command = function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)
		nb <- get.non.missing.wrap(donne)

		if(!is.null(donne)){
			istn <- as.numeric(tclvalue(tcl(stnSumNA.cb, "current")))+1
			istn <- istn-1
			stnSumNA <- donne$id
			if(istn < 1) istn <- length(stnSumNA)
			tclvalue(stnSumNA.val) <- stnSumNA[istn]
			jstn <- tclvalue(stnSumNA.val)

			if(tclvalue(plotORtable) == 'Chart'){
				imgContainer <- DisplayStnNASum(tknotes, jstn, donne, period, notebookTab)
				if(!is.null(imgContainer)){
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
					notebookTab <<- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			}
			if(tclvalue(plotORtable) == 'Table'){
				nb1 <- nombre.non.missing.stn(nb, donne, istn)
				retNBTab <- tableAssessAvaitebookTab_unik(tknotes, nb1, paste('Non-Missing', jstn),
														MissOneStnTab, AllOpenTabType, AllOpenTabData)
				MissOneStnTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
				ReturnExecResults <<- 0
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################

	tkbind(stnSumNA.cb, "<<ComboboxSelected>>", function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)
		nb <- get.non.missing.wrap(donne)

		if(!is.null(donne)){
			istn <- as.numeric(tclvalue(tcl(stnSumNA.cb, "current")))+1
			stnSumNA <- donne$id
			jstn <- stnSumNA[istn]

			if(tclvalue(plotORtable) == 'Chart'){
				imgContainer <- DisplayStnNASum(tknotes, jstn, donne, period, notebookTab)
				if(!is.null(imgContainer)){
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
					notebookTab <<- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			}
			if(tclvalue(plotORtable) == 'Table'){
				nb1 <- nombre.non.missing.stn(nb, donne, istn)
				retNBTab <- tableAssessAvaitebookTab_unik(tknotes, nb1, paste('Non-Missing', jstn),
														MissOneStnTab, AllOpenTabType, AllOpenTabData)
				MissOneStnTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
				ReturnExecResults <<- 0
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################

	tkconfigure(cmd.DistCor, command = function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)

		if(!is.null(donne)){
			tkconfigure(main.win, cursor = 'watch'); tcl('update')
			imgContainer <- try(DisplayDistCorr(tknotes, donne, period), silent = TRUE)
			if(!inherits(imgContainer, "try-error")){
				if(!is.null(imgContainer)){
					ntab <- length(AllOpenTabType)
					AllOpenTabType[[ntab+1]] <<- 'img'
					AllOpenTabData[[ntab+1]] <<- imgContainer
					tkselect(tknotes, ntab)
				}
				tkconfigure(main.win, cursor = '')
			}else{
				InsertMessagesTxt(main.txt.out, 'Distance-Correlation computation failed', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', imgContainer[1]), format = TRUE)
				tkconfigure(main.win, cursor = '')
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################

	tkconfigure(cmd.AllNA, command = function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)

		if(!is.null(donne)){
			tkconfigure(main.win, cursor = 'watch'); tcl('update')
			imgContainer <- try(DisplayAllStnNASum(tknotes, donne, period), silent = TRUE)
			if(!inherits(imgContainer, "try-error")){
				if(!is.null(imgContainer)){
					ntab <- length(AllOpenTabType)
					AllOpenTabType[[ntab+1]] <<- 'img'
					AllOpenTabData[[ntab+1]] <<- imgContainer
					tkselect(tknotes, ntab)
				}
				tkconfigure(main.win, cursor = '')
			}else{
				InsertMessagesTxt(main.txt.out, 'Station missing data summaries failed', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', imgContainer[1]), format = TRUE)
				tkconfigure(main.win, cursor = '')
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################
	MissAllStnTab <- NULL

	tkconfigure(cmd.AllTab, command = function(){
		period <- get.period()
		donne <- get.donnees(period, file.stnfl)
		nb <- get.non.missing.wrap(donne)

		if(!is.null(nb)){
			tkconfigure(main.win, cursor = 'watch'); tcl('update')
			nb <- nombre.non.missing.all(nb, donne)

			retNBTab <- tableAssessAvaitebookTab_unik(tknotes, nb, 'Non-Missing Data', MissAllStnTab, AllOpenTabType, AllOpenTabData)
			MissAllStnTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
			tkconfigure(main.win, cursor = '')
			ReturnExecResults <<- 0
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'we', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	return(cmd.frame)
}

#######################################


