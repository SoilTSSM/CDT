
AssessDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- as.integer(as.numeric(w.scale(23)*0.95)/9)

	###################

	cmd.frame <- tkframe(panel.left)

	chkframe <- tkframe(cmd.frame, relief = 'groove', bd = 2)
	chkframe1 <- ttklabelframe(cmd.frame, text = "Missing Data Summary for each Station", labelanchor = "nw", relief = 'groove', borderwidth = 2)
	tkgrid(chkframe, sticky = 'we')
	tkgrid(chkframe1, sticky = 'we', pady = 10)

	file.period <- tclVar()
	tclvalue(file.period) <- 'Dekadal data'
	cbperiod <- ttkcombobox(chkframe, values = c('Daily data', 'Dekadal data', 'Monthly data'), textvariable = file.period)

	sep1 <- ttkseparator(chkframe)
	labStn1 <- tklabel(chkframe, text = 'Station data file', anchor = 'w', justify = 'left')

	file.stnfl <- tclVar()
	cb.stnfl <- ttkcombobox(chkframe, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	bt.stnfl <- tkbutton(chkframe, text = "...")
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
		}else{
			return(NULL)
		}
	})
	infobulle(cb.stnfl, 'Choose the station data in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the station data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	sep2 <- ttkseparator(chkframe)

	cmd.DistCor <- tkbutton(chkframe, text = "Distance-Correlation")
	cmd.AllNA <- tkbutton(chkframe, text = "Miss.Data Summary")

	tkgrid(cbperiod, row = 0, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 1, sticky = "we")
	tkgrid(sep1, row = 1, column = 0, rowspan = 1, columnspan = 6, pady = 5, sticky = 'we')
	tkgrid(labStn1, row = 2, column = 0, rowspan = 1, columnspan = 6, padx = 1, pady = 1, sticky = "we")
	tkgrid(cb.stnfl, row = 3, column = 0, rowspan = 1, columnspan = 5, padx = 1, pady = 1, sticky = "we")
	tkgrid(bt.stnfl, row = 3, column = 5, rowspan = 1, columnspan = 1, padx = 1, pady = 1, sticky = "e")
	tkgrid(sep2, row = 4, column = 0, rowspan = 1, columnspan = 6, pady = 5, sticky = 'we')

	tkgrid(cmd.DistCor, row = 5, column = 0, rowspan = 1, columnspan = 3, padx = 1, pady = 1, sticky = "we")
	tkgrid(cmd.AllNA, row = 5, column = 3, rowspan = 1, columnspan = 3, padx = 1, pady = 1, sticky = "we")

	stnSumNA.val <- tclVar()
	stnSumNA.cb <- ttkcombobox(chkframe1, values='', textvariable = stnSumNA.val, state = 'normal', width = largeur)
	stnSumNA.prev <- tkbutton(chkframe1, text=" <<- Prev")
	stnSumNA.next <- tkbutton(chkframe1, text = "Next->>")

	tkgrid(stnSumNA.cb, row = 0, column = 0, columnspan = 4, padx = 1, pady = 1)
	tkgrid(stnSumNA.prev, row = 1, column = 0, padx = 5)
	tkgrid(stnSumNA.next, row = 1, column = 3, padx = 5)

	#######################
	tkbind(cb.stnfl,"<<ComboboxSelected>>", function(){
		donne <- getStnOpenData(file.stnfl)
		if(!is.null(donne)){
		stnSumNA <- as.character(donne[1,-1])
		tclvalue(stnSumNA.val) <- stnSumNA[1]
		tkconfigure(stnSumNA.cb, values = stnSumNA, textvariable = stnSumNA.val)
		}
	})

	#######################
	notebookTab <- NULL

	tkconfigure(stnSumNA.next, command = function(){
		donne <- getStnOpenData(file.stnfl)
		if(!is.null(donne)){
			istn <- as.numeric(tclvalue(tcl(stnSumNA.cb, "current")))+1
			istn <- istn+1
			stnSumNA <- as.character(donne[1,-1])
			if(istn > length(stnSumNA)) istn <- 1
			tclvalue(stnSumNA.val) <- stnSumNA[istn]
			jstn <- tclvalue(stnSumNA.val)

			imgContainer <- DisplayStnNASum(tknotes, jstn, donne, tclvalue(file.period), notebookTab)
			if(!is.null(imgContainer)){
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
				notebookTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})
	#######################

	tkconfigure(stnSumNA.prev, command = function(){
		donne <- getStnOpenData(file.stnfl)
		if(!is.null(donne)){
			istn <- as.numeric(tclvalue(tcl(stnSumNA.cb, "current")))+1
			istn <- istn-1
			stnSumNA <- as.character(donne[1,-1])
			if(istn < 1) istn <- length(stnSumNA)
			tclvalue(stnSumNA.val) <- stnSumNA[istn]
			jstn <- tclvalue(stnSumNA.val)

			imgContainer <- DisplayStnNASum(tknotes, jstn, donne, tclvalue(file.period), notebookTab)
			if(!is.null(imgContainer)){
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab, AllOpenTabType, AllOpenTabData)
				notebookTab <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})
	#######################

	tkconfigure(cmd.DistCor, command = function(){
		donne <- getStnOpenData(file.stnfl)
		if(!is.null(donne)){
			tkconfigure(main.win, cursor = 'watch');tcl('update')
			imgContainer <- try(DisplayDistCorr(tknotes, donne, tclvalue(file.period)), silent = TRUE)
			if(!inherits(imgContainer, "try-error")){
				if(!is.null(imgContainer)){
					ntab <- length(AllOpenTabType)
					AllOpenTabType[[ntab+1]] <<- 'img'
					AllOpenTabData[[ntab+1]] <<- imgContainer
					tkselect(tknotes, ntab)
				}
				tkconfigure(main.win, cursor='')
			}else{
				InsertMessagesTxt(main.txt.out, 'Distance-Correlation computation failed', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',imgContainer[1]), format = TRUE)
				tkconfigure(main.win, cursor='')
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})
	#######################
	tkconfigure(cmd.AllNA, command = function(){
		donne <- getStnOpenData(file.stnfl)
		if(!is.null(donne)){
			tkconfigure(main.win, cursor = 'watch');tcl('update')
			imgContainer <- try(DisplayAllStnNASum(tknotes, donne, tclvalue(file.period)), silent = TRUE)
			if(!inherits(imgContainer, "try-error")){
				if(!is.null(imgContainer)){
					ntab <- length(AllOpenTabType)
					AllOpenTabType[[ntab+1]] <<- 'img'
					AllOpenTabData[[ntab+1]] <<- imgContainer
					tkselect(tknotes, ntab)
				}
				tkconfigure(main.win, cursor='')
			}else{
				InsertMessagesTxt(main.txt.out, 'Station missing data summaries failed', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]','',imgContainer[1]), format = TRUE)
				tkconfigure(main.win, cursor='')
			}
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	###
	tcl('update')
	tkgrid(cmd.frame, sticky = 'we', pady = 5)
	###
	return(cmd.frame)
}

#######################################


