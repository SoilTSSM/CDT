
ChkCoordCmdBut <- function(){
	cmd.frame <- tkframe(panel.left)

	chkframe <- tkframe(cmd.frame, relief = 'groove', bd = 2)

	#############
	cmd.outTable <- tkbutton(chkframe, text = "View/Edit Results")
	cmd.correctCoord <- tkbutton(chkframe, text = "Correct Coordinates")

	#############
	tkconfigure(cmd.outTable, command = function(){
		if(!is.null(ReturnExecResults)){
		retdata <- DisplayHomInfo(tknotes, ReturnExecResults$Stndoute,
					paste(getf.no.ext(GeneralParameters$IO.files$STN.file), '_COORDS', sep = ''))
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'StnInfo'
		AllOpenTabData[[ntab+1]] <<- retdata
		tkselect(tknotes, ntab)
		popupAddRemoveRow(tknotes)
		}else InsertMessagesTxt(main.txt.out, 'There is no coordinates check performed yet', format = TRUE)
	})

	tkconfigure(cmd.correctCoord, command = function(){
		if(!is.null(ReturnExecResults)){
			if(!is.na(ReturnExecResults$Stndoute[1, 1])){
				tkconfigure(main.win, cursor = 'watch'); tcl('update')
				ReturnExecResults <<- try(checkCDTcoords(ReturnExecResults, GeneralParameters), silent = TRUE)
				if(!inherits(ReturnExecResults, "try-error")){
					InsertMessagesTxt(main.txt.out, "Coordinates were corrected successfully")
					tkconfigure(main.win, cursor = '')
				}else{
					InsertMessagesTxt(main.txt.out, 'Coordinates correction failed', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', ReturnExecResults[1]), format = TRUE)
					tkconfigure(main.win, cursor = '')
				}
			}
		}else InsertMessagesTxt(main.txt.out, 'There is no coordinates check performed yet', format = TRUE)
	})

	#############

	tkgrid(cmd.outTable, row = 0, column = 0, padx = 1, pady = 1, sticky = "we")
	tkgrid(cmd.correctCoord, row = 0, column = 1, padx = 1, pady = 1, sticky = "we")

	#############
	tkgrid(chkframe, sticky = 'we', ipadx = 5, ipady = 10)
	for(i in 0:1) tkgrid.columnconfigure(chkframe, i, weight = 1)

	#############
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	return(cmd.frame)
}

#######################################


