
HomogCmdBut <- function(GeneralParameters){
	cmd.frame <- tkframe(panel.left, relief = 'groove', bd = 2)

	####################
	homogframe <- tkframe(cmd.frame)

	cmd.preview <- tkbutton(homogframe, text = "Output Preview")
	cmd.plot <- tkbutton(homogframe, text = "Display Output")
	cmd.chgcpt <- tkbutton(homogframe, text = "Change Breakpoints")
	cmd.replot <- tkbutton(homogframe, text = "Display Change")
	cmd.reset <- tkbutton(homogframe, text = "Undo Change")
	cmd.adjust <- tkbutton(homogframe, text = "Adjust")
	cmd.ltbx <- tklistbox(homogframe, height = 3, selectbackground = "yellow", selectforeground = "blue",
							selectmode = "multiple", background = "white")

	########
	PrevwHomIdTab <- NULL
	tkconfigure(cmd.preview, command = function(){
		if(!is.null(ReturnExecResults)){
			res2disp <- ReturnExecResults
			res2disp$refSerie <- NULL

			retNBTab <- consolOutNotebookTab_unik(tknotes, res2disp, paste(res2disp$station, '-Output Preview'),
													PrevwHomIdTab, AllOpenTabType, AllOpenTabData)
			PrevwHomIdTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
	})

	########
	noteHomBrkPlot <- NULL
	tkconfigure(cmd.plot, command = function(){
		if(!is.null(ReturnExecResults) & !is.null(GeneralParameters)){
			imgContainer <- plotHomogBreakPts(tknotes, noteHomBrkPlot, replotBreak = FALSE)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, noteHomBrkPlot, AllOpenTabType, AllOpenTabData)
			noteHomBrkPlot <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
	})

	#######
	noteHomBrkPlot1 <- NULL
	tkconfigure(cmd.replot, command = function(){
		if(!is.null(ReturnExecResults) & !is.null(GeneralParameters)){
			imgContainer <- plotHomogBreakPts(tknotes, noteHomBrkPlot1, replotBreak = TRUE)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, noteHomBrkPlot1, AllOpenTabType, AllOpenTabData)
			noteHomBrkPlot1 <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
	})
	
	#######
	HomEditSetTab <- NULL
	tkconfigure(cmd.chgcpt, command = function(){
		if(!is.null(ReturnExecResults)){
			retNBTab <- tableHomogNotebookTab_unik(tknotes, paste(ReturnExecResults$station, '_Edit', sep = ''),
													HomEditSetTab, AllOpenTabType, AllOpenTabData)
			HomEditSetTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
	})

	#######
	tkconfigure(cmd.reset, command = function(){
		if(!is.null(ReturnExecResults)) undoBreaksChange()
		else InsertMessagesTxt(main.txt.out, 'There is no homogenization results', format = TRUE)
	})

	#######
	tkconfigure(cmd.adjust, command = function(){
		if(!is.null(ReturnExecResults) & !is.null(GeneralParameters)){
			adjDon <<- try(AdjustInHomog(as.numeric(tclvalue(ChoixAjustment))), silent = TRUE)
			if(!inherits(adjDon, "try-error")){
				retdata <- DisplayHomInfo(tknotes, data.frame(adjDon$Info), paste(ReturnExecResults$station, 'Adj.Info'))
				ntab <- length(AllOpenTabType)
				AllOpenTabType[[ntab+1]] <<- 'homInfo'
				AllOpenTabData[[ntab+1]] <<- retdata
				tkselect(tknotes, ntab)
				InsertMessagesTxt(main.txt.out, paste("Adjustment finished successfully for", ReturnExecResults$station))
			}else{
				InsertMessagesTxt(main.txt.out, paste("Adjustment failed for", ReturnExecResults$station), format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', adjDon[1]), format = TRUE)
			}
		}else InsertMessagesTxt(main.txt.out, 'Reinitialize the operation. Parameters or Outputs are not a homogenization results', format = TRUE)
	})

	########

	tkgrid(cmd.preview, row = 0, column = 0, padx = 1, pady = 1, sticky = "ew")
	tkgrid(cmd.plot, row = 0, column = 1, padx = 1, pady = 1, sticky = "ew")
	tkgrid(cmd.chgcpt, row = 1, column = 0, padx = 1, pady = 1, sticky = "ew")
	tkgrid(cmd.replot, row = 1, column = 1, padx = 1, pady = 1, sticky = "ew")
	tkgrid(cmd.reset, row = 2, column = 0, padx = 1, pady = 1, sticky = "ew")
	tkgrid(cmd.adjust, row = 2, column = 1, padx = 1, pady = 1, sticky = "ew")
	tkgrid(cmd.ltbx, row = 3, column = 0, padx = 1, pady = 1, sticky = "ew")

	####################

	fr.adjplot <- tkframe(homogframe)
	cmd.adjplot <- tkbutton(fr.adjplot, text = "Display Adjusted")

	######
	noteHomAdjPlot <- NULL
	tkconfigure(cmd.adjplot, command = function(){
		if(!is.null(ReturnExecResults) & !is.null(adjDon)){
			iselect <- as.numeric(tkcurselection(cmd.ltbx))+1

			imgContainer <- plotadjusted(tknotes, iselect, noteHomAdjPlot)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, noteHomAdjPlot, AllOpenTabType, AllOpenTabData)
			noteHomAdjPlot <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no adjusted data yet', format = TRUE)
	})

	######
	tkgrid(cmd.adjplot, row = 0, column = 0, padx = 1, pady = 1, sticky = "ew")
	tkgrid(fr.adjplot, row = 3, column = 1, padx = 1, pady = 1, sticky = "ew")
	tkgrid.columnconfigure(fr.adjplot, 0, weight = 1)

	####################

	tkgrid(homogframe, sticky = 'we')
	tkgrid.columnconfigure(homogframe, 0, weight = 1)
	tkgrid.columnconfigure(homogframe, 1, weight = 1)

	####################

	xseries <- c("Base series", "Adjusted by mean", "Adjusted by QM")
	for (i in 1:3)  tkinsert(cmd.ltbx, "end", xseries[i])
	tkselection.set(cmd.ltbx, 0)

	homAdjframe <- ttklabelframe(cmd.frame, text = "Adjusted series selection", labelanchor = "nw", relief = "groove", borderwidth = 2)

	ChoixAjustment <- tclVar('1')

	AdjMthdRadio1 <- tkradiobutton(homAdjframe, text = "By Mean", anchor = 'w', justify = 'left')
	AdjMthdRadio2 <- tkradiobutton(homAdjframe, text = "By Quantile Matching", anchor = 'w', justify = 'left')
	AdjMthdRadio3 <- tkradiobutton(homAdjframe, text = "Base Series", anchor = 'w', justify = 'left')
	tkconfigure(AdjMthdRadio1, variable = ChoixAjustment, value = "1")
	tkconfigure(AdjMthdRadio2, variable = ChoixAjustment, value = "2")
	tkconfigure(AdjMthdRadio3, variable = ChoixAjustment, value = "3")

	tkgrid(AdjMthdRadio1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(AdjMthdRadio2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(AdjMthdRadio3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(homAdjframe, padx = 2, pady = 5, sticky = 'we')
	tkgrid.columnconfigure(homAdjframe, 0, weight = 1)

	infobulle(homAdjframe, 'Select the adjusted series to be retained')
	status.bar.display(homAdjframe, TextOutputVar, 'Select the adjusted series to be retained')

	####################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(list(cmd.frame, GeneralParameters))
}

