saveCurrentCDTtask <- function(){
	filetypes <- "{{CDT Files} {.cdt}} {{All files} {*.*}}"
	if(Sys.info()["sysname"] == "Windows")  filename <- tclvalue(tkgetSaveFile(initialfile = "", filetypes = filetypes, defaultextension = TRUE))
	else filename <- tclvalue(tkgetSaveFile(initialfile = "", filetypes = filetypes))
	if(filename == "") return(NULL)
	else if(filename == 'NA' | is.na(filename)){
		InsertMessagesTxt(main.txt.out, 'Current session could not be saved correctly', format = TRUE)
		return(NULL)
	}else{
		lastStnChoix <- tclvalue(lchoixStnFr$env$stn.choix.val)
		listStnChoix <- lchoixStnFr$env$stn.choix
		save(AllOpenFilesType, AllOpenFilesData,
		GeneralParameters, ReturnExecResults,
		listStnChoix, lastStnChoix,
		EnvQcOutlierData, EnvQcZeroChkData,
		EnvHomogzData, adjDon,
		file = filename)
	}
}

#######################################################
OpenOldCDTtask <- function(){
	AllOpenFilesData <<- vector(mode = 'list', length = 0)
	AllOpenFilesType <<- vector(mode = 'list', length = 0)
	tkdelete(all.opfiles, 0, as.numeric(tclvalue(tksize(all.opfiles)))-1)

	fileOpenCDT <- tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = "{{CDT Files} {.cdt}} {{All files} {*.*}}")
	if(tclvalue(fileOpenCDT) != '' | !is.na(tclvalue(fileOpenCDT))) load(tclvalue(fileOpenCDT))
	if(GeneralParameters$action == 'qc.rain'	| GeneralParameters$action == 'qc.temp' | GeneralParameters$action == 'homog' | GeneralParameters$action == 'zero.check'){
		
		lchoixStnFr <<- selectStationCmd()

		if(GeneralParameters$action == 'qc.rain'	| GeneralParameters$action == 'qc.temp'){
			EnvQcOutlierData <<- EnvQcOutlierData
			if(GeneralParameters$AllOrOne == 'one'){
				tkconfigure(lchoixStnFr$env$setting.button, state = 'normal')
				stateReplaceAll <- 'disabled'
			} 
			if(GeneralParameters$AllOrOne == 'all'){
				tkconfigure(lchoixStnFr$env$setting.button, state = 'disabled')
				stateReplaceAll <- 'normal'
			} 
			if(as.character(GeneralParameters$use.method$Values[1]) == '0'){
				tkconfigure(lchoixStnFr$env$stn.choix.prev, state = 'normal')
				tkconfigure(lchoixStnFr$env$stn.choix.next, state = 'normal')
			}
			if(GeneralParameters$action == 'qc.temp') tclvalue(XYCoordinates) <<- paste(c(as.character(GeneralParameters$parameter[[2]][, 4]),
																				as.character(GeneralParameters$parameter[[2]][, 5])), sep = '', collapse = ' ')
			if(GeneralParameters$action == 'qc.rain') tclvalue(XYCoordinates) <<- paste(c(as.character(GeneralParameters$parameter[[2]][, 3]),
																				as.character(GeneralParameters$parameter[[2]][, 4])), sep = '', collapse = ' ')
			lcmd.frame <<- QcCmdBut(stateReplaceAll)
			lcmd.frame_qc <<- 1
		}else if(GeneralParameters$action == 'homog'){
			EnvHomogzData <<- EnvHomogzData
			tkconfigure(lchoixStnFr$env$setting.button, state = 'normal')
			if(as.character(GeneralParameters$use.method$Values[2]) == '0'){
				tkconfigure(lchoixStnFr$env$stn.choix.prev, state = 'normal')
				tkconfigure(lchoixStnFr$env$stn.choix.next, state = 'normal')
			}
			retcmdpars <- HomogCmdBut(GeneralParameters)
			lcmd.frame <<- retcmdpars[[1]]
			lcmd.frame_homo <<- 1
			GeneralParameters <- retcmdpars[[2]]
			adjDon <<- adjDon
		}else if(GeneralParameters$action == 'zero.check'){
			EnvQcZeroChkData <<- EnvQcZeroChkData
			if(GeneralParameters$AllOrOne == 'one'){
				tkconfigure(lchoixStnFr$env$setting.button, state = 'normal')
				stateReplaceAll <- 'disabled'
			}
			if(GeneralParameters$AllOrOne == 'all'){
				tkconfigure(lchoixStnFr$env$setting.button, state = 'disabled')
				stateReplaceAll <- 'normal'
			}
			tkconfigure(lchoixStnFr$env$stn.choix.prev, state = 'normal')
			tkconfigure(lchoixStnFr$env$stn.choix.next, state = 'normal')
			lcmd.frame <<- QcZeroCheckCmd(stateReplaceAll)
			lcmd.frame_qc0Chck <<- 1
		}
		#####
		tclvalue(lchoixStnFr$env$stn.choix.val) <- lastStnChoix
		tkconfigure(lchoixStnFr$env$stn.choix.cb, values = listStnChoix, textvariable = lchoixStnFr$env$stn.choix.val)

		tkconfigure(spinH, state = 'normal')
		tkconfigure(spinV, state = 'normal')

		nopfs <- length(AllOpenFilesType)
		if(nopfs > 0) for(j in 1:nopfs) tkinsert(all.opfiles, "end", AllOpenFilesData[[j]][[1]])

		GeneralParameters <<- GeneralParameters
		ReturnExecResults <<- ReturnExecResults
		lchoixStnFr$env$stn.choix <<- listStnChoix
		AllOpenFilesType <<- AllOpenFilesType
		AllOpenFilesData <<- AllOpenFilesData
	}else InsertMessagesTxt(main.txt.out, 'Not QC or Homogenization', format = TRUE)
}

#####################################################
