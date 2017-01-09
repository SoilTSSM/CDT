
qcGetZeroCheckInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 46
		largeur1 <- 44
	}else{
		largeur <- 39
		largeur1 <- 38
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	##########################################3

	frIO <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])
	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[2]))

	txt.stnfl <- tklabel(frIO, text = 'Input data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frIO, text = "...")

	txt.file.save <- tklabel(frIO, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frIO, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frIO, text = "...")

	#####
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[3])
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	#####
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.file.save, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the daily data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
	infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')

	##########################

	frOpts <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	min.nbrs <- tclVar(as.character(GeneralParameters$param.zero$Values[1]))
	max.nbrs <- tclVar(as.character(GeneralParameters$param.zero$Values[2]))
	min.days <- tclVar(as.character(GeneralParameters$param.zero$Values[3]))
	max.dst <- tclVar(as.character(GeneralParameters$param.zero$Values[4]))
	pct.trsh <- tclVar(as.character(GeneralParameters$param.zero$Values[5]))


	min.nbrsLab <- tklabel(frOpts, text = 'Min.ngbrs', anchor = 'e', justify = 'right')
	max.nbrsLab <- tklabel(frOpts, text = 'Max.ngbrs', anchor = 'e', justify = 'right')
	min.daysLab <- tklabel(frOpts, text = 'Min.days', anchor = 'e', justify = 'right')
	max.dstLab <- tklabel(frOpts, text = 'Max.dist', anchor = 'e', justify = 'right')
	pct.trshLab <- tklabel(frOpts, text = 'Pct.trsh', anchor = 'e', justify = 'right')

	min.nbrsEn <- tkentry(frOpts, width = 6, textvariable = min.nbrs, justify = 'left')
	max.nbrsEn <- tkentry(frOpts, width = 6, textvariable = max.nbrs, justify = 'left')
	min.daysEn <- tkentry(frOpts, width = 6, textvariable = min.days, justify = 'left')
	max.dstEn <- tkentry(frOpts, width = 6, textvariable = max.dst, justify = 'left')
	pct.trshEn <- tkentry(frOpts, width = 6, textvariable = pct.trsh, justify = 'left')

	tkgrid(min.nbrsLab, row = 0, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.nbrsEn, row = 0, column = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.nbrsLab, row = 0, column = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.nbrsEn, row = 0, column = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.daysLab, row = 1, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.daysEn, row = 1, column = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.dstLab, row = 1, column = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.dstEn, row = 1, column = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(pct.trshLab, row = 0, column = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(pct.trshEn, row = 0, column = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(min.nbrsEn, 'Minimum number of neighbors stations to use')
	status.bar.display(min.nbrsEn, TextOutputVar, 'Minimum number of neighbors stations to use')
	infobulle(max.nbrsEn, 'Maximum number of neighbors stations to use')
	status.bar.display(max.nbrsEn, TextOutputVar, 'Maximum number of neighbors stations to use')
	infobulle(min.daysEn, 'Minimum number of days in a month with observation')
	status.bar.display(min.daysEn, TextOutputVar, 'Minimum number of days in a month with observation')
	infobulle(max.dstEn, 'Maximum search  distance [in km] for neighbors stations')
	status.bar.display(max.dstEn, TextOutputVar, 'Maximum search  distance [in km] for neighbors stations')
	infobulle(pct.trshEn, "Minimum threshold (% zero.station/%zero.neighbors)\nto flag that month's observation as problematic")
	status.bar.display(pct.trshEn, TextOutputVar, "Minimum threshold (% zero.station/%zero.neighbors)\nto flag that month's observation as problematic")

	##########################
	tkgrid(frIO, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frOpts, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#########################################

	bt.opt.OK <- tkbutton(frMRG1, text = "OK")
	bt.opt.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			jfile <- getIndex.AllOpenFiles(tclvalue(file.stnfl))
			if(length(jfile) == 0){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}
			dirZeroChk <- file.path(tclvalue(file.save1), paste('ZeroCheck', getf.no.ext(tclvalue(file.stnfl)), sep = '_'))
			dirparams <- file.path(dirZeroChk, 'OriginalData')
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData')

			##
			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.save1))
			GeneralParameters$param.zero$Values <<- c(tclvalue(min.nbrs), tclvalue(max.nbrs), tclvalue(min.days), tclvalue(max.dst), tclvalue(pct.trsh))

			######
			getInitDataParams <- function(GeneralParameters){
				donstn <- getCDTdataAndDisplayMsg(AllOpenFilesData[[jfile]][[2]], 'daily')

				if(!is.null(donstn)){
					lchoixStnFr$env$stn.choix <- as.character(donstn$id)
				}else tkwait.window(tt)
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = AllOpenFilesData[[jfile]][3:4], data = donstn)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, lchoixStnFr$env$stn.choix))
			}

			###
			if(file.exists(fileparams)){
				load(fileparams)
				if(all(AllOpenFilesData[[jfile]][3:4]%in%paramsGAL$dataPars)){
					donstn <- paramsGAL$data
					lchoixStnFr$env$stn.choix <<- as.character(donstn$id)
					rm(paramsGAL)
				}else{
					retDonPar <- getInitDataParams(GeneralParameters)
					donstn <- retDonPar[[1]]$data
					lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
					rm(retDonPar)
				}
			}else{
				retDonPar <- getInitDataParams(GeneralParameters)
				donstn <- retDonPar[[1]]$data
				lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
				rm(retDonPar)
			}

			assign('donnees', donstn, envir = EnvQcZeroChkData)
			assign('baseDir', dirZeroChk, envir = EnvQcZeroChkData)

			####set choix stn
			if(GeneralParameters$retpar == 0){
				if(lchoixStnFr$env$stn.choix[1] != '') tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[1]
				else tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[2]
			}else{
				istn <- as.numeric(tclvalue(tcl(lchoixStnFr$env$stn.choix.cb, "current")))+1
				if(istn > 0) istn <- istn
				else istn <- 1
				tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[istn]
			}

			tkconfigure(lchoixStnFr$env$stn.choix.cb, values = lchoixStnFr$env$stn.choix, textvariable = lchoixStnFr$env$stn.choix.val)
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

			####button command
			if(is.null(lcmd.frame_qc0Chck)){
				lcmd.frame <<- QcZeroCheckCmd(stateReplaceAll)
				lcmd.frame_qc0Chck <<- 1
			}
			GeneralParameters$retpar <<- GeneralParameters$retpar+1
			########
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}

	})

	tkconfigure(bt.opt.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Zeros Check-Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}



