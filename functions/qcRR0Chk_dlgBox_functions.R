
qcGetZeroCheckInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows") largeur <- 30
	else largeur <- 28

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A)

	pr.relief.set <- c('sunken', 'sunken', 'sunken')
	for(i in 0:2) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:2) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##########################################3

	fr.A00 <- tkframe(fr.A0)
	fr.A01 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A01, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##########
	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	frA00.txt <- tklabel(fr.A00, text = 'Input data file')
	tkgrid(frA00.txt)

	cb.stnfl <- ttkcombobox(fr.A01, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur-1)
	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the daily data')

	bt.stnfl <- tkbutton.h(fr.A01, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.stnfl, bt.stnfl)
	tkgrid.configure(cb.stnfl, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.stnfl, row = 0, column = 1, sticky = 'e')
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

	#######

	fr.A11 <- tkframe(fr.A1)
	fr.A12 <- tkframe(fr.A1)
	tkgrid(fr.A11, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A12, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA11.txt <- tklabel(fr.A11, text = 'Directory to save result')
	tkgrid(frA11.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[2]))
	en.file.save <- tkentry(fr.A12, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A12, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[3])
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	#########################################

	fr.A20 <- tkframe(fr.A2)
	tkgrid(fr.A20, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###
	min.nbrsLab <- tklabel.h(fr.A20, 'Min.ngbrs', TextOutputVar,
	'Minimum number of neighbors stations to use',
	'Minimum number of neighbors stations to use')
	max.nbrsLab <- tklabel.h(fr.A20, 'Max.ngbrs', TextOutputVar,
	'Maximum number of neighbors stations to use',
	'Maximum number of neighbors stations to use')
	min.daysLab <- tklabel.h(fr.A20, 'Min.days', TextOutputVar,
	'Minimum number of days in a month with observation',
	'Minimum number of days in a month with observation')
	max.dstLab <- tklabel.h(fr.A20, 'Max.dist', TextOutputVar,
	'Maximum search  distance [in km] for neighbors stations',
	'Maximum search  distance [in km] for neighbors stations')
	pct.trshLab <- tklabel.h(fr.A20, 'Pct.trsh', TextOutputVar,
	"Minimum threshold (% zero.station/%zero.neighbors)\nto flag that month's observation as problematic",
	"Minimum threshold (% zero.station/%zero.neighbors) to flag that month's observation as problematic")

	min.nbrsEn <- tkentry.h(fr.A20, TextOutputVar,
	'Minimum number of neighbors stations to use',
	'Minimum number of neighbors stations to use')
	max.nbrsEn <- tkentry.h(fr.A20, TextOutputVar,
	'Maximum number of neighbors stations to use',
	'Maximum number of neighbors stations to use')
	min.daysEn <- tkentry.h(fr.A20, TextOutputVar,
	'Minimum number of days in a month with observation',
	'Minimum number of days in a month with observation')
	max.dstEn <- tkentry.h(fr.A20, TextOutputVar,
	'Maximum search  distance [in km] for neighbors stations',
	'Maximum search  distance [in km] for neighbors stations')
	pct.trshEn <- tkentry.h(fr.A20, TextOutputVar,
	"Minimum threshold (% zero.station/%zero.neighbors)\nto flag that month's observation as problematic",
	"Minimum threshold (% zero.station/%zero.neighbors) to flag that month's observation as problematic")

	tkgrid(min.nbrsLab, row = 0, column = 0, padx = 1, pady = 1)
	tkgrid(min.nbrsEn, row = 0, column = 1, padx = 1, pady = 1)
	tkgrid(max.nbrsLab, row = 0, column = 2, padx = 1, pady = 1)
	tkgrid(max.nbrsEn, row = 0, column = 3, padx = 1, pady = 1)
	tkgrid(min.daysLab, row = 1, column = 0, padx = 1, pady = 1)
	tkgrid(min.daysEn, row = 1, column = 1, padx = 1, pady = 1)
	tkgrid(max.dstLab, row = 1, column = 2, padx = 1, pady = 1)
	tkgrid(max.dstEn, row = 1, column = 3, padx = 1, pady = 1)
	tkgrid(pct.trshLab, row = 2, column = 0, padx = 1, pady = 1)
	tkgrid(pct.trshEn, row = 2, column = 1, padx = 1, pady = 1)

	tkconfigure(min.nbrsLab, anchor = 'e', justify = 'right')
	tkconfigure(max.nbrsLab, anchor = 'e', justify = 'right')
	tkconfigure(min.daysLab, anchor = 'e', justify = 'right')
	tkconfigure(max.dstLab, anchor = 'e', justify = 'right')
	tkconfigure(pct.trshLab, anchor = 'e', justify = 'right')

	min.nbrs <- tclVar(as.character(GeneralParameters$param.zero$Values[1]))
	max.nbrs <- tclVar(as.character(GeneralParameters$param.zero$Values[2]))
	min.days <- tclVar(as.character(GeneralParameters$param.zero$Values[3]))
	max.dst <- tclVar(as.character(GeneralParameters$param.zero$Values[4]))
	pct.trsh <- tclVar(as.character(GeneralParameters$param.zero$Values[5]))

	tkconfigure(min.nbrsEn, width = 6, textvariable = min.nbrs, justify = 'left')
	tkconfigure(max.nbrsEn, width = 6, textvariable = max.nbrs, justify = 'left')
	tkconfigure(min.daysEn, width = 6, textvariable = min.days, justify = 'left')
	tkconfigure(max.dstEn, width = 6, textvariable = max.dst, justify = 'left')
	tkconfigure(pct.trshEn, width = 6, textvariable = pct.trsh, justify = 'left')

	#########################################

	bt.opt.OK <- tkbutton(frMRG1, text = "OK")
	bt.opt.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{

			all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
			jfile <- which(all.open.file == tclvalue(file.stnfl))
			if(length(jfile) == 0){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}
			dirZeroChk <- file.path(tclvalue(file.save1), paste('ZeroCheck', getf.no.ext(tclvalue(file.stnfl)), sep = '_'), fsep = .Platform$file.sep)
			dirparams <- file.path(dirZeroChk, 'OriginalData', fsep = .Platform$file.sep)
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData', fsep = .Platform$file.sep)

			##
			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.save1))
			GeneralParameters$param.zero$Values <<- c(tclvalue(min.nbrs), tclvalue(max.nbrs), tclvalue(min.days), tclvalue(max.dst), tclvalue(pct.trsh))

			######
			getInitDataParams <- function(GeneralParameters){
				donstn <- getCDTdataAndDisplayMsg(AllOpenFilesData[[jfile]][[2]], 'daily')

				if(!is.null(donstn)){
					stn.choix <- as.character(donstn$id)
				}else tkwait.window(tt)
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = AllOpenFilesData[[jfile]][3:4], data = donstn)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, stn.choix))
			}

			###
			if(file.exists(fileparams)){
				load(fileparams)
				if(all(AllOpenFilesData[[jfile]][3:4]%in%paramsGAL$dataPars)){
					donstn <- paramsGAL$data
					stn.choix <<- as.character(donstn$id)
					rm(paramsGAL)
				}else{
					retDonPar <- getInitDataParams(GeneralParameters)
					donstn <- retDonPar[[1]]$data
					stn.choix <<- retDonPar[[2]]
					rm(retDonPar)
				}
			}else{
				retDonPar <- getInitDataParams(GeneralParameters)
				donstn <- retDonPar[[1]]$data
				stn.choix <<- retDonPar[[2]]
				rm(retDonPar)
			}

			assign('donnees', donstn, envir = EnvQcZeroChkData)
			assign('baseDir', dirZeroChk, envir = EnvQcZeroChkData)

			####set choix stn
			if(GeneralParameters$retpar == 0){
				if(stn.choix[1] != '') tclvalue(stn.choix.val) <- stn.choix[1]
				else tclvalue(stn.choix.val) <- stn.choix[2]
			}else{
				istn <- as.numeric(tclvalue(tcl(stn.choix.cb, "current")))+1
				if(istn > 0) istn <- istn
				else istn <- 1
				tclvalue(stn.choix.val) <- stn.choix[istn]
			}

			tkconfigure(stn.choix.cb, values = stn.choix, textvariable = stn.choix.val)
			if(GeneralParameters$AllOrOne == 'one'){
				tkconfigure(setting.button, state = 'normal')
				stateReplaceAll <- 'disabled'
			}
			if(GeneralParameters$AllOrOne == 'all'){
				tkconfigure(setting.button, state = 'disabled')
				stateReplaceAll <- 'normal'
			}
			tkconfigure(stn.choix.prev, state = 'normal')
			tkconfigure(stn.choix.next, state = 'normal')

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

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################3
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



