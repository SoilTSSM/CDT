rhtests_inputData <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()

	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 35
		wtkcombo <- 32
		spady <- 0
	} else{
		largeur <- 26
		wtkcombo <- 25
		spady <- 1
	}

	###############################################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	frLeft <- tkframe(frDialog, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frDialog, relief = "groove", borderwidth = 2)

	###############################################################

	frIO <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	file.choix1a <- tclVar(GeneralParameters$IO.files$Cand.file)
	file.choix1b <- tclVar(GeneralParameters$IO.files$Ref.file)

	homo.state <- if(GeneralParameters$stn.type$single.series & GeneralParameters$use.refSeries) 'normal' else 'disabled'

	cb.period <- ttkcombobox(frIO, values = cb.periodVAL, textvariable = file.period, width = wtkcombo)
	txt.file.stn1 <- tklabel(frIO, text = 'Data containing the candidate series')
	cb.file.stn1 <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1a, width = wtkcombo)
	bt.file.stn1 <- tkbutton(frIO, text = "...")
	txt.file.stn2 <- tklabel(frIO, text = 'Reference series')
	cb.file.stn2 <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1b, width = wtkcombo, state = homo.state)
	bt.file.stn2 <- tkbutton(frIO, text = "...", state = homo.state)

	#########################

	tkconfigure(bt.file.stn1, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix1a) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1, values = unlist(listOpenFiles), textvariable = file.choix1a)
			tkconfigure(cb.file.stn2, values = unlist(listOpenFiles), textvariable = file.choix1b)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.stn2, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix1b) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1, values = unlist(listOpenFiles), textvariable = file.choix1a)
			tkconfigure(cb.file.stn2, values = unlist(listOpenFiles), textvariable = file.choix1b)
		}else{
			return(NULL)
		}
	})

	#########################

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn1, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn2, row = 4, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')
	infobulle(cb.file.stn1, 'Select the file containing the candidate series')
	status.bar.display(cb.file.stn1, TextOutputVar, 'Select the file containing the candidate series')
	infobulle(bt.file.stn1, 'Browse the file if not listed')
	status.bar.display(bt.file.stn1, TextOutputVar, 'Browse the file if not listed')
	infobulle(cb.file.stn2, 'Select the file containing reference series in the list')
	status.bar.display(cb.file.stn2, TextOutputVar, 'Select the file containing the reference series')
	infobulle(bt.file.stn2, 'Browse the file if not listed')
	status.bar.display(bt.file.stn2, TextOutputVar, 'Browse the file if not listed')

	###############################################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$dir2save)

	txt.file.save <- tklabel(frSave, text = 'Directory to save result')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(GeneralParameters$IO.files$dir2save, "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist. It will be created.'), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path of the directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory to save result')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	###############################################################

	frAggr <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	cmptfun <- tclVar(GeneralParameters$aggr.var$fonction)
	miss.frac <- tclVar(GeneralParameters$aggr.var$miss.frac)
	
	computefun.l <- tklabel(frAggr, text = 'Aggregation')
	cb.cmptfun <- ttkcombobox(frAggr, values = c("mean", "sum"), textvariable = cmptfun, width = 5)
	missfrac.l <- tklabel(frAggr, text = 'Min.frac')
	missfrac.v <- tkentry(frAggr, textvariable = miss.frac, width = 5)

	tkgrid(computefun.l, cb.cmptfun, missfrac.l, missfrac.v)

	infobulle(cb.cmptfun, 'Function to be used to compute dekadal and monthly series')
	status.bar.display(cb.cmptfun, TextOutputVar, 'Function to be used to compute dekadal and monthly series')
	infobulle(missfrac.v, 'Minimum fraction of available data that must be present for the time period to compute')
	status.bar.display(missfrac.v, TextOutputVar, 'Minimum fraction of available data that must be present for the time period to compute')

	###############################################################

	tkgrid(frIO, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frAggr, row = 2, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frOneSTN <- ttklabelframe(frRight, text = 'One Station data - Options', relief = 'sunken', borderwidth = 2)

	frRef <- tkframe(frOneSTN)

	cb.rfseries.val <- tclVar(GeneralParameters$use.refSeries)

	cb.rfseries <- tkcheckbutton(frRef, variable = cb.rfseries.val, text = 'Use reference series', anchor = 'w', justify = 'left')

	tkgrid(cb.rfseries, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)

	infobulle(cb.rfseries, 'Using a reference series to perform the homogenization test')
	status.bar.display(cb.rfseries, TextOutputVar, 'Using a reference series to perform the homogenization test')

	############################

	tkbind(cb.rfseries, "<Button-1>", function(){
		if(tclvalue(cb.rfseries.val) == "0" & tclvalue(cb.1series.val) == "1") state <- 'normal'
		else if(tclvalue(cb.1series.val) == "1") state <- 'disabled'
		else  state <- 'disabled'
		tkconfigure(cb.file.stn2, state = state)
		tkconfigure(bt.file.stn2, state = state)

	})

	###############################################################

	frFFile <- tkframe(frOneSTN)

	cb.1series.val <- tclVar(GeneralParameters$stn.type$single.series)
	rbffrmt <- tclVar(GeneralParameters$stn.type$file.format)
	varcat <- tclVar(GeneralParameters$stn.type$vars)

	ffdt.state <- if(GeneralParameters$stn.type$single.series) 'normal' else 'disabled'

	cb.1series <- tkcheckbutton(frFFile, variable = cb.1series.val, text = 'One Station Series', anchor = 'w', justify = 'left')

	ffrmtL <- tklabel(frFFile, text = 'File Format', anchor = 'w', justify = 'left')
	ffrmt1 <- tkradiobutton(frFFile, text = "One variable", anchor = 'w', justify = 'left', state = ffdt.state)
	ffrmt2 <- tkradiobutton(frFFile, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left', state = ffdt.state)
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	varframe <- tkframe(frFFile)
	var2test1 <- tkradiobutton(varframe, state = ffdt.state)
	var2test2 <- tkradiobutton(varframe, state = ffdt.state)
	var2test3 <- tkradiobutton(varframe, state = ffdt.state)
	tkconfigure(var2test1, variable = varcat, value = "1")
	tkconfigure(var2test2, variable = varcat, value = "2")
	tkconfigure(var2test3, variable = varcat, value = "3")

	tkgrid(var2test1, row = 0, column = 0, padx = 2)
	tkgrid(var2test2, row = 0, column = 1, padx = 2)
	tkgrid(var2test3, row = 0, column = 2, padx = 2)

	tkgrid(cb.1series, row = 0, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(ffrmtL, row = 1, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(ffrmt1, row = 2, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(ffrmt2, row = 3, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(varframe, row = 4, column = 0, sticky = "e", padx = 1, ipadx = 1)

	infobulle(cb.1series, 'Homogenization for one station series')
	status.bar.display(cb.1series, TextOutputVar, 'The data is a series of one station')
	infobulle(ffrmt1, 'In case of single series: The file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single series: The file contains 1 variable')
	infobulle(ffrmt2, 'In case of single series: The file contains Rain, Tmax and Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single series: The file contains Rain, Tmax and Tmin in this order')
	infobulle(varframe, 'Choose the variable to test')
	status.bar.display(varframe, TextOutputVar, 'Choose the variable to test')

	############################

	tkbind(cb.1series, "<Button-1>", function(){
		state1 <- if(tclvalue(cb.1series.val) == "0" & tclvalue(cb.rfseries.val) == "1") 'normal' else 'disabled'
		tkconfigure(cb.file.stn2, state = state1)
		tkconfigure(bt.file.stn2, state = state1)

		state2 <- if(tclvalue(cb.1series.val) == "0") 'normal' else 'disabled'
		tkconfigure(ffrmt1, state = state2)
		tkconfigure(ffrmt2, state = state2)
		tkconfigure(var2test1, state = state2)
		tkconfigure(var2test2, state = state2)
		tkconfigure(var2test3, state = state2)
		tkconfigure(dtfrmt1, state = state2)
		tkconfigure(dtfrmt2, state = state2)
	})

	###############################################################

	frFDate <- tkframe(frOneSTN)

	rbdtfrmt <- tclVar(GeneralParameters$stn.type$date.format)
	vdtfrmt1 <- tclVar("YYYYMMD")
	vdtfrmt2 <- tclVar("YYYY MM D")

	dtfrmtLab <- tklabel(frFDate, text = 'Dates Format', anchor = 'w', justify = 'left')
	dtfrmt1 <- tkradiobutton(frFDate, text = tclvalue(vdtfrmt1), textvariable = vdtfrmt1, anchor = 'w', justify = 'left', state = ffdt.state)
	dtfrmt2 <- tkradiobutton(frFDate, text = tclvalue(vdtfrmt2), textvariable = vdtfrmt2, anchor = 'w', justify = 'left', state = ffdt.state)
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")

	tkgrid(dtfrmtLab, row = 0, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(dtfrmt1, row = 1, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(dtfrmt2, row = 2, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)

	infobulle(dtfrmt1, 'In case of single series: dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single series: dates are merged')
	infobulle(dtfrmt2, 'In case of single series: dates are separated by space, tabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single series: dates are separated by space, tabulation or CSV format')

	############################

	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(vdtfrmt1) <- "YYYYMMDD"
			tclvalue(vdtfrmt2) <- "YYYY MM DD"
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(vdtfrmt1) <- "YYYYMMD"
			tclvalue(vdtfrmt2) <- "YYYY MM D"
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(vdtfrmt1) <- "YYYYMM"
			tclvalue(vdtfrmt2) <- "YYYY MM"
		}
	})

	###############################################################

	tkgrid(frRef, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, ipady = 1)
	tkgrid(frFFile, row = 1, column = 0, sticky = 'we', padx = 1, ipadx = 1, ipady = 1)
	tkgrid(frFDate, row = 2, column = 0, sticky = 'we', padx = 1, ipadx = 1, ipady = 1)

	tkgrid(frOneSTN, sticky = 'nswe')

	###############################################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###############################################################

	bt.opt.OK <- tkbutton(frButt, text = "OK")
	bt.opt.CA <- tkbutton(frButt, text = "Cancel")

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1a) == ""){
			tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
		}else if(tclvalue(cb.1series.val) == '1' & tclvalue(cb.rfseries.val) == '1' & tclvalue(file.choix1b) == ""){
			tkmessageBox(message = "Provide the file containing the reference series", icon = "warning", type = "ok")
		}else{
			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' = 'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$IO.files$Cand.file <<- str_trim(tclvalue(file.choix1a))
			GeneralParameters$IO.files$Ref.file <<- str_trim(tclvalue(file.choix1b))
			GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
			GeneralParameters$aggr.var$fonction <<- str_trim(tclvalue(cmptfun))
			GeneralParameters$aggr.var$miss.frac <<- as.numeric(str_trim(tclvalue(miss.frac)))
			GeneralParameters$stn.type$single.series <<- switch(tclvalue(cb.1series.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$stn.type$file.format <<- str_trim(tclvalue(rbffrmt))
			GeneralParameters$stn.type$date.format <<- str_trim(tclvalue(rbdtfrmt))
			GeneralParameters$stn.type$vars <<- str_trim(tclvalue(varcat))
			GeneralParameters$use.refSeries <<- switch(tclvalue(cb.rfseries.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$getdata <<- TRUE

			##set choix stn
			if(tclvalue(cb.1series.val) == '0'){
				donne <- getCDTdata(file.choix1a, file.period)
				if(!is.null(donne)){
					lchoixStnFr$env$stn.choix <<- donne$id
					tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[1]
					tkconfigure(lchoixStnFr$env$stn.choix.prev, state = 'normal')
					tkconfigure(lchoixStnFr$env$stn.choix.next, state = 'normal')
				}else InsertMessagesTxt(main.txt.out, 'Error loading station data', format = TRUE)
			}else{
				lchoixStnFr$env$stn.choix <<- getf.no.ext(tclvalue(file.choix1a))
				tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix
			}
			tkconfigure(lchoixStnFr$env$stn.choix.cb, values = lchoixStnFr$env$stn.choix, textvariable = lchoixStnFr$env$stn.choix.val)

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.opt.CA, command = function(){
		GeneralParameters$getdata <<- FALSE
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###############################################################

	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Input Data - Settings')
	tkwm.deiconify(tt)

	##################################################################
	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(GeneralParameters)
}

################################################################

rhtests_nghbStnOpts <- function(top.win, GeneralParameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frcont <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frok <- tkframe(tt1)

	###############################################################

	frE12a <- tkframe(frcont, relief = 'sunken', borderwidth = 2)

	min.stn <- tclVar(GeneralParameters$refSeries.choix$min.stn)
	max.stn <- tclVar(GeneralParameters$refSeries.choix$max.stn)
	max.dist <- tclVar(GeneralParameters$refSeries.choix$max.dist)
	elv.diff <- tclVar(GeneralParameters$refSeries.choix$elv.diff)
	min.rho <- tclVar(GeneralParameters$refSeries.choix$min.rho)

	min.stn.l <- tklabel(frE12a, text = 'Min.stn', anchor = 'e', justify = 'right')
	min.stn.v <- tkentry(frE12a, width = 8, textvariable = min.stn)
	max.stn.l <- tklabel(frE12a, text = 'Max.stn', anchor = 'e', justify = 'right')
	max.stn.v <- tkentry(frE12a, width = 8, textvariable = max.stn)
	max.dist.l <- tklabel(frE12a, text = 'Max.dist(km)', anchor = 'e', justify = 'right')
	max.dist.v <- tkentry(frE12a, width = 8, textvariable = max.dist)
	elv.diff.l <- tklabel(frE12a, text = 'Elv.diff(m)', anchor = 'e', justify = 'right')
	elv.diff.v <- tkentry(frE12a, width = 8, textvariable = elv.diff)
	min.rho.l <- tklabel(frE12a, text = 'Min.rho', anchor = 'e', justify = 'right')
	min.rho.v <- tkentry(frE12a, width = 8, textvariable = min.rho)

	tkgrid(min.stn.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.stn.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.stn.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.stn.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.rho.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.rho.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)

	infobulle(min.stn.v, 'Minimum number of neighbor stations to use')
	status.bar.display(min.stn.v, TextOutputVar, 'Minimum number of neighbor stations to use')
	infobulle(max.stn.v, 'Maximum number of neighbor stations to use')
	status.bar.display(max.stn.v, TextOutputVar, 'Maximum number of neighbor stations to use')
	infobulle(max.dist.v, 'Maximum distance of neighbor stations to use (km)')
	status.bar.display(max.dist.v, TextOutputVar, 'Maximum distance of neighbor stations to use (km)')
	infobulle(elv.diff.v, 'Maximum altitude difference of neighbor stations to use (m)')
	status.bar.display(elv.diff.v, TextOutputVar, 'Maximum altitude difference of neighbor stations to use (m)')
	infobulle(min.rho.v, 'Minimum correlation coefficients between candidate and neighbor series')
	status.bar.display(min.rho.v, TextOutputVar, 'Minimum correlation coefficients between candidate and neighbor series')

	###############################################################

	tkgrid(frE12a, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 5)

	###############################################################
	bt.prm.OK <- tkbutton(frok, text = "OK")
	bt.prm.CA <- tkbutton(frok, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$refSeries.choix$min.stn <<- as.numeric(str_trim(tclvalue(min.stn)))
		GeneralParameters$refSeries.choix$max.stn <<- as.numeric(str_trim(tclvalue(max.stn)))
		GeneralParameters$refSeries.choix$max.dist <<- as.numeric(str_trim(tclvalue(max.dist)))
		GeneralParameters$refSeries.choix$elv.diff <<- as.numeric(str_trim(tclvalue(elv.diff)))
		GeneralParameters$refSeries.choix$min.rho <<- as.numeric(str_trim(tclvalue(min.rho)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###############################################################

	tkgrid(frcont, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frok, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Neighbor stations selection')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

#################################################################

rhtests_useElv <- function(top.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- if(Sys.info()["sysname"] == "Windows") 35 else 30

	###############################################################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frcont <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frok <- tkframe(tt1)

	###############################################################

	frE21 <- tkframe(frcont, relief = 'sunken', borderwidth = 2)

	uselv.ch <- tclVar(GeneralParameters$refSeries.choix$interp.dem)

	uselv.dem <- tkradiobutton(frE21, text = "Elevation from DEM", anchor = 'w', justify = 'left')
	uselv.dat <- tkradiobutton(frE21, text = "Elevation from STN data", anchor = 'w', justify = 'left')
	tkconfigure(uselv.dem, variable = uselv.ch, value = "0")
	tkconfigure(uselv.dat, variable = uselv.ch, value = "1")


	tkgrid(uselv.dem, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(uselv.dat, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	infobulle(uselv.dem, 'Check this box if the elevation data will be extracted from DEM')
	status.bar.display(uselv.dem, TextOutputVar, 'Check this box if the elevation data will be extracted from DEM')
	infobulle(uselv.dat, 'Check this box if the elevation data from CDT station data')
	status.bar.display(uselv.dat, TextOutputVar, 'Check this box if the elevation data from CDT station data')

	###############################################################

	frE22 <- tkframe(frcont, relief = 'sunken', borderwidth = 2)

	file.choix2 <- tclVar(GeneralParameters$IO.files$DEM.file)
	statedem <- if(GeneralParameters$refSeries.choix$interp.dem == '0') 'normal' else 'disabled'

	txt.file.elv <- tklabel(frE22, text = 'Elevation Data (NetCDF)', anchor = 'w', justify = 'left')
	cb.file.elv <- ttkcombobox(frE22, values = unlist(listOpenFiles), textvariable = file.choix2, state = statedem, width = largeur)
	bt.file.elv <- tkbutton(frE22, text = "...", state = statedem)

	tkconfigure(bt.file.elv, command = function(){
		nc.opfiles <- getOpenNetcdf(top.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
		}
	})

	tkgrid(txt.file.elv, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.elv, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.elv, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.file.elv, 'Select the file containing the elevation data')
	status.bar.display(cb.file.elv, TextOutputVar, 'Select the file containing the elevation data')
	infobulle(bt.file.elv, 'Browse file if not listed')
	status.bar.display(bt.file.elv, TextOutputVar, 'Browse file if not listed')

	###########

	tkbind(uselv.dat, "<Button-1>", function(){
		if(tclvalue(uselv.ch) == "0"){
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
		}
	})

	tkbind(uselv.dem, "<Button-1>", function(){
		if(tclvalue(uselv.ch) == "1"){
			tkconfigure(cb.file.elv, state = 'normal')
			tkconfigure(bt.file.elv, state = 'normal')
		}
	})

	###############################################################

	tkgrid(frE21, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 5)
	tkgrid(frE22, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 5)

	###############################################################

	bt.prm.OK <- tkbutton(frok, text = "OK")
	bt.prm.CA <- tkbutton(frok, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(file.choix2) == "" & tclvalue(uselv.ch) == '0'){
			tkmessageBox(message = "You have to provide DEM in NetCDF format if you want to extract elevation data from DEM", icon = "warning", type = "ok")
		}else{
			GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.choix2))
			GeneralParameters$refSeries.choix$interp.dem <<- str_trim(tclvalue(uselv.ch))
			GeneralParameters$refSeries.choix$use.elv <<- TRUE

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###############################################################

	tkgrid(frcont, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frok, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Elevation data')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}


