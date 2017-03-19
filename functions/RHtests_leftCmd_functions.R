
RHtestsV4Cmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(34)
		lfindu <- as.integer(w.scale(13)/sfont0)
		largeur <- as.integer(w.scale(27)/sfont0)
		largeur1 <- as.integer(w.scale(25)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(36)
		lfindu <- as.integer(w.scale(11)/sfont0)
		largeur <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(25)/sfont0)
	}

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Data&Params")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "RefSerCreation")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "RHtestsV4")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Output")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

	#######################################################################################################

	#Tab1
	frTab1 <- tkframe(cmd.tab1)
	tkgrid(frTab1, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab1, 0, weight = 1)

	scrw1 <- bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	tkgrid.columnconfigure(scrw1, 0, weight = 1)
	subfr1 <- bwScrollableFrame(scrw1, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr1, 0, weight = 1)

	##############

	prcpdata.val <- tclVar(GeneralParameters$prcp.data$rr.data)
	prcplog.val <- tclVar(GeneralParameters$prcp.data$rr.log)
	p.levStr <- tclVar(sprintf("%1.4f", GeneralParameters$rhtests.pars$p.lev))
	IadjStr <- tclVar(GeneralParameters$rhtests.pars$Iadj)
	MqStr <- tclVar(GeneralParameters$rhtests.pars$Mq)
	Ny4aStr <- tclVar(GeneralParameters$rhtests.pars$Ny4a)
	pthrStr <- tclVar(GeneralParameters$rhtests.pars$pthr)

	PLEV <- sprintf("%1.4f", c(0.75, 0.80, 0.90, 0.95, 0.99, 0.9999))
	ChoixAjustment <- tclVar('3')

	getData.tab1 <- tkbutton(subfr1, text = "Input Data", bg = 'lightgreen', width = largeur)

	sep1.tab1 <- ttkseparator(subfr1)
	framePrcp.tab1 <- ttklabelframe(subfr1, text = 'Precipitation Data', relief = 'groove', borderwidth = 2)
	prcpdata.tab1 <- tkcheckbutton(framePrcp.tab1, variable = prcpdata.val, text = 'Precipitation series', anchor = 'w', justify = 'left')
	prcplog.tab1 <- tkcheckbutton(framePrcp.tab1, variable = prcplog.val, text = 'Use log-transformed series', anchor = 'w', justify = 'left')

	framePars.tab1 <- ttklabelframe(subfr1, text = 'Parameters', relief = 'groove', borderwidth = 2)
	plevLb.tab1 <- tklabel(framePars.tab1, text = 'p.lev', anchor = 'e', justify = 'right')
	plevEd.tab1 <- ttkcombobox(framePars.tab1, values = PLEV, textvariable = p.levStr, width = 6)
	iadjLb.tab1 <- tklabel(framePars.tab1, text = 'Iadj', anchor = 'e', justify = 'right')
	iadjEd.tab1 <- tkentry(framePars.tab1, textvariable = IadjStr, width = 6)
	mqLb.tab1 <- tklabel(framePars.tab1, text = 'Mq', anchor = 'e', justify = 'right')
	mqEd.tab1 <- tkentry(framePars.tab1, textvariable = MqStr, width = 5)
	ny4aLb.tab1 <- tklabel(framePars.tab1, text = 'Ny4a', anchor = 'e', justify = 'right')
	ny4aEd.tab1 <- tkentry(framePars.tab1, textvariable = Ny4aStr, width = 5)
	pthrLb.tab1 <- tklabel(framePars.tab1, text = 'pthr', anchor = 'e', justify = 'right')
	pthrEd.tab1 <- tkentry(framePars.tab1, textvariable = pthrStr, width = 5)

	homAdjframe.tab1 <- ttklabelframe(subfr1, text = "Adjusted series selection", labelanchor = "nw",
												relief = "groove", borderwidth = 2)
	AdjMthdRadio1 <- tkradiobutton(homAdjframe.tab1, text = "By Mean", anchor = 'w', justify = 'left')
	AdjMthdRadio2 <- tkradiobutton(homAdjframe.tab1, text = "By Quantile Matching", anchor = 'w', justify = 'left')
	AdjMthdRadio3 <- tkradiobutton(homAdjframe.tab1, text = "Base Series", anchor = 'w', justify = 'left')
	tkconfigure(AdjMthdRadio1, variable = ChoixAjustment, value = "1")
	tkconfigure(AdjMthdRadio2, variable = ChoixAjustment, value = "2")
	tkconfigure(AdjMthdRadio3, variable = ChoixAjustment, value = "3")

	##########
	donnees <- NULL

	tkconfigure(getData.tab1, command = function(){
		GeneralParameters <<- rhtests_inputData(main.win, GeneralParameters)
		if(GeneralParameters$IO.files$Cand.file != "" & GeneralParameters$getdata) {
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			donnees <<- tryCatch(getRHtestsData(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			if(GeneralParameters$use.refSeries){
				tcl(userefS.tab2, 'select')
				tkconfigure(nghbStnBt.tab2, state = 'normal')
			}else{
				tcl(userefS.tab2, 'deselect')
				tkconfigure(nghbStnBt.tab2, state = 'disabled')
			}
		}
	})

	#############################################

	tkgrid(getData.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 1)

	tkgrid(sep1.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 2)
	tkgrid(framePrcp.tab1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 3, ipady = 2)
	tkgrid(prcpdata.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(prcplog.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(framePars.tab1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 2)
	tkgrid(plevLb.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(plevEd.tab1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mqLb.tab1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mqEd.tab1, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(ny4aLb.tab1, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(ny4aEd.tab1, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(iadjLb.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(iadjEd.tab1, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(pthrLb.tab1, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(pthrEd.tab1, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(homAdjframe.tab1, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 2)
	tkgrid(AdjMthdRadio1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 0)
	tkgrid(AdjMthdRadio2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 0)
	tkgrid(AdjMthdRadio3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 0)

	infobulle(prcpdata.tab1, 'Must be checked in case of precipitation data')
	status.bar.display(prcpdata.tab1, TextOutputVar, 'Must be checked in case of precipitation data')
	infobulle(prcplog.tab1, 'Check if applying a log transformation to monthly  precipitation series')
	status.bar.display(prcplog.tab1, TextOutputVar, 'Check if applying a log transformation to monthly precipitation series')
	infobulle(plevEd.tab1, 'Select the nominal conf. level p.lev value.')
	status.bar.display(plevEd.tab1, TextOutputVar, 'Select the nominal conf. level p.lev value.')
	infobulle(iadjEd.tab1, 'Enter Iadj (Integer between 0 to 10000 inclusive)')
	status.bar.display(iadjEd.tab1, TextOutputVar, 'Enter Iadj (Integer between 0 to 10000 inclusive)')
	infobulle(mqEd.tab1, 'Enter Mq (integer # of points for evaluating PDF)')
	status.bar.display(mqEd.tab1, TextOutputVar, 'Enter Mq (integer # of points for evaluating PDF)')
	infobulle(ny4aEd.tab1, 'Enter Ny4a (integer >= 5, or 0 for choosing the whole segment)')
	status.bar.display(ny4aEd.tab1, TextOutputVar, 'Enter Ny4a (integer >= 5, or 0 for choosing the whole segment)')
	infobulle(pthrEd.tab1, 'Enter the lower precipitation threshold pthr (>= 0) [for .dlyPrcp only]')
	status.bar.display(pthrEd.tab1, TextOutputVar, 'Enter the lower precipitation threshold pthr (>= 0) [for .dlyPrcp only]')
	infobulle(homAdjframe.tab1, 'Select the adjusted series to be retained')
	status.bar.display(homAdjframe.tab1, TextOutputVar, 'Select the adjusted series to be retained')

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr1 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr1, 0, weight = 1)
	##############

	use.rfseries.val <- tclVar(GeneralParameters$use.refSeries)
	use.elv <- tclVar(GeneralParameters$refSeries.choix$use.elv)
	weight.mean <- tclVar(GeneralParameters$refSeries.choix$weight.mean)
	usr.rfseries.val <- tclVar(GeneralParameters$refSeries.by.user)

	stateStn <- if(GeneralParameters$use.refSeries) 'normal' else 'disabled'
	stateRef <- if(GeneralParameters$refSeries.choix$use.elv) 'normal' else 'disabled'
	stateElv <- if(GeneralParameters$refSeries.by.user) 'normal' else 'disabled'

	Weight.Factors <- c('Distance', 'Correlation', 'Optimal', 'Average')

	userefS.tab2 <- tkcheckbutton(subfr1, variable = use.rfseries.val, text = 'Use reference series',
									anchor = 'w', justify = 'left', bg = 'lightgreen', width = largeur1)

	sep1.tab2 <- ttkseparator(subfr1)
	nghbStnLb.tab2 <- tklabel(subfr1, text = 'Neighbor stations selection', anchor = 'w', justify = 'right')
	nghbStnBt.tab2 <- tkbutton(subfr1, text = "Options", state = stateStn)

	sep2.tab2 <- ttkseparator(subfr1)
	useElv.tab2 <- tkcheckbutton(subfr1, text = 'Use Elevation', variable = use.elv, anchor = 'w', justify = 'left')
	useElvBt.tab2 <- tkbutton(subfr1, text = "Settings", state = stateRef)

	sep3.tab2 <- ttkseparator(subfr1)
	wghtfacLb.tab2 <- tklabel(subfr1, text = "Weighting Factors", anchor = 'w', justify = 'left')
	wghtfacCmb.tab2 <- ttkcombobox(subfr1, values = Weight.Factors, textvariable = weight.mean, width = 9)

	sep4.tab2 <- ttkseparator(subfr1)
	usr.rfseries.tab2 <- tkcheckbutton(subfr1, variable = usr.rfseries.val, text = "Stations selected by User", anchor = 'w', justify = 'left')
	usrbt.rfseries.tab2 <- tkbutton(subfr1, text = "Select", state = stateElv)

	##############

	tkconfigure(nghbStnBt.tab2, command = function(){
		GeneralParameters <<- rhtests_nghbStnOpts(main.win, GeneralParameters)
	})

	tkconfigure(useElvBt.tab2, command = function(){
		GeneralParameters <<- rhtests_useElv(main.win, GeneralParameters)
	})

	tkconfigure(usrbt.rfseries.tab2, command = function(){
		file2test <- GeneralParameters$IO.files$Cand.file
		if(file2test != ""){
			donstn <- getStnOpenData(file2test)
			if(is.null(donstn)){
				tkmessageBox(message = "File not found or in the wrong format", icon = "warning", type = "ok")
				return(NULL)
			}else{
				donstn <- getCDTdataAndDisplayMsg(donstn, GeneralParameters$period)
				GeneralParameters <<- refSeriesUsersChoice(main.win, donstn$id, GeneralParameters)
			}
		}else tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
	})

	##############

	tkgrid(userefS.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep1.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(nghbStnLb.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(nghbStnBt.tab2, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep2.tab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(useElv.tab2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(useElvBt.tab2, row = 4, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab2, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(wghtfacLb.tab2, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(wghtfacCmb.tab2, row = 6, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep4.tab2, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(usr.rfseries.tab2, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(usrbt.rfseries.tab2, row = 8, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(userefS.tab2, 'Using a reference series to perform the homogenization test')
	status.bar.display(userefS.tab2, TextOutputVar, 'Using a reference series to perform the homogenization test')
	infobulle(nghbStnBt.tab2, 'Set options to select neighbor stations')
	status.bar.display(nghbStnBt.tab2, TextOutputVar, 'Set options to select neighbor stations')
	infobulle(useElv.tab2, 'Check to use elevation data to choose neighbors stations')
	status.bar.display(useElv.tab2, TextOutputVar, 'Check for using elevation data to choose neighbor stations')
	infobulle(useElvBt.tab2, 'Select elevation data')
	status.bar.display(useElvBt.tab2, TextOutputVar, 'Select elevation data')
	infobulle(wghtfacLb.tab2, 'Method to build weights for linear combinations')
	status.bar.display(wghtfacLb.tab2, TextOutputVar, 'Method to build weights for linear combinations')
	infobulle(wghtfacCmb.tab2, 'Method to build weights for linear combinations')
	status.bar.display(wghtfacCmb.tab2, TextOutputVar, 'Method to build weights for linear combinations')
	infobulle(usr.rfseries.tab2, 'The reference series will be created from stations chosen by user')
	status.bar.display(usr.rfseries.tab2, TextOutputVar, "The reference series will be created from stations chosen by user")
	infobulle(usrbt.rfseries.tab2, 'Select the stations to create the reference series')
	status.bar.display(usrbt.rfseries.tab2, TextOutputVar, 'Select the stations to create the reference series')

	########################################

	tkbind(userefS.tab2, "<Button-1>", function(){
		if(tclvalue(use.rfseries.val) == "0"){
			tkconfigure(nghbStnBt.tab2, state = 'normal')
			if(tclvalue(use.elv) == "1") tkconfigure(useElvBt.tab2, state = 'normal')
			if(tclvalue(usr.rfseries.val) == "1") tkconfigure(usrbt.rfseries.tab2, state = 'normal')
		}else{
			tkconfigure(nghbStnBt.tab2, state = 'disabled')
			tkconfigure(useElvBt.tab2, state = 'disabled')
			tkconfigure(usrbt.rfseries.tab2, state = 'disabled')
			tclvalue(use.elv) <- "0"
			tclvalue(usr.rfseries.val) <- "0"
		}
	})

	tkbind(useElv.tab2, "<Button-1>", function(){
		if(tclvalue(use.rfseries.val) == "1"){
			state <- if(tclvalue(use.elv) == "0") 'normal' else 'disabled'
			tkconfigure(useElvBt.tab2, state = state)
		}
	})

	tkbind(usr.rfseries.tab2, "<Button-1>", function(){
		if(tclvalue(use.rfseries.val) == "1"){
			state <- if(tclvalue(usr.rfseries.val) == "0") 'normal' else 'disabled'
			tkconfigure(usrbt.rfseries.tab2, state = state)
		}
	})

	#######################################################################################################

	#Tab3
	frTab3 <- tkframe(cmd.tab3)
	tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab3, 0, weight = 1)

	scrw3 <- bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	tkgrid.columnconfigure(scrw3, 0, weight = 1)
	subfr3 <- bwScrollableFrame(scrw3, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr3, 0, weight = 1)

	##############

	findU.tab3 <- tkbutton(subfr3, text = "FindU", width = lfindu)
	findUD.tab3 <- tkbutton(subfr3, text = "FindUD")
	stepsize.tab3 <- tkbutton(subfr3, text = "StepSize")
	qmadj.tab3 <- tkbutton(subfr3, text = "QMadj")

	findUr.tab3 <- tkbutton(subfr3, text = "FindU.wRef", width = lfindu)
	findUDr.tab3 <- tkbutton(subfr3, text = "FindUD.wRef")
	stepsizer.tab3 <- tkbutton(subfr3, text = "StepSize.wRef")
	qmadjr.tab3 <- tkbutton(subfr3, text = "QMadj.wRef")

	sep1.tab3 <- ttkseparator(subfr3)
	dlyPrcpLb.tab3 <- tklabel(subfr3, text = 'Homogenization of daily precipitation', anchor = 'w', justify = 'left')

	findUp.tab3 <- tkbutton(subfr3, text = "FindU.dlyPrcp")
	findUDp.tab3 <- tkbutton(subfr3, text = "FindUD.dlyPrcp")
	stepsizep.tab3 <- tkbutton(subfr3, text = "StepSize.dlyPrcp")
	qmadjp.tab3 <- tkbutton(subfr3, text = "getAdjusted Data")

	#############################################

	tkgrid(findU.tab3, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(findUr.tab3, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(findUD.tab3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(findUDr.tab3, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(stepsize.tab3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(stepsizer.tab3, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(qmadj.tab3, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(qmadjr.tab3, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1.tab3, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
	tkgrid(dlyPrcpLb.tab3, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)

	tkgrid(findUp.tab3, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(findUDp.tab3, row = 6, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(stepsizep.tab3, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(qmadjp.tab3, row = 7, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4)
	tkgrid(frTab4, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab4, 0, weight = 1)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	tkgrid.columnconfigure(scrw4, 0, weight = 1)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr4, 0, weight = 1)
	##############

	preview.tab4 <- tkbutton(subfr4, text = "Output Preview", width = lfindu)
	viewplot.tab4 <- tkbutton(subfr4, text = "Display Output", width = lfindu)
	change.tab4 <- tkbutton(subfr4, text = "Edit Breakpoints")
	undo.tab4 <- tkbutton(subfr4, text = "Undo Change")

	tkgrid(preview.tab4, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(viewplot.tab4, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(change.tab4, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(undo.tab4, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(preview.tab4, 'View results <_*Cs.txt > and <_*stat.txt > files')
	status.bar.display(preview.tab4, TextOutputVar, 'View results <_*Cs.txt > and <_*stat.txt > files')
	infobulle(viewplot.tab4, 'Display result plots in pdf file format')
	status.bar.display(viewplot.tab4, TextOutputVar, 'Display result plots in pdf file format')
	infobulle(change.tab4, 'Edit <_mCs.txt > file')
	status.bar.display(change.tab4, TextOutputVar, 'Edit <_mCs.txt > file')
	infobulle(undo.tab4, 'Reinitialize <_mCs.txt > file')
	status.bar.display(undo.tab4, TextOutputVar, 'Reinitialize <_mCs.txt > file')

	#######################################################################################################

	ReturnExecResults <- NULL
	dem_data <- NULL

	##########
	getParamsRHtests <- function(GeneralParameters){
		GeneralParameters$rhtests.pars$p.lev <- as.numeric(str_trim(tclvalue(p.levStr)))
		GeneralParameters$rhtests.pars$Iadj <- as.integer(str_trim(tclvalue(IadjStr)))
		GeneralParameters$rhtests.pars$Mq <- as.integer(str_trim(tclvalue(MqStr)))
		GeneralParameters$rhtests.pars$Ny4a <- as.numeric(str_trim(tclvalue(Ny4aStr)))
		GeneralParameters$rhtests.pars$pthr <- as.numeric(str_trim(tclvalue(pthrStr)))
		GeneralParameters$prcp.data$rr.data <- switch(tclvalue(prcpdata.val), '0' = FALSE, '1' = TRUE)
		GeneralParameters$prcp.data$rr.log <- switch(tclvalue(prcplog.val), '0' = FALSE, '1' = TRUE)
		return(GeneralParameters)
	}

	########
	getRefparsRHtests <- function(GeneralParameters){
		GeneralParameters$use.refSeries <- switch(tclvalue(use.rfseries.val), '0' = FALSE, '1' = TRUE)
		GeneralParameters$refSeries.choix$weight.mean <- str_trim(tclvalue(weight.mean))
		GeneralParameters$refSeries.choix$use.elv <- switch(tclvalue(use.elv), '0' = FALSE, '1' = TRUE)
		GeneralParameters$refSeries.by.user <- switch(tclvalue(usr.rfseries.val), '0' = FALSE, '1' = TRUE)
		return(GeneralParameters)
	}

	########
	getpars.wRef <- function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		GeneralParameters <<- getRefparsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		gDemData <- !GeneralParameters$stn.type$single.series & GeneralParameters$refSeries.choix$use.elv & is.null(dem_data)
		if(gDemData) dem_data <<- getRHtestsDEM(donnees, GeneralParameters)
	}

	##########  without reference series
	tkconfigure(findU.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnFindU(donnees, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(findUD.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnFindUD(donnees, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(stepsize.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnStepSize(donnees, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(qmadj.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnQMadj(donnees, GeneralParameters, as.numeric(tclvalue(ChoixAjustment))),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	########## with reference series
	tkconfigure(findUr.tab3, command = function(){
		getpars.wRef()
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnFindU.wRef(donnees, dem_data, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(findUDr.tab3, command = function(){
		getpars.wRef()
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnFindUD.wRef(donnees, dem_data, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(stepsizer.tab3, command = function(){
		getpars.wRef()
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnStepSize.wRef(donnees, dem_data, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(qmadjr.tab3, command = function(){
		getpars.wRef()
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnQMadj.wRef(donnees, dem_data, GeneralParameters, as.numeric(tclvalue(ChoixAjustment))),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########  daily precipitation
	tkconfigure(findUp.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnFindU.dlyPrcp(donnees, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(findUDp.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnFindUD.dlyPrcp(donnees, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(stepsizep.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			ReturnExecResults <<- tryCatch(executeOnStepSize.dlyPrcp(donnees, GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
			assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
		}
	})

	##########
	tkconfigure(qmadjp.tab3, command = function(){
		GeneralParameters <<- getParamsRHtests(GeneralParameters)
		assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
		if(!is.null(donnees)){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			tryCatch(executeOnadjDLY.dlyPrcp(donnees, GeneralParameters, as.numeric(tclvalue(ChoixAjustment))),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e), finally = {
				tkconfigure(main.win, cursor = '')
			})
		}
	})

	########################################################
	##########  Outputs
	PrevwRHtestsIdTab <- NULL

	tkconfigure(preview.tab4, command = function(){
		if(!is.null(ReturnExecResults)){
			res2disp <- RHtestsPreviewOutput(ReturnExecResults)

			retNBTab <- consolOutNotebookTab_unik(tknotes, res2disp, paste(ReturnExecResults$stn, '-Output Preview'),
													PrevwRHtestsIdTab, AllOpenTabType, AllOpenTabData, rhtests = TRUE)
			PrevwRHtestsIdTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no RHtests output', format = TRUE)
	})

	##########
	tkconfigure(viewplot.tab4, command = function() RHtestsDisplayPdfPlot(ReturnExecResults))

	##########
	RHtestEditTab <- NULL

	tkconfigure(change.tab4, command = function(){
		if(!is.null(ReturnExecResults)){
			retNBTab <- tableRHtestNotebookTab_unik(tknotes, paste(ReturnExecResults$stn, 'mCs.txt', sep = '_'),
													RHtestEditTab, AllOpenTabType, AllOpenTabData)
			RHtestEditTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}else InsertMessagesTxt(main.txt.out, 'There is no RHtests output', format = TRUE)
	})

	##########
	tkconfigure(undo.tab4, command = function(){
		if(!is.null(ReturnExecResults)){
			RHtestsUndoChange(ReturnExecResults)
			InsertMessagesTxt(main.txt.out, paste('File', paste(ReturnExecResults$stn, '_mCs.txt', sep = ''), 'has been reinitialized'))
		}else InsertMessagesTxt(main.txt.out, 'There is no RHtests output', format = TRUE)
	})

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


