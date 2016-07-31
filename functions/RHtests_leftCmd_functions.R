
RHtestsV4Cmd <- function(){

	if(!is.null(GeneralParameters)){
		if(GeneralParameters$action == 'rhtests') GeneralParameters <<- GeneralParameters
		else GeneralParameters <<- init.params('rhtests', 'dekadal')
	}else GeneralParameters <<- init.params('rhtests', 'dekadal')

	###################

	listOpenFiles <- openFile_ttkcomboList()

	###################

	##tkcombo& tkentry width
	#largeur <- 27
	largeur <- as.integer(w.scale(21)/sfont0)
	##tkentry nc filename format tkentry wdth
	#wncdf_ff <- 19
	wncdf_ff <- as.integer(w.scale(14)/sfont0)
	#scrollable frame width
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(18)
		hscrlwin <- h.scale(30)
		#hscrlwin <- h.scale(27)
	}else{
		wscrlwin <- w.scale(21.7)
		hscrlwin <- h.scale(37)
		#hscrlwin <- h.scale(34)
	}

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	plotBut.cmd <- tkframe(cmd.frame)
	tkgrid(tknote.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2)
	tkgrid(plotBut.cmd, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "RHtestsV4")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Parameters")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "RefSerCreation")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Output")

	bwRaiseTab(tknote.cmd, cmd.tab2)

#######################################################################################################

	#Tab1
	frTab1 <- tkframe(cmd.tab1)
	tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	scrw1 <- bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	subfr1 <- bwScrollableFrame(scrw1, width = wscrlwin, height = hscrlwin)

	findU.tab1 <- tkbutton(subfr1, text = "FindU")
	findUD.tab1 <- tkbutton(subfr1, text = "FindUD")
	stepsize.tab1 <- tkbutton(subfr1, text = "StepSize")
	qmadj.tab1 <- tkbutton(subfr1, text = "QMadj")

	findUr.tab1 <- tkbutton(subfr1, text = "FindU.wRef")
	findUDr.tab1 <- tkbutton(subfr1, text = "FindUD.wRef")
	stepsizer.tab1 <- tkbutton(subfr1, text = "StepSize.wRef")
	qmadjr.tab1 <- tkbutton(subfr1, text = "QMadj.wRef")

	sep1.tab1 <- ttkseparator(subfr1)
	dlyPrcpLb.tab1 <- tklabel(subfr1, text = 'Homogenization of daily precipitation', anchor = 'w', justify = 'left')

	findUp.tab1 <- tkbutton(subfr1, text = "FindU.dlyPrcp")
	findUDp.tab1 <- tkbutton(subfr1, text = "FindUD.dlyPrcp")
	stepsizep.tab1 <- tkbutton(subfr1, text = "StepSize.dlyPrcp")
	qmadjp.tab1 <- tkbutton(subfr1, text = "getAdjusted Data")

#	sep2.tab1 <- ttkseparator(subfr1)
#	sep3.tab1 <- ttkseparator(subfr1)

	#############################################
	tkgrid(findU.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(findUr.tab1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(findUD.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(findUDr.tab1, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(stepsize.tab1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(stepsizer.tab1, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(qmadj.tab1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(qmadjr.tab1, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1.tab1, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
	tkgrid(dlyPrcpLb.tab1, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)

	tkgrid(findUp.tab1, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(findUDp.tab1, row = 6, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(stepsizep.tab1, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(qmadjp.tab1, row = 7, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

#	tkgrid(sep2.tab1, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
#	tkgrid(sep3.tab1, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)

#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2) #,relief = 'sunken', bd = 2
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)

	getData.tab2 <- tkbutton(subfr2, text = "Input Data", bg = 'lightgreen')
	#txtsep.tab2 <- tklabel(subfr2, text='',width = as.integer(wscrlwin/sfont0))

	sep1.tab2 <- ttkseparator(subfr2)
	framePrcp.tab2 <- ttklabelframe(subfr2, text = 'Precipitation Data', relief = 'groove', borderwidth = 2)

	prcpdata.val <- tclVar(as.character(GeneralParameters$prcpdata$Values[1]))
	prcpdata.tab2 <- tkcheckbutton(framePrcp.tab2, variable = prcpdata.val, text = 'Precipitation series', anchor = 'w', justify = 'left')
	infobulle(prcpdata.tab2, 'Must be checked in case of precipitation data')
	status.bar.display(prcpdata.tab2, TextOutputVar, 'Must be checked in case of precipitation data')

	prcplog.val <- tclVar(as.character(GeneralParameters$prcpdata$Values[2]))
	prcplog.tab2 <- tkcheckbutton(framePrcp.tab2, variable = prcplog.val, text = 'Use log-transformed series', anchor = 'w', justify = 'left')
	infobulle(prcplog.tab2, 'Check if applying a log transformation to monthly  precipitation series')
	status.bar.display(prcplog.tab2, TextOutputVar, 'Check if applying a log transformation to monthly precipitation series')

	sep2.tab2 <- ttkseparator(subfr2)
	framePars.tab2 <- ttklabelframe(subfr2, text = 'Parameters', relief = 'groove', borderwidth = 2)

	p.levStr <- tclVar(as.character(GeneralParameters$rhtests.pars$Values[1]))
	plevLb.tab2 <- tklabel(framePars.tab2, text = 'p.lev', anchor = 'e', justify = 'right')
#	plevEd.tab2 <- tkentry(framePars.tab2, textvariable = p.levStr, width = 6)
	plevEd.tab2 <- ttkcombobox(framePars.tab2, values = as.character(c(0.75, 0.80, 0.90, 0.95, 0.99, 0.9999)), textvariable = p.levStr, width = 6)
	infobulle(plevEd.tab2, 'Choose the nominal conf. level p.lev value.')
	status.bar.display(plevEd.tab2, TextOutputVar, 'Choose the nominal conf. level p.lev value.')

	IadjStr <- tclVar(as.character(GeneralParameters$rhtests.pars$Values[2]))
	iadjLb.tab2 <- tklabel(framePars.tab2, text = 'Iadj', anchor = 'e', justify = 'right')
	iadjEd.tab2 <- tkentry(framePars.tab2, textvariable = IadjStr, width = 6)
	infobulle(iadjEd.tab2, 'Please enter integer Iadj (0 to 10000 inclusive)')
	status.bar.display(iadjEd.tab2, TextOutputVar, 'Please enter integer Iadj (0 to 10000 inclusive)')

	MqStr <- tclVar(as.character(GeneralParameters$rhtests.pars$Values[3]))
	mqLb.tab2 <- tklabel(framePars.tab2, text = 'Mq', anchor = 'e', justify = 'right')
	mqEd.tab2 <- tkentry(framePars.tab2, textvariable = MqStr, width = 6)
	infobulle(mqEd.tab2, 'Please enter integer Mq (# of points for evaluating PDF)')
	status.bar.display(mqEd.tab2, TextOutputVar, 'Please enter integer Mq (# of points for evaluating PDF)')

	Ny4aStr <- tclVar(as.character(GeneralParameters$rhtests.pars$Values[4]))
	ny4aLb.tab2 <- tklabel(framePars.tab2, text = 'Ny4a', anchor = 'e', justify = 'right')
	ny4aEd.tab2 <- tkentry(framePars.tab2, textvariable = Ny4aStr, width = 6)
	infobulle(ny4aEd.tab2, 'Please enter integer Ny4a ( >= 5, or 0 for choosing the whole segment)')
	status.bar.display(ny4aEd.tab2, TextOutputVar, 'Please enter integer Ny4a ( >= 5, or 0 for choosing the whole segment)')

	pthrStr <- tclVar(as.character(GeneralParameters$rhtests.pars$Values[5]))
	pthrLb.tab2 <- tklabel(framePars.tab2, text = 'pthr', anchor = 'e', justify = 'right')
	pthrEd.tab2 <- tkentry(framePars.tab2, textvariable = pthrStr, width = 6)
	infobulle(pthrEd.tab2, 'Please enter the lower precipitation threshold pthr ( >= 0) [for .dlyPrcp only]')
	status.bar.display(pthrEd.tab2, TextOutputVar, 'Please enter the lower precipitation threshold pthr ( >= 0)  [for .dlyPrcp only]')



	#############################################

	tkgrid(getData.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 1)
	#tkgrid(txtsep.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 1)

	tkgrid(sep1.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
	tkgrid(framePrcp.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 3, ipady = 2)

	tkgrid(prcpdata.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(prcplog.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep2.tab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
	tkgrid(framePars.tab2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)

	tkgrid(plevLb.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(plevEd.tab2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(iadjLb.tab2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(iadjEd.tab2, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mqLb.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mqEd.tab2, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(ny4aLb.tab2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(ny4aEd.tab2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(pthrLb.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(pthrEd.tab2, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


#######################################################################################################

	#Tab3
	frTab3 <- tkframe(cmd.tab3) #,relief = 'sunken', bd = 2
	tkgrid(frTab3, padx = 5, pady = 5, ipadx = 2, ipady = 2)

	scrw3 <- bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	subfr3 <- bwScrollableFrame(scrw3, width = wscrlwin, height = hscrlwin)

	use.rfseries.val <- tclVar(as.character(GeneralParameters$use.ref.series))
	userefS.tab3 <- tkcheckbutton(subfr3, variable = use.rfseries.val, text = 'Use reference series', anchor = 'w', justify = 'left', bg = 'lightgreen') #,width = largeur1-1
	infobulle(userefS.tab3, 'Using a reference series to\nperform the homogenization test')
	status.bar.display(userefS.tab3, TextOutputVar, 'Using a reference series to perform the homogenization test')

	##############
	sep1.tab3 <- ttkseparator(subfr3)

	if(as.character(GeneralParameters$use.ref.series) == '0') stateStn <- 'disabled'
	if(as.character(GeneralParameters$use.ref.series) == '1') stateStn <- 'normal'

	nghbStnLb.tab3 <- tklabel(subfr3, text = 'Neighbor stations selection', anchor = 'w', justify = 'right')
	nghbStnBt.tab3 <- tkbutton(subfr3, text = "Options")
	infobulle(nghbStnBt.tab3, 'Set options to select neighbor stations')
	status.bar.display(nghbStnBt.tab3, TextOutputVar, 'Set options to select neighbor stations')

	tkconfigure(nghbStnBt.tab3, state = stateStn, command = function(){
		GeneralParameters <<- rhtests_nghbStnOpts(main.win, GeneralParameters)
	})


	##############
	sep2.tab3 <- ttkseparator(subfr3)

	if(as.character(GeneralParameters$ref.series.choix$Values[2]) == '0') stateRef <- 'disabled'
	if(as.character(GeneralParameters$ref.series.choix$Values[2]) == '1') stateRef <- 'normal'

	use.elv <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[2]))
	useElv.tab3 <- tkcheckbutton(subfr3, text = 'Use Elevation', variable = use.elv, anchor = 'w', justify = 'left')
	infobulle(useElv.tab3, 'Check to use elevation data\nto choose neighbors stations')
	status.bar.display(useElv.tab3, TextOutputVar, 'Check for using elevation data to choose neighbor stations')
	useElvBt.tab3 <- tkbutton(subfr3, text = "Settings")
	infobulle(useElvBt.tab3, 'Select elevation data')
	status.bar.display(useElvBt.tab3, TextOutputVar, 'Select elevation data')

	tkconfigure(useElvBt.tab3, state = stateRef, command = function(){
		GeneralParameters <<- rhtests_useElv(main.win, GeneralParameters)
	})


	#############
	sep3.tab3 <- ttkseparator(subfr3)

	weight.mean <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[1]))
	wghtfacLb.tab3 <- tklabel(subfr3, text = "Weighting Factors", anchor = 'w', justify = 'left')
	infobulle(wghtfacLb.tab3, 'Method to build weights for linear combinations')
	status.bar.display(wghtfacLb.tab3, TextOutputVar, 'Method to build weights for linear combinations')
	wghtfacCmb.tab3 <- ttkcombobox(subfr3, values = c('Distance', 'Correlation', 'Optimal', 'Average'), textvariable = weight.mean, width = 9)
	infobulle(wghtfacCmb.tab3, 'Method to build weights for linear combinations')
	status.bar.display(wghtfacCmb.tab3, TextOutputVar, 'Method to build weights for linear combinations')

	#############
	sep4.tab3 <- ttkseparator(subfr3)

	if(as.character(GeneralParameters$ref.series.user) == '0') stateElv <- 'disabled'
	if(as.character(GeneralParameters$ref.series.user) == '1') stateElv <- 'normal'

	usr.rfseries.val <- tclVar(as.character(GeneralParameters$ref.series.user))
	usr.rfseries.tab3 <- tkcheckbutton(subfr3, variable = usr.rfseries.val, text = "Stations'Choice by User", anchor = 'w', justify = 'left', width = 19)
	infobulle(usr.rfseries.tab3, 'The reference series will be created\nfrom stations chosen by user')
	status.bar.display(usr.rfseries.tab3, TextOutputVar, "The reference series will be created from stations chosen by user")
	usrbt.rfseries.tab3 <- tkbutton(subfr3, text = "Select")
	infobulle(usrbt.rfseries.tab3, 'Select the stations to create the reference series')
	status.bar.display(usrbt.rfseries.tab3, TextOutputVar, 'Select the stations to create the reference series')

	tkconfigure(usrbt.rfseries.tab3, state = stateElv, command = function(){
		file2test <- as.character(GeneralParameters$file.io$Values[1])
		if(file2test != ""){
			idsstn <- which(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])) == file2test)
			if(length(idsstn) == 0){
				tkmessageBox(message = "File not found or in the wrong format", icon = "warning", type = "ok")
				#return(NULL)
			}else{
				donstn <- AllOpenFilesData[[idsstn]][[2]]
				stnId <- as.character(donstn[1,-1])
				GeneralParameters <<- refSeriesUsersChoice(main.win, stnId, GeneralParameters)
			}
		}else{
			tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
		}
	})

########################################
	tkbind(userefS.tab3,"<Button-1>", function(){
		if(tclvalue(use.rfseries.val) == "0") tkconfigure(nghbStnBt.tab3, state = 'normal')
		else tkconfigure(nghbStnBt.tab3, state = 'disabled')
	})

	tkbind(useElv.tab3,"<Button-1>", function(){
		if(tclvalue(use.rfseries.val) == "1"){
			if(tclvalue(use.elv) == "0") tkconfigure(useElvBt.tab3, state = 'normal')
			else tkconfigure(useElvBt.tab3, state = 'disabled')
		}
	})

	tkbind(usr.rfseries.tab3,"<Button-1>", function(){
		if(tclvalue(use.rfseries.val) == "1"){
			if(tclvalue(usr.rfseries.val) == "0") tkconfigure(usrbt.rfseries.tab3, state = 'normal')
			else tkconfigure(usrbt.rfseries.tab3, state = 'disabled')
		}
	})

########################################

	tkgrid(userefS.tab3, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep1.tab3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(nghbStnLb.tab3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(nghbStnBt.tab3, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep2.tab3, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(useElv.tab3, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(useElvBt.tab3, row = 4, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab3, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(wghtfacLb.tab3, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(wghtfacCmb.tab3, row = 6, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep4.tab3, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, pady = 5)
	tkgrid(usr.rfseries.tab3, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(usrbt.rfseries.tab3, row = 8, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

#######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4) #,relief = 'sunken', bd = 2
	tkgrid(frTab4, padx = 5, pady = 5, ipadx = 2, ipady = 2)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)

	preview.tab4 <- tkbutton(subfr4, text = "Output Preview")
	infobulle(preview.tab4, 'View results <_*Cs.txt > and <_*stat.txt > files')
	status.bar.display(preview.tab4, TextOutputVar, 'View results <_*Cs.txt > and <_*stat.txt > files')

	viewplot.tab4 <- tkbutton(subfr4, text = "Display Output")
	infobulle(viewplot.tab4, 'Display result plots in pdf file format')
	status.bar.display(viewplot.tab4, TextOutputVar, 'Display result plots in pdf file format')

	change.tab4 <- tkbutton(subfr4, text = "Edit Breakpoints")
	infobulle(change.tab4, 'Edit <_mCs.txt > file')
	status.bar.display(change.tab4, TextOutputVar, 'Edit <_mCs.txt > file')

	undo.tab4 <- tkbutton(subfr4, text = "Undo Change")
	infobulle(undo.tab4, 'Reinitialize <_mCs.txt > file')
	status.bar.display(undo.tab4, TextOutputVar, 'Reinitialize <_mCs.txt > file')

	sep1.tab4 <- ttkseparator(subfr4)

	homAdjframe.tab4 <- ttklabelframe(subfr4, text = "Adjusted series selection", labelanchor = "nw", relief = "groove", borderwidth = 2)
	infobulle(homAdjframe.tab4, 'Select the adjusted series to be retained')
	status.bar.display(homAdjframe.tab4, TextOutputVar, 'Select the adjusted series to be retained')


	AdjMthdRadio1 <- tkradiobutton(homAdjframe.tab4, text = "By Mean", anchor = 'w', justify = 'left')
	AdjMthdRadio2 <- tkradiobutton(homAdjframe.tab4, text = "By Quantile Matching", anchor = 'w', justify = 'left')
	AdjMthdRadio3 <- tkradiobutton(homAdjframe.tab4, text = "Base Series", anchor = 'w', justify = 'left')
	ChoixAjustment <- tclVar('3')
	tkconfigure(AdjMthdRadio1, variable = ChoixAjustment, value = "1")
	tkconfigure(AdjMthdRadio2, variable = ChoixAjustment, value = "2")
	tkconfigure(AdjMthdRadio3, variable = ChoixAjustment, value = "3")

	#############################################
	tkgrid(preview.tab4, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(viewplot.tab4, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(change.tab4, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(undo.tab4, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1.tab4, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, pady = 5)
	tkgrid(homAdjframe.tab4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(AdjMthdRadio1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(AdjMthdRadio2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(AdjMthdRadio3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

#######################################################################################################

donnees <- NULL
ReturnExecResults <- NULL

##########
tkconfigure(getData.tab2, command = function(){
	GeneralParameters <<- rhtests_inputData(main.win, GeneralParameters)
	if(as.character(GeneralParameters$file.io$Values[1]) != "" & GeneralParameters$getdata) {
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		donnees <<- tryCatch(getRHtestsData(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		#assign("donnees", donnees, envir = .GlobalEnv)

		useRefs <- as.character(GeneralParameters$use.ref.series)
		if(useRefs == '1'){
			tcl(userefS.tab3, 'select')
			tkconfigure(nghbStnBt.tab3, state = 'normal')
		}else{
			tcl(userefS.tab3, 'deselect')
			tkconfigure(nghbStnBt.tab3, state = 'disabled')
		}
	}
})


##########
getParamsRHtests <- function(GeneralParameters){
	p.lev <- tclvalue(p.levStr)
	Iadj <- tclvalue(IadjStr)
	Mq <- tclvalue(MqStr)
	Ny4a <- tclvalue(Ny4aStr)
	pthr <- tclvalue(pthrStr)
	GeneralParameters$rhtests.pars$Values <- c(p.lev, Iadj, Mq, Ny4a, pthr)
	GeneralParameters$prcpdata$Values <- c(tclvalue(prcpdata.val), tclvalue(prcplog.val))
	return(GeneralParameters)
}


########
getRefparsRHtests <- function(GeneralParameters){
	GeneralParameters$use.ref.series <- tclvalue(use.rfseries.val)
	GeneralParameters$ref.series.choix$Values <- c(tclvalue(weight.mean), tclvalue(use.elv), as.character(GeneralParameters$ref.series.choix$Values[3:8]))
	GeneralParameters$ref.series.user <- tclvalue(usr.rfseries.val)
	return(GeneralParameters)
}


##########  without reference series
tkconfigure(findU.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnFindU(donnees, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(findUD.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnFindUD(donnees, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(stepsize.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnStepSize(donnees, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(qmadj.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnQMadj(donnees, GeneralParameters, as.numeric(tclvalue(ChoixAjustment))),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

########################################################

dem_data <- NULL
getpars.wRef <- function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	GeneralParameters <<- getRefparsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	gDemData <- GeneralParameters$single.series == '0' & as.character(GeneralParameters$ref.series.choix$Values[2]) == '1' & is.null(dem_data)
	if(gDemData) dem_data <<- getRHtestsDEM(donnees, GeneralParameters)
}

########## with reference series
tkconfigure(findUr.tab1, command = function(){
	getpars.wRef()
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnFindU.wRef(donnees, dem_data, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(findUDr.tab1, command = function(){
	getpars.wRef()
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnFindUD.wRef(donnees, dem_data, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(stepsizer.tab1, command = function(){
	getpars.wRef()
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnStepSize.wRef(donnees, dem_data, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(qmadjr.tab1, command = function(){
	getpars.wRef()
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnQMadj.wRef(donnees, dem_data, GeneralParameters, as.numeric(tclvalue(ChoixAjustment))),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

########################################################
##########  daily precipitation
tkconfigure(findUp.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnFindU.dlyPrcp(donnees, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(findUDp.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnFindUD.dlyPrcp(donnees, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(stepsizep.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		ReturnExecResults <<- tryCatch(executeOnStepSize.dlyPrcp(donnees, GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
		assign("ReturnExecResults", ReturnExecResults, envir = .GlobalEnv)
	}
})

##########
tkconfigure(qmadjp.tab1, command = function(){
	GeneralParameters <<- getParamsRHtests(GeneralParameters)
	assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)
	if(!is.null(donnees)){
		tkconfigure(main.win, cursor = 'watch');tcl('update')
		tryCatch(executeOnadjDLY.dlyPrcp(donnees, GeneralParameters, as.numeric(tclvalue(ChoixAjustment))),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally={
			tkconfigure(main.win, cursor='')
		})
	}
})

########################################################
##########  Outputs
PrevwRHtestsIdTab <- NULL
tkconfigure(preview.tab4, command = function(){
	if(!is.null(ReturnExecResults)){
		res2disp <- RHtestsPreviewOutput(ReturnExecResults)

		retNBTab <- consolOutNotebookTab_unik(tknotes, res2disp, paste(ReturnExecResults$stn,'-Output Preview'), PrevwRHtestsIdTab, AllOpenTabType, AllOpenTabData, rhtests = TRUE)
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
		retNBTab <- tableRHtestNotebookTab_unik(tknotes, paste(ReturnExecResults$stn, 'mCs.txt', sep = '_'), RHtestEditTab, AllOpenTabType, AllOpenTabData)
		RHtestEditTab <<- retNBTab$notebookTab
		AllOpenTabType <<- retNBTab$AllOpenTabType
		AllOpenTabData <<- retNBTab$AllOpenTabData
	}else InsertMessagesTxt(main.txt.out, 'There is no RHtests output', format = TRUE)
})

##########
tkconfigure(undo.tab4, command = function(){
	if(!is.null(ReturnExecResults)){
		RHtestsUndoChange(ReturnExecResults)
		InsertMessagesTxt(main.txt.out, paste('File', paste(ReturnExecResults$stn, '_mCs.txt', sep = ''),'has been reinitialized'))
	}else InsertMessagesTxt(main.txt.out, 'There is no RHtests output', format = TRUE)
})


#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	######
	return(cmd.frame)
}


