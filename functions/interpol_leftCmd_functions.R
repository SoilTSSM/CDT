
InterpolationPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		wPreview <- w.scale(21)
		largeur <- as.integer(w.scale(27)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		wbutton <- 26
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46.5)
		wPreview <- w.scale(22)
		largeur <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(23)/sfont0)
		wbutton <- 24
	}

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "General")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Options")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Interpolation")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Colors Options")
	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

	#######################################################################################################

	#Tab1
	frTab1 <- tkframe(cmd.tab1)
	tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab1, 0, weight = 1)

	scrw1 <- bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	tkgrid.columnconfigure(scrw1, 0, weight = 1)
	subfr1 <- bwScrollableFrame(scrw1, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr1, 0, weight = 1)

	##############
	frameStn <- ttklabelframe(subfr1, text = "Station data file", relief = 'groove')

	file.period <- tclVar('Dekadal data')
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	file.stnfl <- tclVar()
	dayLabTab1_Var <- tclVar('Dek')
	idate_yrs <- tclVar('1983')
	idate_mon <- tclVar('1')
	idate_day <- tclVar('1')

	combPrd.tab1 <- ttkcombobox(frameStn, values = cb.periodVAL, textvariable = file.period)
	combStnfl.tab1 <- ttkcombobox(frameStn, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	btStnfl.tab1 <- tkbutton(frameStn, text = "...")

	labDate.tab1 <- tklabel(frameStn, text = "Date", anchor = 'e', justify = 'right')
	yrsLab.tab1 <- tklabel(frameStn, text = 'Year', anchor = 'w', justify = 'left')
	monLab.tab1 <- tklabel(frameStn, text = 'Month', anchor = 'w', justify = 'left')
	dayLab.tab1 <- tklabel(frameStn, text = tclvalue(dayLabTab1_Var), textvariable = dayLabTab1_Var, anchor = 'w', justify = 'left')

	yrs1.tab1 <- tkentry(frameStn, width = 4, textvariable = idate_yrs, justify = "left")
	mon1.tab1 <- tkentry(frameStn, width = 4, textvariable = idate_mon, justify = "left")
	day1.tab1 <- tkentry(frameStn, width = 4, textvariable = idate_day, justify = "left")

	#######################

	tkconfigure(btStnfl.tab1, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)
		}else return(NULL)
	})

	#############################
	tkgrid(combPrd.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(combStnfl.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btStnfl.tab1, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(yrsLab.tab1, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(monLab.tab1, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(dayLab.tab1, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(labDate.tab1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(yrs1.tab1, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(mon1.tab1, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(day1.tab1, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)

	infobulle(combStnfl.tab1, 'Choose the station data in the list')
	status.bar.display(combStnfl.tab1, TextOutputVar, 'Choose the file containing the station data in CDT format')
	infobulle(btStnfl.tab1, 'Browse file if not listed')
	status.bar.display(btStnfl.tab1, TextOutputVar, 'Browse file if not listed')

	#############################
	frameInterp <- ttklabelframe(subfr1, text = "Interpolation Method", relief = 'groove')

	Interp.Method <- tclVar('IDW')
	Interp.Choix <- c('Nearest Neighbor', 'IDW', 'Kriging')

	combInterp.tab1 <- ttkcombobox(frameInterp, values = Interp.Choix, textvariable = Interp.Method, width = largeur)

	tkgrid(combInterp.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	#############################
	frameGrid <- ttklabelframe(subfr1, text = "Grid for interpolation", relief = 'groove')

	varCreateGrd <- tclVar('1')
	file.grdCDF <- tclVar()
	new.grid <- list(minlon = 42, maxlon = 52, reslon = 0.1, minlat = -26, maxlat = -12, reslat = 0.1)
	newgrdPars <- list(New.Grid.Def = new.grid)

	grdCDF.tab1 <- tkradiobutton(frameGrid, text = "From gridded NetCDF data", anchor = 'w', justify = 'left')
	combgrdCDF.tab1 <- ttkcombobox(frameGrid, values = unlist(listOpenFiles), textvariable = file.grdCDF, state = 'disabled', width = largeur)
	btgrdCDF.tab1 <- tkbutton(frameGrid, text = "...")

	grdNEW.tab1 <- tkradiobutton(frameGrid, text = "Create new grid", anchor = 'w', justify = 'left')
	btNewgrid.tab1 <- tkbutton(frameGrid, text = "Create", state = 'normal')

	tkconfigure(grdCDF.tab1, variable = varCreateGrd, value = "0")
	tkconfigure(grdNEW.tab1, variable = varCreateGrd, value = "1")

	###############
	tkconfigure(btgrdCDF.tab1, state = 'disabled', command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdCDF) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(combDem.tab2, values = unlist(listOpenFiles), textvariable = file.plotDem)
			tkconfigure(combShp.tab3, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	tkconfigure(btNewgrid.tab1, command = function(){
		newgrdPars <<- getParamNewGrid(main.win, newgrdPars)
	})

	##############
	tkgrid(grdCDF.tab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(combgrdCDF.tab1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btgrdCDF.tab1, row = 1, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(grdNEW.tab1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btNewgrid.tab1, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(combgrdCDF.tab1, 'Choose the file in the list')
	status.bar.display(combgrdCDF.tab1, TextOutputVar, 'File containing a gridded data in netcdf')
	infobulle(btgrdCDF.tab1, 'Browse file if not listed')
	status.bar.display(btgrdCDF.tab1, TextOutputVar, 'Browse file if not listed')
	infobulle(btNewgrid.tab1, 'Set the new grid')
	status.bar.display(btNewgrid.tab1, TextOutputVar, 'Set the new grid')

	#############################
	tkgrid(frameStn, row = 0, column = 0, sticky = 'we')
	tkgrid(frameInterp, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameGrid, row = 2, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	tkbind(combPrd.tab1, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(dayLabTab1_Var) <- 'Day'
			tkconfigure(day1.tab1, state = 'normal')
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(dayLabTab1_Var) <- 'Dek'
			tkconfigure(day1.tab1, state = 'normal')
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tkconfigure(day1.tab1, state = 'disabled')
		}
	})

	tkbind(grdCDF.tab1, "<Button-1>", function(){
		tkconfigure(btNewgrid.tab1, state = 'disabled')
		tkconfigure(combgrdCDF.tab1, state = 'normal')
		tkconfigure(btgrdCDF.tab1, state = 'normal')
	})

	tkbind(grdNEW.tab1, "<Button-1>", function(){
		tkconfigure(btNewgrid.tab1, state = 'normal')
		tkconfigure(combgrdCDF.tab1, state = 'disabled')
		tkconfigure(btgrdCDF.tab1, state = 'disabled')
	})

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	##############
	frameOpt1 <- ttklabelframe(subfr2, text = "General Options", relief = 'groove')

	maxdist_vars <- tclVar('100')
	nmin_vars <- tclVar('1')
	nmax_vars <- tclVar('4')
	idp_vars <- tclVar('2.0')
	elvdiff_vars <- tclVar('200')
	omax_vars <- tclVar('0')

	maxdistL.tab2 <- tklabel(frameOpt1, text = "Maxdist", anchor = 'e', justify = 'right')
	nminL.tab2 <- tklabel(frameOpt1, text = "Nmin", anchor = 'e', justify = 'right')
	nmaxL.tab2 <- tklabel(frameOpt1, text = "Nmax", anchor = 'e', justify = 'right')
	idpL.tab2 <- tklabel(frameOpt1, text = "Power", anchor = 'e', justify = 'right')
	elvdiffL.tab2 <- tklabel(frameOpt1, text = "ElvDiff", anchor = 'e', justify = 'right')
	omaxL.tab2 <- tklabel(frameOpt1, text = "Omax", anchor = 'e', justify = 'right')

	maxdist.tab2 <- tkentry(frameOpt1, width = 6, textvariable = maxdist_vars, justify = "left")
	nmin.tab2 <- tkentry(frameOpt1, width = 6, textvariable = nmin_vars, justify = "left")
	nmax.tab2 <- tkentry(frameOpt1, width = 6, textvariable = nmax_vars, justify = "left")
	idp.tab2 <- tkentry(frameOpt1, width = 6, textvariable = idp_vars, justify = "left")
	elvdiff.tab2 <- tkentry(frameOpt1, width = 6, textvariable = elvdiff_vars, justify = "left")
	omax.tab2 <- tkentry(frameOpt1, width = 6, textvariable = omax_vars, justify = "left")

	tkgrid(maxdistL.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(maxdist.tab2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(nminL.tab2, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(nmin.tab2, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(nmaxL.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(nmax.tab2, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(idpL.tab2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(idp.tab2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(elvdiffL.tab2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(elvdiff.tab2, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	# tkgrid(omaxL.tab2, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	# tkgrid(omax.tab2, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(maxdist.tab2, 'Radius of search (in km)')
	status.bar.display(maxdist.tab2, TextOutputVar, 'Only data within a distance of ‘maxdist’ from the prediction location are used')
	infobulle(nmin.tab2, 'Minimum number of data to use')
	status.bar.display(nmin.tab2, TextOutputVar, 'Minimum number of data to use')
	infobulle(nmax.tab2, 'Maximum number of data to use')
	status.bar.display(nmax.tab2, TextOutputVar, 'Maximum number of data to use')
	infobulle(idp.tab2, 'Inverse distance weighting power')
	status.bar.display(idp.tab2, TextOutputVar, 'Inverse distance weighting power')
	infobulle(elvdiff.tab2, 'Maximum altitude difference of neighbor stations to use (m)')
	status.bar.display(elvdiff.tab2, TextOutputVar, 'Maximum altitude difference of neighbor stations to use (m)')
	infobulle(omax.tab2, 'Maximum number of data to use from each quadrant')
	status.bar.display(omax.tab2, TextOutputVar, 'Maximum number of data to use from each quadrant, (0 means option not used)')

	#############################

	frameOpt2 <- ttklabelframe(subfr2, text = "Fitting a variogram", relief = 'groove')

	fitVgmChx <- tclVar('0')
	fitVgmMod <- tclVar('Sph')
	vgmModList <- c("Sph", "Exp", "Gau", "Log", "Ste", "Mat")

	fitvgm.tab2 <- tkradiobutton(frameOpt2, text = "Fit a variogram", anchor = 'w', justify = 'left', state = 'disabled')
	fitAvgm.tab2 <- tkradiobutton(frameOpt2, text = "Automatic fitting", anchor = 'w', justify = 'left', state = 'disabled')
	tkconfigure(fitvgm.tab2, variable = fitVgmChx, value = "0")
	tkconfigure(fitAvgm.tab2, variable = fitVgmChx, value = "1")

	combfitVgm.tab2 <- ttkcombobox(frameOpt2, values = vgmModList, textvariable = fitVgmMod, state = 'disabled', width = 6)
	btfitAVgm.tab2 <- tkbutton(frameOpt2, text = "Models", state = 'disabled')

	##############

	tkconfigure(btfitAVgm.tab2, command = function(){
		vgmModList <<- editVgmModel(main.win, vgmModList)
	})

	##############

	tkgrid(fitvgm.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(combfitVgm.tab2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(fitAvgm.tab2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(btfitAVgm.tab2, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(fitvgm.tab2, 'Fit a variogram by choosing a model')
	status.bar.display(fitvgm.tab2, TextOutputVar, 'Fit a variogram by choosing a model')
	infobulle(combfitVgm.tab2, 'Fit a variogram by choosing a model')
	status.bar.display(combfitVgm.tab2, TextOutputVar, 'Fit a variogram by choosing a model')
	infobulle(fitAvgm.tab2, 'Automatically fit a variogram')
	status.bar.display(fitAvgm.tab2, TextOutputVar, 'Automatically fit a variogram')
	infobulle(btfitAVgm.tab2, 'Edit the models to use')
	status.bar.display(btfitAVgm.tab2, TextOutputVar, 'Edit the models to use, see list provided by gstat packages')

	#############################
	frameOpt3 <- ttklabelframe(subfr2, text = "Elevation data", relief = 'groove')

	useELV <- tclVar("0")
	file.plotDem <- tclVar()

	useELV.tab2 <- tkcheckbutton(frameOpt3, text = "Use elevation to interpolate the data", variable = useELV, anchor = 'w', justify = 'left')
	combDem.tab2 <- ttkcombobox(frameOpt3, values = unlist(listOpenFiles), textvariable = file.plotDem, width = largeur-1, state = 'disabled')
	btDem.tab2 <- tkbutton(frameOpt3, state = 'disabled', text = "...")

	##############
	tkconfigure(btDem.tab2, command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.plotDem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(combDem.tab2, values = unlist(listOpenFiles), textvariable = file.plotDem)
			tkconfigure(combShp.tab3, values = unlist(listOpenFiles), textvariable = file.plotShp)

		}
	})

	##############
	tkgrid(useELV.tab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(combDem.tab2, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btDem.tab2, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#############################
	tkgrid(frameOpt1, row = 0, column = 0, sticky = 'we')
	tkgrid(frameOpt2, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameOpt3, row = 2, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	tkbind(combInterp.tab1, "<<ComboboxSelected>>", function(){
		if(tclvalue(Interp.Method) == 'Nearest Neighbor'){
			tkconfigure(maxdist.tab2, state = 'normal')
			tkconfigure(nmin.tab2, state = 'disabled')
			tkconfigure(nmax.tab2, state = 'disabled')
			tkconfigure(idp.tab2, state = 'disabled')
			tkconfigure(omax.tab2, state = 'disabled')

			tkconfigure(fitvgm.tab2, state = 'disabled')
			tkconfigure(fitAvgm.tab2, state = 'disabled')
			tkconfigure(combfitVgm.tab2, state = 'disabled')
			tkconfigure(btfitAVgm.tab2, state = 'disabled')

			tkconfigure(plotVgm.tab3, state = 'disabled')
		}
		if(tclvalue(Interp.Method) == 'IDW'){
			tkconfigure(maxdist.tab2, state = 'normal')
			tkconfigure(nmin.tab2, state = 'normal')
			tkconfigure(nmax.tab2, state = 'normal')
			tkconfigure(idp.tab2, state = 'normal')
			tkconfigure(omax.tab2, state = 'normal')

			tkconfigure(fitvgm.tab2, state = 'disabled')
			tkconfigure(fitAvgm.tab2, state = 'disabled')
			tkconfigure(combfitVgm.tab2, state = 'disabled')
			tkconfigure(btfitAVgm.tab2, state = 'disabled')

			tkconfigure(plotVgm.tab3, state = 'disabled')
		}
		if(tclvalue(Interp.Method) == 'Kriging'){
			tkconfigure(maxdist.tab2, state = 'normal')
			tkconfigure(nmin.tab2, state = 'normal')
			tkconfigure(nmax.tab2, state = 'normal')
			tkconfigure(idp.tab2, state = 'disabled')
			tkconfigure(omax.tab2, state = 'normal')

			tkconfigure(fitvgm.tab2, state = 'normal')
			tkconfigure(fitAvgm.tab2, state = 'normal')
			if(tclvalue(fitVgmChx) == '0'){
				tkconfigure(combfitVgm.tab2, state = 'normal')
				tkconfigure(btfitAVgm.tab2, state = 'disabled')
			}else{
				tkconfigure(combfitVgm.tab2, state = 'disabled')
				tkconfigure(btfitAVgm.tab2, state = 'normal')
			}

			tkconfigure(plotVgm.tab3, state = 'normal')
		}
	})

	######
	tkbind(fitvgm.tab2, "<Button-1>", function(){
		if(tclvalue(Interp.Method) == 'Kriging'){
			tkconfigure(combfitVgm.tab2, state = 'normal')
			tkconfigure(btfitAVgm.tab2, state = 'disabled')
		}
	})
	tkbind(fitAvgm.tab2, "<Button-1>", function(){
		if(tclvalue(Interp.Method) == 'Kriging'){
			tkconfigure(combfitVgm.tab2, state = 'disabled')
			tkconfigure(btfitAVgm.tab2, state = 'normal')
		}
	})

	######
	tkbind(useELV.tab2, "<Button-1>", function(){
		if(tclvalue(useELV) == '0'){
			tkconfigure(combDem.tab2, state = 'normal')
			tkconfigure(btDem.tab2, state = 'normal')
		}else{
			tkconfigure(combDem.tab2, state = 'disabled')
			tkconfigure(btDem.tab2, state = 'disabled')
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

	#############################

	file.save1 <- tclVar()
	file.plotShp <- tclVar()
	blankVal <- tclVar('0')

	plotData.tab3 <- tkbutton(subfr3, text = "Plot Data", width = wbutton)
	plotVgm.tab3 <- tkbutton(subfr3, text = "Plot Variogram", state = 'disabled', width = wbutton)
	InterpData.tab3 <- tkbutton(subfr3, text = "Interpolate", width = wbutton)
	plotInterpVal.tab3 <- tkbutton(subfr3, text = "Plot Interpolated values", width = wbutton)
	remData.tab3 <- tkbutton(subfr3, text = "Delete some observations", width = wbutton)

	frameSaveInt <- ttklabelframe(subfr3, text = "File to save interpolated data", relief = 'groove')
	fl2sav.tab3 <- tkentry(frameSaveInt, textvariable = file.save1, width = largeur1) 
	bfl2sav.tab3 <- tkbutton(frameSaveInt, text = "...")

	frameShp <- ttklabelframe(subfr3, text = "Shapefiles for boundary", relief = 'groove')
	combShp.tab3 <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur) 
	btShp.tab3 <- tkbutton(frameShp, text = "...")

	cbBlank.tab3 <- tkcheckbutton(subfr3, variable = blankVal, text = 'Blank grid', anchor = 'w', justify = 'left')

	# #############
	sep1.tab3 <- ttkseparator(subfr3)
	sep2.tab3 <- ttkseparator(subfr3)
	sep3.tab3 <- ttkseparator(subfr3)
	sep4.tab3 <- ttkseparator(subfr3)
	sep5.tab3 <- ttkseparator(subfr3)

	txt.space1 <- tklabel(subfr3, text = ' ', width = 3)
	txt.space2 <- tklabel(subfr3, text = ' ', width = 3)

	#############################

	tkconfigure(bfl2sav.tab3, command = function(){
		filetypes <- "{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"
		if(Sys.info()["sysname"] == "Windows")  file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes, defaultextension = TRUE)
		else file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes)
		if(is.na(file2save)) tclvalue(file.save1) <- ""
		else tclvalue(file.save1) <- file2save
	})

	tkconfigure(btShp.tab3, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(combStnfl.tab1, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(combgrdCDF.tab1, values = unlist(listOpenFiles), textvariable = file.grdCDF)
			tkconfigure(combDem.tab2, values = unlist(listOpenFiles), textvariable = file.plotDem)
			tkconfigure(combShp.tab3, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}
	})

	#############################
	tkgrid(fl2sav.tab3, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bfl2sav.tab3, row = 0, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#############################
	tkgrid(combShp.tab3, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btShp.tab3, row = 0, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#############################

	tkgrid(txt.space1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(plotData.tab3, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.space2, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep1.tab3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 1)
	tkgrid(plotVgm.tab3, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep2.tab3, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 1)
	tkgrid(InterpData.tab3, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab3, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 1)
	tkgrid(plotInterpVal.tab3, row = 6, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep4.tab3, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 1)
	tkgrid(remData.tab3, row = 8, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	
	tkgrid(sep5.tab3, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 2)
	tkgrid(frameSaveInt, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 1)

	tkgrid(frameShp, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 1)
	tkgrid(cbBlank.tab3, row = 12, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, ipady = 2)

	#############################
	infobulle(cbBlank.tab3, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cbBlank.tab3, TextOutputVar, 'Blank grid outside the country boundaries  or over ocean given by the shapefile')

	#######################################################################################################

	##Used to change some obs
	assign('donnees', NULL, envir = EnvInterpolation)
	assign('getDEMFirst', NULL, envir = EnvInterpolation)
	assign('filechange', '', envir = EnvInterpolation)
	atLev <- NULL

	getDONNE <- function(){
		if(is.null(EnvInterpolation$donnees)){
			donne <- getCDTdata(file.stnfl, file.period)
			donne <- getCDTdata1Date(donne, tclvalue(idate_yrs), tclvalue(idate_mon), tclvalue(idate_day))
			# assign('elvStn', donne$elv, envir = EnvInterpolation)
			if(tclvalue(useELV) == '1'){
				if(EnvInterpolation$filechange != tclvalue(file.plotDem) | is.null(EnvInterpolation$getDEMFirst)){
					demdata <- getNcdfOpenData(file.plotDem)[[2]]
					if(!is.null(demdata)){
						elv <- getDEMatStationsLoc(donne, demdata)
						assign('getDEMFirst', 1, envir = EnvInterpolation)
						assign('filechange', tclvalue(file.plotDem), envir = EnvInterpolation)
					}else{
						elv <- NULL
						InsertMessagesTxt(main.txt.out, 'No DEM data found', format = TRUE)
					}
					 assign('elvDem', elv, envir = EnvInterpolation)
				}else{
					elv <- EnvInterpolation$elvDem
					if(is.null(elv)) InsertMessagesTxt(main.txt.out, 'No DEM data found', format = TRUE)
				}
				donne$elv <- elv
				assign("ELV", elv, envir = EnvInterpolation)
			}else{
				if(is.null(EnvInterpolation$ELV)) donne$elv <- donne$elv
				else donne$elv <- EnvInterpolation$ELV
			}
		}else{
			donne <- EnvInterpolation$donnees
			if(tclvalue(useELV) == '1'){
				if(EnvInterpolation$filechange != tclvalue(file.plotDem) | is.null(EnvInterpolation$getDEMFirst)){
					demdata <- getNcdfOpenData(file.plotDem)[[2]]
					if(!is.null(demdata)){
						elv <- getDEMatStationsLoc(donne, demdata)
						assign('getDEMFirst', 1, envir = EnvInterpolation)
						assign('filechange', tclvalue(file.plotDem), envir = EnvInterpolation)
					}else{
						elv <- NULL
						InsertMessagesTxt(main.txt.out, 'No DEM data found', format = TRUE)
					}
					assign('elvDem', elv, envir = EnvInterpolation)
				}else{
					elv <- EnvInterpolation$elvDem
					#elv <- donne$elv
					if(is.null(elv)) InsertMessagesTxt(main.txt.out, 'No DEM data found', format = TRUE)
				}
				donne$elv <- elv
				assign("ELV", elv, envir = EnvInterpolation)
			}else{
				if(is.null(EnvInterpolation$ELV)) donne$elv <- donne$elv
				else donne$elv <- EnvInterpolation$ELV
			}
		}
		return(donne)
	}

	#############
	notebookTab0 <- NULL

	tkconfigure(plotData.tab3, command = function(){
		if(tclvalue(custom.color) == '0' | length(listCol) == 0){
			n <- as.numeric(tclvalue(nb.color))
			if(is.na(n)) n <- 10
			colFun <- match.fun(tclvalue(preset.color))
			listCol <- colFun(n)
			if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		}

		donne <- getDONNE()
		###########
		if(tclvalue(custom.level) == '0' | length(atLev) == 0){
			if(!is.null(donne)) atLev <<- pretty(donne$z)
		}

		shpf <- getShpOpenData(file.plotShp)[[2]]
		units <- NA

		imgContainer <- displayCDTdata(tknotes, notebookTab0, donne, atLev, listCol, shpf, units)
		if(!is.null(imgContainer)){
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab0, AllOpenTabType, AllOpenTabData)
			notebookTab0 <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	#############
	notebookTab1 <- NULL

	tkconfigure(plotVgm.tab3, command = function(){
		donne <- getDONNE()
		imgContainer <- displayVariogramFun(tknotes, notebookTab1, donne, tclvalue(fitVgmChx),
											tclvalue(fitVgmMod), vgmModList, tclvalue(useELV))
		if(!is.null(imgContainer)){
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab1, AllOpenTabType, AllOpenTabData)
			notebookTab1 <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})


	#############
	outNCdata <- NULL

	tkconfigure(InterpData.tab3, command = function(){
		donne <- getDONNE()
		demdata <- getNcdfOpenData(file.plotDem)[[2]]
		interpolParams <- list(mthd = tclvalue(Interp.Method), grdChx = tclvalue(varCreateGrd),
								ncfila = tclvalue(file.grdCDF), grdCR = newgrdPars,
								file2save = tclvalue(file.save1), vgmChx = tclvalue(fitVgmChx),
								VgmMod = tclvalue(fitVgmMod), vgmModList = vgmModList,
								useELV = tclvalue(useELV), maxdist = tclvalue(maxdist_vars),
								nmin = tclvalue(nmin_vars), nmax = tclvalue(nmax_vars),
								idp = tclvalue(idp_vars), omax = tclvalue(omax_vars),
								elvdiff = tclvalue(elvdiff_vars))

		tkconfigure(main.win, cursor = 'watch')
		tcl('update')

		tryCatch(outNCdata <<- interpolationProc(donne, demdata, interpolParams),
		#warning = function(w) warningFun(w),
		error = function(e) errorFun(e), finally = {
			tkconfigure(main.win, cursor = '')
		})

		if(!is.null(outNCdata)) InsertMessagesTxt(main.txt.out, "Interpolation finished successfully")
	})

	#############
	notebookTab2 <- NULL

	tkconfigure(plotInterpVal.tab3, command = function(){
		if(tclvalue(custom.color) == '0' | length(listCol) == 0){
			n <- as.numeric(tclvalue(nb.color))
			if(is.na(n)) n <- 10
			colFun <- match.fun(tclvalue(preset.color))
			listCol <- colFun(n)
			if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		}

		if(tclvalue(custom.level) == '0' | length(atLev) == 0){
			if(!is.null(outNCdata)) atLev <<- pretty(outNCdata$data$z)
		}

		shpf <- getShpOpenData(file.plotShp)[[2]]
		units <- ""

		imgContainer <- displayNetCDFdata(tknotes, notebookTab2, outNCdata$data, atLev, listCol, shpf,
											units, tclvalue(blankVal), outNCdata$filename)
		if(!is.null(imgContainer)){
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, notebookTab2, AllOpenTabType, AllOpenTabData)
			notebookTab2 <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	#############
	notebookTab3 <- NULL

	tkconfigure(remData.tab3, command = function(){
		donne <- getDONNE()
		if(!is.null(donne)){
			# retNBTab <- tableInterpNotebookTab_unik(tknotes, donne, notebookTab3, AllOpenTabType, AllOpenTabData)
			# notebookTab3 <<- retNBTab$notebookTab
			# AllOpenTabType <<- retNBTab$AllOpenTabType
			# AllOpenTabData <<- retNBTab$AllOpenTabData
			# popupAddRemoveRow(tknotes)

			if(is.null(donne$elv)) elv <- NA
			else elv <- donne$elv
			dat2disp <- data.frame(id = donne$id, lon = donne$lon, lat = donne$lat, z = donne$z, elv = elv)
			dat2disp <- dat2disp[!is.na(dat2disp$z), ]
			dat2disp <- list(donne$date, dat2disp,'/may/be/file/to/save/table')
			retdata <- DisplayInterpData(tknotes, dat2disp, paste('Obs', donne$date, sep = '_'))

			ntab <- length(AllOpenTabType)
			AllOpenTabType[[ntab+1]] <<- 'arrInterp'
			AllOpenTabData[[ntab+1]] <<- retdata
			tkselect(tknotes, ntab)
			popupAddRemoveRow(tknotes)

			ReturnExecResults <<- donne
		}else InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
	})

	#######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4)
	tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab4, 0, weight = 1)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	tkgrid.columnconfigure(scrw4, 0, weight = 1)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr4, 0, weight = 1)

	#############
	nb.color <- tclVar('10')
	preset.color <- tclVar('tim.colors')
	reverse.color <- tclVar(0)
	custom.color <- tclVar(0)
	custom.level <- tclVar(0)
	preset.values <- c('tim.colors', 'rainbow', 'heat.colors', 'cm.colors', 'topo.colors', 'terrain.colors')


	labPresetCol.tab4 <- tklabel(subfr4, text = 'Presets colorkey', anchor = 'w', justify = 'left')
	combPresetCol.tab4 <- ttkcombobox(subfr4, values = preset.values, textvariable = preset.color, width = 13)
	nbPresetCol.tab4 <- tkentry(subfr4, width = 3, textvariable = nb.color, justify = "left")

	labRevCol.tab4 <- tklabel(subfr4, text = 'Reverse', anchor = 'e', justify = 'right')
	chkRevCol.tab4 <- tkcheckbutton(subfr4, variable = reverse.color, anchor = 'w', justify = 'left')

	previewPresetCol.tab4 <- tkcanvas(subfr4, width = wPreview, height = 20, bg = 'white')

	chkCustoCol.tab4 <- tkcheckbutton(subfr4, variable = custom.color, text = 'User customized  colorkey', anchor = 'w', justify = 'left')
	butCustoCol.tab4 <- tkbutton(subfr4, text = "Custom", state = 'disabled')

	chkCustoLev.tab4 <- tkcheckbutton(subfr4, variable = custom.level, text = 'User customized  levels', anchor = 'w', justify = 'left')
	butCustoLev.tab4 <- tkbutton(subfr4, text = "Custom", state = 'disabled')

	sep1.tab4 <- ttkseparator(subfr4)
	sep2.tab4 <- ttkseparator(subfr4)
	sep3.tab4 <- ttkseparator(subfr4)

	#####
	tkgrid(labPresetCol.tab4, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(combPresetCol.tab4, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(nbPresetCol.tab4, row = 0, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(labRevCol.tab4, row = 1, column = 2, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chkRevCol.tab4, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1.tab4, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(previewPresetCol.tab4, row = 3, column = 0, sticky = 'w', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
	tkgrid(sep2.tab4, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(chkCustoCol.tab4, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(butCustoCol.tab4, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3.tab4, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, pady = 5)
	tkgrid(chkCustoLev.tab4, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(butCustoLev.tab4, row = 7, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)


	infobulle(combPresetCol.tab4, 'Predefined color palettes')
	status.bar.display(combPresetCol.tab4, TextOutputVar, 'Predefined color palettes')
	infobulle(nbPresetCol.tab4, 'Number of color levels to be in the palette')
	status.bar.display(nbPresetCol.tab4, TextOutputVar, 'Number of color levels to be in the palette')
	infobulle(chkRevCol.tab4, 'Reverse the color palettes')
	status.bar.display(chkRevCol.tab4, TextOutputVar, 'Reverse the color palettes')

	########################
	##Preview Color
	kolor <- getGradientColor(tim.colors(10), 0:wPreview)
	tkdelete(previewPresetCol.tab4, 'gradlines0')
	for(i in 0:wPreview) tkcreate(previewPresetCol.tab4, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')

	tkbind(combPresetCol.tab4, "<<ComboboxSelected>>", function(){
		n <- as.numeric(tclvalue(nb.color))
		if(is.na(n)) n <- 10
		colFun <- match.fun(tclvalue(preset.color))
		listCol <- colFun(n)
		if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
		kolor <- getGradientColor(listCol, 0:wPreview)
		tkdelete(previewPresetCol.tab4, 'gradlines0')
		for(i in 0:wPreview) tkcreate(previewPresetCol.tab4, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
	})

	#reverse
	tkbind(chkRevCol.tab4, "<Button-1>", function(){
		if(tclvalue(custom.color) == '0'){
			n <- as.numeric(tclvalue(nb.color))
			if(is.na(n)) n <- 10
			colFun <- match.fun(tclvalue(preset.color))
			listCol <- colFun(n)
			if(tclvalue(reverse.color) == '0') listCol <- rev(listCol)
			kolor <- getGradientColor(listCol, 0:wPreview)
			tkdelete(previewPresetCol.tab4, 'gradlines0')
			for(i in 0:wPreview) tkcreate(previewPresetCol.tab4, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
		}
	})
	
	########################
	##Customized color	
	tkbind(chkCustoCol.tab4, "<Button-1>", function(){
		if(tclvalue(custom.color) == '0') tkconfigure(butCustoCol.tab4, state = 'normal')
		else tkconfigure(butCustoCol.tab4, state = 'disabled')
	})

	listCol <- NULL	
	tkconfigure(butCustoCol.tab4, command = function(){
		listCol <<- createColorkey(main.win, listCol)
		if(!is.null(listCol) & length(listCol) > 0){
			kolor <- getGradientColor(listCol, 0:wPreview)
			tkdelete(previewPresetCol.tab4, 'gradlines0')
			for(i in 0:wPreview) tkcreate(previewPresetCol.tab4, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
		}
	})

	########################
	##Customized level	
	tkbind(chkCustoLev.tab4, "<Button-1>", function(){
		if(tclvalue(custom.level) == '0') tkconfigure(butCustoLev.tab4, state = 'normal')
		else tkconfigure(butCustoLev.tab4, state = 'disabled')
	})
	
	tkconfigure(butCustoLev.tab4, command = function(){
		if(is.null(atLev)){
			n <- as.numeric(tclvalue(nb.color))
			if(is.na(n)) n <- 10
			donne <- getCDTdata(file.stnfl, file.period)
			donne <- getCDTdata1Date(donne, tclvalue(idate_yrs), tclvalue(idate_mon), tclvalue(idate_day))
			if(!is.null(donne)){
				atLev <- pretty(donne$z, n = n+1)
			}
		}
		atLev <<- customLevels(main.win, atLev)
	})

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}

#######################################################################################################

editVgmModel <- function(tt, vgmModList){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt1)

	yscr <- tkscrollbar(frGrd0, repeatinterval = 4, command = function(...) tkyview(textObj, ...))
	textObj <- tktext(frGrd0, bg = "white", wrap = "word", height = 5, width = w.opfiles+5,
								yscrollcommand = function(...) tkset(yscr, ...))
	tkgrid(textObj, yscr)
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(textObj, sticky = 'nswe')

	tcl("update", "idletasks")
	if(length(vgmModList) > 0)	 tkinsert(textObj, "end", paste(vgmModList, collapse = ', '))

	################################

	bt.prm.OK <- tkbutton(frGrd1, text = "OK")
	bt.prm.CA <- tkbutton(frGrd1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		retvars <- tclvalue(tkget(textObj, "0.0", "end"))
		retvars <- gsub("[\t\r\n]", "", retvars)
		retvars <- gsub('\\s+', '',retvars)
		retvars <- strsplit(retvars, ",")[[1]]
		retvars <- retvars[!is.na(retvars) & retvars != '']
		vgmModList <<- retvars

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 2, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Edit Variogram Model')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(vgmModList)
}

##############################################################################################

getParamNewGrid <- function(tt, GeneralParameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt1)

	################################

	fr_grd <- ttklabelframe(frGrd0, text = "Create new grid", relief = "groove", borderwidth = 2)

	minLon <- tclVar(GeneralParameters$New.Grid.Def$minlon)
	maxLon <- tclVar(GeneralParameters$New.Grid.Def$maxlon)
	resLon <- tclVar(GeneralParameters$New.Grid.Def$reslon)
	minLat <- tclVar(GeneralParameters$New.Grid.Def$minlat)
	maxLat <- tclVar(GeneralParameters$New.Grid.Def$maxlat)
	resLat <- tclVar(GeneralParameters$New.Grid.Def$reslat)

	grd_llon <- tklabel(fr_grd, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(fr_grd, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = "Min")
	grd_lb2 <- tklabel(fr_grd, text = "Max")
	grd_lb3 <- tklabel(fr_grd, text = "Res")

	grd_vlon1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLon)
	grd_vlon3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLon)
	grd_vlat1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLat)
	grd_vlat3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLat)

	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 0, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(grd_vlon1, 'Minimum longitude in degree decimal')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in degree decimal')
	infobulle(grd_vlon2, 'Maximum longitude in degree decimal')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in degree decimal')
	infobulle(grd_vlon3, 'Resolution in degree decimal')
	status.bar.display(grd_vlon3, TextOutputVar, 'Resolution in degree decimal')
	infobulle(grd_vlat1, 'Minimum latitude in degree decimal')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in degree decimal')
	infobulle(grd_vlat2, 'Maximum latitude in degree decimal')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in degree decimal')
	infobulle(grd_vlat3, 'Resolution in degree decimal')
	status.bar.display(grd_vlat3, TextOutputVar, 'Resolution in degree decimal')

	################################

	tkgrid(fr_grd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frGrd1, text=" OK ")
	bt.prm.CA <- tkbutton(frGrd1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$New.Grid.Def$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
		GeneralParameters$New.Grid.Def$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
		GeneralParameters$New.Grid.Def$reslon <<- as.numeric(str_trim(tclvalue(resLon)))
		GeneralParameters$New.Grid.Def$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
		GeneralParameters$New.Grid.Def$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))
		GeneralParameters$New.Grid.Def$reslat <<- as.numeric(str_trim(tclvalue(resLat)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Grid Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}


