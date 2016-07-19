
mergeDekadInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	#tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21
	##tkentry width, filename format, prefix
	if (Sys.info()["sysname"] == "Windows") largeur1 <- 23
	else largeur1 <- 21

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	fr.C <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.C)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.C, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:3) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:3) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	fr.A00 <- tkframe(fr.A0)
	fr.A01 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A01, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	frA00.txt <- tklabel(fr.A00, text = 'Station data file')
	tkgrid(frA00.txt)

	cb.stnfl <- ttkcombobox(fr.A01, values = unlist(listOpenFiles), textvariable = file.stnfl)
	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')

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
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else{
			return(NULL)
		}
	})

	fr.A10 <- tkframe(fr.A1)
	fr.A11 <- tkframe(fr.A1)
	fr.A12 <- tkframe(fr.A1)
	fr.A13 <- tkframe(fr.A1)

	tkgrid(fr.A10, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A11, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A12, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A13, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA10.txt <- tklabel(fr.A10, text = 'Directory of RFE files')
	tkgrid(frA10.txt)

	dir.rfe <- tclVar(as.character(GeneralParameters$file.io$Values[2]))
	en.dir.rfe <- tkentry(fr.A11, textvariable = dir.rfe, width = largeur)
	infobulle(en.dir.rfe, 'Enter the full path to\ndirectory containing the RFE files\n(already extracted over the area\nof interest)')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the RFE files (already extracted over the area of interest)')
	bt.dir.rfe <- tkbutton.h(fr.A11, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.dir.rfe, bt.dir.rfe)
	tkgrid.configure(en.dir.rfe, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.rfe, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[2]), "")
		if(is.na(dir4rfe)) tclvalue(dir.rfe)<-""
		else tclvalue(dir.rfe) <- dir4rfe
	})

	frA12.txt <- tklabel(fr.A12, text = "Directory of mean bias files")
	tkgrid(frA12.txt)

	##
	dir.bias <- tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.dir.bias <- tkentry(fr.A13, textvariable = dir.bias, width = largeur)
	infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias, TextOutputVar, 'Enter the full path to directory containing the mean bias files')
	bt.dir.bias <- tkbutton.h(fr.A13, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.dir.bias, bt.dir.bias)
	tkgrid.configure(en.dir.bias, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.bias, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.bias, command = function(){
		dir4bias <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(is.na(dir4bias)) tclvalue(dir.bias)<-""
		else tclvalue(dir.bias) <- dir4bias
	})

	fr.A20 <- tkframe(fr.A2)
	fr.A21 <- tkframe(fr.A2)
	tkgrid(fr.A20, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A21, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA20.txt <- tklabel(fr.A20, text = 'Directory to save result')
	tkgrid(frA20.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.file.save <- tkentry(fr.A21, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A21, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[4])
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})
	###############

	fr.A30 <- tkframe(fr.A3)
	fr.A31 <- tkframe(fr.A3)
	fr.A32 <- tkframe(fr.A3)
	fr.A33 <- tkframe(fr.A3)
	tkgrid(fr.A30, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A31, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A32, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A33, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	#####
	frA30.txt <- tklabel(fr.A30, text = "Elevation data(NetCDF)")
	tkgrid(frA30.txt)

	if(as.character(GeneralParameters$blankGrd) == '2') statedem <- 'normal'
	else statedem <- 'disabled'

	####Use DEM
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[5])

	###
	cb.grddem <- ttkcombobox(fr.A31, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem)
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	bt.grddem <- tkbutton.h(fr.A31, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.grddem, bt.grddem)
	tkgrid.configure(cb.grddem, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.grddem, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.grddem, state = statedem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else{
			return(NULL)
		}
	})

	####################################
	file.blkshp <- tclVar()
	tclvalue(file.blkshp) <- as.character(GeneralParameters$file.io$Values[6])

	#####Use shp
	frA32.txt <- tklabel(fr.A32, text = "ESRI shapefiles for blanking")
	tkgrid(frA32.txt)

	if(as.character(GeneralParameters$blankGrd) == '3') stateshp <- 'normal'
	else stateshp <- 'disabled'

	##
	cb.blkshp <- ttkcombobox(fr.A33, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp)
	infobulle(cb.blkshp, 'Choose the file in the list')
	status.bar.display(cb.blkshp, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	bt.blkshp <- tkbutton.h(fr.A33, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.blkshp, bt.blkshp)
	tkgrid.configure(cb.blkshp, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.blkshp, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.blkshp, state = stateshp, command = function(){
		shp.opfiles <- getOpenShp(tt, all.opfiles)

		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			tclvalue(file.blkshp) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.blkshp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else{
			return(NULL)
		}
	})
	###############################################

	pr.relief.set2 <- c('sunken', 'sunken', 'sunken')
	for(i in 0:2) assign(paste('fr.C', i, sep = ''), tkframe(fr.C, relief = pr.relief.set2[i+1], borderwidth = 2))
	for(i in 0:2) tkgrid(get(paste('fr.C', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.C', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############################
	for(i in 0:2) assign(paste('fr.C0', i, sep = ''), tkframe(fr.C0))
	for(i in 0:2) tkgrid(get(paste('fr.C0', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.C0', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 0, ipadx = 1, ipady = 0)


	frC00.txt <- tklabel(fr.C00, text = 'Input RFE filename format', anchor = 'w', justify = 'left')
	rfeflformat <- tclVar(as.character(GeneralParameters$prefix$Values[1]))
	en.rfeflformat <- tkentry(fr.C00, textvariable = rfeflformat, width = largeur1)
	infobulle(en.rfeflformat, 'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
	status.bar.display(en.rfeflformat, TextOutputVar, 'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
	tkgrid(frC00.txt, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(en.rfeflformat, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	####
	frC01.txt <- tklabel(fr.C01, text = 'Mean bias filename prefix', anchor = 'w', justify = 'left')

	meanbsprefix <- tclVar(as.character(GeneralParameters$prefix$Values[2]))
	en.meanbsprefix <- tkentry(fr.C01, textvariable = meanbsprefix, width = largeur1)
	infobulle(en.meanbsprefix, 'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.meanbsprefix, TextOutputVar, 'Prefix for the file name of the mean bias coefficient')

	tkgrid(frC01.txt, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(en.meanbsprefix, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	######
	infobulle(fr.C02, 'Date of merging')
	status.bar.display(fr.C02, TextOutputVar, 'Date of merging')

	date.txt <- tklabel(fr.C02, text = 'Date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(fr.C02, text = 'Year')
	mon.txt <- tklabel(fr.C02, text = 'Month')
	day.txt <- tklabel(fr.C02, text = 'Dekad')

	istart.yrs <- tclVar(as.character(GeneralParameters$dates.mrg$Values[1]))
	istart.mon <- tclVar(as.character(GeneralParameters$dates.mrg$Values[2]))
	istart.day <- tclVar(as.character(GeneralParameters$dates.mrg$Values[3]))

	yrs1.v <- tkentry(fr.C02, width = 5, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(fr.C02, width = 5, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(fr.C02, width = 5, textvariable = istart.day, justify = "right", state = 'normal')

	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(date.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)

	###########################################
	blankGrd <- tclVar(as.character(GeneralParameters$blankGrd))
	tclvalue(blankGrd) <- ifelse(as.character(GeneralParameters$blankGrd) == '1',
	'None', ifelse(as.character(GeneralParameters$blankGrd) == '2', 'Use DEM', 'Use ESRI shapefile'))

	frC10.txt <- tklabel(fr.C1, text = 'Blank grid', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(fr.C1, values = c("None", "Use DEM", "Use ESRI shapefile"), textvariable = blankGrd)
	infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cb.blankGrd, TextOutputVar, 'Blank grid outside the country boundaries  or over ocean given by the DEM mask or the shapefile')

	interpMethod <- tclVar(as.character(GeneralParameters$params.mrg$Values[1]))
	txt.interpMethod <- tklabel(fr.C1, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.interpMethod <- ttkcombobox(fr.C1, values = c('IDW', 'Kriging'), textvariable = interpMethod)
	infobulle(cb.interpMethod, 'Interpolation techniques: Kriging or Inverse Distance Weighted')
	status.bar.display(cb.interpMethod, TextOutputVar, 'Interpolation techniques: Kriging or Inverse Distance Weighted')

	RainNoRain <- tclVar(as.character(GeneralParameters$params.mrg$Values[2]))
	txt.RainNoRain <- tklabel(fr.C1, text = 'Rain-no-Rain mask', anchor = 'w', justify = 'left')
	cb.RainNoRain <- ttkcombobox(fr.C1, values = c('None', 'Gauge', 'Satellite', 'GaugeSatellite'), textvariable = RainNoRain)
	infobulle(cb.RainNoRain, 'Mask applied to handle no rain')
	status.bar.display(cb.RainNoRain, TextOutputVar, 'Mask applied to handle no rain')

	tkgrid(frC10.txt, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(cb.blankGrd, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(txt.interpMethod, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(cb.interpMethod, row = 3, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(txt.RainNoRain, row = 4, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(cb.RainNoRain, row = 5, column = 0, sticky = 'ew', padx = 1, pady = 1)

	############################################

	nmin.l <- tklabel.h(fr.C2, 'NminStn', TextOutputVar,
	'Minimum number of gauges with data to be used to do the merging',
	'Minimum number of gauges with data to be used to do the merging')
	min.non.zero.l <- tklabel.h(fr.C2, 'NminNo0', TextOutputVar,
	'Minimum number of non-zero gauge values to perform the merging',
	'Minimum number of non-zero gauge values to perform the merging')
	max.rnr.dst.l <- tklabel.h(fr.C2, 'MaxRnRDist', TextOutputVar,
	'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask',
	'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	max.dst.l <- tklabel.h(fr.C2, 'MaxDist', TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate data',
	'Maximum distance (in  decimal degree) to be used to interpolate data')
	min.nbrs.l <- tklabel.h(fr.C2, 'MinStn', TextOutputVar,
	'Minimum number of neighbours to be used to interpolate data',
	'Minimum number of neighbours to be used to interpolate data')
	max.nbrs.l <- tklabel.h(fr.C2, 'MaxStn', TextOutputVar,
	'Maximum number of neighbours to be used to interpolate data',
	'Maximum number of neighbours to be used to interpolate data')

	nmin.v <- tkentry.h(fr.C2, TextOutputVar,
	'Minimum number of gauges with data to be used to do the merging',
	'Minimum number of gauges with data to be used to do the merging')
	min.non.zero.v <- tkentry.h(fr.C2, TextOutputVar,
	'Minimum number of non-zero gauge values to perform the merging',
	'Minimum number of non-zero gauge values to perform the merging')
	max.rnr.dst.v <- tkentry.h(fr.C2, TextOutputVar,
	'Maximum distance (in decimal degrees) for interpolating Rain-no-Rain mask',
	'Maximum distance (in decimal degrees) for interpolating Rain-no-Rain mask')
	max.dst.v <- tkentry.h(fr.C2, TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate data',
	'Maximum distance (in  decimal degree) to be used to interpolate data')
	min.nbrs.v <- tkentry.h(fr.C2, TextOutputVar,
	'Minimum number of neighbours to be used to interpolate data',
	'Minimum number of neighbours to be used to interpolate data')
	max.nbrs.v <- tkentry.h(fr.C2, TextOutputVar,
	'Maximum number of neighbours to be used to interpolate data',
	'Maximum number of neighbours to be used to interpolate data')

	tkgrid(nmin.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(nmin.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.non.zero.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.non.zero.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.rnr.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.rnr.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(nmin.l, anchor = 'e', justify = 'right')
	tkconfigure(min.non.zero.l, anchor = 'e', justify = 'right')
	tkconfigure(max.rnr.dst.l, anchor = 'e', justify = 'right')
	tkconfigure(max.dst.l, anchor = 'e', justify = 'right')
	tkconfigure(min.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.nbrs.l, anchor = 'e', justify = 'right')

	nmin <- tclVar(as.character(GeneralParameters$params.int$Values[1]))
	min.non.zero <- tclVar(as.character(GeneralParameters$params.int$Values[2]))
	max.rnr.dst <- tclVar(as.character(GeneralParameters$params.int$Values[3]))
	max.dst <- tclVar(as.character(GeneralParameters$params.int$Values[4]))
	min.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[5]))
	max.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[6]))

	tkconfigure(nmin.v, width = 4, textvariable = nmin, justify = 'right')
	tkconfigure(min.non.zero.v, width = 4, textvariable = min.non.zero, justify = 'right')
	tkconfigure(max.rnr.dst.v, width = 4, textvariable = max.rnr.dst, justify = 'right')
	tkconfigure(max.dst.v, width = 4, textvariable = max.dst, justify = 'right')
	tkconfigure(min.nbrs.v, width = 4, textvariable = min.nbrs, justify = 'right')
	tkconfigure(max.nbrs.v, width = 4, textvariable = max.nbrs, justify = 'right')

	############################################
	tkbind(cb.blankGrd,"<<ComboboxSelected>>", function(){
		if(tclvalue(blankGrd) == "None"){
			tkconfigure(cb.grddem, state = 'disabled')
			tkconfigure(bt.grddem, state = 'disabled')
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
		}
		if(tclvalue(blankGrd) == "Use DEM"){
			tkconfigure(cb.grddem, state = 'normal')
			tkconfigure(bt.grddem, state = 'normal')
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
		}
		if(tclvalue(blankGrd) == "Use ESRI shapefile"){
			tkconfigure(cb.grddem, state = 'disabled')
			tkconfigure(bt.grddem, state = 'disabled')
			tkconfigure(cb.blkshp, state = 'normal')
			tkconfigure(bt.blkshp, state = 'normal')
		}
	})

	############################################
	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(dir.rfe) == "" | tclvalue(dir.rfe) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(blankGrd) == "Use DEM" & tclvalue(file.grddem) == ""){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(blankGrd) == "Use ESRI shapefile" & tclvalue(file.blkshp) == ""){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$prefix$Values <<- c(tclvalue(rfeflformat), tclvalue(meanbsprefix))
			valfl <- as.character(GeneralParameters$file.io$Values)
			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(dir.rfe), tclvalue(dir.bias), tclvalue(file.save1), tclvalue(file.grddem), tclvalue(file.blkshp))
			GeneralParameters$dates.mrg$Values <<- c(tclvalue(istart.yrs), tclvalue(istart.mon), tclvalue(istart.day))
			GeneralParameters$blankGrd <<- ifelse(tclvalue(blankGrd) == 'None', '1', ifelse(tclvalue(blankGrd) == 'Use DEM', '2', '3'))
			GeneralParameters$params.mrg$Values <<- c(tclvalue(interpMethod), tclvalue(RainNoRain))
			GeneralParameters$params.int$Values <<- c(tclvalue(nmin), tclvalue(min.non.zero), tclvalue(max.rnr.dst), tclvalue(max.dst), tclvalue(min.nbrs), tclvalue(max.nbrs))

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Merging data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


###############################################################################


