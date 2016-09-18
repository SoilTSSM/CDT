
coefDownGetInfoTemp <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	##tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	fr.B <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.B)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.B, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken')
	for(i in 0:2) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:2) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################
	fr.A00 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	cb.period <- ttkcombobox(fr.A00, values = cb.periodVAL, textvariable = file.period)
	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	tkgrid(cb.period)

	##########################################
	fr.A10 <- tkframe(fr.A1)
	fr.A11 <- tkframe(fr.A1)
	tkgrid(fr.A10, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A11, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##########
	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	frA10.txt <- tklabel(fr.A10, text = 'Input data file')
	tkgrid(frA10.txt)

	cb.stnfl <- ttkcombobox(fr.A11, values = unlist(listOpenFiles), textvariable = file.stnfl)
	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')

	bt.stnfl <- tkbutton.h(fr.A11, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
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
		}else{
			return(NULL)
		}
	})

	#######

	fr.A21 <- tkframe(fr.A2)
	fr.A22 <- tkframe(fr.A2)
	tkgrid(fr.A21, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A22, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA21.txt <- tklabel(fr.A21, text = 'Directory to save result')
	tkgrid(frA21.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.file.save <- tkentry(fr.A22, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A22, text = "...", TextOutputVar, 'or browse here', '')
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
	pr.relief.set <- c('sunken', 'sunken', 'sunken')
	for(i in 0:1) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:1) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:1) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################################3
	fr.B01 <- tkframe(fr.B0)
	fr.B02 <- tkframe(fr.B0)
	tkgrid(fr.B01, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.B02, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	##
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	#####
	frB01.txt <- tklabel(fr.B01, text = "Elevation data(NetCDF)")
	tkgrid(frB01.txt)

	###from DEM
	cb.grddem <- ttkcombobox(fr.B02, values = unlist(listOpenFiles), textvariable = file.grddem)
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	bt.grddem <- tkbutton.h(fr.B02, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.grddem, bt.grddem)
	tkgrid.configure(cb.grddem, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.grddem, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.grddem, command = function(){
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
		}else{
			return(NULL)
		}
	})

	###
	years1.l <- tklabel.h(fr.B1, 'StartYear', TextOutputVar,
	'Start year to be used to compute regression parameters between station temperature and elevation',
	'Start year to be used to compute regression parameters between station temperature and elevation')
	years2.l <- tklabel.h(fr.B1, 'EndYear', TextOutputVar,
	'End year to be used to compute regression parameters between station temperature and elevation',
	'End year to be used to compute regression parameters between station temperature and elevation')

	years1.v <- tkentry.h(fr.B1, TextOutputVar,
	'Start year to be used to compute regression parameters between station temperature and elevation',
	'Start year to be used to compute regression parameters between station temperature and elevation')
	years2.v <- tkentry.h(fr.B1, TextOutputVar,
	'End year to be used to compute regression parameters between station temperature and elevation',
	'End year to be used to compute regression parameters between station temperature and elevation')

	tkgrid(years1.l, row = 0, column = 0, padx = 10, pady = 5)
	tkgrid(years1.v, row = 0, column = 1, padx = 10, pady = 5)
	tkgrid(years2.l, row = 1, column = 0, padx = 10, pady = 5)
	tkgrid(years2.v, row = 1, column = 1, padx = 10, pady = 5)

	tkconfigure(years1.l, anchor = 'e', justify = 'right')
	tkconfigure(years2.l, anchor = 'e', justify = 'right')

	year1 <- tclVar(as.character(GeneralParameters$dates.coef$Values[1]))
	year2 <- tclVar(as.character(GeneralParameters$dates.coef$Values[2]))

	tkconfigure(years1.v, width = 6, textvariable = year1, justify = 'right')
	tkconfigure(years2.v, width = 6, textvariable = year2, justify = 'right')

	##############
	bt.opt.OK <- tkbutton(frMRG1, text = "OK")
	bt.opt.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(file.grddem) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.grddem), tclvalue(file.save1))
			GeneralParameters$dates.coef$Values <<- c(tclvalue(year1), tclvalue(year2))

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
	tkwm.title(tt, 'Coefficients Downscaling-Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


#################################################################################################3

downGetInfoDekTempReanal <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	##tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21
	##tkentry width, filename format, prefix
	if (Sys.info()["sysname"] == "Windows") largeur1 <- 28
	else largeur1 <- 26

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	fr.C <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.C)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.C, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:3) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:3) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################
	fr.A00 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	cb.period <- ttkcombobox(fr.A00, values = cb.periodVAL, textvariable = file.period)
	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	tkgrid(cb.period)

	#######
	for(i in 0:5) assign(paste('fr.A1', i, sep = ''), tkframe(fr.A1))
	for(i in 0:5) tkgrid(get(paste('fr.A1', i, sep = '')))
	for(i in 0:5) tkgrid.configure(get(paste('fr.A1', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	####
	file.coef <- tclVar()
	tclvalue(file.coef) <- as.character(GeneralParameters$file.io$Values[1])

	###
	frA10.txt <- tklabel(fr.A10, text = 'Downscaling Coefficients file')
	tkgrid(frA10.txt)

	en.file.coef <- tkentry(fr.A11, textvariable = file.coef, width = largeur)
	infobulle(en.file.coef, 'Enter the full path of the file containing the regression coef for downscaling')
	status.bar.display(en.file.coef, TextOutputVar, 'Enter the full path of the file containing the regression coef for downscaling')
	bt.file.coef <- tkbutton.h(fr.A11, text = "...", TextOutputVar, 'or browse here', 'or browse here')
	tkgrid(en.file.coef, row = 0, column = 0, sticky = 'w')
	tkgrid(bt.file.coef, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.coef, command = function(){
		file2coef <- tkgetOpenFile(initialdir = getwd(), initialfile = "",
						filetypes = "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}")
		if(is.na(file2coef)) tclvalue(file.coef) <- ""
		else{
			tclvalue(file.coef) <- file2coef
		}
	})

	####
	frA12.txt <- tklabel(fr.A12, text = 'Directory of Reanalysis files')
	tkgrid(frA12.txt)

	dir.rfe <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.dir.rfe <- tkentry(fr.A13, textvariable = dir.rfe, width = largeur)
	infobulle(en.dir.rfe, 'Enter the full path to\ndirectory containing the Reanalysis files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the Reanalysis files')
	bt.dir.rfe <- tkbutton.h(fr.A13, text = "...", TextOutputVar, 'or browse here', '')
	tkgrid(en.dir.rfe, bt.dir.rfe)
	tkgrid.configure(en.dir.rfe, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.rfe, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(is.na(dir4rfe)) tclvalue(dir.rfe)<-""
		else tclvalue(dir.rfe) <- dir4rfe
	})

	######
	file.grdrfe <- tclVar()
	tclvalue(file.grdrfe) <- as.character(GeneralParameters$file.io$Values[3])

	######
	frA14.txt <- tklabel(fr.A14, text = "Reanalysis sample file")
	tkgrid(frA14.txt)

	###from RFE
	cb.grdrfe <- ttkcombobox(fr.A15, values = unlist(listOpenFiles), textvariable = file.grdrfe)
	infobulle(cb.grdrfe, 'Choose the file in the list')
	status.bar.display(cb.grdrfe, TextOutputVar, 'Choose the file containing a sample of Reanalysis data in netcdf')
	bt.grdrfe <- tkbutton.h(fr.A15, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.grdrfe, bt.grdrfe)
	tkgrid.configure(cb.grdrfe, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.grdrfe, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.grdrfe, command = function(){
		fileopen <- tclvalue(tkgetOpenFile(initialdir = tclvalue(dir.rfe), initialfile = "",
											filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen == "" | is.na(fileopen)) return(NULL)
		nc.opfiles1 <- preview.data.nc(tt, fileopen, "")
		nc.opfiles <- list(basename(fileopen), nc.opfiles1, fileopen)

		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles, "end", basename(fileopen))
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
		}else{
			return(NULL)
		}
	})

	################################################3
	fr.A21 <- tkframe(fr.A2)
	fr.A22 <- tkframe(fr.A2)
	tkgrid(fr.A21, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A22, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	##
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	#####
	frA21.txt <- tklabel(fr.A21, text = "Elevation data(NetCDF)")
	tkgrid(frA21.txt)

	###from DEM
	cb.grddem <- ttkcombobox(fr.A22, values = unlist(listOpenFiles), textvariable = file.grddem)
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	bt.grddem <- tkbutton.h(fr.A22, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.grddem, bt.grddem)
	tkgrid.configure(cb.grddem, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.grddem, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.grddem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
		}else{
			return(NULL)
		}
	})

	#######
	fr.A31 <- tkframe(fr.A3)
	fr.A32 <- tkframe(fr.A3)
	tkgrid(fr.A31, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A32, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA31.txt <- tklabel(fr.A31, text = 'Directory to save result')
	tkgrid(frA31.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[5]))
	en.file.save <- tkentry(fr.A32, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A32, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[5]), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[5])
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	#################################
	pr.relief.set2 <- c('sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:3) assign(paste('fr.C', i, sep = ''), tkframe(fr.C, relief = pr.relief.set2[i+1], borderwidth = 2))
	for(i in 0:3) tkgrid(get(paste('fr.C', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.C', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################

	infobulle(fr.C0, 'Start and end date for downscaling')
	status.bar.display(fr.C0, TextOutputVar, 'Start and end date for downscaling')

	deb.txt <- tklabel(fr.C0, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(fr.C0, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(fr.C0, text = 'Year')
	mon.txt <- tklabel(fr.C0, text = 'Month')
	if(as.character(GeneralParameters$period) == 'dekadal') day.txtVar <- tclVar('Dek')
	else day.txtVar <- tclVar('Day')
	day.txt <- tklabel(fr.C0, text = tclvalue(day.txtVar), textvariable = day.txtVar)

	istart.yrs <- tclVar(as.character(GeneralParameters$dates.down$Values[1]))
	istart.mon <- tclVar(as.character(GeneralParameters$dates.down$Values[2]))
	istart.day <- tclVar(as.character(GeneralParameters$dates.down$Values[3]))
	iend.yrs <- tclVar(as.character(GeneralParameters$dates.down$Values[4]))
	iend.mon <- tclVar(as.character(GeneralParameters$dates.down$Values[5]))
	iend.day <- tclVar(as.character(GeneralParameters$dates.down$Values[6]))

	yrs1.v <- tkentry(fr.C0, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(fr.C0, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(fr.C0, width = 4, textvariable = istart.day, justify = "right", state = 'normal')
	yrs2.v <- tkentry(fr.C0, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(fr.C0, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(fr.C0, width = 4, textvariable = iend.day, justify = "right", state = 'normal')

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	###############
	for(i in 0:3) assign(paste('fr.C1', i, sep = ''), tkframe(fr.C1))
	for(i in 0:3) tkgrid(get(paste('fr.C1', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.C1', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 0)

	frC10.txt <- tklabel(fr.C10, text = 'Input Reanalysis file format')
	tkgrid(frC10.txt)

	inrfeff <- tclVar(as.character(GeneralParameters$IO.file.format$Values[1]))
	en.inrfeff <- tkentry(fr.C11, textvariable = inrfeff, width = largeur1)
	infobulle(en.inrfeff, 'Enter the format of the Reanalysis files names in NetCDF,\nexample: tmax_1961011.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the Reanalysis files names in NetCDF, example: tmax_1961011.nc')
	tkgrid(en.inrfeff, row = 0, column = 0, sticky = 'w')

	tkbind(cb.period,"<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(inrfeff) <- "tmax_%s%s%s.nc"
			infobulle(en.inrfeff, 'Enter the format of the Reanalysis files names in NetCDF,\nexample: tmax_19610101.nc')
			status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the Reanalysis files names in NetCDF, example: tmax_19610101.nc')
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.down$Values[6])
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(inrfeff) <- "tmax_%s%s%s.nc"
			infobulle(en.inrfeff, 'Enter the format of the Reanalysis files names in NetCDF,\nexample: tmax_1961011.nc')
			status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the Reanalysis files names in NetCDF, example: tmax_1961011.nc')
			tclvalue(day.txtVar) <- "Dek"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.down$Values[6])
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(inrfeff) <- "tmax_%s%s.nc"
			infobulle(en.inrfeff, 'Enter the format of the Reanalysis files names in NetCDF,\nexample: tmax_196101.nc')
			status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the Reanalysis files names in NetCDF, example: tmax_196101.nc')
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'disabled')
			tkconfigure(day2.v, state = 'disabled')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.down$Values[6])
		}
	})

	###############
	frC12.txt <- tklabel(fr.C12, text = 'Downscaled data filename prefix')
	tkgrid(frC12.txt)

	downPrefix <- tclVar(as.character(GeneralParameters$IO.file.format$Values[2]))
	downPrefix.v <- tkentry.h(fr.C13, TextOutputVar, 'Prefix for the file name of the downscaled data', 'Prefix for the file name of the downscaled data')
	tkconfigure(downPrefix.v, width = largeur1, textvariable = downPrefix, justify = 'left')
	tkgrid(downPrefix.v, row = 0, column = 0, sticky = 'w')

	###############
	for(i in 0:2) assign(paste('fr.C2', i, sep = ''), tkframe(fr.C2))
	for(i in 0:2) tkgrid(get(paste('fr.C2', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.C2', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 0, ipadx = 1, ipady = 0)

	infobulle(fr.C2, 'Create the grid to interpolate the downscaled data')
	status.bar.display(fr.C2, TextOutputVar, 'Create the grid to interpolate the downscaled data')

	frC20.txt <- tklabel(fr.C20, text = 'Create grid for interpolation')
	tkgrid(frC20.txt)

	varCreateGrd <- tclVar(as.character(GeneralParameters$CreateGrd))
	grdDEM.rbt <- tkradiobutton(fr.C21, text = "From DEM", anchor = 'w', justify = 'left')
	tkgrid(grdDEM.rbt)
	grdNEW.rbt <- tkradiobutton(fr.C22, text = "New Grid", anchor = 'w', justify = 'left')

	tkconfigure(grdDEM.rbt, variable = varCreateGrd, value = "1")
	tkconfigure(grdNEW.rbt, variable = varCreateGrd, value = "2")

	bt.getNewgrid <- tkbutton.h(fr.C22, text = "Create", TextOutputVar, 'Set the new grid', 'Set the new grid')
	tkconfigure(bt.getNewgrid, command = function(){
		GeneralParameters <<- getParamNewGrid(tt, GeneralParameters)
	})
	tkgrid(grdNEW.rbt, row = 0, column = 0)
	tkgrid(bt.getNewgrid, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################
	min.nbrs.l <- tklabel.h(fr.C3, 'MinStn', TextOutputVar,
	'Minimum number of neighbours grids to be used to interpolate the downscaled data',
	'Minimum number of neighbours  grids to be used to interpolate the downscaled data')
	max.nbrs.l <- tklabel.h(fr.C3, 'MaxStn', TextOutputVar,
	'Maximum number of neighbours grids to be  used to interpolate the downscaled data',
	'Maximum number of neighbours grids to be  used to interpolate the downscaled data')
	max.dst.l <- tklabel.h(fr.C3, 'MaxDist', TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate the downscaled data',
	'Maximum distance (in  decimal degree) to be used to interpolate the downscaled data')

	min.nbrs.v <- tkentry.h(fr.C3, TextOutputVar,
	'Minimum number of neighbours grids to be used to interpolate the downscaled data',
	'Minimum number of neighbours grids to be used to interpolate the downscaled data')
	max.nbrs.v <- tkentry.h(fr.C3, TextOutputVar,
	'Maximum number of neighbours grids to be used to interpolate the downscaled data',
	'Maximum number of neighbours grids to be used to interpolate the downscaled data')
	max.dst.v <- tkentry.h(fr.C3, TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate the downscaled data',
	'Maximum distance (in  decimal degree) to be used to interpolate the downscaled data')

	tkgrid(min.nbrs.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(min.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.dst.l, anchor = 'e', justify = 'right')

	min.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[1]))
	max.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[2]))
	max.dst <- tclVar(as.character(GeneralParameters$params.int$Values[3]))

	tkconfigure(min.nbrs.v, width = 4, textvariable = min.nbrs, justify = 'right')
	tkconfigure(max.nbrs.v, width = 4, textvariable = max.nbrs, justify = 'right')
	tkconfigure(max.dst.v, width = 4, textvariable = max.dst, justify = 'right')

	##############
	bt.opt.OK <- tkbutton(frMRG1, text = "OK")
	bt.opt.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.coef) == ""){
			tkmessageBox(message = "Choose the file containing the coefficients to used for downscaling", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(file.grdrfe) == ""){
			tkmessageBox(message = "You have to provide a Reanalysis sample file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.grddem) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(dir.rfe) == "" | tclvalue(dir.rfe) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory containing the Reanalysis files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			if(tclvalue(file.period) == 'Daily data'){
				dates <- format(seq(as.Date(paste(as.numeric(as.character(GeneralParameters$dates.down$Values[1])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[2])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[3])), sep = '-')), 
							as.Date(paste(as.numeric(as.character(GeneralParameters$dates.down$Values[4])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[5])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[6])), sep = '-')), 'day'),'%Y%m%d')
				ncfiles <- sprintf(tclvalue(inrfeff), substr(dates, 1,4), substr(dates, 5,6), substr(dates, 7,8))
			}else if(tclvalue(file.period) == 'Dekadal data'){
				dates <- seq(as.Date(paste(as.numeric(as.character(GeneralParameters$dates.down$Values[1])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[2])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[3])), sep = '-')), 
							as.Date(paste(as.numeric(as.character(GeneralParameters$dates.down$Values[4])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[5])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[6])), sep = '-')), 'day')
				dates <- paste(format(dates[which(as.numeric(format(dates,'%d')) <= 3)],'%Y%m'), as.numeric(format(dates[which(as.numeric(format(dates,'%d')) <= 3)],'%d')), sep = '')
				ncfiles <- sprintf(tclvalue(inrfeff), substr(dates, 1,4), substr(dates, 5,6), substr(dates, 7,7))				
			}else{
				dates <- format(seq(as.Date(paste(as.numeric(as.character(GeneralParameters$dates.down$Values[1])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[2])), 1, sep = '-')), 
							as.Date(paste(as.numeric(as.character(GeneralParameters$dates.down$Values[4])),
							as.numeric(as.character(GeneralParameters$dates.down$Values[5])), 1, sep = '-')), 'month'),'%Y%m')
				ncfiles <- sprintf(tclvalue(inrfeff), substr(dates, 1,4), substr(dates, 5,6))
			}
			ncpath <- file.path(tclvalue(dir.rfe), ncfiles, fsep = .Platform$file.sep)
			existFl <- unlist(lapply(ncpath, file.exists))
			if(!any(existFl)){
				tkmessageBox(message = "The Reanalysis file format or the Reanalysis directory are wrong", icon = "warning", type = "ok")
				tkwait.window(tt)
			}

			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$file.io$Values <<- c(tclvalue(file.coef), tclvalue(file.grddem), tclvalue(file.grdrfe),
													tclvalue(dir.rfe), tclvalue(file.save1))
			GeneralParameters$CreateGrd <<- tclvalue(varCreateGrd)
			GeneralParameters$IO.file.format$Values <<- c(tclvalue(inrfeff), tclvalue(downPrefix))
			GeneralParameters$dates.down$Values <<- c(tclvalue(istart.yrs), tclvalue(istart.mon), tclvalue(istart.day),
														tclvalue(iend.yrs), tclvalue(iend.mon), tclvalue(iend.day))
			GeneralParameters$params.int$Values <<- c(tclvalue(min.nbrs), tclvalue(max.nbrs), tclvalue(max.dst))
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
	tkwm.title(tt, 'Reanalysis Downscaling-Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#################################################################################################3

biasGetInfoTempDown <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	##tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	fr.B <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.B)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.B, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:4) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:4) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#####################################################################
	fr.A00 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	cb.period <- ttkcombobox(fr.A00, values = cb.periodVAL, textvariable = file.period)
	infobulle(cb.period, 'Choose the frequency of data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the frequency of data')
	tkgrid(cb.period)

	#########################################3
	fr.A10 <- tkframe(fr.A1)
	fr.A11 <- tkframe(fr.A1)
	tkgrid(fr.A10, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A11, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	frA10.txt <- tklabel(fr.A10, text = 'Input data file')
	tkgrid(frA10.txt)

	cb.stnfl <- ttkcombobox(fr.A11, values = unlist(listOpenFiles), textvariable = file.stnfl)
	infobulle(cb.stnfl, 'Choose the file containing the gauge data')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')

	bt.stnfl <- tkbutton.h(fr.A11, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
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
		}else{
			return(NULL)
		}
	})

	#####################################
	fr.A20 <- tkframe(fr.A2)
	fr.A21 <- tkframe(fr.A2)
	tkgrid(fr.A20, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A21, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	##
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	#####
	frA20.txt <- tklabel(fr.A20, text = "Elevation data(NetCDF)")
	tkgrid(frA20.txt)

	###from DEM
	cb.grddem <- ttkcombobox(fr.A21, values = unlist(listOpenFiles), textvariable = file.grddem)
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	bt.grddem <- tkbutton.h(fr.A21, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.grddem, bt.grddem)
	tkgrid.configure(cb.grddem, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.grddem, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.grddem, command = function(){
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
		}else{
			return(NULL)
		}
	})

	#########################################3

	fr.A30 <- tkframe(fr.A3)
	fr.A31 <- tkframe(fr.A3)
	tkgrid(fr.A30, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A31, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA30.txt <- tklabel(fr.A30, text = 'Directory of downscaled data')
	tkgrid(frA30.txt)

	dir.down <- tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.dir.down <- tkentry(fr.A31, textvariable = dir.down, width = largeur)
	infobulle(en.dir.down, 'Enter the full path to directory containing the downscaled reanalysis files')
	status.bar.display(en.dir.down, TextOutputVar, 'Enter the full path to directory containing the downscaled reanalysis files')
	bt.dir.down <- tkbutton.h(fr.A31, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.dir.down, bt.dir.down)
	tkgrid.configure(en.dir.down, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.down, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.down, command = function(){
		dir4down <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(is.na(dir4down)) tclvalue(dir.down)<-""
		else tclvalue(dir.down) <- dir4down
	})

	#######
	fr.A40 <- tkframe(fr.A4)
	fr.A41 <- tkframe(fr.A4)
	tkgrid(fr.A40, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A41, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA40.txt <- tklabel(fr.A40, text = 'Directory to save result')
	tkgrid(frA40.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.file.save <- tkentry(fr.A41, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A41, text = "...", TextOutputVar, 'or browse here','')
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

	#################################
	pr.relief.set2 <- c('sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:3) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = pr.relief.set2[i+1], borderwidth = 2))
	for(i in 0:3) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 2)

	##############
	fr.B00 <- tkframe(fr.B0)
	fr.B01 <- tkframe(fr.B0)
	tkgrid(fr.B00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 0)
	tkgrid(fr.B01, row = 1, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 0)

	frB00.txt <- tklabel(fr.B00, text = 'Bias adjustment method')
	tkgrid(frB00.txt)

	biasMethod <- tclVar(as.character(GeneralParameters$bias.method))
	cb.biasMethod <- ttkcombobox(fr.B01, values = c('Bias-kriging', 'Regression-QM'), textvariable = biasMethod)
	infobulle(cb.biasMethod,
	'Method to be used to correct downscaled data: Mean Bias using Kriging Interpolation or\nRegression using Quantile matching')
	status.bar.display(cb.biasMethod, TextOutputVar,
	'Method to be used to correct downscaled data: Mean Bias  using Kriging Interpolation or Regression using Quantile matching')
	tkgrid(cb.biasMethod)

	##################
	for(i in 0:3) assign(paste('fr.B1', i, sep = ''), tkframe(fr.B1))
	for(i in 0:3) tkgrid(get(paste('fr.B1', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.B1', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frB10.txt <- tklabel(fr.B10, text = 'Downscaled data filename prefix')
	tkgrid(frB10.txt, pady = 1)

	downPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[1]))
	downPrefix.v <- tkentry.h(fr.B11, TextOutputVar, 'Prefix for the file name of the downscaled reanalysis data', 'Prefix for the file name of the downscaled reanalysis data')
	tkconfigure(downPrefix.v, width = largeur, textvariable = downPrefix, justify = 'left')
	tkgrid(downPrefix.v, sticky = 'w', pady = 1)

	###############
	frB12.txt <- tklabel(fr.B12, text = 'Mean bias filename prefix')
	tkgrid(frB12.txt, pady = 2)

	meanBiasPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[2]))
	meanBiasPrefix.v <- tkentry.h(fr.B13, TextOutputVar, 'Prefix for the file name of the mean bias coefficient',
	'Prefix for the file name of the mean bias coefficient')
	if(as.character(GeneralParameters$bias.method) == 'Bias-kriging') statebias <- 'normal'
	if(as.character(GeneralParameters$bias.method) == 'Regression-QM') statebias <- 'disabled'

	tkconfigure(meanBiasPrefix.v, width = largeur, textvariable = meanBiasPrefix, justify = 'left', state = statebias)
	tkgrid(meanBiasPrefix.v, sticky = 'w', pady = 1)

	############
	tkbind(cb.biasMethod,"<<ComboboxSelected>>", function(){
		if(tclvalue(biasMethod) == 'Bias-kriging') tkconfigure(meanBiasPrefix.v, state = 'normal')
		if(tclvalue(biasMethod) == 'Regression-QM') tkconfigure(meanBiasPrefix.v, state = 'disabled')
	})

	######################
	years1.l <- tklabel.h(fr.B2, 'StartYear', TextOutputVar, 'Start year to be used to correct downscaled data',
	'Start year to be used to correct downscaled data')
	years2.l <- tklabel.h(fr.B2, 'EndYear', TextOutputVar, 'End year to be used to correct downscaled data',
	'End year to be used to correct downscaled data')

	years1.v <- tkentry.h(fr.B2, TextOutputVar, 'Start year to be used to correct downscaled data', 'Start year to be used to correct downscaled data')
	years2.v <- tkentry.h(fr.B2, TextOutputVar, 'End year to be used to correct downscaled data', 'End year to be used to correct downscaled data')

	tkgrid(years1.l, row = 0, column = 0, padx = 5, pady = 1)
	tkgrid(years1.v, row = 0, column = 1, padx = 5, pady = 1)
	tkgrid(years2.l, row = 1, column = 0, padx = 5, pady = 1)
	tkgrid(years2.v, row = 1, column = 1, padx = 5, pady = 1)

	tkconfigure(years1.l, anchor = 'e', justify = 'right')
	tkconfigure(years2.l, anchor = 'e', justify = 'right')

	year1 <- tclVar(as.character(GeneralParameters$dates.coef$Values[1]))
	year2 <- tclVar(as.character(GeneralParameters$dates.coef$Values[2]))

	tkconfigure(years1.v, width = 8, textvariable = year1, justify = 'right')
	tkconfigure(years2.v, width = 8, textvariable = year2, justify = 'right')

	##################
	min.nbrs.l <- tklabel.h(fr.B3, 'MinStn', TextOutputVar,
	'Minimum number of neighbours used to interpolate the bias',
	'Minimum number of neighbours used to interpolate the bias')
	max.nbrs.l <- tklabel.h(fr.B3, 'MaxStn', TextOutputVar,
	'Maximum number of neighbours used to interpolate the bias',
	'Maximum number of neighbours used to interpolate the bias')
	max.dst.l <- tklabel.h(fr.B3, 'MaxDist', TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate the bias',
	'Maximum distance (in  decimal degree) to be used to interpolate the bias')

	min.nbrs.v <- tkentry.h(fr.B3, TextOutputVar,
	'Minimum number of neighbours to be used to interpolate the bias',
	'Minimum number of neighbours to be used to interpolate the bias')
	max.nbrs.v <- tkentry.h(fr.B3, TextOutputVar,
	'Maximum number of neighbours to be used to interpolate the bias',
	'Maximum number of neighbours to be used to interpolate the bias')
	max.dst.v <- tkentry.h(fr.B3, TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate the bias',
	'Maximum distance (in  decimal degree) to be used to interpolate the bias')

	tkgrid(min.nbrs.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(min.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.dst.l, anchor = 'e', justify = 'right')

	min.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[1]))
	max.nbrs <- tclVar(as.character(GeneralParameters$params.int$Values[2]))
	max.dst <- tclVar(as.character(GeneralParameters$params.int$Values[3]))

	tkconfigure(min.nbrs.v, width = 4, textvariable = min.nbrs, justify = 'right')
	tkconfigure(max.nbrs.v, width = 4, textvariable = max.nbrs, justify = 'right')
	tkconfigure(max.dst.v, width = 4, textvariable = max.dst, justify = 'right')

	##############################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.prm.OK, command = function(){

		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(dir.down) == "" | tclvalue(dir.down) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory containing the downscaled files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.grddem) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.grddem), tclvalue(dir.down),
													tclvalue(file.save1))
			GeneralParameters$bias.method <<- tclvalue(biasMethod)
			GeneralParameters$prefix$Values <<- c(tclvalue(downPrefix), tclvalue(meanBiasPrefix))
			GeneralParameters$dates.coef$Values <<- c(tclvalue(year1), tclvalue(year2))
			GeneralParameters$params.int$Values <<- c(tclvalue(min.nbrs), tclvalue(max.nbrs), tclvalue(max.dst))

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
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Bias coefficients computation Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#######################################################################################################################################

adjGetInfoTempDownReanal <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	## tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	fr.B <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.B)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.B, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:4) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:4) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#####################################################################

	fr.A00 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	cb.period <- ttkcombobox(fr.A00, values = cb.periodVAL, textvariable = file.period)
	infobulle(cb.period, 'Choose the frequency of data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the frequency of data')
	tkgrid(cb.period)

	#########################################3
	fr.A10 <- tkframe(fr.A1)
	fr.A11 <- tkframe(fr.A1)
	tkgrid(fr.A10, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A11, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	frA10.txt <- tklabel(fr.A10, text = 'Input data file')
	tkgrid(frA10.txt)

	cb.stnfl <- ttkcombobox(fr.A11, values = unlist(listOpenFiles), textvariable = file.stnfl)
	infobulle(cb.stnfl, 'Choose the file containing the gauge data')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')

	bt.stnfl <- tkbutton.h(fr.A11, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
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
		}else{
			return(NULL)
		}
	})

	#####################################
	fr.A20 <- tkframe(fr.A2)
	fr.A21 <- tkframe(fr.A2)
	tkgrid(fr.A20, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A21, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	##
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	#####
	frA20.txt <- tklabel(fr.A20, text = "Elevation data(NetCDF)")
	tkgrid(frA20.txt)

	###from DEM
	cb.grddem <- ttkcombobox(fr.A21, values = unlist(listOpenFiles), textvariable = file.grddem)
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	bt.grddem <- tkbutton.h(fr.A21, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.grddem, bt.grddem)
	tkgrid.configure(cb.grddem, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.grddem, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.grddem, command = function(){
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
		}else{
			return(NULL)
		}
	})

	#########################################3

	fr.A30 <- tkframe(fr.A3)
	fr.A31 <- tkframe(fr.A3)
	tkgrid(fr.A30, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A31, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA30.txt <- tklabel(fr.A30, text = 'Directory of downscaled data')
	tkgrid(frA30.txt)

	dir.down <- tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.dir.down <- tkentry(fr.A31, textvariable = dir.down, width = largeur)
	infobulle(en.dir.down, 'Enter the full path to directory containing the downscaled reanalysis files')
	status.bar.display(en.dir.down, TextOutputVar, 'Enter the full path to directory containing the downscaled reanalysis files')
	bt.dir.down <- tkbutton.h(fr.A31, text = "...", TextOutputVar, 'or browse here', '')
	tkgrid(en.dir.down, bt.dir.down)
	tkgrid.configure(en.dir.down, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.down, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.down, command = function(){
		dir4down <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(is.na(dir4down)) tclvalue(dir.down) <- ""
		else tclvalue(dir.down) <- dir4down
	})

	###############################3
	fr.A32 <- tkframe(fr.A3)
	fr.A33 <- tkframe(fr.A3)
	tkgrid(fr.A32, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A33, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	MeanBiasLab <- tclVar('Directory of mean bias files')
	frA32.txt <- tklabel(fr.A32, text = tclvalue(MeanBiasLab), textvariable = MeanBiasLab)
	tkgrid(frA32.txt)

	dir.bias <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.dir.bias <- tkentry(fr.A33, textvariable = dir.bias, width = largeur)
	infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias, TextOutputVar, 'Enter the full path to directory containing the mean bias files')
	bt.dir.bias <- tkbutton.h(fr.A33, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.dir.bias, bt.dir.bias)
	tkgrid.configure(en.dir.bias, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.bias, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.bias, command = function(){
		dir4bias <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(is.na(dir4bias)) tclvalue(dir.bias)<-""
		else tclvalue(dir.bias) <- dir4bias
	})

	#######
	fr.A40 <- tkframe(fr.A4)
	fr.A41 <- tkframe(fr.A4)
	tkgrid(fr.A40, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A41, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA40.txt <- tklabel(fr.A40, text = 'Directory to save result')
	tkgrid(frA40.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[5]))
	en.file.save <- tkentry(fr.A41, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A41, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[5]), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[5])
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	#################################
	pr.relief.set2 <- c('sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:2) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = pr.relief.set2[i+1], borderwidth = 2))
	for(i in 0:2) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 5)

	##############
	fr.B00 <- tkframe(fr.B0)
	fr.B01 <- tkframe(fr.B0)
	tkgrid(fr.B00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.B01, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frB00.txt <- tklabel(fr.B00, text = 'Bias adjustment method')
	tkgrid(frB00.txt)

	biasMethod <- tclVar(as.character(GeneralParameters$bias.method))
	cb.biasMethod <- ttkcombobox(fr.B01, values = c('Bias-kriging', 'Regression-QM'), textvariable = biasMethod)
	infobulle(cb.biasMethod,
	'Method to be used to correct downscaled data: Mean Bias using Kriging Interpolation or\nRegression using Quantile matching')
	status.bar.display(cb.biasMethod, TextOutputVar,
	'Method to be used to correct downscaled data: Mean Bias  using Kriging Interpolation or Regression using Quantile matching')
	tkgrid(cb.biasMethod)

	##################
	for(i in 0:5) assign(paste('fr.B1', i, sep = ''), tkframe(fr.B1))
	for(i in 0:5) tkgrid(get(paste('fr.B1', i, sep = '')))
	for(i in 0:5) tkgrid.configure(get(paste('fr.B1', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frB10.txt <- tklabel(fr.B10, text = 'Downscaled data filename prefix')
	tkgrid(frB10.txt, pady = 1)

	downPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[1]))
	downPrefix.v <- tkentry.h(fr.B11, TextOutputVar, 'Prefix for the file name of the downscaled reanalysis data', 'Prefix for the file name of the downscaled reanalysis data')
	tkconfigure(downPrefix.v, width = largeur, textvariable = downPrefix, justify = 'left')
	tkgrid(downPrefix.v, sticky = 'w', pady = 1)

	###############
	frB12.txt <- tklabel(fr.B12, text = 'Mean bias filename prefix')
	tkgrid(frB12.txt, pady = 1)

	meanBiasPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[2]))
	meanBiasPrefix.v <- tkentry.h(fr.B13, TextOutputVar, 'Prefix for the file name of the mean bias coefficient',
	'Prefix for the file name of the mean bias coefficient')
	if(as.character(GeneralParameters$bias.method) == 'Bias-kriging') statebias <- 'normal'
	if(as.character(GeneralParameters$bias.method) == 'Regression-QM') statebias <- 'disabled'

	tkconfigure(meanBiasPrefix.v, width = largeur, textvariable = meanBiasPrefix, justify = 'left', state = statebias)
	tkgrid(meanBiasPrefix.v, sticky = 'w', pady = 2)
	###############
	frB14.txt <- tklabel(fr.B14, text = 'Adjusted data filename prefix')
	tkgrid(frB14.txt, pady = 1)

	adjPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[3]))
	adjPrefix.v <- tkentry.h(fr.B15, TextOutputVar, 'Prefix for the file name of the adjusted reanalysis data', 'Prefix for the file name of the adjusted reanalysis data')
	tkconfigure(adjPrefix.v, width = largeur, textvariable = adjPrefix, justify = 'left')
	tkgrid(adjPrefix.v, sticky = 'w', pady = 1)

	############
	tkbind(cb.biasMethod,"<<ComboboxSelected>>", function(){
		if(tclvalue(biasMethod) == 'Bias-kriging'){
			tkconfigure(meanBiasPrefix.v, state = 'normal')
			tclvalue(MeanBiasLab) <- 'Directory of mean bias files'
			infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
			status.bar.display(en.dir.bias, TextOutputVar,
			'Enter the full path to directory containing the mean bias files')
			tkconfigure(bt.dir.bias, command = function(){
				dir4bias <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
				if(is.na(dir4bias)) tclvalue(dir.bias) <- ""
				else tclvalue(dir.bias) <- dir4bias
			})
		}
		if(tclvalue(biasMethod) == 'Regression-QM'){
			tkconfigure(meanBiasPrefix.v, state = 'disabled')
			tclvalue(MeanBiasLab) <- "Regression coefficients file"
			infobulle(en.dir.bias, 'Enter the full path to file containing the regression coefficients')
			status.bar.display(en.dir.bias, TextOutputVar,
			'Enter the full path to file containing the regression coefficients')
			tkconfigure(bt.dir.bias, command = function(){
				dir4bias <- tkgetOpenFile(initialdir = getwd(), initialfile = "",
										filetypes = "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}")
				if(is.na(dir4bias)) tclvalue(dir.bias) <- ""
				else tclvalue(dir.bias) <- dir4bias
			})
		}
	})

	#######################
	infobulle(fr.B2, 'Start and end date for adjusting downscaled data')
	status.bar.display(fr.B2, TextOutputVar, 'Start and end date for adjusting downscaled data')

	deb.txt <- tklabel(fr.B2, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(fr.B2, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(fr.B2, text = 'Year')
	mon.txt <- tklabel(fr.B2, text = 'Month')
	if(as.character(GeneralParameters$period) == 'dekadal') day.txtVar <- tclVar('Dek')
	else day.txtVar <- tclVar('Day')
	day.txt <- tklabel(fr.B2, text = tclvalue(day.txtVar), textvariable = day.txtVar)

	istart.yrs <- tclVar(as.character(GeneralParameters$dates.adj$Values[1]))
	istart.mon <- tclVar(as.character(GeneralParameters$dates.adj$Values[2]))
	istart.day <- tclVar(as.character(GeneralParameters$dates.adj$Values[3]))
	iend.yrs <- tclVar(as.character(GeneralParameters$dates.adj$Values[4]))
	iend.mon <- tclVar(as.character(GeneralParameters$dates.adj$Values[5]))
	iend.day <- tclVar(as.character(GeneralParameters$dates.adj$Values[6]))

	yrs1.v <- tkentry(fr.B2, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(fr.B2, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(fr.B2, width = 4, textvariable = istart.day, justify = "right", state = 'normal')
	yrs2.v <- tkentry(fr.B2, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(fr.B2, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(fr.B2, width = 4, textvariable = iend.day, justify = "right", state = 'normal')

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkbind(cb.period,"<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.adj$Values[6])
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(day.txtVar) <- "Dek"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.adj$Values[6])
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'disabled')
			tkconfigure(day2.v, state = 'disabled')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.adj$Values[6])
		}
	})

	##############################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.prm.OK, command = function(){

		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(dir.down) == "" | tclvalue(dir.down) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory containing the downscaled files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.grddem) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(dir.bias) == "" | tclvalue(dir.bias) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory or file containing bias coef ", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.grddem), tclvalue(dir.down),
													tclvalue(dir.bias), tclvalue(file.save1))
			GeneralParameters$bias.method <<- tclvalue(biasMethod)
			GeneralParameters$prefix$Values <<- c(tclvalue(downPrefix), tclvalue(meanBiasPrefix), tclvalue(adjPrefix))
			GeneralParameters$dates.adj$Values <<- c(tclvalue(istart.yrs), tclvalue(istart.mon),
			tclvalue(istart.day), tclvalue(iend.yrs), tclvalue(iend.mon), tclvalue(iend.day))

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
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Bias Adjustment Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#######################################################################################################################################

mrgGetInfoTemp <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	##tkentry width, directory path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	fr.A <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	fr.B <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.B)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.B, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:3) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:3) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#####################################################################

	fr.A00 <- tkframe(fr.A0)
	tkgrid(fr.A00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	cb.period <- ttkcombobox(fr.A00, values = cb.periodVAL, textvariable = file.period)
	infobulle(cb.period, 'Choose the frequency of data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the frequency of data')
	tkgrid(cb.period)

	#########################################3
	fr.A10 <- tkframe(fr.A1)
	fr.A11 <- tkframe(fr.A1)
	fr.A12 <- tkframe(fr.A1)
	fr.A13 <- tkframe(fr.A1)
	tkgrid(fr.A10, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A11, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A12, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A13, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######
	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	frA10.txt <- tklabel(fr.A10, text = 'Input data file')
	tkgrid(frA10.txt)

	cb.stnfl <- ttkcombobox(fr.A11, values = unlist(listOpenFiles), textvariable = file.stnfl)
	infobulle(cb.stnfl, 'Choose the file containing the gauge data')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')

	bt.stnfl <- tkbutton.h(fr.A11, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
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

	##############
	frA12.txt <- tklabel(fr.A12, text = 'Directory of ajdusted data')
	tkgrid(frA12.txt)

	dir.down <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.dir.down <- tkentry(fr.A13, textvariable = dir.down, width = largeur)
	infobulle(en.dir.down, 'Enter the full path to directory containing the downscaled or adjusted reanalysis files')
	status.bar.display(en.dir.down, TextOutputVar, 'Enter the full path to directory containing the downscaled or adjusted reanalysis files')
	bt.dir.down <- tkbutton.h(fr.A13, text = "...", TextOutputVar, 'or browse here', '')
	tkgrid(en.dir.down, bt.dir.down)
	tkgrid.configure(en.dir.down, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.down, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.down, command = function(){
		dir4down <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(is.na(dir4down)) tclvalue(dir.down) <- ""
		else tclvalue(dir.down) <- dir4down
	})

	#####################################
	fr.A20 <- tkframe(fr.A2)
	fr.A21 <- tkframe(fr.A2)
	fr.A22 <- tkframe(fr.A2)
	fr.A23 <- tkframe(fr.A2)

	tkgrid(fr.A20, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A21, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A22, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A23, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	####Use DEM
	file.grddem <- tclVar()
	tclvalue(file.grddem) <- as.character(GeneralParameters$file.io$Values[2])

	#####
	frA20.txt <- tklabel(fr.A20, text = "Elevation data(NetCDF)")
	tkgrid(frA20.txt)

	if(as.character(GeneralParameters$blankGrd) == '2') statedem <- 'normal'
	else statedem <- 'disabled'

	###
	cb.grddem <- ttkcombobox(fr.A21, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem)
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	bt.grddem <- tkbutton.h(fr.A21, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
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
	tclvalue(file.blkshp) <- as.character(GeneralParameters$file.io$Values[3])

	#####Use shp
	frA22.txt <- tklabel(fr.A22, text = "ESRI shapefiles for blanking")
	tkgrid(frA22.txt)

	if(as.character(GeneralParameters$blankGrd) == '3') stateshp <- 'normal'
	else stateshp <- 'disabled'

	##
	cb.blkshp <- ttkcombobox(fr.A23, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp)
	infobulle(cb.blkshp, 'Choose the file in the list')
	status.bar.display(cb.blkshp, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	bt.blkshp <- tkbutton.h(fr.A23, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
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

	#########################################
	fr.A30 <- tkframe(fr.A3)
	fr.A31 <- tkframe(fr.A3)
	tkgrid(fr.A30, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.A31, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frA30.txt <- tklabel(fr.A30, text = 'Directory to save result')
	tkgrid(frA30.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[5]))
	en.file.save <- tkentry(fr.A31, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A31, text = "...", TextOutputVar, 'or browse here', '')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[5]), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[5])
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	#################################
	pr.relief.set2 <- c('sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:3) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = pr.relief.set2[i+1], borderwidth = 2))
	for(i in 0:3) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 5)

	##############
	fr.B00 <- tkframe(fr.B0)
	fr.B01 <- tkframe(fr.B0)
	tkgrid(fr.B00, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
	tkgrid(fr.B01, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frB00.txt <- tklabel(fr.B00, text = 'Blank grid')
	tkgrid(frB00.txt)

	blankGrd <- tclVar(as.character(GeneralParameters$blankGrd))
	tclvalue(blankGrd) <- ifelse(as.character(GeneralParameters$blankGrd) == '1',
	'None', ifelse(as.character(GeneralParameters$blankGrd) == '2', 'Use DEM', 'Use ESRI shapefile'))

	cb.blankGrd <- ttkcombobox(fr.B01, values = c("None", "Use DEM", "Use ESRI shapefile"), textvariable = blankGrd)
	infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cb.blankGrd, TextOutputVar,
	'Blank grid outside the country boundaries  or over ocean given by the DEM mask or the shapefile')
	tkgrid(cb.blankGrd)

	##################
	for(i in 0:3) assign(paste('fr.B1', i, sep = ''), tkframe(fr.B1))
	for(i in 0:3) tkgrid(get(paste('fr.B1', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.B1', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)

	frB10.txt <- tklabel(fr.B10, text = 'Adjusted data filename prefix')
	tkgrid(frB10.txt, pady = 1)

	adjPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[1]))
	adjPrefix.v <- tkentry.h(fr.B11, TextOutputVar, 'Prefix for the file name of the downscaled or adjusted reanalysis data', 'Prefix for the file name of the downscaled or adjusted reanalysis data')
	tkconfigure(adjPrefix.v, width = largeur, textvariable = adjPrefix, justify = 'left')
	tkgrid(adjPrefix.v, sticky = 'w', pady = 1)

	###############
	frB12.txt <- tklabel(fr.B12, text = 'Merged data filename prefix')
	tkgrid(frB12.txt, pady = 1)

	mrgPrefix <- tclVar(as.character(GeneralParameters$prefix$Values[2]))
	en.mrgPrefix <- tkentry(fr.B13, textvariable = mrgPrefix, width = largeur-6)
	mrgSuffix <- tclVar(as.character(GeneralParameters$prefix$Values[3]))
	cb.mrgSuffix <- ttkcombobox(fr.B13, values = c("ALL", "CLM"), textvariable = mrgSuffix, width = 4)

	tkgrid(en.mrgPrefix, row = 0, column = 0, sticky = 'w')
	tkgrid(cb.mrgSuffix, row = 0, column = 1, sticky = 'w')

	infobulle(fr.B13, 'Prefix for the file name of the merged data')
	status.bar.display(fr.B13, TextOutputVar, 'Prefix for the file name of the merged data')

	############
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

	#######################
	infobulle(fr.B2, 'Start and end date for merging data')
	status.bar.display(fr.B2, TextOutputVar, 'Start and end date for merging data')

	deb.txt <- tklabel(fr.B2, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(fr.B2, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(fr.B2, text = 'Year')
	mon.txt <- tklabel(fr.B2, text = 'Month')
	if(as.character(GeneralParameters$period) == 'dekadal') day.txtVar <- tclVar('Dek')
	else day.txtVar <- tclVar('Day')
	day.txt <- tklabel(fr.B2, text = tclvalue(day.txtVar), textvariable = day.txtVar)

	istart.yrs <- tclVar(as.character(GeneralParameters$dates.mrg$Values[1]))
	istart.mon <- tclVar(as.character(GeneralParameters$dates.mrg$Values[2]))
	istart.day <- tclVar(as.character(GeneralParameters$dates.mrg$Values[3]))
	iend.yrs <- tclVar(as.character(GeneralParameters$dates.mrg$Values[4]))
	iend.mon <- tclVar(as.character(GeneralParameters$dates.mrg$Values[5]))
	iend.day <- tclVar(as.character(GeneralParameters$dates.mrg$Values[6]))

	yrs1.v <- tkentry(fr.B2, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(fr.B2, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(fr.B2, width = 4, textvariable = istart.day, justify = "right", state = 'normal')
	yrs2.v <- tkentry(fr.B2, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(fr.B2, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(fr.B2, width = 4, textvariable = iend.day, justify = "right", state = 'normal')

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkbind(cb.period,"<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.mrg$Values[6])
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(day.txtVar) <- "Dek"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.mrg$Values[6])
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'disabled')
			tkconfigure(day2.v, state = 'disabled')
			#tclvalue(day.val) <- as.character(GeneralParameters$dates.mrg$Values[6])
		}
	})

	############
	bt.opt.set <- tkbutton.h(fr.B3, text = "Options - Settings", TextOutputVar, 'Set general options for merging', 'Set general options for merging')
	tkgrid(bt.opt.set, sticky = 'we', padx = 25, pady = 5, ipadx = 1, ipady = 1)
	tkconfigure(bt.opt.set, command = function(){
		GeneralParameters <<- getParamMeringTemp(tt, GeneralParameters)
	})

	##############################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.prm.OK, command = function(){

		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(tclvalue(blankGrd) == "Use DEM" & tclvalue(file.grddem) == "" ){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(blankGrd) == "Use ESRI shapefile" & tclvalue(file.blkshp) == ""){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(dir.down) == "" | tclvalue(dir.down) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory containing the downscaled or adjusted files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.grddem), tclvalue(file.blkshp),
													tclvalue(dir.down), tclvalue(file.save1))
			GeneralParameters$blankGrd <<- ifelse(tclvalue(blankGrd) == 'None', '1', ifelse(tclvalue(blankGrd) == 'Use DEM', '2', '3'))
			GeneralParameters$prefix$Values <<- c(tclvalue(adjPrefix), tclvalue(mrgPrefix), tclvalue(mrgSuffix))
			GeneralParameters$dates.mrg$Values <<- c(tclvalue(istart.yrs), tclvalue(istart.mon), tclvalue(istart.day),
														tclvalue(iend.yrs), tclvalue(iend.mon), tclvalue(iend.day))

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
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Merging Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#########################################################################################

getParamMeringTemp <- function(tt, GeneralParameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0a <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1a <- tkframe(tt1)

	frmrg0 <- tkframe(frMRG0a)
	tkgrid(frmrg0)

	fr.C <- tkframe(frmrg0, relief = "sunken", borderwidth = 2)
	tkgrid(fr.C, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

	nmin.l <- tklabel.h(fr.C, 'Nmin', TextOutputVar, 'Minimum number of gauges with data to be used to do the merging',
	'Minimum number of gauges with data to be used to do the merging')
	min.nbrs.l <- tklabel.h(fr.C, 'MinStn', TextOutputVar, 'Minimum number of neighbours to be used to interpolate data',
	'Minimum number of neighbours to be used to interpolate  data')
	max.nbrs.l <- tklabel.h(fr.C, 'MaxStn', TextOutputVar, 'Maximum number of neighbours to be used to interpolate  data',
	'Maximum number of neighbours to be used to interpolate  data')
	max.dst.l <- tklabel.h(fr.C, 'MaxDist', TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate data',
	'Maximum distance (in  decimal degree) to be used to interpolate data')

	nmin.v <- tkentry.h(fr.C, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging',
	'Minimum number of gauges with data to be used to do the merging')
	min.nbrs.v <- tkentry.h(fr.C, TextOutputVar, 'Minimum number of neighbours to be used to interpolate data',
	'Minimum number of neighbours to be used to interpolate data')
	max.nbrs.v <- tkentry.h(fr.C, TextOutputVar, 'Maximum number of neighbours to be used to interpolate data',
	'Maximum number of neighbours to beused to interpolate data')
	max.dst.v <- tkentry.h(fr.C, TextOutputVar,
	'Maximum distance (in  decimal degree) to be used to interpolate data',
	'Maximum distance (in  decimal degree) to be used to interpolate data')

	tkgrid(nmin.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(nmin.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(nmin.l, anchor = 'e', justify = 'right')
	tkconfigure(min.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.nbrs.l, anchor = 'e', justify = 'right')
	tkconfigure(max.dst.l, anchor = 'e', justify = 'right')

	nmin <- tclVar(as.character(GeneralParameters$params.mrg$Values[1]))
	min.nbrs <- tclVar(as.character(GeneralParameters$params.mrg$Values[2]))
	max.nbrs <- tclVar(as.character(GeneralParameters$params.mrg$Values[3]))
	max.dst <- tclVar(as.character(GeneralParameters$params.mrg$Values[4]))

	tkconfigure(nmin.v, width = 4, textvariable = nmin, justify = 'right')
	tkconfigure(min.nbrs.v, width = 4, textvariable = min.nbrs, justify = 'right')
	tkconfigure(max.nbrs.v, width = 4, textvariable = max.nbrs, justify = 'right')
	tkconfigure(max.dst.v, width = 4, textvariable = max.dst, justify = 'right')

	####################################
	fr.D <- tkframe(frmrg0, relief = "sunken", borderwidth = 2)
	tkgrid(fr.D, row = 1, column = 0, rowspan = 1, sticky = 'nsew', padx = 1, pady = 1)

	interpMethod <- tclVar(as.character(GeneralParameters$params.mrg$Values[5]))
	txt.interpMethod <- tklabel(fr.D, text = 'Interpolation method')
	cb.interpMethod <- ttkcombobox(fr.D, values = c('IDW', 'Kriging'), textvariable = interpMethod)
	infobulle(cb.interpMethod, 'Interpolation techniques: Kriging or  Inverse Distance Weighted')
	status.bar.display(cb.interpMethod, TextOutputVar, 'Interpolation techniques: Kriging or  Inverse Distance Weighted')

	tkgrid(txt.interpMethod, row = 0, column = 0, columnspan = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(cb.interpMethod, row = 1, column = 0, columnspan = 1, sticky = 'ew', padx = 1, pady = 1)

	################################
	bt.prm.OK <- tkbutton(frMRG1a, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1a, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$params.mrg$Values <<- c(tclvalue(nmin), tclvalue(min.nbrs), tclvalue(max.nbrs),
													tclvalue(max.dst), tclvalue(interpMethod))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(frMRG0a, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1a, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

