fillMissDekTemp <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 25
	}else{
		largeur <- 25
		largeur1 <- 24
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################
	frSTN <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$STN.file)

	txt.stnfl <- tklabel(frSTN, text = 'Temperature data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frSTN, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frSTN, text = "...")

	######
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
		}else return(NULL)
	})

	######
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'File containing the dekadal temperature data in CDT format')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	############################################

	frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(GeneralParameters$IO.files$Temp.dir)
	inrfeff <- tclVar(GeneralParameters$IO.files$Temp.File.Format)

	txt.dir.rfe <- tklabel(frRFE, text = 'Directory of temperature files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")
	txt.inrfeff <- tklabel(frRFE, text = 'Temperature file format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frRFE, textvariable = inrfeff, width = largeur)

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(GeneralParameters$IO.files$Temp.dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else ""
	})

	######
	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.rfe, 'Enter the full path to\ndirectory containing the dekadal temperature files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the dekadal temperature files')
	infobulle(bt.dir.rfe, 'or browse here')
	status.bar.display(bt.dir.rfe, TextOutputVar, 'or browse here')
	infobulle(en.inrfeff, 'Enter the format of the temperature files names in NetCDF,\nexample: tmax_adj_1961011.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the temperature files names in NetCDF,\nexample: tmax_adj_1961011.nc')

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$f2save)

	txt.file.save <- tklabel(frSave, text = 'File to save filled data', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	######
	tkconfigure(bt.file.save, command = function(){
		fileORdir2Save(file.save1, isFile = TRUE)
	})

	######
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to the file to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to the file to save extracted data')
	infobulle(bt.file.save, 'Browse here the full path to the  file to save result')
	status.bar.display(bt.file.save, TextOutputVar, 'Browse here the full path to file to save extracted data')

	############################################
	tkgrid(frSTN, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRFE, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$Fill.Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Fill.Date.Range$start.mon)
	istart.dek <- tclVar(GeneralParameters$Fill.Date.Range$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Fill.Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Fill.Date.Range$end.mon)
	iend.dek <- tclVar(GeneralParameters$Fill.Date.Range$end.dek)

	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')

	deb.txt <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frtxtDate, text = 'Year')
	mon.txt <- tklabel(frtxtDate, text = 'Month')
	dek.txt <- tklabel(frtxtDate, text = 'Dekad')
	yrs1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	dek1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.dek, justify = "right")
	yrs2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	dek2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.dek, justify = "right")

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dek.txt, row = 0, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dek1.v, row = 1, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dek2.v, row = 2, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(frtxtDate, 'Start and end date for filling missing dekadal temperature values')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date for filling missing dekadal temperature values')

	############################################

	frParams <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	min.len <- tclVar(GeneralParameters$Fill.Params$min.length)

	txt0.min.len <- tklabel(frParams, text = 'Minimum length non missing data', anchor = 'w', justify = 'left')
	txt.min.len <- tklabel(frParams, text = 'Minimum length', anchor = 'e', justify = 'right')
	en.min.len <- tkentry(frParams, textvariable = min.len, width = 8)

	tkgrid(txt0.min.len, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.len, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.len, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.min.len, 'Minimum length (between 10 and 20) of non missing data\nto be used to perform the regression')
	status.bar.display(en.min.len, TextOutputVar, 'Minimum length  (between 10 and 20) of non missing data\nto be used to perform the regression')

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frParams, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 5)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Choose the file containing the dekadal temperature data", icon = "warning", type = "ok")
		}else if(str_trim(tclvalue(dir.rfe)) == "" | str_trim(tclvalue(dir.rfe)) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory containing the netcdf temeprature files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Choose or enter the path to the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(!(as.numeric(str_trim(tclvalue(istart.dek)))%in%1:3) | !(as.numeric(str_trim(tclvalue(iend.dek)))%in%1:3)){
			tkmessageBox(message = "Dekad must be 1, 2 or 3", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$IO.files$Temp.dir <<- str_trim(tclvalue(dir.rfe))
			GeneralParameters$IO.files$Temp.File.Format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$IO.files$f2save <<- str_trim(tclvalue(file.save1))

			GeneralParameters$Fill.Date.Range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Fill.Date.Range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Fill.Date.Range$start.dek <<- as.numeric(str_trim(tclvalue(istart.dek)))
			GeneralParameters$Fill.Date.Range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Fill.Date.Range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Fill.Date.Range$end.dek <<- as.numeric(str_trim(tclvalue(iend.dek)))

			GeneralParameters$Fill.Params$min.length <<- as.numeric(str_trim(tclvalue(min.len)))

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

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

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
	tkwm.title(tt, 'Gap Filling - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

