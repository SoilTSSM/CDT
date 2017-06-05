
climatoAnalysisEditYrsMon <- function(parent.win, vedit, titre, help, year){
	if(Sys.info()["sysname"] == "Windows"){
		largeur1 <- 30
		largeur2 <- 32
	}else{
		largeur1 <- 40
		largeur2 <- 42
	}

	#########
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	####
	frameEdit <- tkframe(frDialog)
	frameInfo <- tkframe(frDialog)

	#########
	yscr.Edit <- tkscrollbar(frameEdit, repeatinterval = 4,
							command = function(...) tkyview(text.Edit, ...))
	text.Edit <- tktext(frameEdit, bg = "white", wrap = "word",
						height = 4, width = largeur1,
						yscrollcommand = function(...) tkset(yscr.Edit, ...))

	tkgrid(text.Edit, yscr.Edit)
	tkgrid.configure(yscr.Edit, sticky = "ns")
	tkgrid.configure(text.Edit, sticky = 'nswe') 

	tkinsert(text.Edit, "end", vedit)

	#########

	txta.Info <- tktext(frameInfo, cursor = "", wrap = "word", height = 4, width = largeur2)
	tkgrid(txta.Info)

	tkinsert(txta.Info, "1.0", help)
	tkconfigure(txta.Info, state = "disabled")

	#########
	tkgrid(frameEdit, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameInfo, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#########
	vedit <- str_trim(strsplit(vedit, ",")[[1]])
	vedit <- as.numeric(vedit)

	if(year){
		xtm <- "years"
		xlo <- 1800
		xup <- 2100
		if(length(vedit) == 0) vedit <- NA
	}else{
		xtm <- "months"
		xlo <- 1
		xup <- 12
	}

	#########
	bt.opt.OK <- tkbutton(frButt, text = "OK") 
	bt.opt.CA <- tkbutton(frButt, text = "Cancel") 

	tkconfigure(bt.opt.OK, command = function(){
		tmp <- tclvalue(tkget(text.Edit, "0.0", "end"))
		tmp <- gsub("[\r\n]", "", tmp)
		tmp <- str_trim(strsplit(tmp, ",")[[1]])
		tmp <- as.numeric(tmp)
		if(length(tmp) == 0){
			tkmessageBox(message = paste("No", xtm, "edited"), icon = "warning", type = "ok")
		}else if(length(tmp) > 0 & any(is.na(tmp))){
			tkmessageBox(message = paste("Check the", xtm, "that you edited\n", paste0(tmp, collapse = ', ')), icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(any(tmp > xup | tmp < xlo)){
			msg <- if(year) "Ambiguous years. Year must be 4 digits" else "Month number must be between 1 and 12"
			tkmessageBox(message = paste(msg, "\n", paste0(tmp, collapse = ', ')), icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			vedit <<- tmp
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

	tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 10, sticky = 'w')
	tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

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
	tkwm.title(tt, titre)
	tkwm.deiconify(tt)

	##################################################################	
	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(vedit)
}

#######################################################################################################

climatoAnalysisNetcdfData <- function(tt, GeneralParameters, ncDIR, speriod){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frDate <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$ncdf.file$start.year)
	istart.mon <- tclVar(GeneralParameters$ncdf.file$start.mon)
	istart.day <- tclVar(GeneralParameters$ncdf.file$start.day)
	iend.yrs <- tclVar(GeneralParameters$ncdf.file$end.year)
	iend.mon <- tclVar(GeneralParameters$ncdf.file$end.mon)
	iend.day <- tclVar(GeneralParameters$ncdf.file$end.day)

	if(speriod == 'Dekadal data'){
		state <- 'normal'
		istart.day <- if(as.numeric(str_trim(GeneralParameters$ncdf.file$start.day)) > 3) tclVar(3) else istart.day
		iend.day <- if(as.numeric(str_trim(GeneralParameters$ncdf.file$end.day)) > 3) tclVar(3) else iend.day
	}else if(speriod == 'Monthly data'){
		state <- 'disabled'
	}else state <- 'normal'

	deb.txt <- tklabel(frDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frDate, text = 'Year')
	mon.txt <- tklabel(frDate, text = 'Month')
	day.txt <- tklabel(frDate, text = 'Day')

	yrs1.v <- tkentry(frDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frDate, width = 4, textvariable = istart.day, justify = "right", state = state)
	yrs2.v <- tkentry(frDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frDate, width = 4, textvariable = iend.day, justify = "right", state = state)

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

	infobulle(frDate, 'Start and end date of netcdf data')
	status.bar.display(frDate, TextOutputVar, 'Start and end date of netcdf data')

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(GeneralParameters$ncdf.file$format)
	rfesample <- tclVar(GeneralParameters$ncdf.file$sample)

	txt.ncsample <- tklabel(frFF, text = "Netcdf data sample file", anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = 'Netcdf data filenames format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

	###################

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(rfesample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = rfesample)
		}else return(NULL)
	})

	###################

	tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	status.bar.display(cb.ncsample, TextOutputVar, 'File containing a sample of the data in netcdf')
	infobulle(bt.ncsample, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the filename format of netcdf data,\nexample: rr_mrg_19830125_CLM.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the filename format of netcdf data,\nexample: rr_mrg_19830125_CLM.nc')

	###################

	tkgrid(frDate, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(frFF, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$ncdf.file$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
		GeneralParameters$ncdf.file$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
		GeneralParameters$ncdf.file$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
		GeneralParameters$ncdf.file$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
		GeneralParameters$ncdf.file$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
		GeneralParameters$ncdf.file$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))
		GeneralParameters$ncdf.file$format <<- str_trim(tclvalue(inrfeff))
		GeneralParameters$ncdf.file$sample <<- str_trim(tclvalue(rfesample))

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
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

