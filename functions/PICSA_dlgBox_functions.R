
PICSANetcdfData <- function(tt, GeneralParameters, ncDIR, vars){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	if(vars == "rain"){
		inrfeff <- tclVar(GeneralParameters$RAIN$format)
		rfesample <- tclVar(GeneralParameters$RAIN$sample)
	}
	if(vars == "tmax"){
		inrfeff <- tclVar(GeneralParameters$TMAX$format)
		rfesample <- tclVar(GeneralParameters$TMAX$sample)
	}
	if(vars == "tmin"){
		inrfeff <- tclVar(GeneralParameters$TMIN$format)
		rfesample <- tclVar(GeneralParameters$TMIN$sample)
	}
	if(vars == "etp"){
		inrfeff <- tclVar(GeneralParameters$ETP$format)
		rfesample <- tclVar(GeneralParameters$ETP$sample)
	}

	if(vars == "raindekmon"){
		frdekmon <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

		tsepdekmon <- tclVar()
		CbperiodVAL <- c('Dekadal data', 'Monthly data')
		tclvalue(tsepdekmon) <- switch(GeneralParameters$dekmon$time.step,
										'dekadal' = CbperiodVAL[1],
										'monthly' = CbperiodVAL[2])
		inrfeff <- tclVar(GeneralParameters$dekmon$format)
		rfesample <- tclVar(GeneralParameters$dekmon$sample)

		cb.tsepdekmon <- ttkcombobox(frdekmon, values = CbperiodVAL, textvariable = tsepdekmon, width = largeur1)
		tkgrid(cb.tsepdekmon, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	}

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
	if(vars == "raindekmon") tkgrid(frdekmon, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(frFF, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(vars == "rain"){
			GeneralParameters$RAIN$format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$RAIN$sample <<- str_trim(tclvalue(rfesample))
		}
		if(vars == "tmax"){
			GeneralParameters$TMAX$format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$TMAX$sample <<- str_trim(tclvalue(rfesample))
		}
		if(vars == "tmin"){
			GeneralParameters$TMIN$format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$TMIN$sample <<- str_trim(tclvalue(rfesample))
		}
		if(vars == "etp"){
			GeneralParameters$ETP$format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$ETP$sample <<- str_trim(tclvalue(rfesample))
		}

		if(vars == "raindekmon"){
			GeneralParameters$dekmon$time.step <<- switch(tclvalue(tsepdekmon), 
					 									'Dekadal data' = 'dekadal',
														'Monthly data' =  'monthly')
			GeneralParameters$dekmon$format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$dekmon$sample <<- str_trim(tclvalue(rfesample))
		}

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
