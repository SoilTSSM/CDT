
write_json <- function(Robj, file, ...){
	require(jsonlite)
	json <- toJSON(Robj, pretty = TRUE, ...)
	sink(file)
	cat(json, '\n')
	sink()
}

######################################################

configCDT <- function(){
	config.win <- tktoplevel()
	width.conf <- as.integer(tkwinfo("screenwidth", config.win))
	height.conf <- as.integer(tkwinfo("screenheight", config.win))

	tkgrab.set(config.win)
	tkfocus(config.win)

	width.entry <- if(Sys.info()["sysname"] == "Windows") 61 else 43	

	fr1.conf <- tkframe(config.win, relief = 'sunken', borderwidth = 2)
	fr2.conf <- tkframe(config.win, relief = 'sunken', borderwidth = 2)
	fr3.conf <- tkframe(config.win)

	########

	confpath <- file.path(apps.dir, 'configure', 'configure_user.json')
	tclpathF <- file.path(apps.dir, 'configure', 'configure_default.json')

	if(file.exists(confpath)){
		conffile <- fromJSON(confpath)
		dirpath <- tclVar(str_trim(conffile$working.directory))
		misscode <- tclVar(str_trim(conffile$missing.value))
		tktablepath <- tclVar(str_trim(conffile$Tktable.path))
		bwidgetpath <- tclVar(str_trim(conffile$Bwidget.path))
	}else{
		tclPath <- fromJSON(tclpathF)
		dirpath <- tclVar(getwd())
		misscode <- tclVar(str_trim(tclPath$missing.value))
		if(Sys.info()["sysname"] == "Windows") {
			if(.Machine$sizeof.pointer == 8){
					tktablepath <- tclVar(str_trim(tclPath$Windows$Tktable_Win64_bit))
					bwidgetpath <- tclVar(str_trim(tclPath$Windows$Bwidget_Win64_bit))
			}else{
				tktablepath <- tclVar(str_trim(tclPath$Windows$Tktable_Win32_bit))
				bwidgetpath <- tclVar(str_trim(tclPath$Windows$Bwidget_Win32_bit))
			}
		}else if(Sys.info()["sysname"] == "Darwin"){
			tktablepath <- tclVar(str_trim(tclPath$MacOS$Tktable))
			bwidgetpath <- tclVar(str_trim(tclPath$MacOS$Bwidget))
		}else if(Sys.info()["sysname"] == "Linux"){
			tktablepath <- tclVar(str_trim(tclPath$Linux$Tktable))
			bwidgetpath <- tclVar(str_trim(tclPath$Linux$Bwidget))
		}
	}

	########

	txt.wd <- tklabel(fr1.conf, text = 'Set working directory', anchor = 'w', justify = 'left')
	en.dirpath <- tkentry(fr1.conf, textvariable = dirpath, width = width.entry)
	bt.dirpath <- tkbutton(fr1.conf, text = "...")
	txt.missval <- tklabel(fr1.conf, text = 'Missing value code', anchor = 'w', justify = 'left')
	en.missval <- tkentry(fr1.conf, textvariable = misscode, width = 8, justify = 'right')

	tkconfigure(bt.dirpath, command = function(){
		dirpath1 <- tk_choose.dir(getwd(), "")
		tclvalue(dirpath) <- dirpath1
	})

	tkgrid(txt.wd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 14, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dirpath, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 13, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dirpath, row = 1, column = 13, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.missval, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.missval, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########

	txt.tktable <- tklabel(fr2.conf, text = 'Tktable library path', anchor = 'w', justify = 'left')
	en.tktable <- tkentry(fr2.conf, textvariable = tktablepath, width = width.entry)
	bt.tktable <- tkbutton(fr2.conf, text = "...")
	txt.bwidget <- tklabel(fr2.conf, text = 'BWidget library path', anchor = 'w', justify = 'left')
	en.bwidget <- tkentry(fr2.conf, textvariable = bwidgetpath, width = width.entry)
	bt.bwidget <- tkbutton(fr2.conf, text = "...")

	tkconfigure(bt.tktable, command = function(){
		tktablepath1 <- tk_choose.dir(default = "", caption = "")
		tclvalue(tktablepath) <- tktablepath1
	})

	tkconfigure(bt.bwidget, command = function(){
		bwidgetpath1 <- tk_choose.dir(default = "", caption = "")
		tclvalue(bwidgetpath) <- bwidgetpath1
	})

	tkgrid(txt.tktable, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 14, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.tktable, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 13, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.tktable, row = 1, column = 13, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.bwidget, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 14, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.bwidget, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 13, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.bwidget, row = 3, column = 13, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	########

	bt.close.conf <- tkbutton(fr3.conf, text=" OK ") 

	tkconfigure(bt.close.conf, command = function(){
		workdir <- tclvalue(dirpath)
		if(!file.exists(workdir)) dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
		setwd(workdir)
		addTclPath(path = tclvalue(tktablepath))
		addTclPath(path = tclvalue(bwidgetpath))
		is.notkt <- tclRequire("Tktable")
		is.nobw <- tclRequire("BWidget") 
		if(is.logical(is.notkt)){
			tkmessageBox(message = "Tcl package 'Tktable' not found", icon = "error", type = "ok")
			tkwait.window(config.win)
		}
		if(is.logical(is.nobw)){
			tkmessageBox(message = "Tcl package 'BWidget' not found", icon = "error", type = "ok")
			tkwait.window(config.win)
		}

		params <- list(working.directory = tclvalue(dirpath), missing.value = tclvalue(misscode),
						Tktable.path = tclvalue(tktablepath), Bwidget.path = tclvalue(bwidgetpath))
		write_json(params, file.path(apps.dir, 'configure', 'configure_user.json'))
		tkgrab.release(config.win)
		tkdestroy(config.win)
	})

	tkgrid(bt.close.conf, ipadx = 10, pady = 5)

	########
	tkgrid(fr1.conf, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr2.conf, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr3.conf)

	########
	
	tkwm.withdraw(config.win)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", config.win))
	tt.h <- as.integer(tkwinfo("reqheight", config.win))
	tt.x <- as.integer(width.conf*0.5-tt.w*0.5)
	tt.y <- as.integer(height.conf*0.5-tt.h*0.5)
	tkwm.geometry(config.win, paste('+', tt.x, '+',tt.y, sep = ''))
	tkwm.transient(config.win)
	tkwm.title(config.win, 'CDT configuration')
	tkwm.deiconify(config.win)

	tkfocus(config.win)
	tkbind(config.win, "<Destroy>", function() {tkgrab.release(config.win)})
	tkwait.window(config.win)
}
