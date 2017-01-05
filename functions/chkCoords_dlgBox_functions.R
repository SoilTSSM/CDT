excludeOutStn <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	
	if(Sys.info()["sysname"] == "Windows"){
		wtkentry <- as.integer(as.numeric(w.scale(28)*0.95)/9)
		wtkcombo <- as.integer(as.numeric(w.scale(26)*0.95)/9)
	}else{
		wtkentry <- as.integer(as.numeric(w.scale(24)*0.95)/9)
		wtkcombo <- as.integer(as.numeric(w.scale(23)*0.95)/9)
	}

	######################################
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	####################
	frA <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$STN.file)
	file.blkshp <- tclVar(GeneralParameters$IO.files$SHP.file)
	file.save1 <- tclVar(GeneralParameters$IO.files$dir2save)

	txt.stnfl <- tklabel(frA, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frA, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)
	bt.stnfl <- tkbutton(frA, text = "...")

	txt.blkshp <- tklabel(frA, text = "Country Boundaries Shapefiles", anchor = 'w', justify = 'left')
	cb.blkshp <- ttkcombobox(frA, values = unlist(listOpenFiles), textvariable = file.blkshp, width = wtkcombo)
	bt.blkshp <- tkbutton(frA, text = "...")

	txt.file.save <- tklabel(frA, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frA, textvariable = file.save1, width = wtkentry)
	bt.file.save <- tkbutton(frA, text = "...")

	#####
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkconfigure(bt.blkshp, command = function(){
		shp.opfiles <- getOpenShp(parent.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.blkshp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist.\n It will be created.', sep = ' '), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	#####
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.blkshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.blkshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.blkshp, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.file.save, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 5, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the station data in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the station data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.blkshp, 'Choose the file in the list')
	status.bar.display(cb.blkshp, TextOutputVar, 'File containing the ESRI shapefile')
	infobulle(bt.blkshp, 'Browse file if not listed')
	status.bar.display(bt.blkshp, TextOutputVar, 'Browse file if not listed')
	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')

	####################

	frB <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

	buffw <- tclVar(GeneralParameters$buffer)

	bufftxt <- tklabel(frB, text = 'Distance from boundaries(km)')
	en.buffw <- tkentry(frB, textvariable = buffw, width = 6)

	tkgrid(bufftxt, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.buffw, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(frB, 'Distance in km, outside the country boundaries to consider')
	status.bar.display(frB, TextOutputVar, 'Distance in km, outside the country boundaries to consider')

	####################
	tkgrid(frA, sticky = 'we', padx = 5, pady = 5)
	tkgrid(frB, padx = 5, pady = 5)

	######################################

	bt.opt.OK <- tkbutton(frButt, text = "OK")
	bt.opt.CA <- tkbutton(frButt, text = "Cancel")

	#######
	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "Provide the station data file", icon = "warning", type = "ok")
		}else if(tclvalue(file.blkshp) == ""){
			tkmessageBox(message = "Provide the shapefile of country boundaries", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Enter the file to save result", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			idsstn <- getIndex.AllOpenFiles(tclvalue(file.stnfl))
			if(length(idsstn) == 0){
				tkmessageBox(message = "File not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}else{
				GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
				GeneralParameters$IO.files$SHP.file <<- str_trim(tclvalue(file.blkshp))
				GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
				GeneralParameters$buffer <<- as.numeric(str_trim(tclvalue(buffw)))

				####
				if(is.null(lcmd.frame_chk)){
					lcmd.frame <<- ChkCoordCmdBut()
					lcmd.frame_chk <<- 1
				}

				tkgrab.release(tt)
				tkdestroy(tt)
				tkfocus(parent.win)
			}
		}
	})

	#######

	tkconfigure(bt.opt.CA, command = function(){
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
	tkwm.title(tt, 'Exclude Outside Stations')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(GeneralParameters)
}

