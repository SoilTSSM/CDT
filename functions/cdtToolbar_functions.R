
if(Sys.info()["sysname"] == "Windows") {
	horiz <- w.scale(70)/385
	verti <- h.scale(60)/385
}else{
	horiz <- w.scale(70)/480
	verti <- h.scale(60)/480
}
horizS <- round(horiz, 1)
vertiS <- round(verti, 1)

hRedraw <- tkimage.create('photo', '-file', file.path(imgdir, 'RedrawButton24.gif', fsep = .Platform$file.sep))
hRedraw1 <- tkimage.create('photo', '-file', file.path(imgdir, 'RedrawButton-Change24.gif', fsep = .Platform$file.sep))

####################################################################################################

tb.open.file <- tkbutton.toolbar(tools.frame, imgdir, "open24.gif", TextOutputVar, "Open file", "Open file format: txt, csv,...")
tb.save.image <- tkbutton.toolbar(tools.frame, imgdir, "save_img24.gif", TextOutputVar, "Save image", "Save image")
tb.open.table <- tkbutton.toolbar(tools.frame, imgdir, "open_table24.gif", TextOutputVar, "Open table", "Open table")
tb.save.table <- tkbutton.toolbar(tools.frame, imgdir, "save_table24.gif", TextOutputVar, "Save table", "Save table")

###
tb.run <- tkbutton.toolbar(tools.frame, imgdir, "run24.gif", TextOutputVar, "Execute", "Execute the append task")

###
lspinH <- tklabel(tools.frame, text = 'Width:')
spinH <- ttkspinbox(tools.frame, from = 0.5, to = 5.0, increment = 0.1, justify = 'center', width = 6, state = 'disabled')
tkset(spinH, horizS)

infobulle(lspinH, 'Horizontal scale factor for image size')
status.bar.display(lspinH, TextOutputVar, 'Horizontal scale factor for image size')
infobulle(spinH, 'Horizontal scale factor for image size')
status.bar.display(spinH, TextOutputVar, 'Horizontal scale factor for image size')

###
lspinV <- tklabel(tools.frame, text = 'Height:')
spinV <- ttkspinbox(tools.frame, from = 0.5, to = 5.0, increment = 0.1, justify = 'center', width = 6, state = 'disabled')
tkset(spinV, vertiS)

infobulle(lspinV, 'Vertical scale factor for image size')
status.bar.display(lspinV, TextOutputVar, 'Vertical scale factor for image size')
infobulle(spinV, 'Vertical scale factor for image size')
status.bar.display(spinV, TextOutputVar, 'Vertical scale factor for image size')

###
plotRedraw <- tkbutton.toolbar(tools.frame, imgdir, "RedrawButton24.gif", TextOutputVar, "Redraw plot", "Redraw plot")

###
tb.close.tab <- tkbutton.toolbar(tools.frame, imgdir, "close_tab24.gif", TextOutputVar, "Close active Tab", "Close active tab")
tb.exit.win <- tkbutton.toolbar(tools.frame, imgdir, "exit24.gif", TextOutputVar, "Quit CDT", "Quit CDT")

######
tb.separator0 <- ttkseparator(tools.frame, orient = 'vertical')
tb.separator1 <- ttkseparator(tools.frame, orient = 'vertical')
tb.separator2 <- ttkseparator(tools.frame, orient = 'vertical')
tb.separator3 <- ttkseparator(tools.frame, orient = 'vertical')

########
tkgrid(tb.open.file, tb.save.image, tb.separator0, tb.open.table, tb.save.table, tb.separator1, tb.run, tb.separator2, lspinH, spinH, lspinV, spinV, plotRedraw, tb.separator3, tb.close.tab, tb.exit.win)

#######
tkgrid.configure(tb.separator0, sticky = 'ns')
tkgrid.configure(tb.separator1, sticky = 'ns')
tkgrid.configure(tb.separator2, sticky = 'ns', padx = 20)
tkgrid.configure(tb.separator3, sticky = 'ns', padx = 20)

tkgrid.configure(tb.open.file, padx = 5)
tkgrid.configure(tb.save.image, padx = 5)

tkgrid.configure(tb.open.table, padx = 5)
tkgrid.configure(tb.save.table, padx = 5)

###
tkgrid.configure(tb.run, padx = 20, ipadx = 5)

###
tkgrid.configure(plotRedraw, padx = 5)

###
tkgrid.configure(tb.close.tab, padx = 5)
tkgrid.configure(tb.exit.win, padx = 30, sticky = 'e')

#####**************************** Change plot window scale ************************######

tkconfigure(plotRedraw, relief = 'raised', command = function(){
	tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
	if(length(AllOpenTabType) > 0){
		if(AllOpenTabType[[tabid]] == "img"){

			if(class(AllOpenTabData[[tabid]][[2]]) == "tkwin"){
				W <- AllOpenTabData[[tabid]][[2]]
				img <- AllOpenTabData[[tabid]][[2]]
				refreshPlot1(W = W, img = img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
			}
			if(class(AllOpenTabData[[tabid]][[2]]) == "list"){
				W <- AllOpenTabData[[tabid]][[2]][[1]]
				img <- AllOpenTabData[[tabid]][[2]][[2]]
				refreshPlot1(W = W, img = img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
				if(tclvalue(tkwinfo('class', tkwinfo('children', AllOpenTabData[[tabid]][[1]][[2]]))) == "Frame"){
					w <- as.double(tkwinfo("width", panel.right))
					h <- as.double(tkwinfo("height", panel.right))
					setScrollCanvas(W, w, h)
				}
			}
			tkconfigure(plotRedraw, image = hRedraw)
		}
	}
})

#######
tkbind(plotRedraw, "<ButtonRelease>", function(){
	tkconfigure(plotRedraw, image = hRedraw)
})

tkbind(spinH, "<<Increment>>", function(){
	tkconfigure(plotRedraw, image = hRedraw1)
})
tkbind(spinH, "<<Decrement>>", function(){
	tkconfigure(plotRedraw, image = hRedraw1)
})
tkbind(spinV, "<<Increment>>", function(){
	tkconfigure(plotRedraw, image = hRedraw1)
})
tkbind(spinV, "<<Decrement>>", function(){
	tkconfigure(plotRedraw, image = hRedraw1)
})

#####**************************** Configure command toolbars ************************######

tkconfigure(tb.open.file, state = 'normal', command = function(){
	tkconfigure(main.win, cursor = 'watch'); tcl('update')
	dat.opfiles <- getOpenFiles(main.win, all.opfiles)
	tkconfigure(main.win, cursor = '')
	if(!is.null(dat.opfiles)){
		nopf <- length(AllOpenFilesType)
		AllOpenFilesType[[nopf+1]] <<- 'ascii'
		AllOpenFilesData[[nopf+1]] <<- dat.opfiles
	}else{
		return(NULL)
	}
})

#######
tkconfigure(tb.save.image, state = 'normal', command = function(){
	## add options (width, height in px, in, cm)
	## add jpeg/png/gif
	SavePlot()
})

#######
tkconfigure(tb.open.table, state = 'normal', command = function() {
	tab.array <- displayArrayTab(main.win, tknotes)
	if(!is.null(tab.array)){
		ntab <- length(AllOpenTabType)
		AllOpenTabType[[ntab+1]] <<- 'arr'
		AllOpenTabData[[ntab+1]] <<- tab.array
		tkselect(tknotes, ntab)
	}else{
		return(NULL)
	}
})

#######
tkconfigure(tb.save.table, state = 'normal', command = function(){
	if(!is.null(ReturnExecResults)){
		tkconfigure(main.win, cursor = 'watch'); tcl('update')
		tab2sav <- try(SaveNotebookTabArray(tknotes), silent = TRUE)
		if(!inherits(tab2sav, "try-error")){
			InsertMessagesTxt(main.txt.out, "Table saved successfully")
		}else{
			InsertMessagesTxt(main.txt.out, "The table could not be saved", format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', tab2sav[1]), format = TRUE)
			return(NULL)
		}
		tkconfigure(main.win, cursor = '')
	}else{
		return(NULL)
	}
})

#####**************************** Run Task ************************######
tkconfigure(tb.run, state = 'normal', command = function(){
	if(is.null(GeneralParameters)){
		return(NULL)
	}else{
		tkconfigure(main.win, cursor = 'watch'); tcl('update')
		ReturnExecResults <<- tryCatch(Execute_All_Functions(tclvalue(lchoixStnFr$env$stn.choix.val)),
			#warning = function(w) warningFun(w),
			error = function(e) errorFun(e), finally = {
			tkconfigure(main.win, cursor = '')
		})
	}
})

#####**************************** Close CDT ************************######
##??? demande de sauver s'il y a encore des onglets ouverts???
tkconfigure(tb.exit.win, state = 'normal', command = function(){
	on.exit({
		#sink(type = "message")
		#close(msgOUT)
		options(warn = 0)
	})
	tkdestroy(main.win)
})

#####**************************** Close Notebook Tab ************************######
tkconfigure(tb.close.tab, state = 'normal', command = function(){
	tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))
	CloseNotebookTab(tabid)
})

