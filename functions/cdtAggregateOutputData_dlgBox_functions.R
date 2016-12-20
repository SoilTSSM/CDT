
AggregateOutputStationData <- function(parent.win, GeneralParameters){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frA <- tkframe(tt, relief = "sunken", borderwidth = 2)
	frB <- tkframe(tt)
	tkgrid(frA)
	tkgrid(frB)
	frA1 <- tkframe(frA)
	frA2 <- tkframe(frA)
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

	frA1.txt <- tklabel(frA1, text = 'Directory contaning the folders Outputs&OriginalData')
	tkgrid(frA1.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io))
	en.file.save <- tkentry(frA2, textvariable = file.save1, width = 45) #try dynamic width
	infobulle(en.file.save, 'Enter the full path of the directory containing\nthe folders Outputs, OriginalData or CorrectedData')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory containing the folders Outputs, OriginalData or CorrectedData')
	bt.file.save <- tkbutton.h(frA2, text = "...", TextOutputVar, 'or browse here', 'or browse here')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist.\n It will be created.',sep = ' '), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	bt.OK <- tkbutton(frB, text=" OK ")
	tkgrid(bt.OK, ipadx = 10)
	tkconfigure(bt.OK, command = function(){
		GeneralParameters$file.io <<- tclvalue(file.save1)
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Aggregate data')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

###################################################
