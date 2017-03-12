
AggregateOutputStationData <- function(parent.win, GeneralParameters){
	largeur <- 45

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frA <- tkframe(tt, relief = "sunken", borderwidth = 2)
	frB <- tkframe(tt)

	#############################

	file.save1 <- tclVar(GeneralParameters$file.io)

	txt.file.save <- tklabel(frA, text = 'Directory containing the folders Outputs & OriginalData', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frA, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frA, text = "...")

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(str_trim(GeneralParameters$file.io), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist'), icon = "warning", type = "ok")
			tkwait.window(tt)
		}else tclvalue(file.save1) <- file2save1
	})

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path of the directory containing the folders\nOutputs, OriginalData or CorrectedData')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory containing the folders\nOutputs, OriginalData or CorrectedData')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	#############################
	bt.OK <- tkbutton(frB, text = "OK")

	tkconfigure(bt.OK, command = function(){
		GeneralParameters$file.io <<- str_trim(tclvalue(file.save1))
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.OK, padx = 5, pady = 5, ipadx = 10, ipady = 1)

	#############################

	tkgrid(frA)
	tkgrid(frB)

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
