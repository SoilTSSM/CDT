###preview data.frame
preview.data <- function(parent.win, fileopen, title.pop){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	fr0 <- tkframe(tt)
	fr1 <- tkframe(tt)
 	fr2 <- tkframe(tt)

	fr.delim <- ttklabelframe(fr0, text = "Delimiters", labelanchor = "nw", relief = "sunken", borderwidth = 2)
	fr.head <- ttklabelframe(fr0, text = "Header", labelanchor = "nw", relief = "sunken", borderwidth = 2)

	tkgrid(fr.delim, row = 0, column = 0, padx = 5, pady = 2, ipadx = 2, ipady = 2, sticky = 'w')
	tkgrid(fr.head, row = 0, column = 2, padx = 5, pady = 2, ipadx = 2, ipady = 2, sticky = 'e')

	delim1 <- tkradiobutton(fr.delim, text = "Tab")
	delim2 <- tkradiobutton(fr.delim, text = "Sapce")
	delim3 <- tkradiobutton(fr.delim, text = "Semicolon")
	delim4 <- tkradiobutton(fr.delim, text = "Comma")
	delim5 <- tkradiobutton(fr.delim, text = "Other:")
	etrval <- tclVar("")
	vdelim5 <- tkentry(fr.delim, width = 6, textvariable = etrval)

	tkgrid(delim1, row = 0, column = 0, sticky = "w")
	tkgrid(delim3, row = 0, column = 1, sticky = "w")
	tkgrid(delim5, row = 0, column = 2, sticky = "w")
	tkgrid(delim2, row = 1, column = 0, sticky = "w")
	tkgrid(delim4, row = 1, column = 1, sticky = "w")
	tkgrid(vdelim5, row = 1, column = 2, sticky = "e")

	rbval <- tclVar("tb")
	tkconfigure(delim1, variable = rbval, value = "tb")
	tkconfigure(delim2, variable = rbval, value = "sp")
	tkconfigure(delim3, variable = rbval, value = "sc")
	tkconfigure(delim4, variable = rbval, value = "cm")
	tkconfigure(delim5, variable = rbval, value = "ot")

	lb1 <- tklabel(fr.head, text = 'Start import at row')
	lb2 <- tklabel(fr.head, text = 'First row as header')
	etrval1 <- tclVar("1")
	skp <- tkentry(fr.head, width = 4, textvariable = etrval1, justify = "center")
	vhead <- tclVar("FALSE")
	ishead <- c("TRUE", "FALSE")
	head <- ttkcombobox(fr.head, values = ishead, textvariable = vhead, state = "readonly", width = 6, justify = "center")
	tkgrid(lb1, row = 0, column = 0)
	tkgrid(skp, row = 0, column = 1)
	tkgrid(lb2, row = 1, column = 0)
	tkgrid(head, row = 1, column = 1)

#######
 	fr1a <- tkframe(fr1, relief = "groove", borderwidth = 2)
	tkgrid(fr1a, padx = 5)

#	labprvw <- as.integer(tt.w/9)
#fixed to 51
	labprvw <- 51
	labtxt <- tklabel(fr1a, text = paste("Preview of ", fileopen), width = labprvw, anchor = 'w')
	tkgrid(labtxt, row = 0, column = 0, sticky = 'w')

	xscr <- tkscrollbar(fr1a, repeatinterval = 5, orient = "horizontal", command = function(...)tkxview(txta,...))
	yscr <- tkscrollbar(fr1a, repeatinterval = 5, command = function(...)tkyview(txta,...))
	txta <- tktext(fr1a, bg = "white", font = "courier", xscrollcommand = function(...)tkset(xscr,...),
	yscrollcommand = function(...)tkset(yscr,...), wrap = "none")

	tkgrid(txta, yscr)
	tkgrid(xscr)
	tkgrid.configure(txta, row = 1, column = 0, sticky = "nsew")
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(xscr, sticky = "ew")
#	txta.w <- as.integer(tt.w/11)
# fixed to 42
	txta.w <- 42
	tkconfigure(txta, width = txta.w, height = 9)

	is.rdble <- !inherits(try(rdL <- readLines(fileopen, n = 10, warn = F), silent = TRUE), "try-error")
	if(!is.rdble){
		InsertMessagesTxt(main.txt.out, paste("Unable to open file ", fileopen), format = TRUE)
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
		return(NULL)
	}else{
		#tcl("update", "idletasks")
		for(i in 1:length(rdL))	tkinsert(txta, "end", paste(rdL[i], "\n"))
		tcl("update")  #omit if not work and uncomment tcl("update", "idletasks")
	}

######
	fr.miss <- tkframe(fr2, relief = "groove", borderwidth = 2)
	fr.but <- tkframe(fr2)
	tkgrid(fr.miss, row = 0, column = 0, padx = 5, ipadx = 5, sticky = 'w')
	tkgrid(fr.but, row = 0, column = 2, padx = 5, ipadx = 5, sticky = 'e')

	lbmiss <- tklabel(fr.miss, text = 'Missing Value')
	etrmiss <- tclVar("-99")
	missvl <- tkentry(fr.miss, width = 5, textvariable = etrmiss, justify = "center")
	tkgrid(lbmiss, row = 0, column = 0)
	tkgrid(missvl, row = 0, column = 1)
#####
	separator <- NULL
	OK.but <- tkbutton(fr.but, text = "OK", width = 4, command = function() {
		delim <- as.character(tclvalue(rbval))
		if(delim == 'tb') sepr<-""
		if(delim == 'sp') sepr<-""
		if(delim == 'sc') sepr<-";"
		if(delim == 'cm') sepr<-","
		if(delim == 'ot'){
			tclvalue(etrval) <- tclvalue(tkget(vdelim5))
			sepr <- tclvalue(etrval)
		}
		header <- as.logical(tclvalue(vhead))
		tclvalue(etrval1) <- tclvalue(tkget(skp))
		skip <- as.numeric(tclvalue(etrval1))
		missval <- as.character(tclvalue(etrmiss))
		separator <<- list(sepr = sepr, header = header, skip = skip, miss.val = missval)
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})
	CA.but <- tkbutton(fr.but, text = "Cancel", width = 4, command = function() {
		separator <<- NULL
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	#but.space <- tklabel(fr.but, text='',width = 4)
	tkgrid(OK.but, CA.but)
	tkgrid.configure(OK.but, row = 0, column = 0, padx = 5, ipadx = 10, sticky = 'w')
	tkgrid.configure(CA.but, row = 0, column = 1, padx = 5, ipadx = 5, sticky = 'e')

#####################
	tkgrid(fr0, row = 0, column = 0, sticky = 'snwe', padx = 5, pady = 5)
	tkgrid(fr1, row = 1, column = 0, columnspan = 3, padx = 5, pady = 5)
	tkgrid(fr2, row = 2, column = 0, sticky = 'snwe', padx = 5, pady = 5)
	#tcl('update')
	#tkconfigure(fr0, width = as.integer(tkwinfo("reqwidth", fr1)))
	#tkconfigure(fr2, width = as.integer(tkwinfo("reqwidth", fr1)))
	#tcl('update')

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, paste("Data Import Options - ", title.pop))
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(separator)
}

