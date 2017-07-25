###preview data.frame
preview.data <- function(parent.win, fileopen, title.pop){
	if (Sys.info()["sysname"] == "Windows"){
		txta.w <- 39
		labprvw <- 45
	}else{
		txta.w <- 43
		labprvw <- 51
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	fr0 <- tkframe(tt)
	fr1 <- tkframe(tt, relief = "groove", borderwidth = 2)
 	fr2 <- tkframe(tt)

 	##############

	fr.delim <- ttklabelframe(fr0, text = "Delimiters", labelanchor = "nw", relief = "sunken", borderwidth = 2)

	etrval <- tclVar()
	rbval <- tclVar("sp")

	delim1 <- tkradiobutton(fr.delim, text = "Tab", variable = rbval, value = "tb", anchor = 'w', justify = 'left')
	delim2 <- tkradiobutton(fr.delim, text = "Space", variable = rbval, value = "sp", anchor = 'w', justify = 'left')
	delim3 <- tkradiobutton(fr.delim, text = "Semicolon", variable = rbval, value = "sc", anchor = 'w', justify = 'left')
	delim4 <- tkradiobutton(fr.delim, text = "Comma", variable = rbval, value = "cm", anchor = 'w', justify = 'left')
	delim5 <- tkradiobutton(fr.delim, text = "Other:", variable = rbval, value = "ot", anchor = 'w', justify = 'left')
	vdelim5 <- tkentry(fr.delim, width = 6, textvariable = etrval)

	tkgrid(delim1, row = 0, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(delim3, row = 0, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(delim5, row = 0, column = 2, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(delim2, row = 1, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(delim4, row = 1, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(vdelim5, row = 1, column = 2, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############

	fr.head <- ttklabelframe(fr0, text = "Header", labelanchor = "nw", relief = "sunken", borderwidth = 2)

	etrval1 <- tclVar(1)
	vhead <- tclVar("FALSE")
	ishead <- c("TRUE", "FALSE")

	lb1 <- tklabel(fr.head, text = 'Start import at row', anchor = 'w', justify = 'left')
	lb2 <- tklabel(fr.head, text = 'First row as header', anchor = 'w', justify = 'left')
	skp <- tkentry(fr.head, width = 6, textvariable = etrval1, justify = "center")
	head <- ttkcombobox(fr.head, values = ishead, textvariable = vhead, state = "readonly", width = 6, justify = "center")

	tkgrid(lb1, row = 0, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(skp, row = 0, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(lb2, row = 1, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(head, row = 1, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############
	tkgrid(fr.delim, row = 0, column = 0, padx = 5, pady = 2, ipadx = 2, ipady = 2, sticky = 'nswe')
	tkgrid(fr.head, row = 0, column = 1, padx = 5, pady = 2, ipadx = 2, ipady = 2, sticky = 'nswe')

	##############
 
	labtxt <- tklabel(fr1, text = paste("Preview of ", fileopen), width = labprvw, anchor = 'w', justify = 'left')
	infobulle(labtxt, fileopen)

	xscr <- tkscrollbar(fr1, repeatinterval = 5, orient = "horizontal", command = function(...) tkxview(txta, ...))
	yscr <- tkscrollbar(fr1, repeatinterval = 5, command = function(...) tkyview(txta, ...))
	txta <- tktext(fr1, bg = "white", font = "courier", width = txta.w, height = 9,
					xscrollcommand = function(...) tkset(xscr, ...),
					yscrollcommand = function(...) tkset(yscr, ...), wrap = "none")

	tkgrid(labtxt, row = 0, column = 0, sticky = 'we')
	tkgrid(txta, yscr)
	tkgrid(xscr)
	tkgrid.configure(txta, row = 1, column = 0, sticky = "nsew")
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(xscr, sticky = "ew")

	rdL <- try(readLines(fileopen, n = 10, warn = FALSE), silent = TRUE)
	if(inherits(rdL, "try-error")){
		InsertMessagesTxt(main.txt.out, paste("Unable to open file ", fileopen), format = TRUE)
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
		return(NULL)
	}else{
		for(i in 1:length(rdL))	tkinsert(txta, "end", paste(rdL[i], "\n"))
		tcl("update")
	}

	##############

	fr.miss <- tkframe(fr2, relief = "groove", borderwidth = 2)

	confpath <- file.path(apps.dir, 'configure', 'configure_user.json')
	conffile <- fromJSON(confpath)
	etrmiss <- tclVar(str_trim(conffile$missing.value))

	lbmiss <- tklabel(fr.miss, text = 'Missing Value', anchor = 'w', justify = 'left')
	missvl <- tkentry(fr.miss, width = 8, textvariable = etrmiss, justify = "center")

	tkgrid(lbmiss, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(missvl, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	##############

	fr.but <- tkframe(fr2)

	OK.but <- tkbutton(fr.but, text = "OK", width = 8)
	CA.but <- tkbutton(fr.but, text = "Cancel", width = 8)

	separator <- NULL

	tkconfigure(OK.but, command = function() {
		delim <- as.character(tclvalue(rbval))
		if(delim == 'tb') sepr <- ""
		if(delim == 'sp') sepr <- ""
		# if(delim == 'tb') sepr <- "\t"
		# if(delim == 'sp') sepr <- " "
		if(delim == 'sc') sepr <- ";"
		if(delim == 'cm') sepr <- ","
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

	tkconfigure(CA.but, command = function() {
		separator <<- NULL
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(OK.but, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(CA.but, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	##############
	tkgrid(fr.miss, row = 0, column = 0, padx = 1, ipadx = 1, sticky = 'we')
	tkgrid(fr.but, row = 0, column = 1, padx = 5, ipadx = 5, sticky = 'we')

	#####################
	tkgrid(fr0, row = 0, column = 0, sticky = 'snwe', padx = 5, pady = 5)
	tkgrid(fr1, row = 1, column = 0, sticky = 'we', padx = 5, pady = 5)
	tkgrid(fr2, row = 2, column = 0, padx = 5, pady = 5)

	#####################

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, paste("Data Import Options - ", title.pop))
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(separator)
}

