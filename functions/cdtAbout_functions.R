aboutCDT <- function(){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)
	
	font1 <- tkfont.create(family = "times", size = 20, weight = "bold")
	font2 <- tkfont.create(family = "times", size = 12, weight = "bold")
	font3 <- tkfont.create(family = "times", size = 12, underline = TRUE)
	font4 <- tkfont.create(family = "courier", size = 11)

	fr1 <- tkframe(tt)
	fr2 <- tkframe(tt)
	fr3 <- tkframe(tt)
	fr4 <- tkframe(tt)
	tkgrid(fr1)
	tkgrid(fr2)
	tkgrid(fr3)
	tkgrid(fr4)

	fr1a <- tkframe(fr1)
	fr1b <- tkframe(fr1)
	tkgrid(fr1a, row = 0, column = 0)
	tkgrid(fr1b, row = 0, column = 1, sticky = 'ew', padx = 15, ipadx = 1, ipady = 1)
	irilogo <- tkimage.create('photo',"::tcl::logo_origin", file = file.path(imgdir, "iriLogo0.gif", fsep = .Platform$file.sep))
	irilogo1 <- tkimage.create("photo", "::tcl::logo_transfo")
	tcl(irilogo1, "copy", irilogo, subsample = 3)  ##reduce
	img.irilogo <- tklabel(fr1a, image = irilogo1)
	tkgrid(img.irilogo)
	cdtl <- tklabel(fr1b, text = 'Climate Data Tools', font = font1)
	cdtv <- tklabel(fr1b, text = paste('Version', cdtVersion), font = font2)
	tkgrid(cdtl, sticky = 'ew')
	tkgrid(cdtv, sticky = 'ew')

	txtaut0 <- tklabel(fr2, text = 'Authors:',font = font2)
	txtaut1 <- tklabel(fr2, text = 'Rija Faniriantsoa', font = font3, foreground = 'blue', anchor = 'w', justify = 'left')
	txtaut2 <- tklabel(fr2, text = 'Tufa Dinku', font = font3, foreground = 'blue', anchor = 'w', justify = 'left')
	tkgrid(txtaut0, row = 0, column = 0, sticky = 'ew')
	tkgrid(txtaut1, row = 1, column = 1, sticky = 'ew')
	tkgrid(txtaut2, row = 2, column = 1, sticky = 'ew')

	tkbind(txtaut1,"<Enter>", function()tkconfigure(txtaut1, cursor = 'hand1'))
	tkbind(txtaut1,"<Leave>", function()tkconfigure(txtaut1, cursor=''))
	tkbind(txtaut1,"<Button-1>", function()browseURL('http://iri.columbia.edu/contact/staff-directory/rija-faniriantsoa/'))

	tkbind(txtaut2,"<Enter>", function() tkconfigure(txtaut2, cursor = 'hand1'))
	tkbind(txtaut2,"<Leave>", function()tkconfigure(txtaut2, cursor=''))
	tkbind(txtaut2,"<Button-1>", function() browseURL('http://iri.columbia.edu/contact/staff-directory/tufa-dinku/'))

	fr3a <- tkframe(fr3, relief = 'groove', borderwidth = 2)
	tkgrid(fr3a, sticky = 'ew')
	xscr <- tkscrollbar(fr3a, repeatinterval = 5, orient = "horizontal", command = function(...)tkxview(txta,...))
	yscr <- tkscrollbar(fr3a, repeatinterval = 5, command = function(...)tkyview(txta,...))
	txta <- tktext(fr3a, bg = "white", font = "courier", xscrollcommand = function(...)tkset(xscr,...),
	yscrollcommand = function(...)tkset(yscr,...), wrap = "word", height = 8, width = 37)
		 
	tkgrid(txta, yscr)
	tkgrid.configure(txta, sticky = "nsew")
	tkgrid.configure(yscr, sticky = "ns")
	
	tktag.configure(txta, "font2f", font = font2, foreground = 'blue')
	tktag.configure(txta, "font4f", font = font4)

	infofl <- file.path(apps.dir, 'text', 'cdt_info.txt', fsep = .Platform$file.sep)
	rdL <- readLines(infofl, warn = FALSE)
	tcl("update", "idletasks")
	for(i in 1:length(rdL)){
		if(i == 1) tkinsert(txta, "end", paste(rdL[i], "\n"), "font2f")
		else if(i == 2) tkinsert(txta, "end", paste('Version', cdtVersion,"\n"), "font2f")
		else tkinsert(txta, "end", paste(rdL[i], "\n"), "font4f")
	}
	bt.close <- tkbutton(fr4, text = "Close") 
	tkgrid(bt.close)
	tkconfigure(bt.close, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(main.win)
	})	

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("width", tt))
	tt.h <- as.integer(tkwinfo("height", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'About CDT')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(main.win)})
	tkwait.window(tt)
}	
