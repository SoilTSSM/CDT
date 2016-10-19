
##Text out

assign("xscr.out", tkscrollbar(out.frame, repeatinterval = 5, orient = "horizontal",
                    		command = function(...) tkxview(main.txt.out, ...)), envir = .GlobalEnv)
assign("yscr.out", tkscrollbar(out.frame, repeatinterval = 5, command = function(...) tkyview(main.txt.out, ...)), envir = .GlobalEnv)
assign("main.txt.out" , tktext(out.frame, bg = "white", #font = tkfont.create(family = "courier", size = 11),
							    xscrollcommand = function(...) tkset(xscr.out, ...),
							    yscrollcommand = function(...) tkset(yscr.out, ...),
								wrap = "none", height = txtHeight), envir = .GlobalEnv)

tkgrid(main.txt.out, yscr.out)
tkgrid(xscr.out)
tkgrid.configure(yscr.out, sticky = "ns")
tkgrid.configure(xscr.out, sticky = "ew")
tkgrid.configure(main.txt.out, sticky = 'nswe')
tkgrid.columnconfigure(main.txt.out, 0, weight = 1)

####### Copy/Cut/Paste 
msgOutputTxtCopyPaste <- tkmenu(main.txt.out, tearoff = FALSE)
tkadd(msgOutputTxtCopyPaste, "command", label = "Copy <Ctrl-C>", state = 'normal', command = function(){
 	.Tcl(paste("event", "generate", .Tcl.args(.Tk.ID(main.txt.out), "<<Copy>>")))
 })
# tkadd(msgOutputTxtCopyPaste, "command", label = "Cut <Ctrl-X>", state = 'disabled', command = function(){
#  	.Tcl(paste("event", "generate", .Tcl.args(.Tk.ID(main.txt.out), "<<Cut>>")))
#  })
# # tkadd(msgOutputTxtCopyPaste, "separator")
# tkadd(msgOutputTxtCopyPaste, "command", label = "Paste <Ctrl-V>", state = 'disabled', command = function(){
#  	.Tcl(paste("event", "generate", .Tcl.args(.Tk.ID(main.txt.out), "<<Paste>>")))
#  })

defile.msgOutputTxtCopyPaste <- function(x, y) {
	rootx <- as.integer(tkwinfo("rootx", main.txt.out))
	rooty <- as.integer(tkwinfo("rooty", main.txt.out))
	xTxt <- as.integer(x) + rootx
	yTxt <- as.integer(y) + rooty
	.Tcl(paste("tk_popup", .Tcl.args(msgOutputTxtCopyPaste, xTxt, yTxt)))
}

tkbind(main.txt.out, "<Button-3>", function(x, y){
	 defile.msgOutputTxtCopyPaste(x, y)
})

