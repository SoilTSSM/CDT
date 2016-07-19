
status.frame <- tkframe(frstatusbar)
tkgrid(status.frame, row = 0, column = 0, sticky = "ew")

tkgrid.columnconfigure(status.frame, 0, weight = 2)
tkgrid.columnconfigure(status.frame, 1, weight = 2)
tkgrid.columnconfigure(status.frame, 2, weight = 1)
tkgrid.columnconfigure(status.frame, 3, weight = 1)

##
bstatus1 <- tkframe(status.frame, relief = 'sunken', bd = 2)  #
lhelp <- tklabel(bstatus1, anchor = 'w', width = wbstatus1) #change dyn, width = 80
tkgrid(lhelp, sticky = "we")
tkconfigure(lhelp, textvariable = TextOutputVar)

bstatus2 <- tkframe(status.frame, relief = 'sunken', bd = 2)

xpcoord <- tclVar()
bstatus2a <- tkframe(bstatus2)
lxcoords <- tklabel(bstatus2a, textvariable = xpcoord, width = wbstatus2a) #change dyn, width = 22
lxcoords0 <- tklabel(bstatus2a, text = 'X:',justify = 'left', anchor = 'w')
tkgrid(lxcoords0, lxcoords)
tkgrid.configure(lxcoords0, sticky = "we")
tkgrid.configure(lxcoords, sticky = "we")

xy.separator <- ttkseparator(bstatus2, orient = 'vertical')

ypcoord <- tclVar()
bstatus2b <- tkframe(bstatus2)
lycoords <- tklabel(bstatus2b, textvariable = ypcoord, width = wbstatus2b) #change dyn, width = 22
lycoords0 <- tklabel(bstatus2b, text = 'Y:',justify = 'left', anchor = 'w')
tkgrid(lycoords0, lycoords)
tkgrid.configure(lycoords0, sticky = "we")
tkgrid.configure(lycoords, sticky = "we")

tkgrid(bstatus2a, row = 0, column = 0, sticky = "we")
tkgrid(xy.separator, row = 0, column = 1, sticky = 'ns', padx = 5)
tkgrid(bstatus2b, row = 0, column = 2, sticky = "we")
tkgrid.columnconfigure(bstatus2a,1, weight = 1)
tkgrid.columnconfigure(bstatus2b,1, weight = 1)

bstatus3 <- tkframe(status.frame, relief = 'sunken', bd = 2)

zpcoord <- tclVar()
lzcoords <- tklabel(bstatus3, textvariable = zpcoord, width = wbstatus3)  #,width = 20
lzcoords0 <- tklabel(bstatus3, text = 'Z:',justify = 'left', anchor = 'w')
tkgrid(lzcoords0, lzcoords)
tkgrid.configure(lzcoords0, sticky = "we")
tkgrid.configure(lzcoords, sticky = "we")

bstatus4 <- tkframe(status.frame, relief = 'flat', bd = 2)
lbstatus4 <- tklabel(bstatus4, text="")
tkgrid(lbstatus4, sticky = "ew")

tkgrid(bstatus1, row = 0, column = 0, padx = 5, pady = 0, sticky = "we")
tkgrid(bstatus2, row = 0, column = 1, padx = 1, pady = 0, sticky = "we") #
tkgrid(bstatus3, row = 0, column = 2, padx = 1, pady = 0, sticky = "we") #
tkgrid(bstatus4, row = 0, column = 3, sticky = "e")
tkgrid.columnconfigure(bstatus1, 0, weight = 1)
tkgrid.columnconfigure(bstatus2, 0, weight = 1)
tkgrid.columnconfigure(bstatus2, 2, weight = 1)
tkgrid.columnconfigure(bstatus3, 1, weight = 1)
tkgrid.columnconfigure(bstatus4, 0, weight = 1)

