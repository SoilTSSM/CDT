

selectStationCmd <- function(){
	# GeneralParameters, main.win, panel.left, wframe.choix.stn
	choixStnFr <- ttklabelframe(panel.left, text = "Choose station", relief = 'groove')

	choixStnFr$env$stn.choix <- c('')
	choixStnFr$env$stn.choix.val <- tclVar(choixStnFr$env$stn.choix[1])

	choixStnFr$env$stn.choix.cb <- ttkcombobox(choixStnFr, values = choixStnFr$env$stn.choix,
											 textvariable = choixStnFr$env$stn.choix.val,
											 state = 'normal', width = wframe.choix.stn)
	choixStnFr$env$stn.choix.prev <- tkbutton(choixStnFr, text = "<<- Prev", state = 'disabled')
	choixStnFr$env$stn.choix.next <- tkbutton(choixStnFr, text = "Next ->>", state = 'disabled')
	choixStnFr$env$setting.button <- tkbutton(choixStnFr, text = "Options", state = 'disabled')

	choixStnFr$env$button_next <- function(){
		istn <- as.numeric(tclvalue(tcl(choixStnFr$env$stn.choix.cb, "current")))+1
		istn <- istn+1
		if(istn > length(choixStnFr$env$stn.choix)) istn <- 1
		tclvalue(choixStnFr$env$stn.choix.val) <- choixStnFr$env$stn.choix[istn]
	}

	choixStnFr$env$button_prev <- function(){
		istn <- as.numeric(tclvalue(tcl(choixStnFr$env$stn.choix.cb, "current")))+1
		istn <- istn-1
		if(istn < 1) istn <- length(choixStnFr$env$stn.choix)
		tclvalue(choixStnFr$env$stn.choix.val) <- choixStnFr$env$stn.choix[istn]
	}

	tkconfigure(choixStnFr$env$stn.choix.prev, command = choixStnFr$env$button_prev)
	tkconfigure(choixStnFr$env$stn.choix.next, command = choixStnFr$env$button_next)
	tkconfigure(choixStnFr$env$setting.button, command = function(){
		if(GeneralParameters$action == 'zero.check') assign('GeneralParameters', qcGetZeroCheckInfo(main.win, GeneralParameters), envir = .GlobalEnv)
		if(GeneralParameters$action == 'qc.rain') assign('GeneralParameters', qc.get.info.rain(main.win, GeneralParameters), envir = .GlobalEnv)
		if(GeneralParameters$action == 'qc.temp') assign('GeneralParameters', qc.get.info.txtn(main.win, GeneralParameters), envir = .GlobalEnv)
		if(GeneralParameters$action == 'homog') assign('GeneralParameters', homogen.get.info(main.win, GeneralParameters), envir = .GlobalEnv)
	#	if(GeneralParameters$action == 'rhtests') assign('GeneralParameters', rhtests_inputData(main.win, GeneralParameters), envir = .GlobalEnv)
	})

	tkgrid(choixStnFr$env$stn.choix.cb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(choixStnFr$env$stn.choix.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(choixStnFr$env$stn.choix.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(choixStnFr$env$setting.button, row = 0, column = 3, sticky = 'we', rowspan = 2, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(choixStnFr$env$stn.choix.cb, 0, weight = 1)

	tkgrid(choixStnFr, sticky = 'nwe', pady = 5)
	for(i in 0:3) tkgrid.columnconfigure(choixStnFr, i, weight = 1)
	tcl('update')

	return(choixStnFr)
}

#####**************************** LEFT ************************######
###Widgets left panel

frame.opfiles <- ttklabelframe(panel.left, text = "Open Files", relief = 'groove', width = wpanel.left)

lchoixStnFr <- ttklabelframe(panel.left, text = "Choose station", relief = 'groove')
cmd.frame <- tkframe(panel.left, relief = 'groove', bd = 2)
lcmd.frame <- cmd.frame

##### List open files 
scr.opfiles <- tkscrollbar(frame.opfiles, repeatinterval = 5, command = function(...) tkyview(all.opfiles, ...))
all.opfiles <- tklistbox(frame.opfiles, selectmode = "single", height = 5, width = w.opfiles,
						 selectbackground = "yellow", selectforeground = "blue", background = "white",
						 yscrollcommand = function(...) tkset(scr.opfiles, ...))
tkgrid(all.opfiles, row = 0, column = 0, sticky = "nwe")
tkgrid(scr.opfiles, row = 0, column = 1, rowspan = 4, sticky = "ns")

#######
tkgrid(frame.opfiles, sticky = 'nwe')
tkgrid.columnconfigure(frame.opfiles, 0, weight = 1)


#####**************************** RIGHT ************************######
##Onglet right panel

area.frame <- tkframe(panel.right)
tknotes <- ttknotebook(area.frame)

tkgrid(tknotes, row = 0, column = 0, sticky = 'nswe')
tkgrid(area.frame, row = 0, column = 0, sticky = 'nswe')

###
for(i in 0:60) tkgrid.columnconfigure(tknotes, i, weight = 1)
for(i in 0:12) tkgrid.rowconfigure(tknotes, i, weight = 1)

tkgrid.columnconfigure(area.frame, 0, weight = 1)
tkgrid.rowconfigure(area.frame, 0, weight = 1)

#####
pressed_index <- tclVar('')
tkbind(tknotes, "<ButtonPress-1>", function(x, y, W) btn_press(x, y, W))
tkbind(tknotes, "<ButtonRelease-1>", function(x, y, W) btn_releases(x, y, W))

