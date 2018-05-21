################## error and warning handling

warningFun <- function(w){
	txt <- as.character(w)
	retW <- gsub('[\r\n]', '', txt)
	InsertMessagesTxt(main.txt.out, retW, format = TRUE)
	return(NULL)
}

errorFun <- function(e){
	txt <- as.character(e)
	retE <- gsub('[\r\n]', '', txt)
	InsertMessagesTxt(main.txt.out, retE, format = TRUE)
	return(NULL)
}

#####**************************** Initialize variables ************************######

##Onglet and table display
assign('AllOpenTabData', vector(mode = 'list', length = 0), envir = .GlobalEnv)
assign('AllOpenTabType', vector(mode = 'list', length = 0), envir = .GlobalEnv)

##Open data
assign('AllOpenFilesData', vector(mode = 'list', length = 0), envir = .GlobalEnv)
assign('AllOpenFilesType', vector(mode = 'list', length = 0), envir = .GlobalEnv)

###Input params / return results
assign('GeneralParameters', NULL, envir = .GlobalEnv)
assign('ReturnExecResults', NULL, envir = .GlobalEnv)

##Initialization status bars 1
assign('TextOutputVar', tclVar(), envir = .GlobalEnv)

###coordinates from cdt data on qc (use to initialize bbox of plot spatial check)
assign('XYCoordinates', tclVar(), envir = .GlobalEnv)

####Homog adj initialization (a changer en envir)
adjDon <- NULL

###Left panel initialization
assign('lcmd.frame', NULL, envir = .GlobalEnv)
cdt.lcmd.container <- list(
					'lcmd.frame_chk', 'lcmd.frame_assdata',
					'lcmd.frame_filtercdtdat', 'lcmd.frame_merge2cdt',
					'lcmd.frame_qc', 'lcmd.frame_qc0Chck',
					'lcmd.frame_homo', 'lcmd.frame_rhtests',
					'lcmd.frame_interpol', 'lcmd.frame_valid.HOV', 'lcmd.frame_valid.LOOCV',
					'lcmd.frame_extrdata', 'lcmd.frame_summariesData',
					'lcmd.frame_climatoCalc', 'lcmd.frame_anomaliesCalc',
					'lcmd.frame_spatialAnalysis', 'lcmd.frame_dailyRainAnalysis', 'lcmd.frame_PICSA',
					'lcmd.frame_onsetCalc', 'lcmd.frame_cessationCalc', 'lcmd.frame_seaslengthCalc',
					'lcmd.frame_climdexRR', 'lcmd.frame_climdexTT', 'lcmd.frame_SPIData',
					'lcmd.frame_mergePlot', 'lcmd.frame_CDTffrtPlot', 'lcmd.frame_grdNcdfPlot',
					'lcmd.frame_seqNcdfPlot'
				)

lcmd.ret <- lapply(cdt.lcmd.container, assign, NULL, envir = .GlobalEnv)

#####**************************** General&Generic functions ************************######

source(file.path(apps.dir, 'functions', 'cdtGeneral_functions.R'))

#####**************************** MAIN WINDOW ************************######

main.win <- tktoplevel()
tkwm.resizable(main.win, TRUE, TRUE)
tkgrid.columnconfigure(main.win, 0, weight = 1)  
tkgrid.rowconfigure(main.win, 1, weight = 1)

##Window geometry
width.scr <- as.integer(tkwinfo("screenwidth", main.win))
height.scr <- as.integer(tkwinfo("screenheight", main.win))

##function to scale dialog in %
w.scale <- function(per) as.integer(per*width.scr/100)
h.scale <- function(per) as.integer(per*height.scr/100)

#######################################################################
###dim
##font width
sfont0 <- as.numeric(tclvalue(tkfont.measure(main.win, paste0("0123456789",
				paste0(letters[1:26], LETTERS[1:26], collapse = '')))))/(10+2*26)

if(Sys.info()["sysname"] == "Windows"){
	##Output message, tktext height
	txtHeight <- 6
	w.opfiles.perc <- 35
}else{
	txtHeight <- 7
	w.opfiles.perc <- 30
}

##List open files width
w.opfiles <- as.integer(w.scale(w.opfiles.perc)/sfont0)

##left panel width
wpanel.left <- w.scale(30)
hpanel.left <- h.scale(70)
##stations choice width
wframe.choix.stn <- as.integer(w.scale(21.5)/sfont0)
##Status bar width (from left)
wbstatus1 <- as.integer(w.scale(50)/sfont0) 
wbstatus2a <- as.integer(w.scale(15)/sfont0)
wbstatus2b <- as.integer(w.scale(15)/sfont0)
wbstatus3 <- as.integer(w.scale(15)/sfont0)

#####**************************** TOP MENU ************************######

top.menu <- tkmenu(main.win, tearoff = FALSE)
tkconfigure(main.win, menu = top.menu)
source(file.path(apps.dir, 'functions', 'cdtMenu_functions.R'))

#####**************************** TOOLBARS ************************######

tools.frame <- tkframe(main.win, bd = 2, relief = 'ridge')
source(file.path(apps.dir, 'functions', 'cdtToolbar_functions.R'))

#####**************************** MAIN FRAME ************************######

main.frame0 <- tkframe(main.win, bd = 2, relief = 'ridge')
#panedwindow
main.pane0 <- ttkpanedwindow (main.frame0, orient = 'vertical')

#####**************************** MAIN PANEL ************************######

main.frame <- ttkpanedwindow (main.pane0, orient = 'horizontal', height = hpanel.left)
#left panel
panel.left <- tkframe(main.frame, relief = 'raised', bd = 2, width = wpanel.left)  
#right panel
panel.right <- tkframe(main.frame)

source(file.path(apps.dir, 'functions', 'cdtMainPanel_functions.R'))

#####**************************** TEXT OUTPUT ************************######

out.frame <- tkframe(main.pane0, bd = 2, relief = 'groove')
source(file.path(apps.dir, 'functions', 'cdtOutputmsg_functions.R'))

#####**************************** STATUS BAR ************************######

frstatusbar <- tkframe(main.win)
source(file.path(apps.dir, 'functions', 'cdtStatusbar_functions.R'))

#####**************************** Manage grid ************************######

tkadd(main.frame, panel.left)
tkadd(main.frame, panel.right)

#left panel
tkgrid.columnconfigure(panel.left, 0, weight = 1)
# tkgrid.rowconfigure(panel.left, 0, weight = 1)
# tkgrid.rowconfigure(panel.left, 1, weight = 1)
# tkgrid.rowconfigure(panel.left, 2, weight = 1)

#right panel
tkgrid.columnconfigure(panel.right, 0, weight = 1)
tkgrid.rowconfigure(panel.right, 0, weight = 1)

#####
tkgrid(tools.frame, row = 0, column = 0, rowspan = 1, columnspan = 2, sticky = "new", padx = 5)

###
tkadd(main.pane0, main.frame)
tkadd(main.pane0, out.frame)

tkgrid.columnconfigure(main.frame, 0, weight = 1)
tkgrid.columnconfigure(main.frame, 1, weight = 1)
tkgrid.rowconfigure(main.frame, 0, weight = 1)
tkgrid.columnconfigure(out.frame, 0, weight = 1)
tkgrid.rowconfigure(out.frame, 0, weight = 1)

## panned frame
tkgrid(main.pane0, row = 0, column = 0, rowspan = 1, columnspan = 2, sticky = "snew", padx = 1) 
tkgrid.rowconfigure(main.pane0, 0, weight = 1)
tkgrid.rowconfigure(main.pane0, 1, weight = 1)
tkgrid.columnconfigure(main.pane0, 0, weight = 1)

##main frame
tkgrid(main.frame0, row = 1, column = 0, rowspan = 1, columnspan = 2, sticky = "snew", padx = 5) 
tkgrid.rowconfigure(main.frame0, 0, weight = 1)
tkgrid.columnconfigure(main.frame0, 0, weight = 1)

##statusbar
tkgrid(frstatusbar, row = 2, column = 0, rowspan = 1, columnspan = 1, sticky = "snew")
tkgrid.columnconfigure(frstatusbar, 0, weight = 1)

grip.right <- ttksizegrip(main.win)
tkgrid(grip.right, row = 2, column = 1, sticky = "se")

#####**************************** Manage geometry ************************######

tkwm.withdraw(main.win)
tcl('update')
tkwm.geometry(main.win, paste(width.scr, 'x', height.scr, '+', 0, '+', 0, sep = ''))
tkwm.transient(main.win)
tkwm.title(main.win, paste("Climate Data Tools, v", cdtVersion, sep = ''))
tkwm.deiconify(main.win)

#####**************************** fullscreen option ************************######

if(Sys.info()["sysname"] == "Linux") {
	tcl('wm', 'attributes', main.win, fullscreen = FALSE, zoomed = TRUE) #Linux
}else if(Sys.info()["sysname"] == "Darwin"){
	tcl('wm', 'attributes', main.win, fullscreen = FALSE, zoomed = TRUE)
}else if(Sys.info()["sysname"] == "Windows"){
	tcl('wm', 'attributes', main.win, fullscreen = FALSE)
	tcl('wm', 'state', main.win, 'zoomed') #Windows
}

#####**************************** Close CDT ************************######

tcl("wm", "protocol", main.win, "WM_DELETE_WINDOW", function() {
	on.exit({
		options(warn = 0)
	})
	tkdestroy(main.win)
})

#####**************************** LOAD Functions ************************######

tryCatch({
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			InsertMessagesTxt(main.txt.out, "Loading ....")
			source(file.path(apps.dir, 'functions', 'cdtLoad_ALL_functions.R'))
		},
		warning = function(w) warningFun(w),
		error = function(e) errorFun(e),
		finally = {
			tkconfigure(main.win, cursor = '')
			tcl('update')
			InsertMessagesTxt(main.txt.out, "Ready!")
		})

