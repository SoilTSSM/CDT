
PlotCDTDatasetCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(28)/sfont0)
		largeur1 <- as.integer(w.scale(26)/sfont0)
		largeur2 <- 20
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(48)
		largeur0 <- as.integer(w.scale(23)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- 14
	}

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Plot CDT Dataset")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)

	#######################################################################################################

	#Tab1
	frTab1 <- tkframe(cmd.tab1)
	tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab1, 0, weight = 1)

	scrw1 <- bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	tkgrid.columnconfigure(scrw1, 0, weight = 1)
	subfr1 <- bwScrollableFrame(scrw1, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr1, 0, weight = 1)

		#######################

		frameData <- ttklabelframe(subfr1, text = "CDT Dataset", relief = 'groove')

		file.index.data <- tclVar()

		txt.cdtdata <- tklabel(frameData, text = 'Index file (*.rds) of the dataset', anchor = 'w', justify = 'left')
		en.cdtdata <- tkentry(frameData, textvariable = file.index.data, width = largeur0)
		bt.cdtdata <- tkbutton(frameData, text = "...")

		tkconfigure(bt.cdtdata, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(file.index.data) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds

			ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		tkgrid(txt.cdtdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.cdtdata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.cdtdata, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		##############################################

		frameSHP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		shpFile <- tclVar()
		shpAttr <- tclVar()

		txt.addshp <- tklabel(frameSHP, text = "Shapefile for Administrative Boundaries", anchor = 'w', justify = 'left')
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = shpFile, width = largeur1)
		bt.addshp <- tkbutton(frameSHP, text = "...")

		txt.attrshp <- tklabel(frameSHP, text = "Attribute field to be displayed", anchor = 'w', justify = 'left')
		EnvCDTdatasetPlot$shp$cb.attrshp <- ttkcombobox(frameSHP, values = "", textvariable = shpAttr, width = largeur1)

		# bt.TableAttr <- ttkbutton(frameSHP, text = "Open Attribute Table", width = 10)
		# bt.MapPixel <- ttkbutton(frameSHP, text = "Display Map", width = 10)
		bt.TableAttr <- ttkbutton(frameSHP, text = "Open Attribute Table")
		bt.MapPixel <- ttkbutton(frameSHP, text = "Display Map")

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(main.win, all.opfiles)
			if(!is.null(shp.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'shp'
				AllOpenFilesData[[nopf+1]] <<- shp.opfiles

				tclvalue(shpFile) <- AllOpenFilesData[[nopf+1]][[1]]
				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = shpFile)

				shpf <- getShpOpenData(shpFile)
				if(is.null(shpf)){
					EnvCDTdatasetPlot$shp$data <- NULL
					EnvCDTdatasetPlot$shp$ocrds <- NULL
					return(NULL)
				}

				EnvCDTdatasetPlot$shp$data <- shpf
				EnvCDTdatasetPlot$shp$ocrds <- getBoundaries(shpf[[2]])
				AttrTable <- names(shpf[[2]]@data)
				tclvalue(shpAttr) <- AttrTable[1]
				tkconfigure(EnvCDTdatasetPlot$shp$cb.attrshp, values = AttrTable, textvariable = shpAttr)
			}else return(NULL)

			ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		########

		tkconfigure(bt.TableAttr, command = function(){
			shpf <- EnvCDTdatasetPlot$shp$data
			if(!is.null(shpf)){
				onglet <- addNewTab(tknotes, tab.title = shpf[[1]])
				dtab <- tclArrayVar(shpf[[2]]@data)
				table1 <- displayTable(onglet[[2]], tclArray = dtab)
				tab.array <- list(onglet, table1, shpf[[3]])

				ntab <- length(AllOpenTabType)
				AllOpenTabType[[ntab+1]] <<- 'arr'
				AllOpenTabData[[ntab+1]] <<- tab.array
				tkselect(tknotes, ntab)
			}
		})

		########
		EnvCDTdatasetPlot$notebookTab.MapSelect <- NULL

		tkconfigure(bt.MapPixel, command = function(){
			if(!is.null(EnvCDTdatasetPlot$shp$ocrds)){
				imgContainer <- CDTdataset.Display.Map(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTdatasetPlot$notebookTab.MapSelect, AllOpenTabType, AllOpenTabData)
				EnvCDTdatasetPlot$notebookTab.MapSelect <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		########

		tkgrid(txt.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		tkgrid(txt.attrshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
		tkgrid(EnvCDTdatasetPlot$shp$cb.attrshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)

		tkgrid(bt.TableAttr, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
		tkgrid(bt.MapPixel, row = 4, column = 4, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)

		########

		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(shpFile)
			if(is.null(shpf)){
				EnvCDTdatasetPlot$shp$data <- NULL
				EnvCDTdatasetPlot$shp$ocrds <- NULL
				return(NULL)
			}
			EnvCDTdatasetPlot$shp$data <- shpf
			EnvCDTdatasetPlot$shp$ocrds <- getBoundaries(shpf[[2]])
			AttrTable <- names(shpf[[2]]@data)
			tclvalue(shpAttr) <- AttrTable[1]
			tkconfigure(EnvCDTdatasetPlot$shp$cb.attrshp, values = AttrTable, textvariable = shpAttr)

			ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		##############################################

		frameGraph <- ttklabelframe(subfr1, text = "Graph", relief = 'groove')

		#################

		frGph1 <- tkframe(frameGraph)

		typeTSPLOT <- c("Line", "Barplot")
		EnvCDTdatasetPlot$graph$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frGph1, values = typeTSPLOT, textvariable = EnvCDTdatasetPlot$graph$typeTSp, width = largeur2)
		bt.TsGraph.plot <- ttkbutton(frGph1, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frGph1, text = "Options", width = 8)

		EnvCDTdatasetPlot$TSGraphOp <- list(
					bar = list(
							xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
							ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
							axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
							title = list(is.title = FALSE, title = '', position = 'top'),
							colors = list(col = "darkblue")
						),
					line = list(
						xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both',
							col = list(line = "red", points = "blue"),
							lwd = 2, cex = 1.4),
						legend = NULL)
					)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(EnvCDTdatasetPlot$graph$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "Line")
			plot.fun <- match.fun(paste0("MapGraph.GraphOptions.", suffix.fun))
			EnvCDTdatasetPlot$TSGraphOp <- plot.fun(main.win, EnvCDTdatasetPlot$TSGraphOp)
		})

		#########

		EnvCDTdatasetPlot$notebookTab.dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvCDTdatasetPlot$cdtdataset)){
				imgContainer <- CDTdataset.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTdatasetPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
				EnvCDTdatasetPlot$notebookTab.dataGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#########

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		#################

		frGph2 <- tkframe(frameGraph)

		EnvCDTdatasetPlot$graph$lonLOC <- tclVar()
		EnvCDTdatasetPlot$graph$latLOC <- tclVar()

		txt.crdSel <- tklabel(frGph2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
		txt.lonLoc <- tklabel(frGph2, text = "Longitude", anchor = 'e', justify = 'right')
		en.lonLoc <- tkentry(frGph2, textvariable = EnvCDTdatasetPlot$graph$lonLOC, width = 8)
		txt.latLoc <- tklabel(frGph2, text = "Latitude", anchor = 'e', justify = 'right')
		en.latLoc <- tkentry(frGph2, textvariable = EnvCDTdatasetPlot$graph$latLOC, width = 8)

		tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#################

		frGph3 <- tkframe(frameGraph)

		EnvCDTdatasetPlot$graph$lonPAD <- tclVar('0.0')
		EnvCDTdatasetPlot$graph$latPAD <- tclVar('0.0')

		txt.spPAD <- tklabel(frGph3, text = "Spatial Average (Padding)", anchor = 'w', justify = 'left')
		txt.lonPAD <- tklabel(frGph3, text = "Longitude \u00B1", anchor = 'e', justify = 'right')
		en.lonPAD <- tkentry(frGph3, textvariable = EnvCDTdatasetPlot$graph$lonPAD, width = 6)
		txt.latPAD <- tklabel(frGph3, text = "Latitude \u00B1", anchor = 'e', justify = 'right')
		en.latPAD <- tkentry(frGph3, textvariable = EnvCDTdatasetPlot$graph$latPAD, width = 6)

		tkgrid(txt.spPAD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lonPAD, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.lonPAD, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.latPAD, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.latPAD, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


		infobulle(en.lonPAD, 'Add value in decimal degree to get rectangle centered at the target points')
		status.bar.display(en.lonPAD, TextOutputVar, 'Add value in decimal degree to get rectangle centered\nat the target points')
		infobulle(en.latPAD, 'Add value in decimal degree to get rectangle centered at the target points')
		status.bar.display(en.latPAD, TextOutputVar, 'Add value in decimal degree to get rectangle centered\nat the target points')

		#################
		tkgrid(frGph1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frGph2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frGph3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		############################################

		tkgrid(frameData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	get.CDT.dataset.Idx <- function(){
		file.CDT.Idx <- str_trim(tclvalue(file.index.data))
		if(file.CDT.Idx == "") return(NULL)

		read.cdt.dataIdx <- TRUE
		if(!is.null(EnvCDTdatasetPlot$cdtdataset))
			if(!is.null(EnvCDTdatasetPlot$file.CDT.Idx))
				if(EnvCDTdatasetPlot$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE

		if(read.cdt.dataIdx){
			if(file.exists(file.CDT.Idx)){
				OutIndexdata <- try(readRDS(file.CDT.Idx), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load the dataset', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)
					EnvCDTdatasetPlot$cdtdataset <- NULL
					return(NULL)
				}
				EnvCDTdatasetPlot$cdtdataset <- OutIndexdata
				EnvCDTdatasetPlot$cdtdataset$fileInfo <- file.CDT.Idx
				EnvCDTdatasetPlot$file.CDT.Idx <- file.CDT.Idx
			}
		}

		return(0)
	}

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}
