
PlotGriddedNcdfCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46.5)
		largeur0 <- as.integer(w.scale(28)/sfont0)
		largeur1 <- 18
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(50.5)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- 16
	}

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Plot NetCDF Data")

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

		frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		ncdf.file <- tclVar()

		txt.ncfl <- tklabel(frameNC, text = "NetCDF data file", anchor = 'w', justify = 'left')
		cb.ncfl <- ttkcombobox(frameNC, values = unlist(listOpenFiles), textvariable = ncdf.file, width = largeur0)
		bt.ncfl <- tkbutton(frameNC, text = "...")

		tkconfigure(bt.ncfl, command = function(){
			tkconfigure(main.win, cursor = 'watch')
			tcl('update')
			on.exit({
				tkconfigure(main.win, cursor = '')
				tcl('update')
			})

			nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
			if(!is.null(nc.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'netcdf'
				AllOpenFilesData[[nopf+1]] <<- nc.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(ncdf.file) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.ncfl, values = unlist(listOpenFiles), textvariable = ncdf.file)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
			}else return(NULL)
		})

		tkgrid(txt.ncfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.ncfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.ncfl, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		##############################################

		frameMap <- tkframe(subfr1)

		bt.nc.MapOpt <- ttkbutton(frameMap, text = "Options", width = largeur1)
		bt.nc.maps <- ttkbutton(frameMap, text = "PLOT", width = largeur1)

		###################

		EnvOneNCDFPlot$ncMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.nc.MapOpt, command = function(){
			if(!is.null(EnvOneNCDFPlot$ncData$map)){
				atlevel <- pretty(EnvOneNCDFPlot$ncData$map$z, n = 10, min.n = 7)
				if(is.null(EnvOneNCDFPlot$ncMapOp$userLvl$levels)){
					EnvOneNCDFPlot$ncMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvOneNCDFPlot$ncMapOp$userLvl$custom)
						EnvOneNCDFPlot$ncMapOp$userLvl$levels <- atlevel
				}
			}
			EnvOneNCDFPlot$ncMapOp <- MapGraph.MapOptions(main.win, EnvOneNCDFPlot$ncMapOp)
		})

		###################

		EnvOneNCDFPlot$notebookTab.dataNCMap <- NULL

		tkconfigure(bt.nc.maps, command = function(){
			if(str_trim(tclvalue(ncdf.file)) != ""){
				ret <- try(get.NCDF.DATA(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- OneNCDF.Display.Map(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvOneNCDFPlot$notebookTab.dataNCMap, AllOpenTabType, AllOpenTabData)
				EnvOneNCDFPlot$notebookTab.dataNCMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkgrid(bt.nc.MapOpt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(bt.nc.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		##############################################

		frameBlank <- tkframe(subfr1)

		blankGrid <- tclVar(0)

		chk.grid <- tkcheckbutton(frameBlank, variable = blankGrid, text = 'Blank grid outside the boundaries', anchor = 'w', justify = 'left')
		tkgrid(chk.grid)

		tkbind(chk.grid, "<Button-1>", function(){
			if(tclvalue(blankGrid) == "1"){
				stateSHP <- if(tclvalue(EnvOneNCDFPlot$shp$add.shp) == "0") "disabled" else "normal"
			}else stateSHP <- "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
		})

		##############################################

		frameSHP <- ttklabelframe(subfr1, text = "Boundaries", relief = 'groove')

		EnvOneNCDFPlot$shp$add.shp <- tclVar(0)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvOneNCDFPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur0, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(main.win, all.opfiles)
			if(!is.null(shp.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'shp'
				AllOpenFilesData[[nopf+1]] <<- shp.opfiles
				tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
				tkconfigure(cb.ncfl, values = unlist(listOpenFiles), textvariable = ncdf.file)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvOneNCDFPlot$shp$ocrds <- NULL
				EnvOneNCDFPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvOneNCDFPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvOneNCDFPlot$SHPOp <- MapGraph.GraphOptions.LineSHP(main.win, EnvOneNCDFPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		infobulle(cb.addshp, 'Select the file containing the ESRI shapefiles')
		status.bar.display(cb.addshp, TextOutputVar, 'Select the file containing the ESRI shapefiles')
		infobulle(bt.addshp, 'Browse file if not listed')
		status.bar.display(bt.addshp, TextOutputVar, 'Browse file if not listed')

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvOneNCDFPlot$shp$ocrds <- NULL
			EnvOneNCDFPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			if(tclvalue(EnvOneNCDFPlot$shp$add.shp) == "1"){
				stateSHP <- if(tclvalue(blankGrid) == "0") "disabled" else "normal"
			}else stateSHP <- "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)

			stateSHP1 <- if(tclvalue(EnvOneNCDFPlot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(bt.addshpOpt, state = stateSHP1)
		})

		############################################

		tkgrid(frameNC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameMap, row = 1, column = 0, sticky = '', padx = 1, pady = 5, ipadx = 1, ipady = 1)
		tkgrid(frameBlank, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	get.NCDF.DATA <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		loaded.nc <- list(str_trim(tclvalue(ncdf.file)), tclvalue(blankGrid))
		if(is.null(EnvOneNCDFPlot$loaded.nc)){
			getNCFiles <- TRUE
		}else{
			getNCFiles <- if(!isTRUE(all.equal(EnvOneNCDFPlot$loaded.nc, loaded.nc))) TRUE else FALSE
		}

		if(getNCFiles){
			ncdata <- getNcdfOpenData(str_trim(tclvalue(ncdf.file)))
			EnvOneNCDFPlot$ncData$map$x <- ncdata[[2]]$x
			EnvOneNCDFPlot$ncData$map$y <- ncdata[[2]]$y

			if(tclvalue(blankGrid) == "1"){
				shpdata <- getShpOpenData(file.plotShp)[[2]]
				if(is.null(shpdata)){
					InsertMessagesTxt(main.txt.out, "No shapefiles found", format = TRUE)
					return(NULL)
				}
				nc.grid <- list(lon = ncdata[[2]]$x, lat = ncdata[[2]]$y)
				shpdata[['vtmp']] <- 1
				mask <- over(defSpatialPixels(nc.grid), shpdata)[, 'vtmp']
				dim(mask) <- sapply(nc.grid, length)
				EnvOneNCDFPlot$ncData$map$z <- ncdata[[2]]$value * mask
			}else EnvOneNCDFPlot$ncData$map$z <- ncdata[[2]]$value

			EnvOneNCDFPlot$ncData$file2plot <- ncdata[[1]]
			EnvOneNCDFPlot$loaded.nc <- loaded.nc
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
