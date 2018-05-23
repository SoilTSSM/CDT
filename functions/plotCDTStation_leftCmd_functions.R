
PlotCDTStationCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(26)/sfont0)
		largeur3 <- 20
		largeur4 <- 26
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(48.5)
		largeur0 <- as.integer(w.scale(14)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur3 <- 14
		largeur4 <- 20
	}

	GeneralParameters <- list(intstep = "dekadal", cdtstation = "",
							date = list(year = 2017, mon = 1, day = 1, other = ""))

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Plot CDT Station Data")

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

		frameCDTdata <- ttklabelframe(subfr1, text = "Station Data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data', 'Others')
		tclvalue(timeSteps) <- switch(GeneralParameters$intstep, 
										'daily' = CbperiodVAL[1],
										'pentad' = CbperiodVAL[2],
										'dekadal' = CbperiodVAL[3],
										'monthly' = CbperiodVAL[4],
										'others' = CbperiodVAL[5])
		input.file <- tclVar(GeneralParameters$cdtstation)

		txt.cdtdata1 <- tklabel(frameCDTdata, text = "Time step", anchor = 'w', justify = 'left')
		cb.cdtdata1 <- ttkcombobox(frameCDTdata, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)
		txt.cdtdata2 <- tklabel(frameCDTdata, text = 'File containing CDT stations data', anchor = 'w', justify = 'left')
		cb.cdtdata2 <- ttkcombobox(frameCDTdata, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		bt.cdtdata <- tkbutton(frameCDTdata, text = "...")

		tkconfigure(bt.cdtdata, command = function(){
			dat.opfiles <- getOpenFiles(main.win, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.cdtdata2, values = unlist(listOpenFiles), textvariable = input.file)

				ret <- try(splitStnData(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)){
					tclvalue(input.file) <- ""
					EnvCDTStationPlot$don <- NULL
					return(NULL)
				}
			}else{
				tclvalue(input.file) <- ""
				EnvCDTStationPlot$don <- NULL
				return(NULL)
			}
		})

		############

		tkgrid(txt.cdtdata1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.cdtdata1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.cdtdata2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.cdtdata2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.cdtdata, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############

		tkbind(cb.cdtdata1, "<<ComboboxSelected>>", function(){
			tkdestroy(frTS1)
			frTS1 <<- tkframe(frameMap)

			if(str_trim(tclvalue(timeSteps)) == 'Others'){
				txt.other <- tklabel(frTS1, text = 'Dates or Index')
				# en.other <- tkentry(frTS1, width = 16, textvariable = date.other, justify = "center")
				cb.other <- ttkcombobox(frTS1, values = "", textvariable = date.other, width = 16)
				bt.date.prev <- ttkbutton(frTS1, text = "<<", width = 6)
				bt.date.next <- ttkbutton(frTS1, text = ">>", width = 6)

				tkgrid(txt.other, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
				# tkgrid(en.other, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(cb.other, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(bt.date.next, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
			}else{
				stateday <- if(str_trim(tclvalue(timeSteps)) == 'Monthly data') 'disabled' else 'normal'

				txtdek <- switch(str_trim(tclvalue(timeSteps)), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
				day.txtVar <- tclVar(txtdek)

				txt.yrs <- tklabel(frTS1, text = 'Year')
				txt.mon <- tklabel(frTS1, text = 'Month')
				txt.day <- tklabel(frTS1, text = tclvalue(day.txtVar), textvariable = day.txtVar)
				en.yrs <- tkentry(frTS1, width = 5, textvariable = date.year, justify = "center")
				en.mon <- tkentry(frTS1, width = 5, textvariable = date.mon, justify = "center")
				en.day <- tkentry(frTS1, width = 5, textvariable = date.day, justify = "center", state = stateday)
				bt.date.prev <- ttkbutton(frTS1, text = "<<", width = 6)
				bt.date.next <- ttkbutton(frTS1, text = ">>", width = 6)

				##############
				tkgrid(txt.yrs, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(txt.mon, row = 0, column = 2, sticky = 'we', pady = 1, padx = 1)
				tkgrid(txt.day, row = 0, column = 3, sticky = 'we', pady = 1, padx = 1)

				tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
				tkgrid(en.yrs, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(en.mon, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
				tkgrid(en.day, row = 1, column = 3, sticky = 'we', pady = 1, padx = 1)
				tkgrid(bt.date.next, row = 1, column = 4, sticky = 'we', pady = 1, padx = 1)
			}

			##############
			tkconfigure(bt.date.prev, command = function(){
				if(is.null(EnvCDTStationPlot$don)) return(NULL) 
				temps <- str_trim(tclvalue(timeSteps))

				if(temps == 'Others'){
					##
				}else{
					yrs <- as.numeric(str_trim(tclvalue(date.year)))
					mon <- as.numeric(str_trim(tclvalue(date.mon)))
					dpk <- as.numeric(str_trim(tclvalue(date.day)))

					if(temps == 'Daily data') todaty <- paste(yrs, mon, dpk, sep = '-')
					if(temps == 'Pentad data'){
						if(is.na(dpk) | dpk < 1 | dpk > 6){
							InsertMessagesTxt(main.txt.out, "Pentad must be  between 1 and 6", format = TRUE)
							return(NULL)
						}
						todaty <- paste(yrs, mon, dpk, sep = '-')
					}
					if(temps == 'Dekadal data'){
						if(is.na(dpk) | dpk < 1 | dpk > 3){
							InsertMessagesTxt(main.txt.out, "Dekad must be 1, 2 or 3", format = TRUE)
							return(NULL)
						}
						todaty <- paste(yrs, mon, dpk, sep = '-')
					}
					if(temps == 'Monthly data') todaty <- paste(yrs, mon, 1, sep = '-')

					daty <- try(as.Date(todaty), silent = TRUE)
					if(inherits(daty, "try-error") | is.na(daty)){
						InsertMessagesTxt(main.txt.out, paste("Date invalid", todaty), format = TRUE)
						return(NULL)
					}
					if(temps == 'Daily data') daty <- daty-1
					if(temps == 'Pentad data') daty <- addPentads(daty, -1)
					if(temps == 'Dekadal data') daty <- addDekads(daty, -1)
					if(temps == 'Monthly data') daty <- addMonths(daty, -1)

					if(daty < EnvCDTStationPlot$first.date) daty <- EnvCDTStationPlot$last.date
					daty <- format(daty, '%Y%m%d')
					tclvalue(date.year) <- as.numeric(substr(daty, 1, 4))
					tclvalue(date.mon) <- as.numeric(substr(daty, 5, 6))
					tclvalue(date.day) <- as.numeric(substr(daty, 7, 8))
				}

				######
				getStnMap()

				####
				imgContainer <- CDTdataStation.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvCDTStationPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			})

			tkconfigure(bt.date.next, command = function(){
				if(is.null(EnvCDTStationPlot$don)) return(NULL) 
				temps <- str_trim(tclvalue(timeSteps))

				if(temps == 'Others'){
					##
				}else{
					yrs <- as.numeric(str_trim(tclvalue(date.year)))
					mon <- as.numeric(str_trim(tclvalue(date.mon)))
					dpk <- as.numeric(str_trim(tclvalue(date.day)))

					if(temps == 'Pentad data'){
						if(is.na(dpk) | dpk < 1 | dpk > 6){
							InsertMessagesTxt(main.txt.out, "Pentad must be  between 1 and 6", format = TRUE)
							return(NULL)
						}
					}
					if(temps == 'Dekadal data'){
						if(is.na(dpk) | dpk < 1 | dpk > 3){
							InsertMessagesTxt(main.txt.out, "Dekad must be 1, 2 or 3", format = TRUE)
							return(NULL)
						}
					}
					if(temps == 'Monthly data') dpk <- 1

					todaty <- paste(yrs, mon, dpk, sep = '-')
					daty <- try(as.Date(todaty), silent = TRUE)
					if(inherits(daty, "try-error") | is.na(daty)){
						InsertMessagesTxt(main.txt.out, paste("Invalid date", todaty), format = TRUE)
						return(NULL)
					}
					if(temps == 'Daily data') daty <- daty+1
					if(temps == 'Pentad data') daty <- addPentads(daty, 1)
					if(temps == 'Dekadal data') daty <- addDekads(daty, 1)
					if(temps == 'Monthly data') daty <- addMonths(daty, 1)

					if(daty > EnvCDTStationPlot$last.date) daty <- EnvCDTStationPlot$first.date
					daty <- format(daty, '%Y%m%d')
					tclvalue(date.year) <- as.numeric(substr(daty, 1, 4))
					tclvalue(date.mon) <- as.numeric(substr(daty, 5, 6))
					tclvalue(date.day) <- as.numeric(substr(daty, 7, 8))
				}

				######
				getStnMap()

				####
				imgContainer <- CDTdataStation.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvCDTStationPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			})

				tkgrid(frTS1, row = 1, column = 0, sticky = '', pady = 1, columnspan = 3)
			})

		tkbind(cb.cdtdata2, "<<ComboboxSelected>>", function(){
			ret <- try(splitStnData(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)){
				tclvalue(input.file) <- ""
				EnvCDTStationPlot$don <- NULL
				return(NULL)
			}
		})

		##############################################

		frameMap <- ttklabelframe(subfr1, text = "Map", relief = 'groove')

		typeMapPLOT <- c("Points", "Pixels")
		EnvCDTStationPlot$map$typeMap <- tclVar("Points")
		pointSizeI <- 0.7

		cb.Map.type <- ttkcombobox(frameMap, values = typeMapPLOT, textvariable = EnvCDTStationPlot$map$typeMap, width = largeur3)
		bt.Map.plot <- ttkbutton(frameMap, text = "PLOT", width = 7)
		bt.Map.Opt <- ttkbutton(frameMap, text = "Options", width = 8)

		##############
		EnvCDTStationPlot$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'),
												pointSize = pointSizeI)

		tkconfigure(bt.Map.Opt, command = function(){
			if(!is.null(EnvCDTStationPlot$stndata$map)){
				atlevel <- pretty(EnvCDTStationPlot$stndata$map$z, n = 10, min.n = 7)
				if(is.null(EnvCDTStationPlot$dataMapOp$userLvl$levels)){
					EnvCDTStationPlot$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvCDTStationPlot$dataMapOp$userLvl$custom)
						EnvCDTStationPlot$dataMapOp$userLvl$levels <- atlevel
				}
			}
			EnvCDTStationPlot$dataMapOp <- MapGraph.MapOptions(main.win, EnvCDTStationPlot$dataMapOp)
			if(str_trim(tclvalue(EnvCDTStationPlot$map$typeMap)) == "Points")
				pointSizeI <<- EnvCDTStationPlot$dataMapOp$pointSize
		})

		EnvCDTStationPlot$notebookTab.dataMap <- NULL

		tkconfigure(bt.Map.plot, command = function(){
			if(is.null(EnvCDTStationPlot$don)) return(NULL)
			getStnMap()

			####
			imgContainer <- CDTdataStation.Display.Maps(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
			EnvCDTStationPlot$notebookTab.dataMap <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		})

		##############
		tkbind(cb.Map.type, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(EnvCDTStationPlot$map$typeMap)) == "Points"){
				EnvCDTStationPlot$dataMapOp$pointSize <- pointSizeI
			}else EnvCDTStationPlot$dataMapOp$pointSize <- NULL

			getStnMap()
		})

		##############

		frTS1 <- tkframe(frameMap)

		date.year <- tclVar(GeneralParameters$date$year)
		date.mon <- tclVar(GeneralParameters$date$mon)
		date.day <- tclVar(GeneralParameters$date$day)
		date.other <- tclVar(GeneralParameters$date$other)

		if(GeneralParameters$intstep == 'others'){
			txt.other <- tklabel(frTS1, text = 'Dates or Index')
			cb.other <- ttkcombobox(frTS1, values = "", textvariable = date.other, width = 16)
			bt.date.prev <- ttkbutton(frTS1, text = "<<", width = 6)
			bt.date.next <- ttkbutton(frTS1, text = ">>", width = 6)

			tkgrid(txt.other, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
			tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
			tkgrid(cb.other, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
			tkgrid(bt.date.next, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
		}else{
			txtdek <- switch(GeneralParameters$intstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
			day.txtVar <- tclVar(txtdek)
			stateday <- if(GeneralParameters$intstep == 'monthly') 'disabled' else 'normal'

			txt.yrs <- tklabel(frTS1, text = 'Year')
			txt.mon <- tklabel(frTS1, text = 'Month')
			txt.day <- tklabel(frTS1, text = tclvalue(day.txtVar), textvariable = day.txtVar)
			en.yrs <- tkentry(frTS1, width = 5, textvariable = date.year, justify = "center")
			en.mon <- tkentry(frTS1, width = 5, textvariable = date.mon, justify = "center")
			en.day <- tkentry(frTS1, width = 5, textvariable = date.day, justify = "center", state = stateday)
			bt.date.prev <- ttkbutton(frTS1, text = "<<", width = 6)
			bt.date.next <- ttkbutton(frTS1, text = ">>", width = 6)

			##############
			tkgrid(txt.yrs, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
			tkgrid(txt.mon, row = 0, column = 2, sticky = 'we', pady = 1, padx = 1)
			tkgrid(txt.day, row = 0, column = 3, sticky = 'we', pady = 1, padx = 1)

			tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
			tkgrid(en.yrs, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
			tkgrid(en.mon, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
			tkgrid(en.day, row = 1, column = 3, sticky = 'we', pady = 1, padx = 1)
			tkgrid(bt.date.next, row = 1, column = 4, sticky = 'we', pady = 1, padx = 1)
		}

		##############
		tkconfigure(bt.date.prev, command = function(){
			if(is.null(EnvCDTStationPlot$don)) return(NULL) 
			temps <- str_trim(tclvalue(timeSteps))

			if(temps == 'Others'){
				idaty <- which(EnvCDTStationPlot$don$dates == str_trim(tclvalue(date.other)))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(EnvCDTStationPlot$don$dates)
				tclvalue(date.other) <- EnvCDTStationPlot$don$dates[idaty]
			}else{
				yrs <- as.numeric(str_trim(tclvalue(date.year)))
				mon <- as.numeric(str_trim(tclvalue(date.mon)))
				dpk <- as.numeric(str_trim(tclvalue(date.day)))

				if(temps == 'Daily data') todaty <- paste(yrs, mon, dpk, sep = '-')
				if(temps == 'Pentad data'){
					if(is.na(dpk) | dpk < 1 | dpk > 6){
						InsertMessagesTxt(main.txt.out, "Pentad must be  between 1 and 6", format = TRUE)
						return(NULL)
					}
					todaty <- paste(yrs, mon, dpk, sep = '-')
				}
				if(temps == 'Dekadal data'){
					if(is.na(dpk) | dpk < 1 | dpk > 3){
						InsertMessagesTxt(main.txt.out, "Dekad must be 1, 2 or 3", format = TRUE)
						return(NULL)
					}
					todaty <- paste(yrs, mon, dpk, sep = '-')
				}
				if(temps == 'Monthly data') todaty <- paste(yrs, mon, 1, sep = '-')

				daty <- try(as.Date(todaty), silent = TRUE)
				if(inherits(daty, "try-error") | is.na(daty)){
					InsertMessagesTxt(main.txt.out, paste("Date invalid", todaty), format = TRUE)
					return(NULL)
				}
				if(temps == 'Daily data') daty <- daty-1
				if(temps == 'Pentad data') daty <- addPentads(daty, -1)
				if(temps == 'Dekadal data') daty <- addDekads(daty, -1)
				if(temps == 'Monthly data') daty <- addMonths(daty, -1)

				if(daty < EnvCDTStationPlot$first.date) daty <- EnvCDTStationPlot$last.date
				daty <- format(daty, '%Y%m%d')
				tclvalue(date.year) <- as.numeric(substr(daty, 1, 4))
				tclvalue(date.mon) <- as.numeric(substr(daty, 5, 6))
				tclvalue(date.day) <- as.numeric(substr(daty, 7, 8))
			}

			######
			getStnMap()

			####
			imgContainer <- CDTdataStation.Display.Maps(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
			EnvCDTStationPlot$notebookTab.dataMap <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		})

		tkconfigure(bt.date.next, command = function(){
			if(is.null(EnvCDTStationPlot$don)) return(NULL) 
			temps <- str_trim(tclvalue(timeSteps))

			if(temps == 'Others'){
				idaty <- which(EnvCDTStationPlot$don$dates == str_trim(tclvalue(date.other)))
				idaty <- idaty+1
				if(idaty > length(EnvCDTStationPlot$don$dates)) idaty <- 1
				tclvalue(date.other) <- EnvCDTStationPlot$don$dates[idaty]
			}else{
				yrs <- as.numeric(str_trim(tclvalue(date.year)))
				mon <- as.numeric(str_trim(tclvalue(date.mon)))
				dpk <- as.numeric(str_trim(tclvalue(date.day)))

				if(temps == 'Pentad data'){
					if(is.na(dpk) | dpk < 1 | dpk > 6){
						InsertMessagesTxt(main.txt.out, "Pentad must be  between 1 and 6", format = TRUE)
						return(NULL)
					}
				}
				if(temps == 'Dekadal data'){
					if(is.na(dpk) | dpk < 1 | dpk > 3){
						InsertMessagesTxt(main.txt.out, "Dekad must be 1, 2 or 3", format = TRUE)
						return(NULL)
					}
				}
				if(temps == 'Monthly data') dpk <- 1

				todaty <- paste(yrs, mon, dpk, sep = '-')
				daty <- try(as.Date(todaty), silent = TRUE)
				if(inherits(daty, "try-error") | is.na(daty)){
					InsertMessagesTxt(main.txt.out, paste("Invalid date", todaty), format = TRUE)
					return(NULL)
				}
				if(temps == 'Daily data') daty <- daty+1
				if(temps == 'Pentad data') daty <- addPentads(daty, 1)
				if(temps == 'Dekadal data') daty <- addDekads(daty, 1)
				if(temps == 'Monthly data') daty <- addMonths(daty, 1)

				if(daty > EnvCDTStationPlot$last.date) daty <- EnvCDTStationPlot$first.date
				daty <- format(daty, '%Y%m%d')
				tclvalue(date.year) <- as.numeric(substr(daty, 1, 4))
				tclvalue(date.mon) <- as.numeric(substr(daty, 5, 6))
				tclvalue(date.day) <- as.numeric(substr(daty, 7, 8))
			}

			######
			getStnMap()

			####
			imgContainer <- CDTdataStation.Display.Maps(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
			EnvCDTStationPlot$notebookTab.dataMap <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		})

		##############
		tkgrid(cb.Map.type, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.Map.Opt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.Map.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(frTS1, row = 1, column = 0, sticky = '', pady = 1, columnspan = 3)

		##############################################

		frameGraph <- ttklabelframe(subfr1, text = "Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		EnvCDTStationPlot$graph$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameGraph, values = typeTSPLOT, textvariable = EnvCDTStationPlot$graph$typeTSp, width = largeur3)
		bt.TsGraph.plot <- ttkbutton(frameGraph, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameGraph, text = "Options", width = 8)

		EnvCDTStationPlot$TSGraphOp <- list(
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
			suffix.fun <- switch(str_trim(tclvalue(EnvCDTStationPlot$graph$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "Line")
			plot.fun <- match.fun(paste0("MapGraph.GraphOptions.", suffix.fun))
			EnvCDTStationPlot$TSGraphOp <- plot.fun(main.win, EnvCDTStationPlot$TSGraphOp)
		})

		EnvCDTStationPlot$notebookTab.dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(is.null(EnvCDTStationPlot$don)) return(NULL)
			getStnTS()

			####
			imgContainer <- CDTdataStation.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
			EnvCDTStationPlot$notebookTab.dataGraph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		})

		##############

		frTS2 <- tkframe(frameGraph)

		EnvCDTStationPlot$graph$stnIDTSp <- tclVar()

		bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 5)
		cb.stnID <- ttkcombobox(frTS2, values = "", textvariable = EnvCDTStationPlot$graph$stnIDTSp, width = largeur4)
		bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 5)
		tkgrid(bt.stnID.prev, cb.stnID, bt.stnID.next)

		##############
		tkconfigure(bt.stnID.prev, command = function(){
			if(is.null(EnvCDTStationPlot$don)) return(NULL)
			istn <- which(EnvCDTStationPlot$don$id == str_trim(tclvalue(EnvCDTStationPlot$graph$stnIDTSp)))
			istn <- istn-1
			if(istn < 1) istn <- length(EnvCDTStationPlot$don$id)
			tclvalue(EnvCDTStationPlot$graph$stnIDTSp) <- EnvCDTStationPlot$don$id[istn]

			getStnTS()

			####
			imgContainer <- CDTdataStation.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
			EnvCDTStationPlot$notebookTab.dataGraph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		})

		tkconfigure(bt.stnID.next, command = function(){
			if(is.null(EnvCDTStationPlot$don)) return(NULL)
			istn <- which(EnvCDTStationPlot$don$id == str_trim(tclvalue(EnvCDTStationPlot$graph$stnIDTSp)))
			istn <- istn+1
			if(istn > length(EnvCDTStationPlot$don$id)) istn <- 1
			tclvalue(EnvCDTStationPlot$graph$stnIDTSp) <- EnvCDTStationPlot$don$id[istn]

			getStnTS()

			####
			imgContainer <- CDTdataStation.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTStationPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
			EnvCDTStationPlot$notebookTab.dataGraph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		})

		##############
		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(frTS2, row = 1, column = 0, sticky = '', pady = 1, columnspan = 3)

		##############################################

		frameSHP <- ttklabelframe(subfr1, text = "Boundaries", relief = 'groove')

		EnvCDTStationPlot$shp$add.shp <- tclVar(0)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvCDTStationPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
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

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvCDTStationPlot$shp$ocrds <- NULL
				EnvCDTStationPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvCDTStationPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvCDTStationPlot$SHPOp <- MapGraph.GraphOptions.LineSHP(main.win, EnvCDTStationPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvCDTStationPlot$shp$ocrds <- NULL
			EnvCDTStationPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvCDTStationPlot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		############################################

		tkgrid(frameCDTdata, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	splitStnData <- function(){
		EnvCDTStationPlot$stndata <- NULL
		intstep <- switch(str_trim(tclvalue(timeSteps)), 
							'Daily data' = 'daily',
							'Pentad data' = 'pentad',
							'Dekadal data' =  'dekadal',
							'Monthly data' = 'monthly',
							'Others' = 'others')

		don <- getStnOpenData(str_trim(tclvalue(input.file)))
		if(is.null(don)) return(NULL)

		if(intstep == "others"){
			don <- splitCDTData1(don)
			EnvCDTStationPlot$tsdates <- seq_along(don$dates)

			##########
			tkconfigure(cb.other, values = don$dates)
			tclvalue(date.other) <- don$dates[1]
		}else{
			don <- getCDTdataAndDisplayMsg(don, intstep)
			if(is.null(don)) return(NULL)

			##########
			en.daty <- don$dates[length(don$dates)]

			if(intstep == "daily"){
				EnvCDTStationPlot$tsdates <- as.Date(don$dates, "%Y%m%d")
				dpk <- as.numeric(substr(en.daty, 7, 8))
			}
			if(intstep == "pentad"){
				pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(don$dates, 7, 7))]
				EnvCDTStationPlot$tsdates <- as.Date(paste0(substr(don$dates, 1, 6), pen), "%Y%m%d")
				dpk <- as.numeric(substr(en.daty, 7, 7))
			}
			if(intstep == "dekadal"){
				dek <- c(1, 11, 21)[as.numeric(substr(don$dates, 7, 7))]
				EnvCDTStationPlot$tsdates <- as.Date(paste0(substr(don$dates, 1, 6), dek), "%Y%m%d")
				dpk <- as.numeric(substr(en.daty, 7, 7))
			}
			if(intstep == "monthly"){
				EnvCDTStationPlot$tsdates <- as.Date(paste0(don$dates, 1), "%Y%m%d")
				dpk <- 1
			}

			first.date <- if(intstep == "monthly") paste0(don$dates[1], 1) else don$dates[1]
			last.date <- if(intstep == "monthly") paste0(don$dates[length(don$dates)], 1) else don$dates[length(don$dates)]
			EnvCDTStationPlot$first.date <- as.Date(first.date, "%Y%m%d")
			EnvCDTStationPlot$last.date <- as.Date(last.date, "%Y%m%d")

			##########
			tclvalue(date.year) <- as.numeric(substr(en.daty, 1, 4))
			tclvalue(date.mon) <- as.numeric(substr(en.daty, 5, 6))
			tclvalue(date.day) <- dpk
		}

		##########
		tkconfigure(cb.stnID, values = don$id)
		tclvalue(EnvCDTStationPlot$graph$stnIDTSp) <- don$id[1]

		EnvCDTStationPlot$tstep <- intstep
		EnvCDTStationPlot$don <- don

		##########
		getStnTS()
		getStnMap()
		return(0)
	}

	getStnTS <- function(){
		istn <- which(EnvCDTStationPlot$don$id == str_trim(tclvalue(EnvCDTStationPlot$graph$stnIDTSp)))
		if(length(istn) == 0){
			EnvCDTStationPlot$stndata$series <- NULL
			InsertMessagesTxt(main.txt.out, paste(str_trim(tclvalue(EnvCDTStationPlot$graph$stnIDTSp)), "doesn't exist"), format = TRUE)
		}else{
			EnvCDTStationPlot$stndata$series$ts <- EnvCDTStationPlot$don$data[, istn]
			EnvCDTStationPlot$stndata$series$id <- str_trim(tclvalue(EnvCDTStationPlot$graph$stnIDTSp))
		}
	}

	getStnMap <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		typemap <- str_trim(tclvalue(EnvCDTStationPlot$map$typeMap))

		if(EnvCDTStationPlot$tstep != "others"){
			yrs <- as.numeric(str_trim(tclvalue(date.year)))
			mon <- as.numeric(str_trim(tclvalue(date.mon)))
			dpk <- as.numeric(str_trim(tclvalue(date.day)))
			getSpat <- list(yrs, mon, dpk, typemap)
		}else getSpat <- list(str_trim(tclvalue(date.other)), typemap)

		if(!is.null(EnvCDTStationPlot$stndata$spatial)){
			formatSpData <- if(!isTRUE(all.equal(EnvCDTStationPlot$stndata$spatial, getSpat))) TRUE else FALSE
		}else formatSpData <- TRUE

		if(formatSpData){
			if(EnvCDTStationPlot$tstep != "others"){
				if(EnvCDTStationPlot$tstep == "daily")
					daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m%d")
				if(EnvCDTStationPlot$tstep == "pentad"){
					pen <- as.Date(paste(yrs, mon, dpk, sep = "-"))
					daty <- paste0(format(pen, "%Y%m"), dpk)
				}
				if(EnvCDTStationPlot$tstep == "dekadal"){
					dek <- as.Date(paste(yrs, mon, dpk, sep = "-"))
					daty <- paste0(format(dek, "%Y%m"), dpk)
				}
				if(EnvCDTStationPlot$tstep == "monthly")
					daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m")
			}else daty <- str_trim(tclvalue(date.other))

			idaty <- which(EnvCDTStationPlot$don$dates == daty)

			if(length(idaty) == 0){
				EnvCDTStationPlot$stndata$map <- NULL
				InsertMessagesTxt(main.txt.out, "Invalid date or index", format = TRUE)
			}else{
				if(typemap == "Points"){
					EnvCDTStationPlot$stndata$map$x <- EnvCDTStationPlot$don$lon
					EnvCDTStationPlot$stndata$map$y <- EnvCDTStationPlot$don$lat
					EnvCDTStationPlot$stndata$map$z <- as.numeric(EnvCDTStationPlot$don$data[idaty, ])
				}

				if(typemap == "Pixels"){
					nx <- nx_ny_as.image(diff(range(EnvCDTStationPlot$don$lon)))
					ny <- nx_ny_as.image(diff(range(EnvCDTStationPlot$don$lat)))
					tmp <- cdt.as.image(as.numeric(EnvCDTStationPlot$don$data[idaty, ]), nx = nx, ny = ny,
										pts.xy = cbind(EnvCDTStationPlot$don$lon, EnvCDTStationPlot$don$lat))
					EnvCDTStationPlot$stndata$map$x <- tmp$x
					EnvCDTStationPlot$stndata$map$y <- tmp$y
					EnvCDTStationPlot$stndata$map$z <- tmp$z
				}

				EnvCDTStationPlot$stndata$map$t <- daty
				EnvCDTStationPlot$stndata$map$p <- typemap
			}

			EnvCDTStationPlot$stndata$spatial <- getSpat
		}
	}

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}
