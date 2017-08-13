AggregateNcdf_GetInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 42
		largeur1 <- 46
		largeur2 <- 13
		largeur3 <- 29
	}else{
		largeur <- 32
		largeur1 <- 34
		largeur2 <- 11
		largeur3 <- 31
	}

	###################

	infoOpenNC <- function(ncfichier){
		ncDataInfo <- getRFESampleData(ncfichier)
		ncDataInfo <- SpatialPixels(points = ncDataInfo$rfeGrd, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
		nxy <- ncDataInfo@grid
		minlon <- nxy@cellcentre.offset["lon"]
		reslon <- nxy@cellsize["lon"]
		maxlon <- minlon+reslon*(nxy@cells.dim["lon"]-1)
		minlat <- nxy@cellcentre.offset["lat"]
		reslat <- nxy@cellsize["lat"]
		maxlat <- minlat+reslat*(nxy@cells.dim["lat"]-1)

		if(tclvalue(use.ncgrid) == '0'){
			tclvalue(minLon) <- round(minlon, 6)
			tclvalue(maxLon) <- round(maxlon, 6)
			# tclvalue(resLon) <- round(reslon, 6)
			tclvalue(minLat) <- round(minlat, 6)
			tclvalue(maxLat) <- round(maxlat, 6)
			# tclvalue(resLat) <- round(reslat, 6)
		}

		tkconfigure(txta.ncinfo, state = "normal")
		tkdelete(txta.ncinfo, "0.0", "end")
		tkinsert(txta.ncinfo, "end", paste("min.lon:", round(minlon, 6), "/ max.lon:",
				round(maxlon, 6), "/ res.lon:", round(reslon, 6), "\n"), "txtfonttag")
		tkinsert(txta.ncinfo, "end", paste("min.lat:", round(minlat, 6), "/ max.lat:",
				round(maxlat, 6), "/ res.lat:", round(reslat, 6)), "txtfonttag")
		tkconfigure(txta.ncinfo, state = "disabled")
	}

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frNCDF <- tkframe(frLeft, relief = "sunken", borderwidth = 2)

	frNCDATA <- ttklabelframe(frNCDF, text = "NetCDF Data", relief = 'groove')

	nbcnfile <- tclVar()
	NBNCF <- c('One NetCDF file', 'Several NetCDF files')
	tclvalue(nbcnfile) <- switch(GeneralParameters$nb.ncfile, 
									'one' = NBNCF[1],
									'several' = NBNCF[2])
	ncfiledir <- tclVar(GeneralParameters$ncdf$fileordir)
	ncsample <- tclVar(GeneralParameters$ncdf$sample)

	statesample <- if(GeneralParameters$nb.ncfile == "one") "disabled" else "normal"
	txtfiledir <- if(GeneralParameters$nb.ncfile == "one") "NetCDF data file" else "Directory containing the netcdf data (*.nc)"
	fileINdir <- tclVar(txtfiledir)

	cb.nbncf <- ttkcombobox(frNCDATA, values = NBNCF, textvariable = nbcnfile, width = largeur)

	txt.ncfldir <- tklabel(frNCDATA, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	if(GeneralParameters$nb.ncfile == "one"){
		cb.ncfldir <- ttkcombobox(frNCDATA, values = unlist(listOpenFiles), textvariable = ncfiledir, width = largeur)
	}else{
		cb.ncfldir <- tkentry(frNCDATA, textvariable = ncfiledir, width = largeur1)
	}
	bt.ncfldir <- tkbutton(frNCDATA, text = "...")

	txt.ncsample <- tklabel(frNCDATA, text = "NetCDF data sample file", anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frNCDATA, values = unlist(listOpenFiles), textvariable = ncsample, width = largeur, state = statesample)
	bt.ncsample <- tkbutton(frNCDATA, text = "...", state = statesample)

	####
	frNCINFO <- tkframe(frNCDF, relief = 'groove', borderwidth = 2)

	txta.ncinfo <- tktext(frNCINFO, bg = "white", font = "courier", cursor = "", wrap = "word", height = 2, width = largeur3)
	txtafont <- tkfont.create(family = "times", size = 9)
	tktag.configure(txta.ncinfo, "txtfonttag", font = txtafont)

	###################

	tkconfigure(bt.ncfldir, command = function(){
		if(tclvalue(nbcnfile) == 'One NetCDF file'){
			nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'netcdf'
				AllOpenFilesData[[nopf+1]] <<- nc.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(ncfiledir) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.ncfldir, values = unlist(listOpenFiles), textvariable = ncfiledir)
				tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = ncsample)
				tkconfigure(cb.ncgrid, values = unlist(listOpenFiles), textvariable = file.ncgrid)

				####
				infoOpenNC(tclvalue(ncfiledir))
			}else return(NULL)
		}else{
			file2convert <- tk_choose.dir(getwd(), "")
			tclvalue(ncfiledir) <- if(!is.na(file2convert)) file2convert else ""
		}
	})

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(tclvalue(ncfiledir))) tclvalue(ncfiledir) else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(ncsample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = ncsample)
			tkconfigure(cb.ncgrid, values = unlist(listOpenFiles), textvariable = file.ncgrid)

			####
			infoOpenNC(tclvalue(ncsample))
		}else return(NULL)
	})

	###################

	tkgrid(cb.nbncf, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncfldir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncfldir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncfldir, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncsample, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	###################
	tkgrid(txta.ncinfo, row = 5, column = 0, sticky = "nsew", rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################

	tkgrid(frNCDATA, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frNCINFO, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################
	tkbind(cb.nbncf, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.ncfldir)
		tclvalue(ncfiledir) <- ''

		if(tclvalue(nbcnfile) == 'One NetCDF file'){
			tclvalue(fileINdir) <- "NetCDF data file"

			cb.ncfldir <- ttkcombobox(frNCDATA, values = unlist(listOpenFiles), textvariable = ncfiledir, width = largeur)

			#######

			tkconfigure(bt.ncfldir, command = function(){
				nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = getwd())
				if(!is.null(nc.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'netcdf'
					AllOpenFilesData[[nopf+1]] <<- nc.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(ncfiledir) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.ncfldir, values = unlist(listOpenFiles), textvariable = ncfiledir)
					tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = ncsample)
					tkconfigure(cb.ncgrid, values = unlist(listOpenFiles), textvariable = file.ncgrid)

					####
					infoOpenNC(tclvalue(ncfiledir))
				}else return(NULL)
			})

			tkconfigure(cb.ncsample, state = 'disabled')
			tkconfigure(bt.ncsample, state = 'disabled')
		}


		if(tclvalue(nbcnfile) == 'Several NetCDF files'){
			tclvalue(fileINdir) <- "Directory containing the netcdf data (*.nc)"

			cb.ncfldir <- tkentry(frNCDATA, textvariable = ncfiledir, width = largeur1)

			#######
			tkconfigure(bt.ncfldir, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(ncfiledir) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(cb.ncsample, state = 'normal')
			tkconfigure(bt.ncsample, state = 'normal')
		}

		#######
		tkgrid(cb.ncfldir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		if(tclvalue(nbcnfile) == 'One NetCDF file'){
			tkbind(cb.ncfldir, "<<ComboboxSelected>>", function(){
				infoOpenNC(tclvalue(ncfiledir))
			})
		}

		tkfocus(tt)
	})

	if(tclvalue(nbcnfile) == 'One NetCDF file'){
		tkbind(cb.ncfldir, "<<ComboboxSelected>>", function(){
			infoOpenNC(tclvalue(ncfiledir))
		})
	}

	tkbind(cb.ncsample, "<<ComboboxSelected>>", function(){
		infoOpenNC(tclvalue(ncsample))
	})

	############################################

	frNCGRID <- tkframe(frLeft, relief = "sunken", borderwidth = 2)

	use.ncgrid <- tclVar(GeneralParameters$ncdf.grid$use.ncgrid)
	file.ncgrid <- tclVar(GeneralParameters$ncdf.grid$file)

	statencgrid <- if(GeneralParameters$ncdf.grid$use.ncgrid) 'normal' else 'disabled'

	chk.ncgrid <- tkcheckbutton(frNCGRID, variable = use.ncgrid, text =  "Use Grid from other NetCDF data", anchor = 'w', justify = 'left')
	frncgrid <- tkframe(frNCGRID, relief = 'groove', borderwidth = 2)

	txt.ncgrid <- tklabel(frncgrid, text = "NetCDF file to be used", anchor = 'w', justify = 'left')
	cb.ncgrid <- ttkcombobox(frncgrid, values = unlist(listOpenFiles), textvariable = file.ncgrid, width = largeur, state = statencgrid)
	bt.ncgrid <- tkbutton(frncgrid, text = "...", state = statencgrid)

	########
	tkconfigure(bt.ncgrid, command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = getwd())
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.ncgrid) <- AllOpenFilesData[[nopf+1]][[1]]

			if(tclvalue(nbcnfile) == 'One NetCDF file') tkconfigure(cb.ncfldir, values = unlist(listOpenFiles), textvariable = ncfiledir)
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = ncsample)
			tkconfigure(cb.ncgrid, values = unlist(listOpenFiles), textvariable = file.ncgrid)

			####
			ncDataInfo <- getRFESampleData(tclvalue(file.ncgrid))
			ncDataInfo <- SpatialPixels(points = ncDataInfo$rfeGrd, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
			nxy <- ncDataInfo@grid
			minlon <- nxy@cellcentre.offset["lon"]
			reslon <- nxy@cellsize["lon"]
			maxlon <- minlon+reslon*(nxy@cells.dim["lon"]-1)
			minlat <- nxy@cellcentre.offset["lat"]
			reslat <- nxy@cellsize["lat"]
			maxlat <- minlat+reslat*(nxy@cells.dim["lat"]-1)

			tclvalue(minLon) <- round(minlon, 6)
			tclvalue(maxLon) <- round(maxlon, 6)
			tclvalue(resLon) <- round(reslon, 6)
			tclvalue(minLat) <- round(minlat, 6)
			tclvalue(maxLat) <- round(maxlat, 6)
			tclvalue(resLat) <- round(reslat, 6)
		}else return(NULL)
	})


	tkgrid(chk.ncgrid, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frncgrid, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncgrid, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncgrid, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncgrid, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	###################

	tkbind(chk.ncgrid, "<Button-1>", function(){
		statencgrid <- if(tclvalue(use.ncgrid) == '0') "normal" else "disabled"
		tkconfigure(cb.ncgrid, state = statencgrid)
		tkconfigure(bt.ncgrid, state = statencgrid)

		statemethod <- if(tclvalue(use.ncgrid) == '0') "disabled" else "normal"
		tkconfigure(cb.but, state = statemethod)

		METHODS <- if(tclvalue(but) == "Aggregate" & tclvalue(use.ncgrid) == '1') c('mean', 'bilinear') else 'bilinear'
		tkconfigure(cb.method, values = METHODS)
		tclvalue(method) <- if(tclvalue(but) == "Aggregate" & tclvalue(use.ncgrid) == '1') tclvalue(method) else 'bilinear'

		stateDefGrid <- if(tclvalue(use.ncgrid) == '0') "disabled" else "normal"
		tkconfigure(grd_vlon1, state = stateDefGrid)
		tkconfigure(grd_vlon2, state = stateDefGrid)
		tkconfigure(grd_vlon3, state = stateDefGrid)
		tkconfigure(grd_vlat1, state = stateDefGrid)
		tkconfigure(grd_vlat2, state = stateDefGrid)
		tkconfigure(grd_vlat3, state = stateDefGrid)
	})

	tkbind(cb.ncgrid, "<<ComboboxSelected>>", function(){
		ncDataInfo <- getRFESampleData(tclvalue(file.ncgrid))
		ncDataInfo <- SpatialPixels(points = ncDataInfo$rfeGrd, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
		nxy <- ncDataInfo@grid
		minlon <- nxy@cellcentre.offset["lon"]
		reslon <- nxy@cellsize["lon"]
		maxlon <- minlon+reslon*(nxy@cells.dim["lon"]-1)
		minlat <- nxy@cellcentre.offset["lat"]
		reslat <- nxy@cellsize["lat"]
		maxlat <- minlat+reslat*(nxy@cells.dim["lat"]-1)

		tclvalue(minLon) <- round(minlon, 6)
		tclvalue(maxLon) <- round(maxlon, 6)
		tclvalue(resLon) <- round(reslon, 6)
		tclvalue(minLat) <- round(minlat, 6)
		tclvalue(maxLat) <- round(maxlat, 6)
		tclvalue(resLat) <- round(reslat, 6)
	})

	############################################
	tkgrid(frNCDF, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frNCGRID, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	##############################################  RIGHT   ############################################

	frMTH <- tkframe(frRight, relief = "sunken", borderwidth = 2)

	but <- tclVar(GeneralParameters$but)
	method <- tclVar(GeneralParameters$method)

	METHODS <- if(GeneralParameters$but == "Aggregate" & tclvalue(use.ncgrid) == '0') c('mean', 'bilinear') else 'bilinear'
	tclvalue(method) <- if(GeneralParameters$but == "Aggregate" & tclvalue(use.ncgrid) == '0') tclvalue(method) else 'bilinear'

	statemethod <- if(GeneralParameters$ncdf.grid$use.ncgrid) 'disabled' else 'normal'

	txt.Aggreg <- tklabel(frMTH, text = "Aggregation method", anchor = 'w', justify = 'left')
	txt.but <- tklabel(frMTH, text = "Action", anchor = 'w', justify = 'left')
	cb.but <- ttkcombobox(frMTH, values = c("Aggregate", "Disaggregate"), textvariable = but, width = largeur2, state = statemethod)
	txt.method <- tklabel(frMTH, text = "Method", anchor = 'w', justify = 'left')
	cb.method <- ttkcombobox(frMTH, values = METHODS, textvariable = method, width = largeur2)

	tkgrid(txt.Aggreg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.but, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.but, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.method, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.method, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	###################

	tkbind(cb.but, "<<ComboboxSelected>>", function(){
		METHODS <- if(tclvalue(but) == "Aggregate") c('mean', 'bilinear') else 'bilinear'
		tkconfigure(cb.method, values = METHODS)
		tclvalue(method) <- if(tclvalue(but) == "Aggregate") tclvalue(method) else 'bilinear'
	})

	############################################

	frRES <- tkframe(frRight, relief = "sunken", borderwidth = 2)

	minLon <- tclVar(GeneralParameters$res$minlon)
	maxLon <- tclVar(GeneralParameters$res$maxlon)
	resLon <- tclVar(GeneralParameters$res$reslon)
	minLat <- tclVar(GeneralParameters$res$minlat)
	maxLat <- tclVar(GeneralParameters$res$maxlat)
	resLat <- tclVar(GeneralParameters$res$reslat)

	stateDefGrid <- if(GeneralParameters$ncdf.grid$use.ncgrid) 'disabled' else 'normal'

	txt.newgrid <- tklabel(frRES, text = "New grid boundaries and resolutions", anchor = 'w', justify = 'left')

	grd_llon <- tklabel(frRES, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(frRES, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(frRES, text = "Min")
	grd_lb2 <- tklabel(frRES, text = "Max")
	grd_lb3 <- tklabel(frRES, text = "Res")

	grd_vlon1 <- tkentry(frRES, width = 8, justify = "right", textvariable = minLon, state = stateDefGrid)
	grd_vlon2 <- tkentry(frRES, width = 8, justify = "right", textvariable = maxLon, state = stateDefGrid)
	grd_vlon3 <- tkentry(frRES, width = 8, justify = "right", textvariable = resLon, state = stateDefGrid)
	grd_vlat1 <- tkentry(frRES, width = 8, justify = "right", textvariable = minLat, state = stateDefGrid)
	grd_vlat2 <- tkentry(frRES, width = 8, justify = "right", textvariable = maxLat, state = stateDefGrid)
	grd_vlat3 <- tkentry(frRES, width = 8, justify = "right", textvariable = resLat, state = stateDefGrid)

	tkgrid(txt.newgrid, row = 0, column = 0, sticky = "ew", rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(grd_lb1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 3, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 3, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 3, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 3, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(grd_vlon1, 'Minimum longitude in degree decimal')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in degree decimal')
	infobulle(grd_vlon2, 'Maximum longitude in degree decimal')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in degree decimal')
	infobulle(grd_vlon3, 'Resolution in degree decimal')
	status.bar.display(grd_vlon3, TextOutputVar, 'Resolution in degree decimal')
	infobulle(grd_vlat1, 'Minimum latitude in degree decimal')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in degree decimal')
	infobulle(grd_vlat2, 'Maximum latitude in degree decimal')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in degree decimal')
	infobulle(grd_vlat3, 'Resolution in degree decimal')
	status.bar.display(grd_vlat3, TextOutputVar, 'Resolution in degree decimal')

	############################################

	frSave <- tkframe(frRight, relief = "sunken", borderwidth = 2)

	dir2save <- tclVar(GeneralParameters$output)

	txt.dir2save <- tklabel(frSave, text = 'Directory to save results', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur1)
	bt.dir2save <- tkbutton(frSave, text = "...")

	#####

	tkconfigure(bt.dir2save, command = function(){
		initialdir <- if(str_trim(GeneralParameters$output) != "") GeneralParameters$output else getwd()
		dir2savepth <- tk_choose.dir(initialdir, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- initialdir
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to save results')
	status.bar.display(en.dir2save, TextOutputVar, 'Enter the full path to directory to save results')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frMTH, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
	tkgrid(frRES, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$nb.ncfile <<- switch(str_trim(tclvalue(nbcnfile)),
												'One NetCDF file' = 'one',
												'Several NetCDF files' = 'several')
		GeneralParameters$ncdf$fileordir <<- str_trim(tclvalue(ncfiledir))
		GeneralParameters$ncdf$sample <<- str_trim(tclvalue(ncsample))

		GeneralParameters$ncdf.grid$use.ncgrid <<- switch(tclvalue(use.ncgrid), '0' = FALSE, '1' = TRUE)
		GeneralParameters$ncdf.grid$file <<- str_trim(tclvalue(file.ncgrid))

		GeneralParameters$but <<- str_trim(tclvalue(but))
		GeneralParameters$method <<- str_trim(tclvalue(method))

		GeneralParameters$res$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
		GeneralParameters$res$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
		GeneralParameters$res$reslon <<- as.numeric(str_trim(tclvalue(resLon)))
		GeneralParameters$res$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
		GeneralParameters$res$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))
		GeneralParameters$res$reslat <<- as.numeric(str_trim(tclvalue(resLat)))

		GeneralParameters$output <<- str_trim(tclvalue(dir2save))

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	####
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###########################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Regridding - NetCDF Data')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(GeneralParameters)
}

