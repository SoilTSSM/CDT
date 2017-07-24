
getInfoNetcdfData <- function(tt, Parameters, ncDIR, tstep = 'Dekadal data', scale = FALSE){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	if(scale){
		scale.tstep <- tclVar()
		tclvalue(scale.tstep) <- switch(Parameters$tstep,
										'pentad' = 'Pentad data',
										'dekadal' = 'Dekadal data',
										'monthly' = 'Monthly data')
		
		TSTEPVAL <- c('Pentad data', 'Dekadal data', 'Monthly data')
		if(tstep == 'Pentad data') TSTEPVAL <- c('Dekadal data', 'Monthly data')
		if(tstep == 'Dekadal data') TSTEPVAL <- c('Monthly data', '')
		cb.scale.tstep <- ttkcombobox(frFF, values = TSTEPVAL, textvariable = scale.tstep, width = largeur1)
	}

	inrfeff <- tclVar(Parameters$format)
	rfesample <- tclVar(Parameters$sample)

	txt.ncsample <- tklabel(frFF, text = "Netcdf data sample file", anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = 'Netcdf data filenames format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

	###################

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(rfesample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = rfesample)
		}else return(NULL)
	})

	###################

	if(scale) tkgrid(cb.scale.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	ddk <- if(tstep%in%c('Pentad data', 'Dekadal data')) 1 else '01'
	example <- do.call(sprintf, c(list(fmt = Parameters$format),
				as.list(c(1981, '01', ddk)[seq(length(gregexpr('%s', Parameters$format)[[1]]))])))

	status.bar.display(cb.ncsample, TextOutputVar, 'File containing a sample of the data in netcdf')
	infobulle(bt.ncsample, 'Browse file if not listed')
	infobulle(en.inrfeff, paste('Enter the filename format of netcdf data, example:', example))
	status.bar.display(en.inrfeff, TextOutputVar, paste('Enter the filename format of netcdf data, example:', example))

	###################

	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(rfesample)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			Parameters$format <<- str_trim(tclvalue(inrfeff))
			Parameters$sample <<- str_trim(tclvalue(rfesample))

			if(scale){
				Parameters$tstep <<- switch(str_trim(tclvalue(scale.tstep)),
												'Pentad data' = 'pentad',
												'Dekadal data' = 'dekadal',
												'Monthly data' = 'monthly')
				lenS <- length(gregexpr('%s', Parameters$format)[[1]])
				if((Parameters$tstep%in%c('pentad', 'dekadal') & lenS != 3) |
					(Parameters$tstep == 'monthly' & lenS != 2))
				{
					tkmessageBox(message = "Wrong filename format", icon = "warning", type = "ok")
					tkwait.window(tt1)
				}
			}

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(tt)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(Parameters)
}

###################################################################################################

getInterpolationPars <- function(tt, Parameters, interpChoix = 0){

	largeur <- if(Sys.info()["sysname"] == "Windows") 38 else 36
	NNHelpFun <- function(){
		infobulle(en.pars1, 'Maximum distance belong longitude, n times of grid interpolation resolution')
		status.bar.display(en.pars1, TextOutputVar, 'Maximum distance belong longitude, n times of grid interpolation resolution')
		infobulle(en.pars2, 'Maximum distance belong latitude, n times of grid interpolation resolution')
		status.bar.display(en.pars2, TextOutputVar, 'Maximum distance belong latitude, n times of grid interpolation resolution')
		infobulle(en.pars3, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')
		status.bar.display(en.pars3, TextOutputVar, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')
	}
	GstatHelpFun <- function(){
		infobulle(en.pars1, 'Minimum number of neighbors to be used to interpolate data')
		status.bar.display(en.pars1, TextOutputVar, 'Minimum number of neighbors to be used to interpolate data')
		infobulle(en.pars2, 'Maximum number of neighbors to be used to interpolate data')
		status.bar.display(en.pars2, TextOutputVar, 'Maximum number of neighbors to be used to interpolate data')
		infobulle(en.pars3, 'Maximum distance (in decimal degree) to be used to interpolate data')
		status.bar.display(en.pars3, TextOutputVar, 'Maximum distance (in decimal degree) to be used to interpolate data')
	}

	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frInterp <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	interp.method <- tclVar()
	cb.InterpVAL <- c('Inverse Distance Weighted', 'Kriging')
	if(interpChoix == 1) cb.InterpVAL <- c(cb.InterpVAL, 'Nearest Neighbor')
	if(interpChoix == 2) cb.InterpVAL <- c(cb.InterpVAL, 'Fast Bilinear Interpolator')
	tclvalue(interp.method) <- switch(Parameters$interp.method, 
										'IDW' = cb.InterpVAL[1],
										'Kriging' = cb.InterpVAL[2],
										'NN' = cb.InterpVAL[3],
										'FBL' = cb.InterpVAL[3])

	frInterpMthd <- ttklabelframe(frInterp, text = 'Interpolation method', relief = 'groove', borderwidth = 2)
	cb.Interp <- ttkcombobox(frInterpMthd, values = cb.InterpVAL, textvariable = interp.method, width = largeur)
	tkgrid(cb.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	tkbind(cb.Interp, "<<ComboboxSelected>>", function(){
		tkconfigure(frInterpPars, text = tclvalue(interp.method))

		texts <- if(tclvalue(interp.method) == 'Nearest Neighbor') texts1 else texts2
		tclvalue(txtvar1) <- texts[1]
		tclvalue(txtvar2) <- texts[2]
		tclvalue(txtvar3) <- texts[3]

		tkconfigure(txt.Lab1, text = tclvalue(txtvar1))
		tkconfigure(txt.Lab2, text = tclvalue(txtvar2))
		tkconfigure(txt.Lab3, text = tclvalue(txtvar3))

		statePars <- if(interpChoix == 2 & tclvalue(interp.method) == 'Fast Bilinear Interpolator') 'disabled' else 'normal'
		tkconfigure(en.pars1, state = statePars)
		tkconfigure(en.pars2, state = statePars)
		tkconfigure(en.pars3, state = statePars)

		varpars <<- if(tclvalue(interp.method) == 'Nearest Neighbor') varpars1 else varpars2
		if((parsenv$previous[1]%in%c('Inverse Distance Weighted', 'Kriging') & tclvalue(interp.method) == 'Nearest Neighbor') |
		(parsenv$previous[1] == 'Nearest Neighbor' & tclvalue(interp.method)%in%c('Inverse Distance Weighted', 'Kriging')))
		{
			tmp1 <- Parameters[[varpars[1]]]
			tmp2 <- Parameters[[varpars[2]]]
			tmp3 <- Parameters[[varpars[3]]]
		}else{
			tmp1 <- parsenv$previous[2]
			tmp2 <- parsenv$previous[3]
			tmp3 <- parsenv$previous[4]
		}
		tclvalue(parsenv$pars1) <- tmp1
		tclvalue(parsenv$pars2) <- tmp2
		tclvalue(parsenv$pars3) <- tmp3

		parsenv$previous <- c(tclvalue(interp.method), tclvalue(parsenv$pars1),
								tclvalue(parsenv$pars2), tclvalue(parsenv$pars3))
		if(tclvalue(interp.method) == 'Nearest Neighbor') NNHelpFun() else GstatHelpFun()
	})

	tkbind(cb.Interp, "<Button-1>", function(){
		parsenv$previous <- c(tclvalue(interp.method), tclvalue(parsenv$pars1),
							tclvalue(parsenv$pars2),  tclvalue(parsenv$pars3))
	})

	#####
	frInterpPars <- ttklabelframe(frInterp, text = tclvalue(interp.method), relief = 'groove', borderwidth = 2)

	parsenv <- new.env()

	texts1 <- c('Multi.Lon', 'Multi.Lat', 'Multi.Elv')
	varpars1 <- c('rad.lon', 'rad.lat', 'rad.elv')
	varpars2 <- texts2 <- c('nmin', 'nmax', 'maxdist')
	texts <- if(Parameters$interp.method == 'NN') texts1 else texts2
	varpars <- if(Parameters$interp.method == 'NN') varpars1 else varpars2
	txtvar1 <- tclVar(texts[1])
	txtvar2 <- tclVar(texts[2])
	txtvar3 <- tclVar(texts[3])
	parsenv$pars1 <- tclVar(Parameters[[varpars[1]]])
	parsenv$pars2 <- tclVar(Parameters[[varpars[2]]])
	parsenv$pars3 <- tclVar(Parameters[[varpars[3]]])
	parsenv$previous <- c(tclvalue(interp.method), tclvalue(parsenv$pars1),
						tclvalue(parsenv$pars2), tclvalue(parsenv$pars3))

	statePars <- if(interpChoix == 2 & Parameters$interp.method == 'FBL') 'disabled' else 'normal'

	txt.Lab1 <- tklabel(frInterpPars, text = tclvalue(txtvar1), anchor = 'e', justify = 'right')
	txt.Lab2 <- tklabel(frInterpPars, text = tclvalue(txtvar2), anchor = 'e', justify = 'right')
	txt.Lab3 <- tklabel(frInterpPars, text = tclvalue(txtvar2), anchor = 'e', justify = 'right')
	en.pars1 <- tkentry(frInterpPars, width = 4, textvariable = parsenv$pars1, justify = 'right', state = statePars)
	en.pars2 <- tkentry(frInterpPars, width = 4, textvariable = parsenv$pars2, justify = 'right', state = statePars)
	en.pars3 <- tkentry(frInterpPars, width = 4, textvariable = parsenv$pars3, justify = 'right', state = statePars)

	tkgrid(txt.Lab1, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(en.pars1, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(txt.Lab2, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(en.pars2, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(txt.Lab3, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(en.pars3, row = 0, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	if(Parameters$interp.method == 'NN') NNHelpFun() else GstatHelpFun()

	########
	tkgrid(frInterpMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterpPars, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########
	tkgrid(frInterp, row = 0, column = 0, sticky = 'snwe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		Parameters[[varpars[1]]] <<- as.numeric(str_trim(tclvalue(parsenv$pars1)))
		Parameters[[varpars[2]]] <<- as.numeric(str_trim(tclvalue(parsenv$pars2)))
		Parameters[[varpars[3]]] <<- as.numeric(str_trim(tclvalue(parsenv$pars3)))
		Parameters$interp.method <<- switch(str_trim(tclvalue(interp.method)),
										'Inverse Distance Weighted' = 'IDW',
										'Kriging' = 'Kriging',
										'Nearest Neighbor' = 'NN',
										'Fast Bilinear Interpolator' = 'FBL')
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Interpolation parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(Parameters)
}

###################################################################################################


getNewGridParams <- function(tt, Parameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt1)

	################################

	fr_grd <- ttklabelframe(frGrd0, text = "Create new grid", relief = "groove", borderwidth = 2)

	minLon <- tclVar(Parameters$minlon)
	maxLon <- tclVar(Parameters$maxlon)
	resLon <- tclVar(Parameters$reslon)
	minLat <- tclVar(Parameters$minlat)
	maxLat <- tclVar(Parameters$maxlat)
	resLat <- tclVar(Parameters$reslat)

	grd_llon <- tklabel(fr_grd, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(fr_grd, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = "Min")
	grd_lb2 <- tklabel(fr_grd, text = "Max")
	grd_lb3 <- tklabel(fr_grd, text = "Res")

	grd_vlon1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLon)
	grd_vlon3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLon)
	grd_vlat1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLat)
	grd_vlat3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLat)

	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 0, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

	################################

	tkgrid(fr_grd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frGrd1, text=" OK ")
	bt.prm.CA <- tkbutton(frGrd1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
		Parameters$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
		Parameters$reslon <<- as.numeric(str_trim(tclvalue(resLon)))
		Parameters$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
		Parameters$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))
		Parameters$reslat <<- as.numeric(str_trim(tclvalue(resLat)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Grid Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(Parameters)
}

