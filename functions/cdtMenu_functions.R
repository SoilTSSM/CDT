
##########xxxxxxxxxxxxxxxxxx Menu File xxxxxxxxxxxxxxxxxx##########

menu.file <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "File", menu = menu.file, activebackground = 'lightblue')

	##########
	tkadd(menu.file, "command", label = "Open data.frame", command = function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
		}else return(NULL)
	})

	##########
	tkadd(menu.file, "command", label = "Open Netcdf file", command = function(){
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
		}else return(NULL)
	})

	##########
	tkadd(menu.file, "command", label = "Open ESRI Shapefile", command = function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
		}else return(NULL)
	})

	##########
	tkadd(menu.file, "separator")

	##########
	tkadd(menu.file, "command", label = "Open QC/Homogenization session", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state()
		OpenOldCDTtask()
	})

	##########
	tkadd(menu.file, "command", label = "Save QC/Homogenization session", command = function() saveCurrentCDTtask())

	##########
	tkadd(menu.file, "separator")

	##########
	tkadd(menu.file, "command", label = "Save table", command = function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		if(!is.null(ReturnExecResults)){
			tab2sav <- try(SaveNotebookTabArray(tknotes), silent = TRUE)
			if(!inherits(tab2sav, "try-error")){
				InsertMessagesTxt(main.txt.out, "Table saved successfully")
			}else{
				InsertMessagesTxt(main.txt.out, "The table could not be saved", format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', tab2sav[1]), format = TRUE)
				return(NULL)
			}
		}else return(NULL)
	 })

	##########
	tkadd(menu.file, "command", label = "Save table As...", command = function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
		if(!is.na(tabid)){
			if(AllOpenTabType[[tabid]]%in%c("arr", "arrAssess")){
				filetypes <- "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
				if(Sys.info()["sysname"] == "Windows") file.to.save <- tclvalue(tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes, defaultextension = TRUE))
				else file.to.save <- tclvalue(tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				Objarray <- AllOpenTabData[[tabid]][[2]]

				dat2sav <- tclArray2dataframe(Objarray)
				colnoms <- if(AllOpenTabType[[tabid]] == "arr") TRUE else FALSE
				writeFiles(dat2sav, file.to.save, col.names = colnoms)
			}else return(NULL)
		}else return(NULL)
	})

	##########
	tkadd(menu.file, "command", label = "Save Image As...", command = function() SavePlot())

	##########
	tkadd(menu.file, "separator")

	##########
	tkadd(menu.file, "command", label = "Configurations", command = function() configCDT())

	##########
	tkadd(menu.file, "separator")

	##########
	tkadd(menu.file, "command", label = "Quit CDT", command = function(){
		on.exit({
			#sink(type = "message")
			#close(msgOUT)
			options(warn = 0)
		})
		tkdestroy(main.win)
	})

##########xxxxxxxxxxxxxxxxxx Menu Data Preparation xxxxxxxxxxxxxxxxxx##########

menu.dataprep <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Data Preparation", menu = menu.dataprep, activebackground = 'lightblue')

	##########
	tkadd(menu.dataprep, "command", label = "Format CDTs Input Data", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('cdtInput.stn', 'daily')
		GeneralParameters <<- AggregateInputStationData(main.win, initpars)
	})

	##########
	tkadd(menu.dataprep, "command", label = "Merge two CDT Data", command = function(){
		refreshCDT.lcmd.env()
		if(is.null(lcmd.frame_merge2cdt)){
			lcmd.frame <<- merge2CDTDataPanelCmd()
			lcmd.frame_merge2cdt <<- 1
		}
	})

	##########
	tkadd(menu.dataprep, "command", label = "Filtering CDT Data", command = function(){
		refreshCDT.lcmd.env()
		if(is.null(lcmd.frame_filtercdtdat)){
			lcmd.frame <<- filterCDTDataPanelCmd()
			lcmd.frame_filtercdtdat <<- 1
		}
	})

	################ Create CDT dataset for data analysis
	tkadd(menu.dataprep, "separator")

	##########
	tkadd(menu.dataprep, "command", label = "Transform NetCDF files into CDT Dataset", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('create.CdtDataset', 'dekadal')
		GeneralParameters <<- cdtDataset_getParams(main.win, initpars)
	})

	############## DOWNLOAD
	tkadd(menu.dataprep, "separator")

	##########
	tkadd(menu.dataprep, "command", label = "Download DEM", command = function(){
		refreshCDT.lcmd.env()
		getDEMFun(main.win)
	})

	##########
	tkadd(menu.dataprep, "command", label = "Download Country boundary", command = function(){
		refreshCDT.lcmd.env()
		getCountryShapefile(main.win)
	})

	##########
	tkadd(menu.dataprep, "command", label = "Download RFE data", command = function(){
		refreshCDT.lcmd.env()
		DownloadRFE(main.win)
	})

	################ FILL TEMP
	tkadd(menu.dataprep, "separator")

	##########
	tkadd(menu.dataprep, "command", label = "Filling missing dekadal temperature values", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('fill.temp', 'dekadal')
		GeneralParameters <<- fillMissDekTemp(main.win, initpars)
	})

	################## CONVERSION
	tkadd(menu.dataprep, "separator")

	##########
	menu.dataConv <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.dataprep, "cascade", label = "Data Format Conversion", menu = menu.dataConv)

		########
		# CPT
		tkadd(menu.dataConv, "command", label = "Conversion to CPT data format", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('convert.CPTdata', 'daily')
			GeneralParameters <<- CPT.convert_getParams(main.win, initpars)
		})

		##########
		tkadd(menu.dataConv, "separator")

		########
		# Raster 
		tkadd(menu.dataConv, "command", label = "Converting NetCDF/GeoTIFF/ESRI .hdr Labelled", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('convert.nc.tif.bil', 'daily')
			GeneralParameters <<- rasterData.convert_getParams(main.win, initpars)
		})

		##########
		tkadd(menu.dataConv, "separator")

		########
		# Grads
		tkadd(menu.dataConv, "command", label = "Create GrADS Descriptor File from CDT NetCDF files", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('grads.ctl', 'daily')
			GeneralParameters <<- grads_create.ctl_getParams(main.win, initpars)
		})

##########xxxxxxxxxxxxxxxxxx Data Aggregation xxxxxxxxxxxxxxxxxx##########

menu.AggrData <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Aggregate Data", menu = menu.AggrData, activebackground = 'lightblue')

	##########
	tkadd(menu.AggrData, "command", label = "Aggregating Time Series", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('aggregate.ts', 'daily', TRUE)
		GeneralParameters <<- AggregateTS_GetInfo(main.win, initpars)
	})

	###########
	tkadd(menu.AggrData, "separator")

	##########
	tkadd(menu.AggrData, "command", label = "Aggregate/Disaggregate Spatial NetCDF data", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('aggregate.nc', 'daily')
		GeneralParameters <<- AggregateNcdf_GetInfo(main.win, initpars)
	})

##########xxxxxxxxxxxxxxxxxx Menu Quality Control xxxxxxxxxxxxxxxxxx##########

menu.qchom <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Quality Control", menu = menu.qchom, activebackground = 'lightblue')

	########## Availability
	tkadd(menu.qchom, "command", label = "Assess Data Availability", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state()
		if(is.null(lcmd.frame_assdata)){
			lcmd.frame <<- AssessDataPanelCmd()
			lcmd.frame_assdata <<- 1
		}
	})

	##########
	tkadd(menu.qchom, "separator")

	############ check coordninates
	tkadd(menu.qchom, "command", label = "Check Stations Coordinates", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('chk.coords', 'daily')
		GeneralParameters <<- excludeOutStn(main.win, initpars)
	})

	##########
	tkadd(menu.qchom, "separator")

	##########
	##QC one Run
	menu.qc <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.qchom, "cascade", label = "QC Check: One station", menu = menu.qc)

		##########Rain
		menu.qcRain <- tkmenu(menu.qc, tearoff = FALSE)
		tkadd(menu.qc, "cascade", label = "Rainfall", menu = menu.qcRain)

			##Zeros check
			tkadd(menu.qcRain, "command", label = "False-Zeros check", command = function(){
				refreshCDT.lcmd.env()
				lchoixStnFr <<- selectStationCmd()
				initpars <- initialize.parameters('zero.check', 'daily')
				initpars$AllOrOne <- 'one'
				GeneralParameters <<- qcGetZeroCheckInfo(main.win, initpars)
			})

			##Outliers check
			tkadd(menu.qcRain, "command", label = "Outliers Check", command = function(){
				refreshCDT.lcmd.env()
				lchoixStnFr <<- selectStationCmd()
				initpars <- initialize.parameters('qc.rain', 'daily', TRUE)
				initpars$AllOrOne <- 'one'
				GeneralParameters <<- qc.get.info.rain(main.win, initpars)
			 })

		##########
		tkadd(menu.qc, "separator")

		##########Temperatures
		tkadd(menu.qc, "command", label = "Temperatures", command = function(){
			refreshCDT.lcmd.env()
			lchoixStnFr <<- selectStationCmd()
			initpars <- initialize.parameters('qc.temp', 'daily', TRUE)
			initpars$AllOrOne <- 'one'
			GeneralParameters <<- qc.get.info.txtn(main.win, initpars)
		})

	##########
	##Qc all Run
	menu.qc1 <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.qchom, "cascade", label = "QC Check: All stations", menu = menu.qc1)

		##########Rain
		menu.qcRain1 <- tkmenu(menu.qc1, tearoff = FALSE)
		tkadd(menu.qc1, "cascade", label = "Rainfall", menu = menu.qcRain1)

			##Zeros check
			tkadd(menu.qcRain1, "command", label = "False-Zeros check", command = function(){
				refreshCDT.lcmd.env()
				lchoixStnFr <<- selectStationCmd()
				initpars <- initialize.parameters('zero.check', 'daily')
				initpars$AllOrOne <- 'all'
				GeneralParameters <<- qcGetZeroCheckInfo(main.win, initpars)
			})

			##Outliers check
			tkadd(menu.qcRain1, "command", label = "Outliers Check", command = function(){
				refreshCDT.lcmd.env()
				lchoixStnFr <<- selectStationCmd()
				initpars <- initialize.parameters('qc.rain', 'daily', TRUE)
				initpars$AllOrOne <- 'all'
				GeneralParameters <<- qc.get.info.rain(main.win, initpars)
			 })

		##########
		tkadd(menu.qc1, "separator")

		##########Temperatures
		tkadd(menu.qc1, "command", label = "Temperatures", command = function(){
			refreshCDT.lcmd.env()
			lchoixStnFr <<- selectStationCmd()
			initpars <- initialize.parameters('qc.temp', 'daily', TRUE)
			initpars$AllOrOne <- 'all'
			GeneralParameters <<- qc.get.info.txtn(main.win, initpars)
		})

	##########
	tkadd(menu.qchom, "separator")

	##########
	tkadd(menu.qchom, "command", label = "Aggregate zeros checked stations", command = function(){
		refreshCDT.lcmd.env()
		initpars <- init.params('agg.zc', 'daily')
		GeneralParameters <<- AggregateOutputStationData(main.win, initpars)
	})

	##########
	tkadd(menu.qchom, "command", label = "Aggregate checked stations", command = function(){
		refreshCDT.lcmd.env()
		initpars <- init.params('agg.qc', 'daily')
		GeneralParameters <<- AggregateOutputStationData(main.win, initpars)
	})

	##########
	tkadd(menu.qchom, "separator")

	##########
	# Menu homogenization

	menu.homog <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.qchom, "cascade", label = "Homogeneity Test", menu = menu.homog)

		##########
		###RHtestsV4
		tkadd(menu.homog, "command", label = "RHtestsV4", command = function(){
			agreementfl <- file.path(apps.dir, 'configure', 'RHtestsV4_User_Agreement')
			if(file.exists(agreementfl)) proceed <- TRUE
			else{
				yesAgree <- RHtests_license()
				if(yesAgree){
					proceed <- TRUE
					cat("I Agree", file = agreementfl)
				}
				else proceed <- FALSE
			}
			if(proceed){
				GeneralParameters <<- initialize.parameters('rhtests', 'dekadal')
				refreshCDT.lcmd.env()
				lchoixStnFr <<- selectStationCmd()
				spinbox.state(state = 'normal')
				if(is.null(lcmd.frame_rhtests)){
					lcmd.frame <<- RHtestsV4Cmd()
					lcmd.frame_rhtests <<- 1
				}
			}
		})

		##########
		tkadd(menu.homog, "separator")

		#########
		###Methods used by CDT
		tkadd(menu.homog, "command", label = "CDT Homogenization Methods", command = function(){
			refreshCDT.lcmd.env()
			lchoixStnFr <<- selectStationCmd()
			initpars <- initialize.parameters('homog', 'dekadal')
			GeneralParameters <<- homogen.get.info(main.win, initpars)
		})

	##########
	tkadd(menu.qchom, "command", label = "Aggregate homogenized stations", command = function(){
		refreshCDT.lcmd.env()
		initpars <- init.params('agg.hom', 'dekadal')
		GeneralParameters <<- AggregateOutputStationData(main.win, initpars)
	})

##########xxxxxxxxxxxxxxxxxx Menu Merging Data xxxxxxxxxxxxxxxxxx##########

menu.mrg <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Merging Data", menu = menu.mrg, activebackground = 'lightblue')

	########################################
	tkadd(menu.mrg, "command", label = "Spatial  Interpolation", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_interpol)){
			lcmd.frame <<- InterpolationPanelCmd()
			lcmd.frame_interpol <<- 1
		}
	})

	######################################## DOWNSCALING
	tkadd(menu.mrg, "separator")

	##########temperature
	menu.mrg.down <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.mrg, "cascade", label = "Temperature - Reanalysis Downscaling", menu = menu.mrg.down)

		##########
		tkadd(menu.mrg.down, "command", label = "Compute Downscaling Coefficients", background = 'lightblue', command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('coefdown.temp', 'dekadal')
			GeneralParameters <<- Temp_coefDownGetInfo(main.win, initpars)
		})

		##########
		tkadd(menu.mrg.down, "separator")

		##########
		tkadd(menu.mrg.down, "command", label = "Reanalysis Downscaling", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('down.temp', 'dekadal')
			# GeneralParameters <<- downGetInfoDekTempReanal(main.win, initpars)
			GeneralParameters <<- Temp_reanalDownGetInfo(main.win, initpars)
		})

	######################################## SIMPLIFIED
	tkadd(menu.mrg, "separator")

	##########
	menu.mrg1 <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.mrg, "cascade", label = "Merging", menu = menu.mrg1, state = "normal")

		##########
		tkadd(menu.mrg1, "command", label = "Merging Rainfall", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('merge.rain.one', 'dekadal')
			GeneralParameters <<- Precip_mergeGetInfoALL(main.win, initpars)
		})

		##########
		tkadd(menu.mrg1, "separator")

		##########
		tkadd(menu.mrg1, "command", label = "Merging Temperature", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('merge.temp.one', 'dekadal')
			GeneralParameters <<- Temp_mergeGetInfoALL(main.win, initpars)
		})


	######################################## ADVANCED
	tkadd(menu.mrg, "separator")

	##########
	menu.mrg2 <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.mrg, "cascade", label = "Merging - Advanced", menu = menu.mrg2)

		##########rain advanced
		menu.mrg.rain2 <- tkmenu(top.menu, tearoff = FALSE)
		tkadd(menu.mrg2, "cascade", label = "Merging Rainfall", menu = menu.mrg.rain2)

			##########
			tkadd(menu.mrg.rain2, "command", label = "Compute mean Gauge-RFE bias", background = 'lightblue', command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('coefbias.rain', 'dekadal')
				GeneralParameters <<- coefBiasGetInfoRain(main.win, initpars)
			})

			##########
			tkadd(menu.mrg.rain2, "separator")

			##########
			tkadd(menu.mrg.rain2, "command", label = "Apply bias correction", command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('rmbias.rain', 'dekadal')
				GeneralParameters <<- rmvBiasGetInfoRain(main.win, initpars)
			})

			##########
			tkadd(menu.mrg.rain2, "separator")

			##########
			tkadd(menu.mrg.rain2, "command", label = "Compute Spatio-temporal Trend Coefficients", background = 'lightblue', command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('coefLM.rain', 'dekadal')
				GeneralParameters <<- coefLMGetInfoRain(main.win, initpars)
			})

			##########
			tkadd(menu.mrg.rain2, "separator")

			##########
			tkadd(menu.mrg.rain2, "command", label = "Merging Data", command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('merge.rain', 'dekadal')
				GeneralParameters <<- mergeGetInfoRain(main.win, initpars)
			})

		########################################
		tkadd(menu.mrg2, "separator")

		##########temperature advanced
		menu.mrg.temp2 <- tkmenu(top.menu, tearoff = FALSE)
		tkadd(menu.mrg2, "cascade", label = "Merging Temperature", menu = menu.mrg.temp2)

			##########
			tkadd(menu.mrg.temp2, "command", label = "Compute Bias Coefficients", background = 'lightblue', command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('coefbias.temp', 'dekadal')
				GeneralParameters <<- biasGetInfoTempDown(main.win, initpars)
			})

			##########
			tkadd(menu.mrg.temp2, "separator")

			##########
			tkadd(menu.mrg.temp2, "command", label = "Apply bias correction", command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('adjust.temp', 'dekadal')
				GeneralParameters <<- adjGetInfoTempDownReanal(main.win, initpars)
			})

			##########
			tkadd(menu.mrg.temp2, "separator")

			##########
			tkadd(menu.mrg.temp2, "command", label = "Compute Spatio-temporal Trend Coefficients", background = 'lightblue', command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('coefLM.temp', 'dekadal')
				GeneralParameters <<- coefLMGetInfoTemp(main.win, initpars)
			})

			##########
			tkadd(menu.mrg.temp2, "separator")

			##########
			tkadd(menu.mrg.temp2, "command", label = "Merging Data", command = function(){
				refreshCDT.lcmd.env()
				initpars <- initialize.parameters('merge.temp', 'dekadal')
				GeneralParameters <<- mrgGetInfoTemp(main.win, initpars)
			})

	######################################## SCALE MERGED DATA
	tkadd(menu.mrg, "separator")

	##########
	tkadd(menu.mrg, "command", label = "Scale merged data", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('scale.merged', 'daily')
		GeneralParameters <<- Merging_ScaleDataInfo(main.win, initpars)
	})

	######################################## UPDATE DEK
	tkadd(menu.mrg, "separator")

	##########
	tkadd(menu.mrg, "command", label = "Updating dekadal Rainfall", command = function(){
		refreshCDT.lcmd.env()
		initpars <- initialize.parameters('merge.dekrain', 'dekadal')
		GeneralParameters <<- mergeDekadInfoRain(main.win, initpars)
	})

	######################################## VALIDATION
	tkadd(menu.mrg, "separator")

	##########
	menu.valid <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.mrg, "cascade", label = "Validation method", menu = menu.valid)

		##########
		menu.valid1 <- tkmenu(top.menu, tearoff = FALSE)
		tkadd(menu.valid, "cascade", label = "Hold-Out Validation", menu = menu.valid1)

			########
			# Precipitation validation
			tkadd(menu.valid1, "command", label = "Precipitation", command = function(){
				refreshCDT.lcmd.env()
				spinbox.state(state = 'normal')
				if(is.null(lcmd.frame_valid.HOV)){
					lcmd.frame <<- Validation.HOV.PanelCmd('RR')
					lcmd.frame_valid.HOV <<- 1
				}
			})

			##########
			tkadd(menu.valid1, "separator")

			#########
			# Temperature validation
			tkadd(menu.valid1, "command", label = "Temperature", command = function(){
				refreshCDT.lcmd.env()
				spinbox.state(state = 'normal')
				if(is.null(lcmd.frame_valid.HOV)){
					lcmd.frame <<- Validation.HOV.PanelCmd('TT')
					lcmd.frame_valid.HOV <<- 1
				}
			})

		##########
		tkadd(menu.valid, "separator")

		##########
		menu.valid2 <- tkmenu(top.menu, tearoff = FALSE)
		tkadd(menu.valid, "cascade", label = "Leave-One-Out Cross-Validation", menu = menu.valid2)

			########
			# Precipitation validation
			tkadd(menu.valid2, "command", label = "Precipitation", command = function(){
				refreshCDT.lcmd.env()
				spinbox.state(state = 'normal')
				if(is.null(lcmd.frame_valid.LOOCV)){
					lcmd.frame <<- Validation.LOOCV.PanelCmd('RR')
					lcmd.frame_valid.LOOCV <<- 1
				}
			})

			##########
			tkadd(menu.valid2, "separator")

			#########
			# Temperature validation
			tkadd(menu.valid2, "command", label = "Temperature", command = function(){
				refreshCDT.lcmd.env()
				spinbox.state(state = 'normal')
				if(is.null(lcmd.frame_valid.LOOCV)){
					lcmd.frame <<- Validation.LOOCV.PanelCmd('TT')
					lcmd.frame_valid.LOOCV <<- 1
				}
			})

##########xxxxxxxxxxxxxxxxxx Menu Data Analysis xxxxxxxxxxxxxxxxxx##########

menu.dataproc <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Data Analysis", menu = menu.dataproc, activebackground = 'lightblue')

	######################################## EXTRACT
	tkadd(menu.dataproc, "command", label = "Data Extraction", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_extrdata)){
			lcmd.frame <<- ExtractDataPanelCmd()
			lcmd.frame_extrdata <<- 1
		}
	})

	######################################## SUMMARY STATs
	tkadd(menu.dataproc, "separator")

	##########

	tkadd(menu.dataproc, "command", label = "Summary Statistics", command = function(){
		## data type
		## CDTstations & CDTdataset
		## convert CDTdataset to CDTnetcdf (possibility to save CDTdataset output to ncdf)
		## 

		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_summariesData)){
			lcmd.frame <<- summariesDataPanelCmd()
			lcmd.frame_summariesData <<- 1
		}
	})

	######################################## Compute Climate Variables
	tkadd(menu.dataproc, "separator")

	##########

	menu.ClimVars <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.dataproc, "cascade", label = "Derived Climate Variables", menu = menu.ClimVars)

		########
		# Temp Var
		tkadd(menu.ClimVars, "command", label = "Temperature Variables", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('compute.dervTemp', 'daily')
			GeneralParameters <<- computeTvars_getParams(main.win, initpars)
		})

		##########
		tkadd(menu.ClimVars, "separator")

		########
		# PET 
		tkadd(menu.ClimVars, "command", label = "Potential Evapotranspiration", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('compute.PET', 'daily')
			GeneralParameters <<- computePET_getParams(main.win, initpars)
		})

		##########
		tkadd(menu.ClimVars, "separator")

		########
		# WB 
		tkadd(menu.ClimVars, "command", label = "Water Balance", command = function(){
			refreshCDT.lcmd.env()
			initpars <- initialize.parameters('compute.WB', 'daily')
			GeneralParameters <<- computeWB_getParams(main.win, initpars)
		})

	######################################## CLIMATOLOGIES COMPUTATIONS
	tkadd(menu.dataproc, "separator")

	##########

	menu.ClimCalc <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.dataproc, "cascade", label = "Compute Climatologies", menu = menu.ClimCalc)

		########
		# Climatologies 
		tkadd(menu.ClimCalc, "command", label = "Climatologies", command = function(){
			## Time steps: daily, pentad, dekadal, monthly
			## data type
			## CDTstations & (CDTdataset & CDTnetcdf)
			## CDTstations (time series & maps)
			## combine  CDTdataset & CDTnetcdf to get time series & maps

			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_climatoCalc)){
				lcmd.frame <<- climatologiesCalcPanelCmd()
				lcmd.frame_climatoCalc <<- 1
			}
		})

		##########
		tkadd(menu.ClimCalc, "separator")

		########
		# Anomalies 
		tkadd(menu.ClimCalc, "command", label = "Anomalies", command = function(){
			## Time steps: daily, pentad, dekadal, monthly
			## data type
			## CDTstations & (CDTdataset & CDTnetcdf)
			## CDTstations (time series & maps)
			## combine CDTdataset & CDTnetcdf to get time series & maps

			## input: climatologies deja calculer
			## input: choix Anomalies OU Standardized Anomalies

			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_anomaliesCalc)){
				lcmd.frame <<- anomaliesCalcPanelCmd()
				lcmd.frame_anomaliesCalc <<- 1
			}
		})

	######################################## CLIMATO
	tkadd(menu.dataproc, "separator")

	##########
	tkadd(menu.dataproc, "command", label = "Spatial Analysis", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_spatialAnalysis)){
			lcmd.frame <<- spatialAnalysisPanelCmd()
			lcmd.frame_spatialAnalysis <<- 1
		}
	})

	######################################## CLIMATO DAILY
	tkadd(menu.dataproc, "separator")

	##########
	tkadd(menu.dataproc, "command", label = "Daily Rainfall Analysis", command = function(){
		## data type
		## CDTstations & CDTdataset
		## possibility to save CDTdataset output to ncdf

		refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_dailyRainAnalysis)){
				lcmd.frame <<- dailyRainAnalysisPanelCmd()
				lcmd.frame_dailyRainAnalysis <<- 1
			}
	})

	######################################## RAINY SEASON
	tkadd(menu.dataproc, "separator")

	##########

	menu.rainySeas <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.dataproc, "cascade", label = "Rain Season Characteristics", menu = menu.rainySeas)

		########
		# Onset
		tkadd(menu.rainySeas, "command", label = "Season Onset", command = function(){
			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_onsetCalc)){
				lcmd.frame <<- OnsetCalcPanelCmd()
				lcmd.frame_onsetCalc <<- 1
			}
		})

		##########
		tkadd(menu.rainySeas, "separator")

		########
		# Cessation
		tkadd(menu.rainySeas, "command", label = "Season Cessation", command = function(){
			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_cessationCalc)){
				lcmd.frame <<- CessationCalcPanelCmd()
				lcmd.frame_cessationCalc <<- 1
			}
		})

		##########
		tkadd(menu.rainySeas, "separator")

		########
		# Length
		tkadd(menu.rainySeas, "command", label = "Season Length", command = function(){
			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_seaslengthCalc)){
				lcmd.frame <<- SeasonLengthCalcPanelCmd()
				lcmd.frame_seaslengthCalc <<- 1
			}
		})

	######################################## PICSA
	tkadd(menu.dataproc, "separator")

	##########
	tkadd(menu.dataproc, "command", label = "PICSA", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_PICSA)){
			lcmd.frame <<- PICSACalcPanelCmd()
			lcmd.frame_PICSA <<- 1
		}
	})

	######################################## CLIMDEX
	tkadd(menu.dataproc, "separator")

	##########

	menu.climdex <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.dataproc, "cascade", label = "Climate Extremes Indices", menu = menu.climdex)

		########
		# Precipitation 
		tkadd(menu.climdex, "command", label = "Precipitation", command = function(){
			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_climdexRR)){
				lcmd.frame <<- climdexPanelCmd.RR()
				lcmd.frame_climdexRR <<- 1
			}
		})

		##########
		tkadd(menu.climdex, "separator")

		#########
		# Temperature 
		tkadd(menu.climdex, "command", label = "Temperature", command = function(){
			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_climdexTT)){
				lcmd.frame <<- climdexPanelCmd.TT()
				lcmd.frame_climdexTT <<- 1
			}
		})

	######################################## Drought Indices
	tkadd(menu.dataproc, "separator")

	##########

	menu.drought <- tkmenu(top.menu, tearoff = FALSE)
	tkadd(menu.dataproc, "cascade", label = "Drought Indices", menu = menu.drought)

		########
		## Standardized Precipitation Index (SPI)
		## https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi
		tkadd(menu.drought, "command", label = "Standardized Precipitation Index (SPI)", command = function(){
			refreshCDT.lcmd.env()
			spinbox.state(state = 'normal')
			if(is.null(lcmd.frame_SPIData)){
				lcmd.frame <<- SPICalcPanelCmd()
				lcmd.frame_SPIData <<- 1
			}
		})

		##########
		# tkadd(menu.drought, "separator")

		#########
		# ## Standardized Precipitation Evapotranspiration Index (SPEI)
		# ## https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-evapotranspiration-index-spei
		# tkadd(menu.drought, "command", label = "Standardized Precipitation Evapotranspiration Index (SPEI)", command = function(){
		# 	refreshCDT.lcmd.env()
		# 	spinbox.state(state = 'normal')
		# 	if(is.null(lcmd.frame_SPEIData)){
		# 		lcmd.frame <<- SPEICalcPanelCmd()
		# 		lcmd.frame_SPEIData <<- 1
		# 	}
		# })

		##########
		# tkadd(menu.drought, "separator")

		#########
		## Palmer Drought Severity Index (PDSI)
		## https://climatedataguide.ucar.edu/climate-data/palmer-drought-severity-index-pdsi
		# tkadd(menu.drought, "command", label = "Palmer Drought Severity Index (PDSI)", command = function(){
		# 	refreshCDT.lcmd.env()
		# 	spinbox.state(state = 'normal')
		# 	if(is.null(lcmd.frame_PDSIData)){
		# 		lcmd.frame <<- PDSICalcPanelCmd()
		# 		lcmd.frame_PDSIData <<- 1
		# 	}
		# })

##########xxxxxxxxxxxxxxxxxx Menu Plot Data xxxxxxxxxxxxxxxxxx##########

menu.plot <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Plot Data", menu = menu.plot, activebackground = 'lightblue')

	######################################## CDT station
	tkadd(menu.plot, "command", label = "Plot CDT Station Data", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_CDTstnPlot)){
			lcmd.frame <<- PlotCDTStationCmd()
			lcmd.frame_CDTstnPlot <<- 1
		}
	})

	######################################## CDT dataset
	tkadd(menu.plot, "separator")

	##########
	tkadd(menu.plot, "command", label = "Plot CDT Gridded Dataset", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_CDTdatasetPlot)){
			lcmd.frame <<- PlotCDTDatasetCmd()
			lcmd.frame_CDTdatasetPlot <<- 1
		}
	})

	######################################## NCDF
	tkadd(menu.plot, "separator")

	##########
	tkadd(menu.plot, "command", label = "Plot NetCDF Data (One File)", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_grdNcdfPlot)){
			lcmd.frame <<- PlotGriddedNcdfCmd()
			lcmd.frame_grdNcdfPlot <<- 1
		}
	})

	######################################## NCDF
	tkadd(menu.plot, "separator")

	##########
	tkadd(menu.plot, "command", label = "Plot NetCDF Data (Sequential Files)", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_seqNcdfPlot)){
			lcmd.frame <<- PlotSeqNcdfCmd()
			lcmd.frame_seqNcdfPlot <<- 1
		}
	})

	######################################## NCDF
	tkadd(menu.plot, "separator")

	##########
	tkadd(menu.plot, "command", label = "Plot NetCDF Data (Several Files)", state = 'disabled', command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		# if(is.null(lcmd.frame_grdNcdfPlot)){
		# 	lcmd.frame <<- PlotGriddedNcdfCmd()
		# 	lcmd.frame_grdNcdfPlot <<- 1
		# }
	})

	######################################## MERGED
	tkadd(menu.plot, "separator")

	##########
	tkadd(menu.plot, "command", label = "Plot Merging Outputs", command = function(){
		refreshCDT.lcmd.env()
		spinbox.state(state = 'normal')
		if(is.null(lcmd.frame_mergePlot)){
			lcmd.frame <<- PlotMergingOutputCmd()
			lcmd.frame_mergePlot <<- 1
		}
	})

##########xxxxxxxxxxxxxxxxxx Menu help xxxxxxxxxxxxxxxxxx##########

menu.aide <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
tkadd(top.menu, "cascade", label = "Help", menu = menu.aide, activebackground = 'lightblue')

	##########
	tkadd(menu.aide, "command", label = "CDT User Guide", command = function(){
		browseURL(paste0('file://',file.path(apps.dir, 'help', 'html', 'index.html', fsep = .Platform$file.sep)))
	})

	##########
	tkadd(menu.aide, "separator")

	##########
	tkadd(menu.aide, "command", label = "About CDT", command = function() aboutCDT())

	########
