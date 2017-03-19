
###############################
#initialize parameters

init.params <- function(action, period){
	
	## initialize spinbox
	spinbox.state()

	#############
	#homogenization CDT
	if(action == 'homog'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Homogenization_CDT.json'))
		ret.params <- c(list(action = action, period = period), ret.params, list(stn.user.choice = NULL, retpar = 0))
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	#RHtestsV4
	if(action == 'rhtests'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Homogenization_RHtests.json'))
		ret.params <- c(list(action = action, period = period), ret.params, list(stn.user.choice = NULL, getdata = FALSE))
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}
	
	##########
	#qc.txtn
	if(action == 'qc.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'QC_Temperatures.json'))
		stnInfo <- data.frame("Station.ID" = NA, "Lower.Bounds" = ret.params$limits$Lower.Bounds,
							"Upper.Bounds" = ret.params$limits$Upper.Bounds, "Lon" = NA, "Lat" = NA)
		ret.params <- c(list(action = action, period = period), ret.params, list(stnInfo = stnInfo, retpar = 0))
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	############
	#qc.rainfall
	##Zeros check
	if(action == 'zero.check'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'False_Zero_Check.json'))
		ret.params <- c(list(action = action, period = period), ret.params, list(retpar = 0))
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	##Outliers check
	if(action == 'qc.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'QC_Rainfall.json'))
		stnInfo <- data.frame("Station.ID" = NA, "Upper.Bounds" = ret.params$limits$Upper.Bounds, "Lon" = NA, "Lat" = NA)
		ret.params <- c(list(action = action, period = period), ret.params, list(stnInfo = stnInfo, retpar = 0))
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	#########################################################################
	## Merging Rainfall
	###Mean bias
	if(action == 'coefbias.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Mean_Bias_Factor_RR.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	###Remove bias
	if(action == 'rmbias.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Bias_Correction_RR.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	### Compute LM coef
	if(action == 'coefLM.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Compute_LM_Coef_RR.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	###Merging rainfall
	if(action == 'merge.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Merging_RR.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	####dekadal update
	if(action == 'merge.dekrain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Update_dekadal_RR.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	#################################################################
	## Merging temperature
	##compute regression parameters for downscaling
	if(action == 'coefdown.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Downscalling_Coef_TT.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	######downscaling
	if(action == 'down.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Downscalling_TT.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	##Bias coeff
	if(action == 'coefbias.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Mean_Bias_Factor_TT.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	##Adjustment
	if(action == 'adjust.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Bias_Correction_TT.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	### Compute LM coef
	if(action == 'coefLM.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Compute_LM_Coef_TT.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	##Merging
	if(action == 'merge.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Merging_TT.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	#############################################################################################3
	
	if(action == 'chk.coords'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Check_STN_Coords.json'))
		ret.params <- c(list(action = action, period = 'daily'), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	######################
	if(action == 'agg.qc'){
		file.io <- getwd()
		ret.params <- list(action = action, period = 'daily', file.io = file.io)
	}
	if(action == 'agg.zc'){
		file.io <- getwd()
		ret.params <- list(action = action, period = 'daily', file.io = file.io)
	}
	if(action == 'agg.hom'){
		file.io <- getwd()
		ret.params <- list(action = action, period = 'daily', file.io = file.io)
	}

	################
	if(action == 'cdtInput.stn'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Fomat_CDT_inputData.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
	}

	################
	if(action == 'agg.ts'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Aggregate_time_series.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
	}

	################
	if(action == 'fill.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Filling_CDT_Temperature.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
	}

	###########################################

	if(action == 'extrct.ts'){
		file.io <- data.frame(c('NetCDF.dir', 'Shp.file', 'file2save'), c('', '', getwd()))
		names(file.io) <- c('Parameters', 'Values')
		prefix <- data.frame(c('cdfFileFormat', 'tsPrefix'), c("rr_mrg_%s%s%s.nc", "rr_adj"))
		names(prefix) <- c('Parameters', 'Values')
		dates.ts <- data.frame(c('istart.yrs', 'istart.mon', 'istart.dek', 'iend.yrs', 'iend.mon', 'iend.dek'),
								c('1983', '1', '1', '2014', '12', '3'))
		names(dates.ts) <- c('Parameters', 'Values')

		ret.params <- list(action = action, period = period, file.io = file.io, prefix = prefix, dates.ts = dates.ts)
	}

	#############
	return(ret.params)
}

