
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

	#### Simplified
	if(action == 'merge.rain.one'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Precip_Merging_One.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	#### Advanced

	###Mean bias
	if(action == 'coefbias.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Precip_Bias_Factor_Calc.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	###Remove bias
	if(action == 'rmbias.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Precip_Bias_Correction.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	### Compute LM coef
	if(action == 'coefLM.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Precip_LM_Coef_Calc.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	###Merging rainfall
	if(action == 'merge.rain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Precip_Merging_Adv.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	####dekadal update
	if(action == 'merge.dekrain'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Update_dekadal_RR.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	#################################################################
	## Merging temperature

	##compute regression parameters for downscaling
	if(action == 'coefdown.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_downscalling_Coef.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$IO.files$dir2save) == "") ret.params$IO.files$dir2save <- getwd()
	}

	######downscaling
	if(action == 'down.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_downscalling_reanalysis.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	#### Simplified
	if(action == 'merge.temp.one'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_Merging_One.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	#### Advanced

	##Bias coeff
	if(action == 'coefbias.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_Bias_Factor_Calc.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	##Adjustment
	if(action == 'adjust.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_Bias_Correction.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	### Compute LM coef
	if(action == 'coefLM.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_LM_Coef_Calc.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	##Merging
	if(action == 'merge.temp'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Temp_Merging_Adv.json'))
		ret.params <- c(list(action = action, period = period), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	#################################################################
	## Scale merged data
	if(action == 'scale.merged'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Scaling_MergedData.json'))
		ret.params <- c(list(action = action), ret.params)
		if(str_trim(ret.params$outdir) == "") ret.params$outdir <- getwd()
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
	if(action == 'aggregate.ts'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Aggregate_time_series.json'))
		ret.params <- c(list(action = action, in.tstep = period), ret.params)
	}

	################
	if(action == 'aggregate.nc'){
		ret.params <- fromJSON(file.path(apps.dir, 'init_params', 'Aggregate_spatial_netcdf.json'))
		ret.params <- c(list(action = action), ret.params)
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
