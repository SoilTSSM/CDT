
###############################
#initialize parameters

init.params <- function(action, period){
	
	## initialize spinbox
	spinbox.state()

	#############
	#homogenization CDT
	if(action == 'homog'){
		hom.opts <- data.frame(c("crop.bounds", "h", "conf.lev", "Kmax", "min.int"), c("0", "0.025", "95.0", "10", "24"))
		names(hom.opts) <- c('Parameters', 'Values')
		file.io <- data.frame(c('candidate', 'reference', 'DEM.netcdf', 'dir2save'), c('', '', '', getwd()))
		names(file.io) <- c('Parameters', 'Values')
		use.method <- data.frame(c('hom.test', 'single.series', 'use.ref'), c("SNHT(Alexandersson & Moberg, 1997)", "0", "0"))
		names(use.method) <- c('Parameters', 'Values')
		file.date.format <- data.frame(c('file.format', 'date.format', 'vars'), c("1", "1", "2"))
		names(file.date.format) <- c('Parameters', 'Values')
		ref.series.choix <- data.frame(c('diff.ratio', 'weight.mean', 'use.elv', 'interp.dem', 'min.stn', 'max.stn', 'max.dist', 'elv.diff', 'min.rho'),
										c('1', '1', '0', '1', '2', '7', '100', '200', '0.5'))
		names(ref.series.choix) <- c('Parameters', 'Values')
		compute.var <- data.frame(c('function', 'miss.frac'), c('mean', '0.95'))
		names(compute.var) <- c('Parameters', 'Values')
		Adjust.method <- data.frame(c('minAdjmo', 'minAdjdk', 'minAdjdy', 'SegAdjmo', 'SegAdjdk', 'SegAdjdy'),
									c('32', '32', '32', '0', '0', '0'))
		names(Adjust.method) <- c('Parameters', 'Values')
		ref.series.user <- '0'
		stn.user.choice <- NULL
		ret.params <- list(action = action, period = period, file.io = file.io, use.method = use.method, hom.opts = hom.opts,
							file.date.format = file.date.format, ref.series.choix = ref.series.choix, compute.var = compute.var,
							Adjust.method = Adjust.method, ref.series.user = ref.series.user, stn.user.choice = stn.user.choice, retpar = 0)
	}

	#RHtestsV4
	if(action == 'rhtests'){
		file.io <- data.frame(c('candidate', 'reference', 'DEM.netcdf', 'dir2save'), c('', '', '', getwd()))
		names(file.io) <- c('Parameters', 'Values')
		file.date.format <- data.frame(c('file.format', 'date.format', 'vars'), c("1", "1", "2"))
		names(file.date.format) <- c('Parameters', 'Values')
		compute.var <- data.frame(c('function', 'miss.frac'), c('mean', '0.95'))
		names(compute.var) <- c('Parameters', 'Values')
		rhtests.pars <- data.frame(c('p.lev', 'Iadj', 'Mq', 'Ny4a', 'pthr'), c("0.95", "10000", "12", "0", "0"))
		names(rhtests.pars) <- c('Parameters', 'Values')
		single.series <- '0'
		use.ref.series <- '0'
		ref.series.choix <- data.frame(c('weight.mean', 'use.elv', 'interp.dem', 'min.stn', 'max.stn', 'max.dist', 'elv.diff', 'min.rho'),
										c('Distance', '0', '1', '2', '7', '100', '200', '0.5'))
		names(ref.series.choix) <- c('Parameters', 'Values')
		ref.series.user <- '0'
		stn.user.choice <- NULL
		prcpdata <- data.frame(c('rr.data', 'rr.log'), c('0', '0'))
		names(prcpdata) <- c('Parameters', 'Values')
		ret.params <- list(action = action, period = period, file.io = file.io, file.date.format = file.date.format, single.series = single.series,
							rhtests.pars = rhtests.pars, compute.var = compute.var, prcpdata = prcpdata, ref.series.choix = ref.series.choix,
							use.ref.series = use.ref.series, ref.series.user = ref.series.user, stn.user.choice = stn.user.choice, getdata = FALSE)
	}
	
	##########
	#qc.txtn
	if(action == 'qc.temp'){
		test.tx <- '1'
		file.io <- data.frame(c('input.txtn1', 'input.txtn2', 'DEM.netcdf', 'dir2save'), c('', '', '', getwd()))
		names(file.io) <- c('Parameters', 'Values')
		use.method <- data.frame(c('single.series', 'consist.check', 'use.elv', 'interp.dem'), c('0', '0', '0', '0'))
		names(use.method) <- c('Parameters', 'Values')
		file.date.format <- data.frame(c('file.format', 'date.format'), c("1", "1"))
		names(file.date.format) <- c('Parameters', 'Values')
		param.temp <- data.frame(c('min.stn', 'max.stn', 'win.len', 'conf.lev', 'max.dist', 'elv.diff'),
									c('2', '7', '30', '99.73', '50', '200'))
		names(param.temp) <- c('Parameters', 'Values')
		limControl <- data.frame(NA,-40,60, NA, NA)
		names(limControl) <- c('Station ID', 'Lower Bounds', 'Upper Bounds', 'Lon', 'Lat')
		parameter <- list(param.temp, limControl)
		AllOrOne <- 'one'
		ret.params <- list(action = action, period = period, test.tx = test.tx, file.io = file.io, use.method = use.method,
							file.date.format = file.date.format, parameter = parameter, AllOrOne = AllOrOne, retpar = 0)
	}

	############
	#qc.rainfall
	##Zeros check
	if(action == 'zero.check'){
		file.io <- data.frame(c('input.rainfall', 'dir2save'), c('', getwd()))
		names(file.io) <- c('Parameters', 'Values')
		param.zero <- data.frame(c('min.nbrs', 'max.nbrs', 'min.days', 'max.dst', 'pct.trsh'), c('3', '6', '22', '100', '1.5'))
		names(param.zero) <- c('Parameters', 'Values')
		AllOrOne <- 'one'
		ret.params <- list(action = action, period = period, file.io = file.io, param.zero = param.zero, AllOrOne = AllOrOne, retpar = 0)
	}

	##Outliers check
	if(action == 'qc.rain'){
		file.io <- data.frame(c('input.rainfall', 'DEM.netcdf', 'dir2save'), c('', '', getwd()))
		names(file.io) <- c('Parameters', 'Values')
		use.method <- data.frame(c('single.series', 'use.elv', 'interp.dem'), c('0', '0', '0'))
		names(use.method) <- c('Parameters', 'Values')
		file.date.format <- data.frame(c('file.format', 'date.format'), c("1", "1"))
		names(file.date.format) <- c('Parameters', 'Values')
		param.rain <- data.frame(c('min.stn', 'conf.lev', 'max.dist', 'elv.diff'), c('5', '99.73', '45', '200'))
		names(param.rain) <- c('Parameters', 'Values')
		limControl <- data.frame(NA, 300, NA, NA)
		names(limControl) <- c('Station ID', 'Upper Bounds', 'Lon', 'Lat')
		spatparam <- list(ispmax = '1.0', ispobs = '10.0', isdmin = '3.0', isdobs = '1.0', isdq1 = '10.0', ftldev = '2.8')
		parameter <- list(param.rain, limControl, spatparam)

		AllOrOne <- 'one'
		ret.params <- list(action = action, period = period, file.io = file.io, use.method = use.method,
							file.date.format = file.date.format, parameter = parameter, AllOrOne = AllOrOne, retpar = 0)
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

