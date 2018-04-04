CPT.convertProcs <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, paste("Conversion to CPT data format ......"))

	cptInfo <- list(name = GeneralParameters$cptinfo$name,
					units = GeneralParameters$cptinfo$units,
					missval = GeneralParameters$cptinfo$missval)

	if(GeneralParameters$data.type == "cdtstation"){
		cdtdata <- getStnOpenData(GeneralParameters$cdtstation)
		if(is.null(cdtdata)) return(NULL)

		ret <- CPT.convertStationData.Files(cdtdata, GeneralParameters$output, cptInfo)
	}

	if(GeneralParameters$data.type == "cdtnetcdf"){
		ncDataInfo <- getRFESampleData(GeneralParameters$cdtnetcdf$sample)
		if(is.null(ncDataInfo)){
			InsertMessagesTxt(main.txt.out, "No netcdf data sample found", format = TRUE)
			return(NULL)
		}

		ncInfo <- list(dir = GeneralParameters$cdtnetcdf$dir,
						format = GeneralParameters$cdtnetcdf$format,
						xo = ncDataInfo$rfeILon,
						yo = ncDataInfo$rfeILat,
						varid = ncDataInfo$rfeVarid)
		ret <- CPT.convertGridData.Files(ncInfo, GeneralParameters$output, cptInfo)
	}

	return(ret)
}