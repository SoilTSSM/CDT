

rasterData.convert_Proc <- function(params){
	InsertMessagesTxt(main.txt.out, paste("Raster data format conversion ......"))

	regex.in <- switch(params$type.in,
					"nc" = c("*\\.nc$", "\\.nc$"),
					"tif" = c("*\\.tif$", "\\.tif$"),
					"bil" = c("*\\.bil$", "\\.bil$"))
	sub.out <- switch(params$type.out, "nc" = ".nc", "tif" = ".tif", "bil" = ".bil")
	format.out <- switch(params$type.out, "nc" = "CDF", "tif" = "GTiff", "bil" = "EHdr")

	files.in <- list.files(params$dir.in, regex.in[1])
	if(length(files.in) == 0){
		nsertMessagesTxt(main.txt.out, paste("no", params$type.in, "files found\n"), format = TRUE)
		return(NULL)
	}
	files.out <- gsub(regex.in[2], sub.out, files.in)

	ret <- lapply(seq_along(files.in), function(jj){
		path.in <- file.path(params$dir.in, files.in[jj])
		if(params$type.in == "bil"){
			path.hdr <- gsub("\\.bil$", ".hdr", path.in)
			if(!file.exists(path.hdr)) return(NULL)
		}

		don.raster <- try(raster(path.in), silent = TRUE)
		if(inherits(don.raster, "try-error")) return(NULL)

		crs(don.raster) <- NA
		crs(don.raster) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

		don.raster[is.nan(don.raster) | is.infinite(don.raster)] <- NA
		if(params$type.out == "nc") don.raster[don.raster == params$nc.opts$missval] <- NA

		path.out <- file.path(params$dir.out, files.out[jj])
		write.args <- list(x = don.raster, filename = path.out, format = format.out, overwrite = TRUE)
		if(params$type.out == "nc")
			write.args <- c(write.args, c(params$nc.opts[names(params$nc.opts) != "missval"], list(compression = 9)))

		res <- do.call(writeRaster, write.args)
		return(0)
	})

	return(0)
}
