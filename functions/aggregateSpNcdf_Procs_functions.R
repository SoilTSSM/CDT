AggregateSpNc_Execute <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, 'Regrid netCDF data ...')

	outputdir <- GeneralParameters$output
	if(is.na(outputdir) | outputdir%in%c("", "NA")){
		InsertMessagesTxt(main.txt.out, "Directory to save results is missing", format = TRUE)
		return(NULL)
	}
	outputNC <- file.path(outputdir, "Regrided_NetCDF")
	dir.create(outputNC, showWarnings = FALSE, recursive = TRUE)

	if(GeneralParameters$nb.ncfile == "one"){
		ncDataInfo <- getRFESampleData(GeneralParameters$ncdf$fileordir)
		if(is.null(ncDataInfo)){
			InsertMessagesTxt(main.txt.out, "Unable to read NetCDF file to be regridded", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$nb.ncfile == "several"){
		ncDataInfo <- getRFESampleData(GeneralParameters$ncdf$sample)
		if(is.null(ncDataInfo)){
			InsertMessagesTxt(main.txt.out, "Unable to read the sample NetCDF file", format = TRUE)
			return(NULL)
		}
	}
	ncinfo <- list(xo = ncDataInfo$rfeILon, yo = ncDataInfo$rfeILat, varid = ncDataInfo$rfeVarid)
	old.grid <- defSpatialPixels(list(lon = ncDataInfo$lon, lat = ncDataInfo$lat))

	if(GeneralParameters$ncdf.grid$use.ncgrid){
		ncDataNew <- getRFESampleData(GeneralParameters$ncdf.grid$file)
		if(is.null(ncDataNew)){
			InsertMessagesTxt(main.txt.out, "Unable to read NetCDF file whose grid will be used", format = TRUE)
			return(NULL)
		}
		grd.lon <- ncDataNew$lon
		grd.lat <- ncDataNew$lat
	}else{
		X0 <- GeneralParameters$res$minlon
		X1 <- GeneralParameters$res$maxlon
		Y0 <- GeneralParameters$res$minlat
		Y1 <- GeneralParameters$res$maxlat
		pX <- GeneralParameters$res$reslon
		pY <- GeneralParameters$res$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)

		if(GeneralParameters$method != "bilinear"){
			if(grd.lon[length(grd.lon)]+pX/2 < X1) grd.lon <- c(grd.lon, grd.lon[length(grd.lon)]+pX)
			if(grd.lat[length(grd.lat)]+pY/2 < Y1) grd.lat <- c(grd.lat, grd.lat[length(grd.lat)]+pY)
		}
	}
	new.grid <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))

	is.regridNC <- is.diffSpatialPixelsObj(old.grid, new.grid, tol = 1e-07)
	if(!is.regridNC){
		InsertMessagesTxt(main.txt.out, "Nothing to do the files have the same grid", format = TRUE)
		return(NULL)
	}

	if(!GeneralParameters$ncdf.grid$use.ncgrid){
		if(GeneralParameters$but == "Aggregate"){
			if(all(new.grid@grid@cellsize < old.grid@grid@cellsize)){
				InsertMessagesTxt(main.txt.out, "Nothing to do the new grid is finer than the grid of NetCDF data", format = TRUE)
				return(NULL)
			}
		}else{
			if(all(old.grid@grid@cellsize < new.grid@grid@cellsize)){
				InsertMessagesTxt(main.txt.out, "Nothing to do the new grid is coarser than the grid of NetCDF data", format = TRUE)
				return(NULL)
			}
		}
	}

	if(GeneralParameters$method == "bilinear"){
		gridInterp <- list(x = grd.lon, y = grd.lat)
		grid.dim <- c(length(grd.lon), length(grd.lat))
	}else{
		## maybe use raster::extract
		ixy <- over(old.grid, new.grid)
		grid.dim <- new.grid@grid@cells.dim
	}

	if(GeneralParameters$nb.ncfile == "one"){
		jfile <- getIndex.AllOpenFiles(GeneralParameters$ncdf$fileordir)
		ncfile <- AllOpenFilesData[[jfile]][[3]]

		nc <- nc_open(ncfile)
		xlon <- nc$dim[[ncinfo$xo]]$vals
		xlat <- nc$dim[[ncinfo$yo]]$vals
		zval <- ncvar_get(nc, varid = ncinfo$varid)

		nc.name <- ncinfo$varid
		nc.longname <- nc$var[[nc.name]]$longname
		nc.units <- nc$var[[nc.name]]$units
		nc.missval <- nc$var[[nc.name]]$missval
		nc.prec <- nc$var[[nc.name]]$prec
		nc_close(nc)

		xo <- order(xlon)
		xlon <- xlon[xo]
		yo <- order(xlat)
		xlat <- xlat[yo]
		if(ncinfo$yo == 1){
			zval <- matrix(c(zval), nrow = length(xlon), ncol = length(xlat), byrow = TRUE)
		}
		zval <- zval[xo, yo]

		dx <- ncdim_def("Lon", "degreeE", grd.lon)
		dy <- ncdim_def("Lat", "degreeN", grd.lat)
		grd.nc.out <- ncvar_def(nc.name, nc.units, list(dx, dy), nc.missval, longname = nc.longname, prec = nc.prec)

		if(GeneralParameters$method == "bilinear"){
			z.out <- interp.surface.grid(list(x = xlon, y = xlat, z = zval), gridInterp)
			z.out <- z.out$z
			z.out[is.na(z.out)] <- nc.missval
		}else{
			z.out <- matrix(tapply(c(zval), ixy, mean, na.rm = TRUE), grid.dim[1], grid.dim[2])
			z.out[is.na(z.out) | is.nan(z.out) | is.infinite(z.out)] <- nc.missval
		}

		outfl <- file.path(outputNC, basename(ncfile))
		nc2 <- nc_create(outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, z.out)
		nc_close(nc2)
	}

	if(GeneralParameters$nb.ncfile == "several"){
		allncfiles <- list.files(GeneralParameters$ncdf$fileordir, ".nc", full.names = TRUE)
		fexist <- sapply(allncfiles, file.exists)
		if(length(fexist) == 0){
			InsertMessagesTxt(main.txt.out, "No NetCDF files found", format = TRUE)
			return(NULL)
		}
		
		allncfiles <- allncfiles[fexist]
		if(length(allncfiles) == 0){
			InsertMessagesTxt(main.txt.out, "No NetCDF files found", format = TRUE)
			return(NULL)
		}

		nc <- nc_open(allncfiles[1])
		xlon <- nc$dim[[ncinfo$xo]]$vals
		xlat <- nc$dim[[ncinfo$yo]]$vals

		nc.name <- ncinfo$varid
		nc.longname <- nc$var[[nc.name]]$longname
		nc.units <- nc$var[[nc.name]]$units
		nc.missval <- nc$var[[nc.name]]$missval
		nc.prec <- nc$var[[nc.name]]$prec
		nc_close(nc)

		xo <- order(xlon)
		xlon <- xlon[xo]
		yo <- order(xlat)
		xlat <- xlat[yo]
		xnlon <- length(xlon)
		xnlat <- length(xlat)

		dx <- ncdim_def("Lon", "degreeE", grd.lon)
		dy <- ncdim_def("Lat", "degreeN", grd.lat)
		grd.nc.out <- ncvar_def(nc.name, nc.units, list(dx, dy), nc.missval, longname = nc.longname, prec = nc.prec)

		for(jj in seq_along(allncfiles)){
			nc <- nc_open(allncfiles[jj])
			zval <- ncvar_get(nc, varid = ncinfo$varid)
			nc_close(nc)
			if(ncinfo$yo == 1){
				zval <- matrix(c(zval), nrow = xnlon, ncol = xnlat, byrow = TRUE)
			}
			zval <- zval[xo, yo]

			if(GeneralParameters$method == "bilinear"){
				z.out <- interp.surface.grid(list(x = xlon, y = xlat, z = zval), gridInterp)
				z.out <- z.out$z
				z.out[is.na(z.out)] <- nc.missval
			}else{
				z.out <- matrix(tapply(c(zval), ixy, mean, na.rm = TRUE), grid.dim[1], grid.dim[2])
				z.out[is.na(z.out) | is.nan(z.out) | is.infinite(z.out)] <- nc.missval
			}

			outfl <- file.path(outputNC, basename(allncfiles[jj]))
			nc2 <- nc_create(outfl, grd.nc.out)
			ncvar_put(nc2, grd.nc.out, z.out)
			nc_close(nc2)
		}
	}

	return(0)
}

