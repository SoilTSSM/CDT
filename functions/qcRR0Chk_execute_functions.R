execZeroCheck <- function(get.stn){
	min.nbrs <- GeneralParameters$params.zero$min.nbrs
	max.nbrs <- GeneralParameters$params.zero$max.nbrs
	min.days <- GeneralParameters$params.zero$min.days
	max.dst <- GeneralParameters$params.zero$max.dist
	pct.trsh <- GeneralParameters$params.zero$min.thrs
	Zparams <- c(max.dst, max.nbrs, min.nbrs, min.days, pct.trsh)

	###
	ONE_Station <- function(get.stn, Zparams){
		status <- 0
		msg <- NULL

		zerochkdir <- file.path(EnvQcZeroChkData$baseDir, 'Outputs')
		corrdir <- file.path(EnvQcZeroChkData$baseDir, 'CorrectedData')

		corrdirstn <- file.path(corrdir, get.stn)
		if(!file.exists(corrdirstn)) dir.create(corrdirstn, showWarnings = FALSE, recursive = TRUE)
		fileoutdat <- file.path(corrdirstn, paste(get.stn, '.txt', sep = ''))

		pos <- which(EnvQcZeroChkData$donnees$id == get.stn)
		sdon <- data.frame(EnvQcZeroChkData$donnees$dates, EnvQcZeroChkData$donnees$data[, pos])
		write.table(sdon, fileoutdat, col.names = FALSE, row.names = FALSE)

		coords <- list(id = EnvQcZeroChkData$donnees$id, lon = EnvQcZeroChkData$donnees$lon, lat = EnvQcZeroChkData$donnees$lat)

		zeroRet <- try(ZeroCheckStn(pos, EnvQcZeroChkData$donnees$data, EnvQcZeroChkData$donnees$dates, coords, Zparams), silent = TRUE)
		if(!inherits(zeroRet, "try-error")){
			outsdir <- file.path(zerochkdir, get.stn)
			if(!file.exists(outsdir)) dir.create(outsdir, showWarnings = FALSE, recursive = TRUE)

			fileoutqc <- file.path(outsdir, paste(get.stn, '.txt', sep = ''))
			write.table(zeroRet, fileoutqc, col.names = TRUE, row.names = FALSE)
			ret.res <- list(action = GeneralParameters$action, station = get.stn, res = zeroRet,
							AllOrOne = GeneralParameters$AllOrOne, outputdir = zerochkdir, datadir = corrdir)
			msg <- paste("Zeros check finished successfully for", get.stn)
			status <- 'ok'
		}else{
			ret.res <- list(action = GeneralParameters$action, station = get.stn, res = NULL,
							AllOrOne = GeneralParameters$AllOrOne, outputdir = zerochkdir, datadir = corrdir)
			msg <- paste(paste("Zeros check failed for", get.stn), '\n', gsub('[\r\n]', '', zeroRet[1]), sep = '')
			status <- 'no'
		}
		on.exit({
			if(status == 'ok') InsertMessagesTxt(main.txt.out, msg)
			if(status == 'no') InsertMessagesTxt(main.txt.out, msg, format = TRUE)
		})
		return(ret.res)
	}

	###
	ALL_StationsLoop <- function(jlstn, Zparams, zerochkdir, corrdir){
		status <- 0
		msg <- NULL

		corrdirstn <- file.path(corrdir, jlstn)
		if(!file.exists(corrdirstn)) dir.create(corrdirstn, showWarnings = FALSE, recursive = TRUE)
		fileoutdat <- file.path(corrdirstn, paste(jlstn, '.txt', sep = ''))

		pos <- which(EnvQcZeroChkData$donnees$id == jlstn)
		sdon <- data.frame(EnvQcZeroChkData$donnees$dates, EnvQcZeroChkData$donnees$data[, pos])
		write.table(sdon, fileoutdat, col.names = FALSE, row.names = FALSE)

		coords <- list(id = EnvQcZeroChkData$donnees$id, lon = EnvQcZeroChkData$donnees$lon, lat = EnvQcZeroChkData$donnees$lat)

		zeroRet <- try(ZeroCheckStn(pos, EnvQcZeroChkData$donnees$data, EnvQcZeroChkData$donnees$dates, coords, Zparams), silent = TRUE)
		if(!inherits(zeroRet, "try-error")){
			outsdir <- file.path(zerochkdir, jlstn)
			if(!file.exists(outsdir)) dir.create(outsdir, showWarnings = FALSE, recursive = TRUE)

			fileoutqc <- file.path(outsdir, paste(jlstn, '.txt', sep = ''))
			write.table(zeroRet, fileoutqc, col.names = TRUE, row.names = FALSE)
			retResStn <- list(station = jlstn, res = zeroRet)
			msg <- paste("Zeros check finished successfully for", jlstn)
			status <- 'ok'
		}else{
			retResStn <- list(station = jlstn, res = NULL)
			msg <- paste(paste("Zeros check failed for", jlstn), '\n', gsub('[\r\n]', '', zeroRet[1]), sep = '')
			status <- 'no'
		}
		tcl("update")
		on.exit({
			if(status == 'ok') InsertMessagesTxt(main.txt.out, msg)
			if(status == 'no') InsertMessagesTxt(main.txt.out, msg, format = TRUE)
		})
		return(retResStn)
	}

	###
	ALL_Stations <- function(Zparams){
		zerochkdir <- file.path(EnvQcZeroChkData$baseDir, 'Outputs')
		corrdir <- file.path(EnvQcZeroChkData$baseDir, 'CorrectedData')

		tcl("update", "idletasks")

		retResAllStn <- lapply(as.character(EnvQcZeroChkData$donnees$id),
								function(jlstn) ALL_StationsLoop(jlstn, Zparams, zerochkdir, corrdir))

		ret.res <- list(action = GeneralParameters$action, res = retResAllStn,
						AllOrOne = GeneralParameters$AllOrOne, outputdir = zerochkdir, datadir = corrdir)
		return(ret.res)
	}

	###
	if(GeneralParameters$AllOrOne == 'one') ret.res <- ONE_Station(get.stn, Zparams)
	if(GeneralParameters$AllOrOne == 'all') ret.res <- ALL_Stations(Zparams)
	return(ret.res)
}


