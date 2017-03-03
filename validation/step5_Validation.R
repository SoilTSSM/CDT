	## data time step
	freqData <- 'dekadal'

	## year range
	year1 <- 1983 
	year2 <- 2010

	## Define Season
	start.month <- 1
	length.month <- 12

	## Validation data file (CDT data)
	stnValidFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Valid.txt'

	# ## Training data file (CDT data)
	# stnFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Train.txt'

	## Station missing values code
	miss.val <- -99

	## Merged data directory
	mrgDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/Merged'

	## Statistics Output directory
	validationDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/Validation_Stat'

	#####################################################################################

	#### COMBINAISON

	MRGcomb <- merging.combination()

	##same aux.var, "noD", "D", "SA", "DSA"
	ix1 <- (MRGcomb$bias.auxvar == MRGcomb$mrg.auxvar) & MRGcomb$bias.auxvar%in%c("noD", "D", "SA", "DSA", "LoLa", "DLoLa", "DSALoLa")
	ix2 <- MRGcomb$mrg.method == "Reg.Kriging"

	## Bias and LMCoef same aux.var and interpolation
	ix3 <- (MRGcomb$mrg.method == "Saptio.Tempo.LM") & (MRGcomb$bias.interp == MRGcomb$sptrend.interp) & (MRGcomb$bias.auxvar == MRGcomb$sptrend.auxvar)

	ix <- ix1 & (ix2 | ix3)
	MRGcomb <- MRGcomb[ix, ]

	#####################################################################################

	bs.mthd <- paste(MRGcomb$bias.method, MRGcomb$bias.interp, MRGcomb$bias.auxvar, sep = '_')
	lm.mthd <- paste(MRGcomb$sptrend.interp, MRGcomb$sptrend.auxvar, sep = '_')
	mrg.mthd <- paste(MRGcomb$mrg.method, MRGcomb$mrg.interp, MRGcomb$mrg.auxvar, sep = '_')

	combMering <- paste(bs.mthd, lm.mthd, mrg.mthd, sep = '-')

	#####################################################################################

	validation.Stats <- function(x, y, freqData){
		x <- x[!is.na(x) & !is.nan(x)]
		y <- y[!is.na(y) & !is.nan(y)]
		mn0 <- mean(x)
		sum1 <- sum((y-x)^2)
		sum2 <- sum((x-mn0)^2)
		eff <- 1. - sum1/sum2
		mae <- mean(abs(y-x))
		mer <- mean(y-x)
		bis <- sum(y)/sum(x)
		cor <- cor(x, y)

		stat <- c(cor, eff, bis, mae, mer)
		name.stat <- c("CORR", "NSE", "BIAS", "MAE", "ME")

		if(freqData == 'daily'){
			rnr <- 1.0
			n1 <- length(which(x >= rnr & y >= rnr))  # Ht
			n2 <- length(which(x < rnr & y >= rnr))  # Fa
			n3 <- length(which(x >= rnr & y < rnr))  # Ms
			n4 <- length(which(x < rnr & y < rnr))   # Cn
			n <- n1+n2+n3+n4

			fbs <- (n1+n2)/(n1+n3)
			csi <- n1/(n-n4)
			pod <- n1/(n1+n3)
			far <- n2/(n1+n2)

			a <- n1*1.0
			b <- n2*1.0
			c <- n3*1.0
			d <- n4*1.0
			C0 <- a+d
			N0 <- a+b+c+d
			E0 <- ((a+c)*(a+b)+(b+d)*(c+d))/(a+b+c+d)
			hss <- (C0-E0)/(N0-E0)

			stat <- c(stat, pod, far, fbs, csi, hss)
			name.stat <- c(name.stat, "POD", "FAR", "FBS", "CSI", "HSS")
		}
		stat <- data.frame(matrix(round(stat, 3), nrow = 1))
		names(stat) <- name.stat
		return(stat)
	}

	#####################################################################################

	## validation coordinates
	validStnData <- read.table(stnValidFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	validStnData <- getCDTdataAndDisplayMsg(validStnData, freqData)

	validMrgData <- lapply(seq_along(combMering), function(ll){
		don <- read.table(file.path(mrgDIR, combMering[ll], "Merged_Validation_Data.txt"))
		list(dates = as.character(don[, 1]), data = don[, -1])
	})

	#####################################################################################

	ix <- which(1:12 == start.month)
	im <- (ix:(ix+(length.month-1)))%%12
	im[im == 0] <- 12
	im <- str_pad(im, width = 2, pad = '0')

	## Get season
	ix <- which(1:12 == start.month)
	im <- (ix:(ix+(length.month-1)))%%12
	im[im == 0] <- 12
	im <- str_pad(im, width = 2, pad = '0')

	seasSTN <- substr(validStnData$dates, 1, 4)%in%as.character(year1:year2) & substr(validStnData$dates, 5, 6)%in%im
	seasMRG <- substr(validMrgData[[1]]$dates, 1, 4)%in%as.character(year1:year2) & substr(validMrgData[[1]]$dates, 5, 6)%in%im

	Obs <- validStnData$data[seasSTN, , drop = FALSE]
	Mrg <- lapply(validMrgData, function(x)x$data[seasMRG, , drop = FALSE])

	#####################################################################################

	## calculate statistics
	validStat <- lapply(Mrg, function(x){
		mod <- as.data.frame(x)
		stn <- as.data.frame(Obs)
		names(stn) <- names(mod) <- validStnData$id
		iNA<- is.na(mod) | is.na(stn)
		stn[iNA] <- NA
		mod[iNA] <- NA
		stat.stn <- mapply(function(x, y) validation.Stats(x, y, freqData), stn, mod)
		X0 <- c(as.matrix(stn))
		Y0 <- c(as.matrix(mod))
		stat.allstn <- validation.Stats(X0, Y0, freqData)
		X <- apply(stn, 1, mean, na.rm = TRUE)
		Y <- apply(mod, 1, mean, na.rm = TRUE)
		stat.smean <- validation.Stats(X, Y, freqData)
		list(stn = stat.stn, all.stn = stat.allstn, stn.mean = stat.smean)
	})

	## statistics for each station
	stats.by.stn <- lapply(validStat, '[[', 1)

	## statistics for all stations
	stats.all.stn <- apply(t(sapply(validStat, '[[', 2)), 2, as.numeric)

	## statistics for stations average
	stats.stn.avrg <- apply(t(sapply(validStat, '[[', 3)), 2, as.numeric)

	#####################################################################################
	dir.create(validationDIR, showWarnings = FALSE, recursive = TRUE)

	stats.stn.average <- data.frame(Method = combMering, stats.stn.avrg, stringsAsFactors = FALSE)

	write.table(stats.stn.average, file.path(validationDIR, 'STATS_STATIONS_AVERAGE.csv'),
				col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ',')

	stats.stn.ALL <- data.frame(Method = combMering, stats.all.stn, stringsAsFactors = FALSE)

	write.table(stats.stn.ALL, file.path(validationDIR, 'STATS_ALL_STATIONS.csv'),
				col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ',')

	names(stats.by.stn) <- combMering
	stats.by.stn1 <- do.call(rbind, stats.by.stn)
	rownom <- rownames(stats.by.stn1)
	mthd0 <- rep("", length(rownom))
	mthd1 <- rep(FALSE, length(rownom))
	mthd1[seq(1, length(rownom), length(unique(rownom)))] <- TRUE
	mthd0[mthd1] <- combMering
	stats.by.stn1 <- data.frame(mthd0, rownom, apply(stats.by.stn1, 2, as.numeric), stringsAsFactors = FALSE)
	names(stats.by.stn1) <- c("Method", "Stats", validStnData$id)

	write.table(stats.by.stn1, file.path(validationDIR, 'STATS_EACH_STATION.csv'),
				col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ',')
	save(stats.by.stn, file = file.path(validationDIR, 'STATS_EACH_STATION.RDATA'))


