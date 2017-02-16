	## data time step
	freqData <- 'dekadal'

	## year range
	year1 <- 1983 
	year2 <- 2010

	## Define Season
	start.month <- 1
	length.month <- 12

	## Merged data directory
	mrgDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/Merged'

	## Validation data file (CDT data)
	stnValidFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Valid.txt'

	## Station missing values code
	miss.val <- -99

	# ## Training data file (CDT data)
	# stnFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Train.txt'

	## Statistics Output directory
	outDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/Validation_Stat'

	#####################################################################################

	#### COMBINAISON

	MRGcomb <- merging.combination()

	##same aux.var, "noD", "D", "SA", "DSA"
	ix1 <- (MRGcomb$bias.auxvar == MRGcomb$mrg.auxvar) & MRGcomb$bias.auxvar%in%c("noD", "D", "SA", "DSA")
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

	ix <- which(1:12 == start.month)
	im <- (ix:(ix+(length.month-1)))%%12
	im[im == 0] <- 12
	im <- str_pad(im, width = 2, pad = '0')

	seas <- substr(validStnData$dates, 1, 4)%in%as.character(year1:year2) & substr(validStnData$dates, 5, 6)%in%im
	stnData <- data.frame(validStnData$data[seas, , drop = FALSE])
	names(stnData) <- validStnData$id
	validData <- lapply(validMrgData, function(x){
		don <- x$data[substr(x$dates, 1, 4)%in%as.character(year1:year2) & substr(x$dates, 5, 6)%in%im, , drop = FALSE]
		iNA<- is.na(don) | is.na(stnData)
		stnData[iNA] <- NA
		don[iNA] <- NA
		stat.stn <- mapply(function(x, y) validation.Stats(x, y, freqData), stnData, don)
		X <- apply(stnData, 1, mean, na.rm = TRUE)
		Y <- apply(don, 1, mean, na.rm = TRUE)
		stat.smean <- validation.Stats(X, Y, freqData)
		list(stn = stat.stn, stn.mean = stat.smean)
	})

	stat.mean <- apply(t(sapply(validData, '[[', 2)), 2, as.numeric)
	stats.stn.average <- data.frame(Method = combMering, stat.mean, stringsAsFactors = FALSE)
	stats.stn <- lapply(validData, '[[', 1)
	names(stats.stn) <- combMering
	stats.stn1 <- do.call(rbind, stats.stn)
	rownom <- rownames(stats.stn1)
	mthd0 <- rep("", length(rownom))
	mthd1 <- rep(FALSE, length(rownom))
	mthd1[seq(1, length(rownom), length(unique(rownom)))] <- TRUE
	mthd0[mthd1] <- combMering
	stats.stn1 <- data.frame(Method = mthd0, Stats = rownom,
					apply(stats.stn1, 2, as.numeric), stringsAsFactors = FALSE)

	write.table(stats.stn.average, file.path(outDIR, 'STATS_STATIONS_AVERAGE.csv'),
	col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ',')

	write.table(stats.stn1, file.path(outDIR, 'STATS_EACH_STATION.csv'),
	col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ',')

	save(stats.stn, file = file.path(outDIR, 'STATS_EACH_STATION.RDATA'))


