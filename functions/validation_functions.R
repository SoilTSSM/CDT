
ValidationStatisticsFun <- function(x, y, dichotomous, thres = 1.0){
	nbcol <- ncol(x)
	nbrow <- nrow(x)
	naCol <- base::colSums(is.na(x))
	colNA <- naCol == nbrow
	nbX <- nbrow - naCol
	xmy <- y-x
	mnx <- base::colMeans(x, na.rm = TRUE)
	mny <- base::colMeans(y, na.rm = TRUE)
	varx <- matrixStats::colVars(x, na.rm = TRUE)
	vary <- matrixStats::colVars(y, na.rm = TRUE)
	X1 <- x - matrix(mnx, nbrow, nbcol, byrow = TRUE)
	Y1 <- y - matrix(mny, nbrow, nbcol, byrow = TRUE)

	sum1 <- base::colSums(xmy^2, na.rm = TRUE)
	sum1[colNA] <- NA
	sum2 <- base::colSums(X1^2, na.rm = TRUE)
	sum2[colNA] <- NA

	NSE <- 1 - sum1/sum2
	MAE <- base::colMeans(abs(xmy), na.rm = TRUE)
	ME <- base::colMeans(xmy, na.rm = TRUE)
	BIAS <- base::colSums(y, na.rm = TRUE)/base::colSums(x, na.rm = TRUE)
	# RMSE <- sqrt(base::colMeans(xmy^2, na.rm = TRUE))

	COV <- base::colSums(X1 * Y1, na.rm = TRUE) / (nbX - 1)
	COV[colNA] <- NA
	CORR <- sqrt(COV^2 / (varx * vary))

	STATS <- rbind(CORR, NSE, BIAS, MAE, ME)
	descrip <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error')
	if(dichotomous){
		N1 <- base::colSums(x >= thres & y >= thres, na.rm = TRUE)
		N1[colNA] <- NA
		N2 <- base::colSums(x < thres & y >= thres, na.rm = TRUE)
		N2[colNA] <- NA
		N3 <- base::colSums(x >= thres & y < thres, na.rm = TRUE)
		N3[colNA] <- NA
		N4 <- base::colSums(x < thres & y < thres, na.rm = TRUE)
		N4[colNA] <- NA
		N <- N1+N2+N3+N4

		FBS <- (N1+N2)/(N1+N3)
		CSI <- N1/(N-N4)
		POD <- N1/(N1+N3)
		FAR <- N2/(N1+N2)

		C0 <- N1+N4
		E0 <- ((N1+N3)*(N1+N2)+(N2+N4)*(N3+N4))/N
		HSS <- (C0-E0)/(N-E0)
		STATS <- rbind(STATS, POD, FAR, FBS, CSI, HSS)
		descrip <- c(descrip, 'Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias',
								'Critical Success Index', 'Heidke Skill Score')
	}

	STATS[is.nan(STATS)] <- NA
	STATS[is.infinite(STATS)] <- NA
	STATS <- round(STATS, 3)
	return(list(statistics = STATS, description = descrip))
}

