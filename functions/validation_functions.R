
ValidationStatisticsFun <- function(x, y, dichotomous){
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
	RMSE <- sqrt(base::colMeans(xmy^2, na.rm = TRUE))

	COV <- base::colSums(X1 * Y1, na.rm = TRUE) / (nbX - 1)
	COV[colNA] <- NA
	CORR <- sqrt(COV^2 / (varx * vary))

	##################
	# Dichotomous
	thres <- dichotomous$opr.thres
	fun <- match.fun(dichotomous$opr.fun)

	YesOBS <- fun(x, thres)
	YesFCST <- fun(y, thres)

	##################

	# hit
	Hit <- YesOBS & YesFCST
	# false alarm
	False <- !YesOBS & YesFCST
	# miss
	Miss <- YesOBS & !YesFCST
	# correct negative
	TrueNull <- !YesOBS & !YesFCST

	##################

	N1 <- base::colSums(Hit, na.rm = TRUE)
	N1[colNA] <- NA

	N2 <- base::colSums(False, na.rm = TRUE)
	N2[colNA] <- NA

	N3 <- base::colSums(Miss, na.rm = TRUE)
	N3[colNA] <- NA

	N4 <- base::colSums(TrueNull, na.rm = TRUE)
	N4[colNA] <- NA

	##################

	N <- N1+N2+N3+N4

	FBS <- (N1+N2)/(N1+N3)
	CSI <- N1/(N-N4)
	POD <- N1/(N1+N3)
	FAR <- N2/(N1+N2)

	C0 <- N1+N4
	E0 <- ((N1+N3)*(N1+N2)+(N2+N4)*(N3+N4))/N
	HSS <- (C0-E0)/(N-E0)

	##################

	Fct.hit <- y
	Fct.hit[!Hit] <- NA
	Fct.hit.v <- base::colSums(Fct.hit, na.rm = TRUE)
	Fct.hit.i <- base::colSums(!is.na(Fct.hit), na.rm = TRUE)
	Fct.hit.v[colNA] <- NA
	Fct.hit.i[colNA] <- NA

	Fct.false <- y
	Fct.false[!False] <- NA
	Fct.false.v <- base::colSums(Fct.false, na.rm = TRUE)
	Fct.false.i <- base::colSums(!is.na(Fct.false), na.rm = TRUE)
	Fct.false.v[colNA] <- NA
	Fct.false.i[colNA] <- NA

	Obs.miss <- x
	Obs.miss[!Miss] <- NA
	Obs.miss.v <- base::colSums(Obs.miss, na.rm = TRUE)
	Obs.miss.i <- base::colSums(!is.na(Obs.miss), na.rm = TRUE)
	Obs.miss.v[colNA] <- NA
	Obs.miss.i[colNA] <- NA

	##################

	# Volumetric Hit Index
	VHI <- Fct.hit.v / (Fct.hit.v + Obs.miss.v)

	# Quantile Probability of Detection
	QPOD <- Fct.hit.i / (Fct.hit.i +Obs.miss.i)

	# Volumetric False Alarm Ratio
	VFAR <- Fct.false.v / (Fct.hit.v + Fct.false.v)

	# Quantile False Alarm Ratio
	QFAR <- Fct.false.i / (Fct.hit.i + Fct.false.i)

	# Volumetric Miss Index
	VMI <- Obs.miss.v / (Fct.hit.v + Obs.miss.v)

	# Volumetric Critical Success Index 
	VCSI <- Fct.hit.v / (Fct.hit.v + Obs.miss.v + Fct.false.v)

	# Quantile Critical Success Index
	QCSI <- Fct.hit.i / (Fct.hit.i + Obs.miss.i + Fct.false.i)

	##################

	STATS <- rbind(CORR, NSE, BIAS, MAE, ME, RMSE, POD, FAR, FBS, CSI, HSS,
					VHI, QPOD, VFAR, QFAR, VMI, VCSI, QCSI)
	descrip <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error',
				'Mean Error', 'Root Mean Square Error', 'Probability Of Detection', 'False Alarm Ratio',
				'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score',
				'Volumetric Hit Index', 'Quantile Probability of Detection', 'Volumetric False Alarm Ratio',
				'Quantile False Alarm Ratio', 'Volumetric Miss Index', 'Volumetric Critical Success Index',
				'Quantile Critical Success Index')

	STATS[is.nan(STATS)] <- NA
	STATS[is.infinite(STATS)] <- NA
	STATS <- round(STATS, 3)
	return(list(statistics = STATS, description = descrip))
}

