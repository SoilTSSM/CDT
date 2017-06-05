
filterCDTdata <- function(GeneralParameters){
	if(GeneralParameters$filein == ""){
		InsertMessagesTxt(main.txt.out, 'No input CDT data found', format = TRUE)
		return(NULL)
	}

	donne <- getStnOpenData(GeneralParameters$filein)
	if(is.null(donne)) return(NULL)
	donneInfo <- getStnOpenDataInfo(GeneralParameters$filein)
	if(is.null(donneInfo)) return(NULL)

	ix <- grepl('[[:digit:]]', donne[, 1])
	seph <- rle(ix)
	ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
	if(length(ipos) == 0){
		InsertMessagesTxt(main.txt.out, 'The input data is not in a standard unambiguous CDT format', format = TRUE)
		return(NULL)
	}
	if(ipos[1] != 1){
		InsertMessagesTxt(main.txt.out, 'The input data is not in a standard unambiguous CDT format', format = TRUE)
		return(NULL)
	}
	header <- as.matrix(donne[1:seph$lengths[ipos[1]], , drop = FALSE])
	daty <- donne[ix, 1]
	donne <- as.matrix(donne[ix, -1, drop = FALSE])
	len <- nrow(donne)
	pnadonne <- (len - base::colSums(is.na(donne)))/len

	opfilter <- match.fun(GeneralParameters$opfilter)
	istn <- as.logical(opfilter(pnadonne, GeneralParameters$valfilter/100))

	donne <- donne[, istn, drop = FALSE]
	header <- header[, c(TRUE, istn), drop = FALSE]
	donne <- rbind(header, cbind(daty, donne))
	donne[is.na(donne)] <- donneInfo[[3]]$miss.val

	writeFiles(donne, GeneralParameters$file2save)

	return(0)
}


