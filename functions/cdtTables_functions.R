
#open and display table on new tab
openTable <- function(parent.win, parent, fileopen){
	if (!nchar(fileopen)) return(NULL)
	else{
		title.tab <- basename(fileopen)

		delimter <- preview.data(parent.win, fileopen, title.tab)
		if(!is.null(delimter)){
			f.name <- if(delimter$sepr == "") 'read.table' else 'read.csv'
			readFun <- match.fun(f.name)
			is.rdble <- !inherits(try(dat.file <- readFun(fileopen, header = delimter$header, sep = delimter$sepr, skip = delimter$skip, na.strings = delimter$miss.val, quote = "\"'", strip.white = TRUE, colClasses = "character"), silent = TRUE), "try-error")
			if(!is.rdble){
				InsertMessagesTxt(main.txt.out, paste("Unable to read file ", fileopen), format = TRUE)
				return(NULL)
			}else{
				onglet <- addNewTab(parent, tab.title = title.tab)
				dtab <- tclArrayVar(dat.file)
				table1 <- displayTable(onglet[[2]], tclArray = dtab)
				return(list(onglet, table1, fileopen))
			}
		}else{
			return(NULL)
		}
	}
}


##################################################################################################################
tclArrayVar <- function(Rarray = NULL){
	# http://www.sciviews.org/recipes/tcltk/TclTk-using-tk-table-widget/
	if(!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray)) != 2) stop("Array must be one-dimensional or two-dimensional.")

	n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 1L
	name <- paste("::RTcl", n, sep = "")
	arr.env <- list(env = new.env(), nrow = 0, ncol = 0, ndim = 0)
	assign(name, NULL, envir = arr.env$env)
	reg.finalizer(arr.env$env, function(env) tcl("unset", ls(env)))
	class(arr.env) <- "tclArrayVar"

	if(is.null(Rarray)){
		ndim <- 2
		.Tcl(paste("set ", name, "(0,0) \"\"", sep = ""))
	}else{
		if(is.vector(Rarray)){
			ndim <- 1
			Rarray <- as.data.frame(Rarray)
		}else{
			ndim <- 2
			Rarray <- as.data.frame(Rarray)
		}

		if(ndim == 2){
			vec <- unlist(lapply(c(Rarray), function(x) as.character(x)))
			vec <- ifelse(is.na(vec) | is.null(vec),'',vec)
			mat <- data.frame(expand.grid(1:nrow(Rarray), 1:ncol(Rarray)), vec)
			tabs <- data.frame(paste("set ", name, "(", mat[,1], ",", mat[,2], ") \"", paste(mat[,3]), "\"", sep = ""))
			apply(tabs, 1, function(i) .Tcl(i))
		}else{
			vec <- unlist(lapply(c(Rarray), function(x) as.character(x)))
			vec <- ifelse(is.na(vec) | is.null(vec),'',vec)
			mat <- data.frame(1:nrow(Rarray), 1, vec)
			tabs <- data.frame(paste("set ", name, "(", mat[,1], ",", mat[,2], ") \"", paste(mat[,3]), "\"", sep = ""))
			apply(tabs, 1, function(i) .Tcl(i))
		}

		if(!is.null(rownames(Rarray))){
			mat <- data.frame(1:nrow(Rarray), 0, rownames(Rarray))
			tabs <- data.frame(paste("set ", name, "(", mat[,1], ",", mat[,2], ") \"", paste(mat[,3]), "\"", sep = ""))
			apply(tabs, 1, function(i) .Tcl(i))
		}else{
			mat <- data.frame(1:nrow(Rarray), 0,1:nrow(Rarray))
			tabs <- data.frame(paste("set ", name, "(", mat[,1], ",", mat[,2], ") \"", paste(mat[,3]), "\"", sep = ""))
			apply(tabs, 1, function(i) .Tcl(i))
		}

		if(!is.null(colnames(Rarray))){
			mat <- data.frame(0,1:ncol(Rarray), colnames(Rarray))
			tabs <- data.frame(paste("set ", name, "(", mat[,1], ",", mat[,2], ") \"", paste(mat[,3]), "\"", sep = ""))
			apply(tabs, 1, function(i) .Tcl(i))
		}else{
			mat <- data.frame(0,1:ncol(Rarray), 1:ncol(Rarray))
			tabs <- data.frame(paste("set ", name, "(", mat[,1], ",", mat[,2], ") \"", paste('X', mat[,3], sep = ''), "\"", sep = ""))
			apply(tabs, 1, function(i) .Tcl(i))
		}
		arr.env$nrow <- nrow(Rarray)
		arr.env$ncol <- ncol(Rarray)
	}

	arr.env$ndim <- ndim
	#arr.env$array <- Rarray
	return(arr.env)
}


assign("[.tclArrayVar", function(object, i, j = NULL) {
	if(is.null(j) && object$ndim != 1) stop("Object is not a one-dimensional Tclarray")
	if(!is.null(j) && object$ndim != 2) stop("Object is not a two-dimensional Tclarray")
	if(object$ndim == 1) 	j <- 1
	tclArrayName <- ls(object$env)
	tclvalue(paste(tclArrayName, "(", i, ",", j, ")", sep = ""))
})

assign("[<-.tclArrayVar", function(object, i, j = NULL, value) {
	if(is.null(j) && object$ndim != 1) stop("Object is not a one-dimensional Tclarray")
	if(!is.null(j) && object$ndim != 2) stop("Object is not a two-dimensional Tclarray")
	if(object$ndim == 1) 	j <- 1
	tclArrayName <- ls(object$env)
	if(is.null(value) || is.na(value) || value == "") .Tcl(paste("set ", tclArrayName, "(", i, ",", j, ") \"\"", sep = ""))
	else .Tcl(paste("set ", tclArrayName, "(", i, ",", j, ") ", value, sep = ""))
	if(i > object$nrow) object$nrow <- i
	return(object)
})

################################################################################
displayTable <- function(parent, tclArray = NULL, colwidth = "10"){
	if(!is.null(tclArray)){
		for(j in (tclArray$ncol+1):(tclArray$ncol+20)) tclArray[0, j] <- paste('X', j, sep = '')
		for(i in (tclArray$nrow+1):(tclArray$nrow+50)) tclArray[i, 0] <- i
	}else{
		tclArray <- tclArrayVar()
		for(j in 1:40) tclArray[0, j] <- paste('X', j, sep = '')
		for(i in 1:100) tclArray[i, 0] <- i
	}

	table <- tkwidget(parent, "table", rows = tclArray$nrow, cols = tclArray$ncol+20, titlerows = 1, titlecols = 1, resizeborders = "both", colorigin = 0, roworigin = 0, highlightcolor = "blue", anchor = "nw", drawmode = "single", colwidth = colwidth, colstretchmode = "all", rowstretchmode = "all", foreground = "black",
	rowseparator = "\n", colseparator = "\t",
	selecttitle = 1, cache = 1, validate = 1, highlightthickness = 0, ipadx = 1, ipady = 1, wrap = 1, justify = "right",
	xscrollcommand = function(...)tkset(xscr,...), yscrollcommand = function(...)tkset(yscr,...))

	tktag.configure(table, "active", foreground = 'red')
	tktag.configure(table, "sel", foreground = 'blue', background = 'yellow')
	tcl(table, 'width', '0', '5')

	xscr <- tkscrollbar(parent, orient = "horizontal", command = function(...)tkxview(table,...))
	yscr <- tkscrollbar(parent, command = function(...)tkyview(table,...))
	tkgrid(table, yscr)
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid(xscr, sticky = "ew")
	tkconfigure(table, variable = ls(tclArray$env), background = "white", selectmode = "extended")
	tkgrid.configure(table, sticky = 'nswe')
	tkgrid.rowconfigure(table, 0, weight = 1)
	tkgrid.columnconfigure(table, 0, weight = 1)
	tclArray$ncol <- tclArray$ncol+20
	return(list(table, tclArray))

}

#######################

tclArray2dataframe0 <- function(object){
	ncol <- as.numeric(tclvalue(tkindex(object[[1]],'end', 'col')))
	nrow <- as.numeric(tclvalue(tkindex(object[[1]],'end', 'row')))
	Rarray <- ls(object[[2]]$env)
	rownom <- unlist(lapply(paste(Rarray, "(", 1:nrow, ",", 0, ")", sep = ""), function(x) tclvalue(x)))
	colnom <- unlist(lapply(paste(Rarray, "(", 0, ",", 1:ncol, ")", sep = ""), function(x) tclvalue(x)))
	colnames <- colnom[-grep('^X[[:digit:]]+$', colnom)]
	ncolnames <- length(colnames)

	grid.array <- expand.grid(1:nrow, 1:ncol)
	amat.array <- paste(Rarray, "(", grid.array[,1], ",", grid.array[,2], ")", sep = "")
	exist.array <- unlist(lapply(paste("info", "exists", amat.array, sep = " "), function(x) ifelse(tclvalue(.Tcl(x)) == "1", TRUE, FALSE)))
	amat.array <- amat.array[exist.array]
	grid.array <- grid.array[exist.array,]
	values.array <- unlist(lapply(amat.array, function(x) tclvalue(x)))
	data.array <- data.frame(grid.array, values.array)

	if(nrow(data.array) > 0){
		data.array[,3] <- gsub('\\s', NA, as.character(data.array[,3]))
		data.array[data.array == ""] <- NA
		ret.array <- reshape.array(data.array)
		ncretArr <- ncol(ret.array)
		if(ncretArr == ncolnames) colnames(ret.array) <- colnames
		if(ncretArr > ncolnames) colnames(ret.array) <- colnom[1:ncol(ret.array)]
		if(ncretArr < ncolnames){
			ret.array <- cbind(ret.array, matrix(NA, ncol = ncolnames-ncretArr, nrow = nrow(ret.array)))
			colnames(ret.array) <- colnames
		}
		rownames(ret.array) <- rownom[1:nrow(ret.array)]
		ret.array <- ret.array[apply(ret.array, 1, function(x) sum(!is.na(x)) > 0),]
		if(nrow(ret.array) == 0) ret.array[1,] <- NA
	}else{
		ret.array <- data.frame(matrix(NA, ncol = ncolnames))
		names(ret.array) <- colnames
	}
	return(ret.array)
}


#######################
tclArray2dataframe <- function(object){
	ncol <- as.numeric(tclvalue(tkindex(object[[1]],'end', 'col'))) #ou as.numeric(tclvalue(tcl(object[[1]],'cget','-cols')))-1
	#ou as.numeric(tclvalue(tkcget(object[[1]],'-cols')))-1
	nrow <- as.numeric(tclvalue(tkindex(object[[1]],'end', 'row')))  #ou as.numeric(tclvalue(tcl(object[[1]],'cget','-rows')))-1
	Rarray <- ls(object[[2]]$env)
	##row names
	rownom <- unlist(lapply(paste(Rarray, "(", 1:nrow, ",", 0, ")", sep = ""), function(x) tclvalue(x)))
	##col names
	colnom <- unlist(lapply(paste(Rarray, "(", 0, ",", 1:ncol, ")", sep = ""), function(x) tclvalue(x)))
	##values
	grid.array <- expand.grid(1:nrow, 1:ncol)
	amat.array <- paste(Rarray, "(", grid.array[,1], ",", grid.array[,2], ")", sep = "")
	exist.array <- unlist(lapply(paste("info", "exists", amat.array, sep = " "), function(x) ifelse(tclvalue(.Tcl(x)) == "1", TRUE, FALSE)))
	amat.array <- amat.array[exist.array]
	grid.array <- grid.array[exist.array,]
	values.array <- unlist(lapply(amat.array, function(x) tclvalue(x)))
	data.array <- data.frame(grid.array, values.array)
	if(nrow(data.array) > 0){
		ret.array <- reshape.array(data.array)
		ret.array[ret.array == ""] <- NA
		colnames(ret.array) <- colnom[1:ncol(ret.array)]
		rownames(ret.array) <- rownom[1:nrow(ret.array)]
		return(ret.array)
	}else{
		return(NULL)
	}
}

###reshape data.frame to matrix
reshape.array <- function(dat){
	dat <- as.data.frame(dat)
	names(dat) <- c('x', 'y', 'z')
	z <- matrix(NA, nrow = max(dat$x), ncol = max(dat$y))
	z[cbind(dat$x, dat$y)] <- as.character(dat$z)
	return(as.data.frame(z))
}

reshape.array.old <- function(dat){
	dat <- as.data.frame(dat)
	names(dat) <- c('x', 'y', 'z1')
	xy <- expand.grid(x = 1:max(dat$x), y = 1:max(dat$y))
	dxy <- data.frame(xy, z = 1:dim(xy)[1])
	dat <- merge(dat, dxy, by = c('y', 'x'), all = T, sort = FALSE)
	dat <- dat[order(dat[,4]),]
	z <- matrix(dat[,3], nrow = max(dat$x), ncol = max(dat$y))
	return(as.data.frame(z))
}

########################
###change in a table
getTableInChange <- function(parent){
	tabid <- as.numeric(tclvalue(tkindex(parent, 'current')))+1
	table1 <- AllOpenTabData[[tabid]][[2]][[1]]
	data_arr <- AllOpenTabData[[tabid]][[2]][[2]]

	valEnter0 <- NULL
	valEnter <- NULL
	valLeave <- NULL

	enterTableCell <- function(x, y){
		irow <- as.numeric(tclvalue(tkindex(table1, paste("@", x, ",", y, sep = ""), "row")))
		icol <- as.numeric(tclvalue(tkindex(table1, paste("@", x, ",", y, sep = ""), "col")))
		irowL <- as.numeric(tclvalue(tkindex(table1, 'end', 'row')))
		icolL <- as.numeric(tclvalue(tkindex(table1, 'end', 'col')))
		if((irow > 0 & irow <= irowL) & (icol > 0 & icol <= icolL)){
			valexist <- as.logical(as.integer(tclvalue(tcl("info", "exists", paste(ls(data_arr$env), "(", irow, ",", icol, ")", sep = "")))))
			if(valexist) vdat <- data_arr[irow, icol]
			else vdat <- ""
			return(vdat)
		}
	}

	leaveTableCell <- function(){
		if(tclvalue(tcl(table1, 'icursor')) != '-1'){
			irow <- as.numeric(tclvalue(tkindex(table1, "active", "row")))
			icol <- as.numeric(tclvalue(tkindex(table1, "active", "col")))
			irowL <- as.numeric(tclvalue(tkindex(table1, 'end', 'row')))
			icolL <- as.numeric(tclvalue(tkindex(table1, 'end', 'col')))
			if((irow > 0 & irow <= irowL) & (icol > 0 & icol <= icolL)){
				vdat <- tclvalue(tcl(table1, 'curvalue'))
				return(vdat)
			}
		}else{
			vdat <- valLeave
			return(vdat)
		}
	}

	tkbind(table1, "<Button-1>", function(x, y){
		valEnter0 <<- valEnter
		valEnter <<- enterTableCell(x, y)
		valLeave <<- leaveTableCell()
		test <- valEnter0 == valLeave
		if(length(test) == 1){
			if(!test) tkconfigure(tb.save.table, state = 'normal')
			#else  tkconfigure(tb.save.table, state = 'normal')
		}
	})
}



