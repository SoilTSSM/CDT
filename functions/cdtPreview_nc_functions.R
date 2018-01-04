###preview netcdf
preview.data.nc <- function(parent.win, openncf, title.pop){
	if (Sys.info()["sysname"] == "Windows"){
		txta.w <- 38
		txta.h <- 8
		largeur <- 34
	}else{
		txta.w <- 40
		txta.h <- 7
		largeur <- 25
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	####################################

	nc <- try(nc_open(openncf), silent = TRUE)
	if(inherits(nc, "try-error")){
		InsertMessagesTxt(main.txt.out, paste("Unable to open file ", openncf), format = TRUE)
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
		return(NULL)
	}

	ncdim <- nc$ndims
	ncvar <- nc$nvars
	var.info <- data.frame(matrix(NA, ncol = 4, nrow = ncvar))
	var.size <- vector(mode = 'list', length = ncvar)
	var.dimids <- vector(mode = 'list', length = ncvar)
	var.dim.info <- vector(mode = 'list', length = ncvar)
	var.dim.val <- vector(mode = 'list', length = ncvar)

	for(i in 1:ncvar){
		vardim <- nc$var[[i]]$dim
		ndim <- length(vardim)
		dim.info <- data.frame(matrix(NA, ncol = 3, nrow = ncdim))
		dim.val <- vector(mode = 'list', length = ncdim)
		for(j in 1:ndim){
			dim.info[j, 1] <- vardim[[j]]$name
			dim.info[j, 2] <- vardim[[j]]$len
			dim.info[j, 3] <- vardim[[j]]$units
			dim.val[[j]] <- vardim[[j]]$vals
		}
		var.dim.info[[i]] <- dim.info
		var.dim.val[[i]] <- dim.val
		var.info[i, 1] <- nc$var[[i]]$name
		var.info[i, 2] <- nc$var[[i]]$ndims
		var.info[i, 3] <- nc$var[[i]]$units
		var.info[i, 4] <- nc$var[[i]]$longname
		var.size[[i]] <- nc$var[[i]]$size #dim de dims
		var.dimids[[i]] <- nc$var[[i]]$dimids  #ordre de dims
	}

	######
	print.nc <- vector(mode = 'character', length = 0)
	print.nc[[1]] <- paste("file", title.pop, "has", ncdim, "dimensions:")
	for(i in 1:ncdim) print.nc[[i+1]] <- paste(nc$dim[[i]]$name, "  Size:", nc$dim[[i]]$len)
	print.nc[[ncdim+2]] <- paste("----------------------------------------")
	print.nc[[ncdim+3]] <- paste("file", title.pop, "has", ncvar, "variables:")
	for(i in 1:ncvar){
		nd <- nc$var[[i]]$ndims
		dimstring <- '['
		if(nd > 0){
			for(j in 1:nd){
				dimstring <- paste(dimstring, nc$var[[i]]$dim[[j]]$name, sep = '')
				if(j < nd) dimstring <- paste(dimstring, ',', sep = '')
			}
		}
		dimstring <- paste(dimstring, '] ', sep = '')
		print.nc[[i+ncdim+3]] <- paste(nc$var[[i]]$prec, ' ', nc$var[[i]]$name, dimstring,
								' Longname:', nc$var[[i]]$longname, ' Missval:', nc$var[[i]]$missval, sep = '')
	}

	####################################

	fr.haut <- tkframe(tt)

	dim.choose <- ttklabelframe(fr.haut, text = "Data Information", labelanchor = "nw", relief = "sunken", borderwidth = 2)

	var.choix <- c('Choose Variable', paste(var.info[, 1], var.info[, 4], sep = '::'))
	var.dim <- tclVar(var.choix[1])
	X.choix <- c('')
	X.dim <- tclVar(X.choix[1])
	Y.choix <- c('')
	Y.dim <- tclVar(Y.choix[1])

	txt.lb1 <- tklabel(dim.choose, text = 'Variable:', anchor = 'e', justify = 'right')
	txt.lb2 <- tklabel(dim.choose, text = 'Longitude:', anchor = 'e', justify = 'right')
	txt.lb3 <- tklabel(dim.choose, text = 'Latitude:', anchor = 'e', justify = 'right')

	cb.var <- ttkcombobox(dim.choose, values = var.choix, textvariable = var.dim, state = "readonly", width = largeur)
	cb.X <- ttkcombobox(dim.choose, values = X.choix, textvariable = X.dim, state = 'disabled', width = largeur)
	cb.Y <- ttkcombobox(dim.choose, values = Y.choix, textvariable = Y.dim, state = 'disabled', width = largeur)

	tkgrid(txt.lb1, row = 0, column = 0, sticky = 'we', padx = 5, pady = 5)
	tkgrid(cb.var, row = 0, column = 1, sticky = 'we', padx = 5, pady = 5)
	tkgrid(txt.lb2, row = 1, column = 0, sticky = 'we', padx = 5, pady = 5)
	tkgrid(cb.X, row = 1, column = 1, sticky = 'we', padx = 5, pady = 5)
	tkgrid(txt.lb3, row = 2, column = 0, sticky = 'we', padx = 5, pady = 5)
	tkgrid(cb.Y, row = 2, column = 1, sticky = 'we', padx = 5, pady = 5)

	####
	tkbind(cb.var, "<<ComboboxSelected>>", function(){
		ichoix <- which(var.choix == tclvalue(var.dim))
		if(ichoix != 1){
			ivar <<- ichoix-1
			v.ndims <- var.info[ivar, 2]
			X.choix <- c('', var.dim.info[[ivar]][1:v.ndims, 1])
			Y.choix <- c('', var.dim.info[[ivar]][1:v.ndims, 1])
			tkconfigure(cb.X, state = 'normal', values = X.choix)
			tkconfigure(cb.Y, state = 'normal', values = Y.choix)
		}else{
			ivar <<- NULL
			X.choix <- c('')
			Y.choix <- c('')
			tkconfigure(cb.X, state = 'disabled', values = X.choix)
			tkconfigure(cb.Y, state = 'disabled', values = Y.choix)
		}
	})

	#############
	fr.button <- tkframe(fr.haut)

	OK.but <- tkbutton(fr.button, text = "OK", width = 8)
	CA.but <- tkbutton(fr.button, text = "Cancel", width = 8)

	retval <- NULL
	tkconfigure(OK.but, command = function() {
		if(str_trim(tclvalue(X.dim)) == ""){
			tkmessageBox(message = "Select longitude dim", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(Y.dim)) == ""){
			tkmessageBox(message = "Select latitude dim", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			if(!is.null(ivar)){
				v.size <- var.size[[ivar]]
				v.unit <- var.info[ivar, 3]
				v.ndims <- var.info[ivar, 2]
				d.units <- var.dim.info[[ivar]][1:v.ndims, 3]
				d.dim <- var.dim.info[[ivar]][1:v.ndims, 1]
				idx <- which(d.dim == str_trim(tclvalue(X.dim)))
				idy <- which(d.dim == str_trim(tclvalue(Y.dim)))
				lon <- var.dim.val[[ivar]][[idx]]
				lat <- var.dim.val[[ivar]][[idy]]

				d.units <- d.units[c(idx, idy)]
				dat <- ncvar_get(nc, varid = as.character(var.info[ivar, 1]))

				irevlat <- all(lat == cummax(lat))
				xo <- order(lon)
				lon <- lon[xo]
				yo <- order(lat)
				lat <- lat[yo]
			
				if(idx == 1){
					dat <- dat[xo, yo]
				}else{
					dat <- matrix(c(dat), nrow = v.size[idx], ncol = v.size[idy], byrow = TRUE)
					dat <- dat[xo, yo]
				}

				retval <<- list(x = lon, y = lat, value = dat, var.unit = v.unit,
								dim.units = d.units, varid = as.character(var.info[ivar, 1]),
								ilon = idx, ilat = idy, irevlat = irevlat,
								file = openncf)

			}else retval <<- NULL

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
			nc_close(nc)
		}
	})

	tkconfigure(CA.but, command = function() {
		retval <<- NULL
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
		nc_close(nc)
	})

	#####

	tkgrid(OK.but, row = 0, column = 0, padx = 5, pady = 5)
	tkgrid(CA.but, row = 1, column = 0, padx = 5, pady = 5)

	######

	tkgrid(dim.choose, row = 0, column = 0, sticky = 'w', padx = 1, pady = 5)
	tkgrid(fr.button, row = 0, column = 1, sticky = 'e', padx = 5, pady = 5)

	####################################

	fr.bas <- tkframe(tt)

	xscr <- tkscrollbar(fr.bas, repeatinterval = 5, orient = "horizontal", command = function(...) tkxview(txta, ...))
	yscr <- tkscrollbar(fr.bas, repeatinterval = 5, command = function(...) tkyview(txta, ...))
	txta <- tktext(fr.bas, bg = "white", font = "courier", wrap = "none",
					xscrollcommand = function(...) tkset(xscr, ...),
					yscrollcommand = function(...) tkset(yscr, ...),
					width = txta.w, height = txta.h)

	tkgrid(txta, yscr)
	tkgrid(xscr)
	tkgrid.configure(txta, row = 0, column = 0, sticky = "nsew")
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(xscr, sticky = "ew")

	for(i in 1:length(print.nc)) tkinsert(txta, "end", paste(print.nc[i], "\n"))
	tcl("update")

	####################################

	tkgrid(fr.haut, row = 0, column = 0, sticky = 'we', padx = 5, pady = 1)
	tkgrid(fr.bas, row = 1, column = 0, sticky = 'we', padx = 5, pady = 5)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, paste("Data Import Options - ", title.pop))
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(retval)
}

