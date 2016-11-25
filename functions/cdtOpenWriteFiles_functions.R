
##write files (table or csv)
writeFiles <- function(dat2save, file2save, row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE,...){
	require(tools)
	extFl <- file_ext(basename(file2save))
	if(extFl == "csv" | extFl == "CSV") sep = ','
	else  sep = ' '
	write.table(dat2save, file = file2save, row.names = row.names, col.names = col.names,
							quote = quote, sep = sep, append = append,...)
}

########################

###open files (data.frame)

openFiles <- function(parent.win, parent, fileopen){
	if (!nchar(fileopen)) return(NULL)
	else{
		title.tab <- basename(fileopen)

		delimter <- preview.data(parent.win, fileopen, title.tab)
		if(!is.null(delimter)){
			f.name <- if(delimter$sepr == "") 'read.table' else 'read.csv'
			readFun <- match.fun(f.name)
			is.rdble <- !inherits(try(dat.file <- readFun(fileopen, header = delimter$header, sep = delimter$sepr,
									skip = delimter$skip-1, na.strings = delimter$miss.val, quote = "\"'",
									strip.white = TRUE, stringsAsFactors = FALSE, colClasses = "character",
									comment.char = ""), silent = TRUE), "try-error")
			if(!is.rdble){
				InsertMessagesTxt(main.txt.out, paste("Unable to read file ", fileopen), format = TRUE)
				return(NULL)
			}else{
				tkinsert(parent, "end", title.tab)
				return(list(title.tab, as.data.frame(dat.file), fileopen, delimter))
			}
		}else{
			return(NULL)
		}
	}
}

########################
##Open ascii files
getOpenFiles <- function(parent.win, parent){
	fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "",
						filetypes = "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"))
	if(fileopen == "") return(NULL)
	if(length(AllOpenFilesData) != 0){
		existff <- unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]]))
		if(basename(fileopen)%in%existff){
			tkmessageBox(message = "File already exists", icon = "warning", type = "ok")
			return(NULL)
		}else{
			dat.opfiles <- openFiles(parent.win, parent, fileopen)
			if(!is.null(dat.opfiles)) return(dat.opfiles)
			else return(NULL)
		}
	}else{
		dat.opfiles <- openFiles(parent.win, parent, fileopen)
		if(!is.null(dat.opfiles)) return(dat.opfiles)
		else return(NULL)
	}
}


########################
##Open netcdf files
getOpenNetcdf <- function(parent.win, parent){
	fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "",
							filetypes = "{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
	if(fileopen == "" | is.na(fileopen)) return(NULL)
	if(length(AllOpenFilesData) != 0){
		existff <- unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]]))
		if(basename(fileopen)%in%existff){
			tkmessageBox(message = "File already exists", icon = "warning", type = "ok")
			return(NULL)
		}else{
			title.tab <- basename(fileopen)
			nc.opfiles <- preview.data.nc(parent.win, fileopen, title.tab)
			if(!is.null(nc.opfiles)){
				tkinsert(parent, "end", title.tab)
				return(list(title.tab, nc.opfiles, fileopen))
			}else{
				return(NULL)
			}
		}
	}else{
		title.tab <- basename(fileopen)
		nc.opfiles <- preview.data.nc(parent.win, fileopen, title.tab)
		if(!is.null(nc.opfiles)){
			tkinsert(parent, "end", title.tab)
			return(list(title.tab, nc.opfiles, fileopen))
		}else{
			return(NULL)
		}
	}
}

#########################
getOpenShp <- function(parent.win, parent){
	#parent.win don't use yet
	fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes="{{ESRI Shapefile} {.shp}} {{All files} *}"))
	if(fileopen == "") return(NULL)
	if(length(AllOpenFilesData) != 0){
		existff <- unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]]))
		if(basename(fileopen)%in%existff){
			tkmessageBox(message = "File already exists", icon = "warning", type = "ok")
			return(NULL)
		}else{
			title.tab <- basename(fileopen)
			is.rdble <- !inherits(try(shp.opfiles <- readShapePoly(fileopen), silent = TRUE), "try-error")
			if(!is.rdble){
				InsertMessagesTxt(main.txt.out, paste("Unable to read file ", fileopen), format = TRUE)
				return(NULL)
			}else{
				tkinsert(parent, "end", title.tab)
				return(list(title.tab, shp.opfiles, fileopen))
			}
		}
	}else{
		title.tab <- basename(fileopen)
		is.rdble <- !inherits(try(shp.opfiles <- readShapePoly(fileopen), silent = TRUE), "try-error")
		if(!is.rdble){
			InsertMessagesTxt(main.txt.out, paste("Unable to read file ", fileopen), format = TRUE)
			return(NULL)
		}else{
			tkinsert(parent, "end", title.tab)
			return(list(title.tab, shp.opfiles, fileopen))
		}
	}
}

