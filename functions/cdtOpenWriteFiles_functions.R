
##write files (table or csv)
writeFiles <- function(data2save, file2save, row.names = FALSE, col.names = FALSE,
						quote = FALSE, append = FALSE, sep = " ", ...){
	extFl <- tolower(str_trim(file_ext(basename(file2save))))
	sep <- if(extFl == "csv") ',' else sep
	write.table(data2save, file = file2save,
				row.names = row.names, col.names = col.names,
				quote = quote, sep = sep, append = append, ...)
}

########################

###open files (data.frame)

openFiles <- function(parent.win, parent, fileopen){
	if (!nchar(fileopen)) return(NULL)
	else{
		delimter <- preview.data(parent.win, fileopen, basename(fileopen))
		if(!is.null(delimter)){
			dat.file <- try(read.table(fileopen, header = delimter$header, sep = delimter$sepr,
									skip = delimter$skip-1, na.strings = delimter$miss.val, quote = "\"'",
									strip.white = TRUE, stringsAsFactors = FALSE,
									colClasses = "character", comment.char = ""), silent = TRUE)
			# dat.file <- try(fread(fileopen, header = delimter$header, sep = delimter$sepr,
			# 						skip = delimter$skip-1, na.strings = delimter$miss.val, quote = "\"",
			# 						strip.white = TRUE, stringsAsFactors = FALSE, colClasses = "character",
			# 						showProgress = FALSE, data.table = FALSE), silent = TRUE)


			if(inherits(dat.file, "try-error")){
				InsertMessagesTxt(main.txt.out, paste("Unable to read file ", fileopen), format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', dat.file[1]), format = TRUE)
				return(NULL)
			}else{
				tkinsert(parent, "end", basename(fileopen))
				return(list(basename(fileopen), dat.file, fileopen, delimter))
			}
		}else return(NULL)
	}
}

########################
##Open ascii files
getOpenFiles <- function(parent.win, parent, filetype = 'txt', initialdir = getwd()){
	if(filetype == 'txt') filetypes <- "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
	if(filetype == 'csv')filetypes <- "{{CSV Files} {.csv .CSV}} {{Text Files} {.txt .TXT}} {{All files} *}"
	fileopen <- tclvalue(tkgetOpenFile(initialdir = initialdir, initialfile = "", filetypes = filetypes))
	if(fileopen == "") return(NULL)
	if(length(AllOpenFilesData) != 0){
		existff <- unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]]))
		if(basename(fileopen)%in%existff){
			tkmessageBox(message = "File already exists", icon = "warning", type = "ok")
			return(NULL)
		}
	}
	dat.opfiles <- openFiles(parent.win, parent, fileopen)
	if(!is.null(dat.opfiles)) return(dat.opfiles)
	else return(NULL)
}

########################
##Open netcdf files
getOpenNetcdf <- function(parent.win, parent, initialdir = getwd()){
	filetypes <- "{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"
	fileopen <- tclvalue(tkgetOpenFile(initialdir = initialdir, initialfile = "", filetypes = filetypes))
	if(fileopen == "" | is.na(fileopen)) return(NULL)
	if(length(AllOpenFilesData) != 0){
		existff <- unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]]))
		if(basename(fileopen)%in%existff){
			tkmessageBox(message = "File already exists", icon = "warning", type = "ok")
			return(NULL)
		}
	}

	nc.opfiles <- preview.data.nc(parent.win, fileopen, basename(fileopen))
	if(!is.null(nc.opfiles)){
		tkinsert(parent, "end", basename(fileopen))
		return(list(basename(fileopen), nc.opfiles, fileopen))
	}else return(NULL)
}

#########################
getOpenShp <- function(parent.win, parent){
	#parent.win don't use yet
	filetypes <- "{{ESRI Shapefile} {.shp}} {{All files} *}"
	fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
	if(fileopen == "") return(NULL)
	if(length(AllOpenFilesData) != 0){
		existff <- unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]]))
		if(basename(fileopen)%in%existff){
			tkmessageBox(message = "File already exists", icon = "warning", type = "ok")
			return(NULL)
		}
	}

	shp.opfiles <- try(readShapePoly(fileopen), silent = TRUE)
	if(inherits(shp.opfiles, "try-error")){
		InsertMessagesTxt(main.txt.out, paste("Unable to read file ", fileopen), format = TRUE)
		return(NULL)
	}else{
		tkinsert(parent, "end", basename(fileopen))
		return(list(basename(fileopen), shp.opfiles, fileopen))
	}
}
