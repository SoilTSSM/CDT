###Onglets manupilation

.Tcl(paste("image create photo img_close -file ", '"', file.path(imgdir, "closeTabButton0.gif"), '"',sep = ""))
.Tcl(paste("image create photo img_closeactive  -file ", '"', file.path(imgdir, "closeTabButton1.gif"), '"',sep = ""))
.Tcl(paste("image create photo img_closepressed -file ", '"', file.path(imgdir, "closeTabButton2.gif"), '"',sep = ""))

try(.Tcl('ttk::style element create Fermer image [list img_close {active pressed !disabled} img_closepressed {active  !disabled} img_closeactive ] -border 4 -sticky e'), silent = TRUE)

.Tcl('ttk::style layout TNotebook {TNotebook.client -sticky nswe}')

.Tcl("ttk::style layout TNotebook.Tab {
		TNotebook.tab -sticky nswe -children {
			TNotebook.padding  -side top -sticky nswe -children {
				TNotebook.focus -side top -sticky nswe -children {
					TNotebook.label -side left -sticky {}
					TNotebook.Fermer  -side right -sticky e
				}
			}
		}
	}", sep = '\n')

btn_press <- function(x, y, W){
	elem <- tclvalue(tcl(W, 'identify', 'element', x, y))
	index <- tclvalue(tcl(W, 'identify', 'tab', x, y))
		if(elem == 'Fermer'){
			.Tcl(paste(W, 'state pressed'))
			tclvalue(pressed_index) <<- index
		}
}
#
btn_releases <- function(x, y, W){
	if(!as.logical(.Tcl(paste(W, 'instate pressed')))) return(NULL)
	else{
		elem <- tclvalue(tcl(W, 'identify', 'element', x, y))
		index <- tclvalue(tcl(W, 'identify', 'tab', x, y))
		if(elem == 'Fermer' &  tclvalue(pressed_index) == index){
			CloseNotebookTab(index)
		}
		.Tcl(paste(W, 'state !pressed'))
		tclvalue(pressed_index) <<- ''
	}
}

########################################################################
##ADD TAB
addNewTab <- function(parent, tab.title = NULL){
	if(is.null(tab.title)) tab.title <- paste('Tab', infoTabs(parent), '  ')
	tab <- ttkframe(parent)
	tkadd(parent, tab, text = paste(tab.title, '  '))
	tkgrid.columnconfigure(tab, 0, weight = 1)
	tkgrid.rowconfigure(tab, 0, weight = 1)

	ftab <- tkframe(tab, bd = 2, relief = 'sunken', bg = 'white')
	tkgrid(ftab, row = 0, column = 0, sticky = 'nswe')
	tkgrid.columnconfigure(ftab, 0, weight = 1)
	tkgrid.rowconfigure(ftab, 0, weight = 1)
	return(list(tab, ftab))
}

#####################
##Count created tabs
infoTabs <- function(parent){
	open.tabs <- unlist(strsplit(tclvalue(tkwinfo("children", parent)), ' '))
	end.tabs <- as.numeric(unlist(strsplit(open.tabs[length(open.tabs)], "\\.")))
	id.tabs <- end.tabs[length(end.tabs)]
	if(length(id.tabs) == 0) id.tabs <- 0
	return(id.tabs+1)
}

########################################################################
#display array in Onglet

displayArrayTab <- function(parent.win, parent){
	filetypes <- "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
	fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
	tkconfigure(main.win, cursor = 'watch');tcl('update')
	tab.array <- openTable(parent.win, parent, fileopen)
	tkconfigure(main.win, cursor = '')
	if(!is.null(tab.array)) return(tab.array)
	else return(NULL)
}

########################################################################
#display output console
displayConsOutput <- function(parent, out2disp, rhtests = FALSE){
	xscr <- tkscrollbar(parent, repeatinterval = 5, orient = "horizontal",
						command = function(...) tkxview(txta, ...))
	yscr <- tkscrollbar(parent, repeatinterval = 5,
						command = function(...) tkyview(txta, ...))
	txta <- tktext(parent, bg = "white", font = "courier", wrap = "none", 
					xscrollcommand = function(...) tkset(xscr, ...),
					yscrollcommand = function(...) tkset(yscr, ...))
	tkgrid(txta, yscr)
	tkgrid(xscr)
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(xscr, sticky = "ew")
	tkgrid.configure(txta, sticky = 'nswe')
	tkgrid.rowconfigure(txta, 0, weight = 1)
	tkgrid.columnconfigure(txta, 0, weight = 1)
	if(rhtests){
		tkinsert(txta, "end", out2disp)
	}else{
		tempfile <- file.path(apps.dir, 'tmp_display', fsep = .Platform$file.sep)
		sink(tempfile)
		op <- options()
		options(width = 160)
		print(out2disp)
		options(op)
		sink()
		rdL <- readLines(tempfile, warn = FALSE)
		for(i in 1:length(rdL)) tkinsert(txta, "end", paste(rdL[i], "\n"))
		unlink(tempfile)
	}
}

########################################################################
displayConsOutputTabs <- function(parent, out2disp, title, rhtests = FALSE){ #tknotes
	onglet <- addNewTab(parent, tab.title = title)
	displayConsOutput(onglet[[2]], out2disp, rhtests)
	return(onglet)
}


########################################################################

DisplayQcHom <- function(parent, outqchom, title){
	onglet <- addNewTab(parent, tab.title = title)
	dtab <- tclArrayVar(outqchom[[1]])
	col <- if(ReturnExecResults$action == 'homog' | GeneralParameters$action == "rhtests") '15' else '10'
	table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = col)
	return(list(onglet, table1, outqchom[-1]))
}

########################################################################
DisplayHomInfo <- function(parent, homInfo, title){
	onglet <- addNewTab(parent, tab.title = title)
	dtab <- tclArrayVar(homInfo)
	table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = '24')
	return(list(onglet, table1))
}

########################################################################
DisplayInterpData <- function(parent, data, title){
	onglet <- addNewTab(parent, tab.title = title)
	dtab <- tclArrayVar(data[[2]])
	table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = '15')
	return(list(onglet, table1, data[-2]))
}

########################################################################
##Close tab table and save (tab array)
CloseNotebookTab <- function(index){
	#tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
	tabid <- as.numeric(index)+1
	if(is.na(tabid)){
		return(NULL)
	}else{
		arrTypes <- c("arr", "arrhom", "arrRHtest", "arrqc", "arrzc", "arrInterp",
						"homInfo", "StnInfo", "arrValid")
		if(AllOpenTabType[[tabid]]%in%arrTypes){
			tkdestroy(AllOpenTabData[[tabid]][[1]][[1]])
		}else if(AllOpenTabType[[tabid]] == "ctxt"){
			tkdestroy(AllOpenTabData[[tabid]][[1]])
		}else if(AllOpenTabType[[tabid]] == "img"){
			tkdestroy(AllOpenTabData[[tabid]][[1]][[2]])
			tkdestroy(AllOpenTabData[[tabid]][[1]][[1]])
		}else{
			return(NULL)
		}
		AllOpenTabData[tabid] <<- NULL
		AllOpenTabType[tabid] <<- NULL
	}
}

########################################################################
SaveNotebookTabArray <- function(parent){
	tabid <- as.numeric(tclvalue(tkindex(parent, 'current')))+1
	if(length(AllOpenTabType) > 0){
		if(AllOpenTabType[[tabid]] == "arr"){
			filetosave <- AllOpenTabData[[tabid]][[3]]
			Objarray <- AllOpenTabData[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)
			write.table(dat2sav, filetosave, row.names = FALSE, col.names = TRUE)
		}else if(AllOpenTabType[[tabid]] == "arrhom"){
			if(ReturnExecResults$action == 'homog' & ReturnExecResults$period == 'daily'){
				filetosave <- AllOpenTabData[[tabid]][[3]]
				f2sdly <- filetosave[[1]]
				f2sdek <- filetosave[[2]]
				f2smon <- filetosave[[3]]
				Objarray <- AllOpenTabData[[tabid]][[2]]
				dat2format <- tclArray2dataframe(Objarray)
				dat2sav <- reHomOutFormat(dat2format)
				write.table(dat2sav[[1]], f2sdly, row.names = FALSE, col.names = TRUE)
				write.table(dat2sav[[2]], f2sdek, row.names = FALSE, col.names = TRUE)
				write.table(dat2sav[[3]], f2smon, row.names = FALSE, col.names = TRUE)
			}else if(ReturnExecResults$action == 'homog' & ReturnExecResults$period == 'dekadal'){
				filetosave <- AllOpenTabData[[tabid]][[3]]
				f2sdek <- filetosave[[1]]
				f2smon <- filetosave[[2]]
				Objarray <- AllOpenTabData[[tabid]][[2]]
				dat2format <- tclArray2dataframe(Objarray)
				dat2sav <- reHomOutFormat(dat2format)
				write.table(dat2sav[[1]], f2sdek, row.names = FALSE, col.names = TRUE)
				write.table(dat2sav[[2]], f2smon, row.names = FALSE, col.names = TRUE)
			}else if(ReturnExecResults$action == 'homog' & ReturnExecResults$period == 'monthly'){
				filetosave <- AllOpenTabData[[tabid]][[3]]
				f2smon <- filetosave[[1]]
				Objarray <- AllOpenTabData[[tabid]][[2]]
				dat2format <- tclArray2dataframe(Objarray)
				dat2sav <- reHomOutFormat(dat2format)
				write.table(dat2sav[[1]], f2smon, row.names = FALSE, col.names = TRUE)
			}else{
				InsertMessagesTxt(main.txt.out, 'The table could not be saved correctly', format = TRUE)
			}
		}else if(AllOpenTabType[[tabid]] == "arrRHtest"){
			f2save <- AllOpenTabData[[tabid]][[3]][[1]]
			Objarray <- AllOpenTabData[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)
			head <- readLines(f2save, n = 1)
			cat(paste(head, '\n'), file = f2save)
			if(!is.null(dat2sav)){
				dat2sav <- dat2sav[!is.na(dat2sav[, 3]), ]
				nline <- nrow(dat2sav)
				if(nline > 0){
					tmp4 <- strsplit(str_trim(gsub("[()-]", " ", dat2sav[, 4])), ' ')
					tmp4 <- lapply(tmp4, function(x) if(length(x) == 0) c(NA, NA) else x)
					tmp7 <- strsplit(str_trim(gsub("[()-]", " ", dat2sav[, 7])), ' ')
					tmp7 <- lapply(tmp7, function(x) if(length(x) == 0) c(NA, NA) else x)
					tmp <- cbind(dat2sav[, 1:3], do.call('rbind', tmp4), dat2sav[, 5:6], do.call('rbind', tmp7))
					tmp <- apply(tmp, 2, as.character)
					if(is.null(dim(tmp))) tmp <- matrix(tmp, nrow = 1)
					colClasses <- c('numeric', 'character', rep('numeric', 7))
					tmp0 <- data.frame(matrix(NA, ncol = ncol(tmp), nrow = nrow(tmp)))
					for(j in 1:ncol(tmp)) tmp0[, j] <- as(tmp[, j], colClasses[j])
					for(j in 1:nline){
						cat(paste(
							ifelse(is.na(tmp0[j, 1]), sprintf("%1.0s", ''), sprintf("%1.0f", tmp0[j, 1])), " ",
							sprintf("%-4.4s", ifelse(is.na(tmp0[j, 2]), '',tmp0[j, 2])),
							ifelse(is.na(tmp0[j, 3]), sprintf("%10.0s", ''), sprintf("%10.0f", tmp0[j, 3])), " (",
							ifelse(is.na(tmp0[j, 4]), sprintf("%6.4s", ''), sprintf("%6.4f", tmp0[j, 4])), "-",
							ifelse(is.na(tmp0[j, 5]), sprintf("%6.4s", ''), sprintf("%6.4f", tmp0[j, 5])), ")",
							ifelse(is.na(tmp0[j, 6]), sprintf("%6.3s", ''), sprintf("%6.3f", tmp0[j, 6])),
							ifelse(is.na(tmp0[j, 7]), sprintf("%10.4s", ''), sprintf("%10.4f", tmp0[j, 7])), " (",
							ifelse(is.na(tmp0[j, 8]), sprintf("%10.4s", ''), sprintf("%10.4f", tmp0[j, 8])), "-",
							ifelse(is.na(tmp0[j, 9]), sprintf("%10.4s", ''), sprintf("%10.4f", tmp0[j, 9])), ")\n",
						sep = ""), file = f2save, append = TRUE)
					}
				}
			}
		}else if(AllOpenTabType[[tabid]] == "arrqc"){
			f2save <- AllOpenTabData[[tabid]][[3]][[1]]
			Objarray <- AllOpenTabData[[tabid]][[2]]
			dat2sav <- tclArray2dataframe0(Objarray)
			write.table(dat2sav, f2save, row.names = FALSE, col.names = TRUE)
		}else if(AllOpenTabType[[tabid]] == "arrzc"){
			f2save <- AllOpenTabData[[tabid]][[3]][[1]]
			Objarray <- AllOpenTabData[[tabid]][[2]]
			dat2sav <- tclArray2dataframe0(Objarray)
			write.table(dat2sav, f2save, row.names = FALSE, col.names = TRUE)
		}else if(AllOpenTabType[[tabid]] == "arrInterp"){
			Objarray <- AllOpenTabData[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)
			elvd <- as.numeric(as.character(dat2sav$elv))
			if(sum(!is.na(elvd)) == 0) elvd <- NULL
			donnees <- list(date = AllOpenTabData[[tabid]][[3]][[1]],
							lon = as.numeric(as.character(dat2sav$lon)),
							lat = as.numeric(as.character(dat2sav$lat)),
							id = as.character(dat2sav$id),
							z = as.numeric(as.character(dat2sav$z)),
							elv = elvd)
#			cat(AllOpenTabData[[tabid]][[3]][[2]],'\n')
			assign('donnees', donnees, envir = EnvInterpolation)
		}else if(AllOpenTabType[[tabid]] == "StnInfo"){
			if(ReturnExecResults$action == 'chk.coords'){
				f2save <- file.path(ReturnExecResults$outdir, paste(getf.no.ext(GeneralParameters$IO.files$STN.file), '_2CORRECT_STATIONS.txt', sep = ''))
				Objarray <- AllOpenTabData[[tabid]][[2]]
				dat2sav <- tclArray2dataframe(Objarray)
				if(is.null(dat2sav)){
					dat2sav <- data.frame(NA, NA, NA, NA, NA)
					names(dat2sav) <- c('Info', 'ID.Station', 'Longitude', 'Latitude', 'ID.Col')
				}
				write.table(dat2sav, f2save, row.names = FALSE, col.names = TRUE)
			}else{
				InsertMessagesTxt(main.txt.out, 'The table could not be saved correctly', format = TRUE)
			}
		}else{
			return(NULL)
		}
	}else{
		return(NULL)
	}
}


####################################################

SavePlot <- function(){
	tabid <- as.numeric(tclvalue(tkindex(tknotes, 'current')))+1
	if(length(AllOpenTabType) > 0){
		if(AllOpenTabType[[tabid]] == "img"){
			filetypes <- "{JPEG {.jpeg .jpg}} {{All files} {*.*}}"
			if(Sys.info()["sysname"] == "Windows") filename <- tclvalue(tkgetSaveFile(initialfile = "", filetypes = filetypes, defaultextension = TRUE))
			else filename <- tclvalue(tkgetSaveFile(initialfile = "", filetypes = filetypes))
			if (filename != ""){
				#jpeg(file = filename, width = 960, height = 480)
				#AllOpenTabData[[tabid]][[2]]$fun()
				width <- as.numeric(tclvalue(tkget(spinH)))*480
				height <- as.numeric(tclvalue(tkget(spinV)))*480
				jpeg(file = filename, width = width, height = height)
				if(class(AllOpenTabData[[tabid]][[2]]) == "tkwin") AllOpenTabData[[tabid]][[2]]$fun()
				else AllOpenTabData[[tabid]][[2]][[2]]$fun()
				dev.off()
			}
		}
	}else{
		return(NULL)
	}
}

############################################

