########  Popup Menu

popup.opfiles <- tkmenu(all.opfiles, tearoff = FALSE)
tkadd(popup.opfiles, "command", label = "Remove data", command = function() removeList())
tkadd(popup.opfiles, "separator")
tkadd(popup.opfiles, "command", label = "Open in table", command = function(){
	tp.file <- AllOpenFilesType[[as.numeric(tclvalue(tkcurselection(all.opfiles)))+1]]
	if(length(tp.file) != 0){
		if(tp.file == 'ascii'){
			tkconfigure(main.win, cursor = 'watch');tcl('update')
			tab.array <- displayInTable(tknotes)
			tkconfigure(main.win, cursor='')
			ntab <- length(AllOpenTabType)
			AllOpenTabType[[ntab+1]] <<- 'arr'
			AllOpenTabData[[ntab+1]] <<- tab.array
			tkselect(tknotes, ntab)
			#getTableInChange(tknotes)
		}else InsertMessagesTxt(main.txt.out, 'This data type can not be displayed on a table', format = TRUE)
	}else return(NULL)
})

defile.popup <- function(x, y) {
	rootx <- as.integer(tkwinfo("rootx", all.opfiles))
	rooty <- as.integer(tkwinfo("rooty", all.opfiles))
	xTxt <- as.integer(x) + rootx
	yTxt <- as.integer(y) + rooty
	tkselection.clear(all.opfiles, "0", as.character(length(AllOpenFilesData)-1))
	idsel <- tclvalue(tkindex(all.opfiles, paste("@", x,",", y, sep = "")))
	tkselection.set(all.opfiles, idsel)
	#tkitemconfigure(all.opfiles, idsel, selectbackground = "yellow", selectforeground = "blue")
	.Tcl(paste("tk_popup", .Tcl.args(popup.opfiles, xTxt, yTxt)))
}

tkbind(all.opfiles, "<Button-3>", function(x, y){
	if(!is.na(as.numeric(tclvalue(tkcurselection(all.opfiles))))) defile.popup(x, y)
})

########################################
###Remove
removeList <- function(){
	id.act <- as.numeric(tclvalue(tkcurselection(all.opfiles)))+1
	tkdelete(all.opfiles, id.act-1)
	AllOpenFilesType[id.act] <<- NULL
	AllOpenFilesData[id.act] <<- NULL
	#AllOpenFilesType <<- AllOpenFilesType[-id.act]
	#AllOpenFilesData <<- AllOpenFilesData[-id.act]
}

###Display in a table
displayInTable <- function(parent){
	id.act <- as.numeric(tclvalue(tkcurselection(all.opfiles)))+1
	onglet <- addNewTab(parent, tab.title = AllOpenFilesData[[id.act]][[1]])
	dat.file <- AllOpenFilesData[[id.act]][[2]]
	dtab <- tclArrayVar(dat.file)
	table1 <- displayTable(onglet[[2]], tclArray = dtab)
	return(list(onglet, table1, AllOpenFilesData[[id.act]][[3]]))
}

#########################################################################################################


popupAddRemoveRow0 <- function(parent, tabData, tabid){
	#tabid <- as.numeric(tclvalue(tkindex(parent, 'current')))+1
	table1 <- tabData[[tabid]][[2]][[1]]
	data_arr <- tabData[[tabid]][[2]][[2]]
	nl <- data_arr$nrow
	popup.EditTable <- tkmenu(table1, tearoff = FALSE)
	tkadd(popup.EditTable, "command", label = "Insert row above", command = function(){
	 	nl <<- insertRowAbove(table1, data_arr,nl)
	 })
	tkadd(popup.EditTable, "command", label = "Insert row below", command = function(){
	 	nl <<- insertRowBelow(table1, data_arr,nl)
	 })
	tkadd(popup.EditTable, "separator")
	tkadd(popup.EditTable, "command", label = "Delete selected row", command = function(){
		nl <<- deleteSelRow(table1, data_arr,nl)
	})

	defile.popup1 <- function(x, y) {
		rootx <- as.integer(tkwinfo("rootx", table1))
		rooty <- as.integer(tkwinfo("rooty", table1))
		xTxt <- as.integer(x) + rootx
		yTxt <- as.integer(y) + rooty
		if(tclvalue(tkindex(table1, paste("@", x,",", y, sep = ""), "col")) == "0"){
			#tkselection.clear(table1, "all")
			selrow <- tclvalue(tkindex(table1, paste("@", x,",", y, sep = ""), "row"))
			tkselection.set(table1, paste(selrow, 0, sep = ','), paste(selrow,data_arr$ncol, sep = ','))
			#selrow <- unlist(strsplit(tclvalue(tcl(table1, "curselection")),' '))
			#tkselection.set(table1, selrow[1], selrow[length(selrow)])
			.Tcl(paste("tk_popup", .Tcl.args(popup.EditTable, xTxt, yTxt)))
		}
	}

	tkbind(table1, "<Button-3>", function(x, y){
		 defile.popup1(x, y)
	})
}

##################################################
popupAddRemoveRow <- function(parent){
	tabid <- as.numeric(tclvalue(tkindex(parent, 'current')))+1
	table1 <- AllOpenTabData[[tabid]][[2]][[1]]
	data_arr <- AllOpenTabData[[tabid]][[2]][[2]]
	nl <- data_arr$nrow
	popup.EditTable <- tkmenu(table1, tearoff = FALSE)
	tkadd(popup.EditTable, "command", label = "Insert row above", command = function(){
	 	nl <<- insertRowAbove(table1, data_arr,nl)
	 })
	tkadd(popup.EditTable, "command", label = "Insert row below", command = function(){
	 	nl <<- insertRowBelow(table1, data_arr,nl)
	 })
	tkadd(popup.EditTable, "separator")
	tkadd(popup.EditTable, "command", label = "Delete selected row", command = function(){
		nl <<- deleteSelRow(table1, data_arr,nl)
	})

	defile.popup1 <- function(x, y) {
		rootx <- as.integer(tkwinfo("rootx", table1))
		rooty <- as.integer(tkwinfo("rooty", table1))
		xTxt <- as.integer(x) + rootx
		yTxt <- as.integer(y) + rooty
		if(tclvalue(tkindex(table1, paste("@", x,",", y, sep = ""), "col")) == "0"){
			#tkselection.clear(table1, "all")
			selrow <- tclvalue(tkindex(table1, paste("@", x,",", y, sep = ""), "row"))
			tkselection.set(table1, paste(selrow, 0, sep = ','), paste(selrow,data_arr$ncol, sep = ','))
			#selrow <- unlist(strsplit(tclvalue(tcl(table1, "curselection")),' '))
			#tkselection.set(table1, selrow[1], selrow[length(selrow)])
			.Tcl(paste("tk_popup", .Tcl.args(popup.EditTable, xTxt, yTxt)))
		}
	}

	tkbind(table1, "<Button-3>", function(x, y){
		 defile.popup1(x, y)
	})
}
#############################################
insertRowAbove <- function(parent,data_arr,nl){
	tkinsert(parent, "rows", unlist(strsplit(tclvalue(tcl(parent, "curselection")),','))[1], "-1")
	nl <- nl+1
	data_arr$nrow <- nl #as.numeric(tclvalue(tkindex(parent, 'end', 'row')))
	for(i in 1:nl) data_arr[i, 0] <- i
	nl
}

insertRowBelow <- function(parent,data_arr,nl){
	tkinsert(parent, "rows", unlist(strsplit(tclvalue(tcl(parent, "curselection")),','))[1], "1")
	nl <- nl+1
	data_arr$nrow <- nl
	for(i in 1:nl) data_arr[i, 0] <- i
	nl
}

deleteSelRow <- function(parent,data_arr,nl){
	#tkdelete(parent, "rows", unlist(strsplit(tclvalue(tcl(parent, "curselection")),','))[1], "1")
	#nl <- nl-1
	tmp <- unlist(strsplit(tclvalue(tcl(parent, "curselection")),' '))
	tmp <- do.call('rbind', strsplit(tmp,','))
	remrow <- sort(as.numeric(levels(as.factor(tmp[,1]))))
	tkdelete(parent, "rows", remrow[1], length(remrow))
	nl <- nl-length(remrow)

	data_arr$nrow <- nl
	for(i in 1:nl) data_arr[i, 0] <- i
	nl
}



