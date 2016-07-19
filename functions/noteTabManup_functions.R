
######################################
#Get Tab containing image display
imageNotebookTab_unik <- function(parent, imgContainer, notebookTab, tabType, tabData){
	ntab <- length(tabType)
	if(is.null(notebookTab)){
		tabType[[ntab+1]] <- 'img'
		tabData[[ntab+1]] <- imgContainer
		ntbkIdTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab[[2]])
			if(length(idTabs) > 0){
				ntbkIdTab <- tabData[[idTabs]][[1]][[1]]$ID
				tabData[[idTabs]][[2]] <- imgContainer[[2]]
				tkselect(parent, idTabs-1)
			}else{
				tabType[[ntab+1]] <- 'img'
				tabData[[ntab+1]] <- imgContainer
				ntbkIdTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
			}
		}else{
			tabType[[1]] <- 'img'
			tabData[[1]] <- imgContainer
			ntbkIdTab <- tabData[[1]][[1]][[1]]$ID
			tkselect(parent, 0)
		}
	}
	retTab <- list(notebookTab = list(imgContainer[[1]], ntbkIdTab), AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

######################################
#Open new tab if not exist

imageNotebookTab_open <- function(parent, notebookTab, tabTitle, tabType, tabData){
	if(is.null(notebookTab)){
		onglet <- addNewTab(parent, tab.title = tabTitle)
	}else{
		ntab <- length(tabType)
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab[[2]])

			if(length(idTabs) > 0){
				onglet <- notebookTab[[1]]
				if(class(tabData[[idTabs]][[2]]) == "tkwin"){
					if(tclvalue(tkwinfo('class', tkwinfo('children', tabData[[idTabs]][[1]][[2]]))) == "Label") tkdestroy(tabData[[idTabs]][[2]])
					#new (scorllWin)
					else tcl('destroy', tkwinfo('children', tabData[[idTabs]][[1]][[2]]))
				}else{#or class(tabData[[idTabs]][[2]]) == "list"
					##tabData[[idTabs]][[1]][[2]] < ==  == onglet[[2]]
					##canvas qc, extrait data
					if(tclvalue(tkwinfo('class', tkwinfo('children', tabData[[idTabs]][[1]][[2]]))) == "Canvas") tkdestroy(tabData[[idTabs]][[2]][[1]])
					#new (scorllWin)
					else tcl('destroy', tkwinfo('children', tabData[[idTabs]][[1]][[2]]))
				}

				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', tabTitle)
			}else{
				onglet <- addNewTab(parent, tab.title = tabTitle)
			}
		}else{
			onglet <- addNewTab(parent, tab.title = tabTitle)
		}
	}
	return(onglet)
}

######################################
##Qc table edit output

tableQcEditNotebookTab_unik <- function(parent, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)
	QcFormat <- QcOutFormat()

	if(is.null(notebookTab)){
		tableDisp <- DisplayQcHom(parent, QcFormat, titleTab)
		tabType[[ntab+1]] <- 'arrqc'
		tabData[[ntab+1]] <- tableDisp
		table1 <- tabData[[ntab+1]][[2]][[1]]
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		popupAddRemoveRow0(parent, tabData, ntab+1)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(QcFormat[[1]])
				tabData[[idTabs]][[3]][[1]] <- QcFormat[[2]]
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 15)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				table1 <- tabData[[idTabs]][[2]][[1]]
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
				popupAddRemoveRow0(parent, tabData, idTabs)
			}else{
				tableDisp <- DisplayQcHom(parent, QcFormat, titleTab)
				tabType[[ntab+1]] <- 'arrqc'
				tabData[[ntab+1]] <- tableDisp
				table1 <- tabData[[ntab+1]][[2]][[1]]
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				popupAddRemoveRow0(parent, tabData, ntab+1)
			}
		}else{
			tableDisp <- DisplayQcHom(parent, QcFormat, titleTab)
			tabType[[ntab+1]] <- 'arrqc'
			tabData[[ntab+1]] <- tableDisp
			table1 <- tabData[[ntab+1]][[2]][[1]]
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			popupAddRemoveRow0(parent, tabData, ntab+1)
		}
	}

	if(ReturnExecResults$action == 'qc.temp'){
		if(as.character(GeneralParameters$use.method$Values[1]) == "0"){
			if(as.character(GeneralParameters$use.method$Values[2]) == "1"){
				.Tcl(paste(table1, 'tag', 'celltag', 'ttestval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttestval", bg = "aquamarine")
				.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 9, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1")
				.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 10, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1")
			}else{
				.Tcl(paste(table1, 'tag', 'celltag', 'ttestval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 7, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttestval", bg = "aquamarine")
				.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1")
				.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 9, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1")
			}
		}else{
			if(as.character(GeneralParameters$use.method$Values[2]) == "1"){
				.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 7, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1")
				.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1")
			}else{
				.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 6, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1")
				.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 7, sep = ',',collapse=' ')))
				tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1")
			}
		}
	}
	if(ReturnExecResults$action == 'qc.rain'){
		if(as.character(GeneralParameters$use.method$Values[1]) == "0"){
			.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',',collapse=' ')))
			tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1")
			.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 9, sep = ',',collapse=' ')))
			tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1")
		}else{
			.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 7, sep = ',',collapse=' ')))
			tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1")
			.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',',collapse=' ')))
			tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1")
		}
	}

	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

######################################
##Zeros check table display

tableZeroCheckNotebookTab_unik <- function(parent, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)
	ZChkFormat <- QcOutZeroChkFormat()

	if(is.null(notebookTab)){
		tableDisp <- DisplayQcHom(parent, ZChkFormat, titleTab)
		tabType[[ntab+1]] <- 'arrzc'
		tabData[[ntab+1]] <- tableDisp
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		popupAddRemoveRow0(parent, tabData, ntab+1)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(ZChkFormat[[1]])
				tabData[[idTabs]][[3]][[1]] <- ZChkFormat[[2]]
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 10)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
				popupAddRemoveRow0(parent, tabData, idTabs)
			}else{
				tableDisp <- DisplayQcHom(parent, ZChkFormat, titleTab)
				tabType[[ntab+1]] <- 'arrzc'
				tabData[[ntab+1]] <- tableDisp
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				popupAddRemoveRow0(parent, tabData, ntab+1)
			}
		}else{
			tableDisp <- DisplayQcHom(parent, ZChkFormat, titleTab)
			tabType[[ntab+1]] <- 'arrzc'
			tabData[[ntab+1]] <- tableDisp
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			popupAddRemoveRow0(parent, tabData, ntab+1)
		}
	}
	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

######################################
##Zeros check neighbors stations table

tableZeroCheckNotebookTab_unik1 <- function(parent, neibMonData, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)

	if(is.null(notebookTab)){
		tableDisp <- DisplayQcHom(parent, neibMonData, titleTab)
		tabType[[ntab+1]] <- 'arrzc'
		tabData[[ntab+1]] <- tableDisp
		table1 <- tabData[[ntab+1]][[2]][[1]]
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(neibMonData[[1]])
				tabData[[idTabs]][[3]][[1]] <- neibMonData[[2]]
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 10)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				table1 <- tabData[[idTabs]][[2]][[1]]
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
			}else{
				tableDisp <- DisplayQcHom(parent, neibMonData, titleTab)
				tabType[[ntab+1]] <- 'arrzc'
				tabData[[ntab+1]] <- tableDisp
				table1 <- tabData[[ntab+1]][[2]][[1]]
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
			}
		}else{
			tableDisp <- DisplayQcHom(parent, neibMonData, titleTab)
			tabType[[ntab+1]] <- 'arrzc'
			tabData[[ntab+1]] <- tableDisp
			table1 <- tabData[[ntab+1]][[2]][[1]]
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
		}
	}

	.Tcl(paste(table1, 'tag', 'celltag', 'ZchkStn', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 2, sep = ',',collapse=' ')))
	tcl(table1, "tag", "configure", "ZchkStn", bg = "lightgoldenrod1")
	.Tcl(paste(table1, 'tag', 'celltag', 'idZchkStn1', paste(1,1:as.numeric(tclvalue(tkindex(table1, 'end', 'col'))), sep = ',',collapse=' ')))
	tcl(table1, "tag", "configure", "idZchkStn1", bg = "lightcyan1")
	.Tcl(paste(table1, 'tag', 'celltag', 'idZchkStn2', paste(2,1:as.numeric(tclvalue(tkindex(table1, 'end', 'col'))), sep = ',',collapse=' ')))
	tcl(table1, "tag", "configure", "idZchkStn2", bg = "lightcyan1")

	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

######################################
##Homogenization table edit output

tableHomogNotebookTab_unik <- function(parent, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)
	HomFormat <- HomOutFormat()

	if(is.null(notebookTab)){
		tableDisp <- DisplayQcHom(parent, HomFormat, titleTab)
		tabType[[ntab+1]] <- 'arrhom'
		tabData[[ntab+1]] <- tableDisp
		table1 <- tabData[[ntab+1]][[2]][[1]]
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		popupAddRemoveRow0(parent, tabData, ntab+1)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(HomFormat[[1]])
				tabData[[idTabs]][[3]][[1]] <- HomFormat[[2]]
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 15)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				table1 <- tabData[[idTabs]][[2]][[1]]
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
				popupAddRemoveRow0(parent, tabData, idTabs)
			}else{
				tableDisp <- DisplayQcHom(parent, HomFormat, titleTab)
				tabType[[ntab+1]] <- 'arrhom'
				tabData[[ntab+1]] <- tableDisp
				table1 <- tabData[[ntab+1]][[2]][[1]]
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				popupAddRemoveRow0(parent, tabData, ntab+1)
			}
		}else{
			tableDisp <- DisplayQcHom(parent, HomFormat, titleTab)
			tabType[[ntab+1]] <- 'arrhom'
			tabData[[ntab+1]] <- tableDisp
			table1 <- tabData[[ntab+1]][[2]][[1]]
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			popupAddRemoveRow0(parent, tabData, ntab+1)
		}
	}

	idp <- which(!is.na(as.character(HomFormat[[1]]$Period)))
	.Tcl(paste(table1, 'tag', 'celltag', 'KolCol1', paste(idp, 1, sep = ',',collapse=' ')))
	tcl(table1, "tag", "configure", "KolCol1", bg = "lightcyan1")
	.Tcl(paste(table1, 'tag', 'celltag', 'KolCol3', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 3, sep = ',',collapse=' ')))
	tcl(table1, "tag", "configure", "KolCol3", bg = "lightgoldenrod1")

	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

######################################
##RHtest table edit output

tableRHtestNotebookTab_unik <- function(parent, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)
	HomFormat <- RHtests_mCsFormat(ReturnExecResults)

	if(is.null(notebookTab)){
		tableDisp <- DisplayQcHom(parent, HomFormat, titleTab)
		tabType[[ntab+1]] <- 'arrRHtest'
		tabData[[ntab+1]] <- tableDisp
		table1 <- tabData[[ntab+1]][[2]][[1]]
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		popupAddRemoveRow0(parent, tabData, ntab+1)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(HomFormat[[1]])
				tabData[[idTabs]][[3]][[1]] <- HomFormat[[2]]
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 15)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				table1 <- tabData[[idTabs]][[2]][[1]]
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
				popupAddRemoveRow0(parent, tabData, idTabs)
			}else{
				tableDisp <- DisplayQcHom(parent, HomFormat, titleTab)
				tabType[[ntab+1]] <- 'arrRHtest'
				tabData[[ntab+1]] <- tableDisp
				table1 <- tabData[[ntab+1]][[2]][[1]]
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				popupAddRemoveRow0(parent, tabData, ntab+1)
			}
		}else{
			tableDisp <- DisplayQcHom(parent, HomFormat, titleTab)
			tabType[[ntab+1]] <- 'arrRHtest'
			tabData[[ntab+1]] <- tableDisp
			table1 <- tabData[[ntab+1]][[2]][[1]]
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			popupAddRemoveRow0(parent, tabData, ntab+1)
		}
	}

	.Tcl(paste(table1, 'tag', 'celltag', 'KolCol3', paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 3, sep = ',',collapse=' ')))
	tcl(table1, "tag", "configure", "KolCol3", bg = "lightgoldenrod1")

	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

######################################
##Get Tab containing console output display

consolOutNotebookTab_unik <- function(parent, Todisplay, titleTab, notebookTab, tabType, tabData, rhtests = FALSE){
	ntab <- length(tabType)
	if(is.null(notebookTab)){
		containertab <- displayConsOutputTabs(parent, Todisplay, title = titleTab, rhtests = rhtests)
		tabType[[ntab+1]] <- 'ctxt'
		tabData[[ntab+1]] <- containertab
		notebookTab <- tabData[[ntab+1]][[1]]$ID
		tkselect(parent, ntab)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				##
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[2]]))))
				displayConsOutput(tabData[[idTabs]][[2]], Todisplay, rhtests = rhtests)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				##
				notebookTab <- tabData[[idTabs]][[1]]$ID
				tkselect(parent, idTabs-1)
			}else{
				containertab <- displayConsOutputTabs(parent, Todisplay, title = titleTab, rhtests = rhtests)
				tabType[[ntab+1]] <- 'ctxt'
				tabData[[ntab+1]] <- containertab
				notebookTab <- tabData[[ntab+1]][[1]]$ID
				tkselect(parent, ntab)
			}
		}else{
			containertab <- displayConsOutputTabs(parent, Todisplay, title = titleTab, rhtests = rhtests)
			tabType[[ntab+1]] <- 'ctxt'
			tabData[[ntab+1]] <- containertab
			notebookTab <- tabData[[ntab+1]][[1]]$ID
			tkselect(parent, 0)
		}
	}
	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}



######################################
##Display data to interpolate

tableInterpNotebookTab_unik <- function(parent, donne, notebookTab, tabType, tabData){
	ntab <- length(tabType)

	dat2disp <- data.frame(id = donne$id, lon = donne$lon, lat = donne$lat, z = donne$z)
	dat2disp <- dat2disp[!is.na(dat2disp$z),]
	dat2disp <- list(donne$date, dat2disp, donne$elv)
	titleTab <- paste('Obs', donne$date, sep = '_')

	if(is.null(notebookTab)){
		tableDisp <- DisplayInterpData(parent, dat2disp, titleTab)
		tabType[[ntab+1]] <- 'arrInterp'
		tabData[[ntab+1]] <- tableDisp
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		#popupAddRemoveRow(parent)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(dat2disp)
				table1 <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 10)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)

				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
			}else{
				tableDisp <- DisplayInterpData(parent, dat2disp, titleTab)
				tabType[[ntab+1]] <- 'arrInterp'
				tabData[[ntab+1]] <- tableDisp
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				#popupAddRemoveRow(parent)
			}
		}else{
			tableDisp <- DisplayInterpData(parent, dat2disp, titleTab)
			tabType[[ntab+1]] <- 'arrInterp'
			tabData[[ntab+1]] <- tableDisp
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			#popupAddRemoveRow(parent)
		}
	}
	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}


######################################
##Display statistiques de validation

tableValidationNotebookTab_unik <- function(parent, toDispl, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)

	if(is.null(notebookTab)){
		tableDisp <- DisplayHomInfo(parent, toDispl, titleTab)
		tabType[[ntab+1]] <- 'arrValid'
		tabData[[ntab+1]] <- tableDisp
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		#popupAddRemoveRow0(parent, tabData, ntab+1)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(toDispl)
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 25)
				#table1 <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 10)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]],'-text', titleTab)
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
				#popupAddRemoveRow0(parent, tabData, idTabs)
			}else{
				tableDisp <- DisplayHomInfo(parent, toDispl, titleTab)
				tabType[[ntab+1]] <- 'arrValid'
				tabData[[ntab+1]] <- tableDisp
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				#popupAddRemoveRow0(parent, tabData, ntab+1)
			}
		}else{
			tableDisp <- DisplayHomInfo(parent, toDispl, titleTab)
			tabType[[ntab+1]] <- 'arrValid'
			tabData[[ntab+1]] <- tableDisp
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			#popupAddRemoveRow0(parent, tabData, ntab+1)
		}
	}
	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}
