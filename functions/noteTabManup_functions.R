
######################################
#Get Tab containing image display
imageNotebookTab_unik<-function(parent,imgContainer,notebookTab,tabType,tabData){
	ntab<-length(tabType)
	if(is.null(notebookTab)){
		tabType[[ntab+1]]<-'img'
		tabData[[ntab+1]]<-imgContainer
		ntbkIdTab<-tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent,ntab)
	}else{
		if(ntab>0){
			AllNoteTab<-sapply(1:ntab,function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs<-which(AllNoteTab==notebookTab[[2]])
			if(length(idTabs)>0){
				ntbkIdTab<-tabData[[idTabs]][[1]][[1]]$ID
				tabData[[idTabs]][[2]]<-imgContainer[[2]]
				tkselect(parent,idTabs-1)
			}else{
				tabType[[ntab+1]]<-'img'
				tabData[[ntab+1]]<-imgContainer
				ntbkIdTab<-tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent,ntab)
			}
		}else{
			tabType[[1]]<-'img'
			tabData[[1]]<-imgContainer
			ntbkIdTab<-tabData[[1]][[1]][[1]]$ID
			tkselect(parent,0)
		}
	}
	retTab<-list(notebookTab=list(imgContainer[[1]],ntbkIdTab),tab.type=tabType,tab.data=tabData)
	return(retTab)
}

######################################
#Open new tab if not exist

imageNotebookTab_open<-function(parent,notebookTab,tabTitle,tabType,tabData){
	if(is.null(notebookTab)){
		onglet<-addNewTab(parent,tab.title=tabTitle)
	}else{
		ntab<-length(tabType)
		if(ntab>0){
			AllNoteTab<-sapply(1:ntab,function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs<-which(AllNoteTab==notebookTab[[2]])

			if(length(idTabs)>0){
				onglet<-notebookTab[[1]]
				if(class(tabData[[idTabs]][[2]])=="tkwin"){
					if(tclvalue(tkwinfo('class',tkwinfo('children',tabData[[idTabs]][[1]][[2]])))=="Label") tkdestroy(tabData[[idTabs]][[2]])
					#new (scorllWin)
					else tcl('destroy',tkwinfo('children',tabData[[idTabs]][[1]][[2]]))
				}else{#or class(tabData[[idTabs]][[2]])=="list"
					##tabData[[idTabs]][[1]][[2]] <====onglet[[2]]
					##canvas qc, extrait data
					if(tclvalue(tkwinfo('class',tkwinfo('children',tabData[[idTabs]][[1]][[2]])))=="Canvas") tkdestroy(tabData[[idTabs]][[2]][[1]])
					#new (scorllWin)
					else tcl('destroy',tkwinfo('children',tabData[[idTabs]][[1]][[2]]))
				}

				tcl(parent,'tab',tabData[[idTabs]][[1]][[1]],'-text',tabTitle)
			}else{
				onglet<-addNewTab(parent,tab.title=tabTitle)
			}
		}else{
			onglet<-addNewTab(parent,tab.title=tabTitle)
		}
	}
	return(onglet)
}


######################################
##remove
##Number of dry day table display

#tableNddNotebookTab_unik<-function(parent,titleTab,notebookTab,tabType,tabData){
#	ntab<-length(tabType)
#	NddFormat<-QcOutNddFormat()
#
#	if(is.null(notebookTab)){
#		tableDisp<-DisplayQcHom(parent,NddFormat,titleTab)
#		tabType[[ntab+1]]<-'arrqc'
#		tabData[[ntab+1]]<-tableDisp
#		notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
#		tkselect(parent,ntab)
#	}else{
#		if(ntab>0){
#			AllNoteTab<-sapply(1:ntab,function(j){
#				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
#				else tabData[[j]][[1]][[1]]
#			})
#			idTabs<-which(AllNoteTab==notebookTab)
#			if(length(idTabs)>0){
#				.Tcl(paste('destroy',tclvalue(tkwinfo("children",tabData[[idTabs]][[1]][[2]]))))

#				dtab<-tclArrayVar(NddFormat[[1]])
#				table1<-displayTable(tabData[[idTabs]][[1]][[2]],tclArray=dtab,colwidth=10)
#				tcl(parent,'tab',tabData[[idTabs]][[1]][[1]],'-text',titleTab)
#
#				notebookTab<-tabData[[idTabs]][[1]][[1]]$ID
#				tkselect(parent,idTabs-1)
#			}else{
#				tableDisp<-DisplayQcHom(parent,NddFormat,titleTab)
#				tabType[[ntab+1]]<-'arrqc'
#				tabData[[ntab+1]]<-tableDisp
#				notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
#				tkselect(parent,ntab)
#			}
#		}else{
#			tableDisp<-DisplayQcHom(parent,NddFormat,titleTab)
#			tabType[[ntab+1]]<-'arrqc'
#			tabData[[ntab+1]]<-tableDisp
#			notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
#			tkselect(parent,ntab)
#		}
#	}
#	retTab<-list(notebookTab=notebookTab,tab.type=tabType,tab.data=tabData)
#	return(retTab)
#}


######################################
##Zeros check table display

tableZeroCheckNotebookTab_unik<-function(parent,titleTab,notebookTab,tabType,tabData){
	ntab<-length(tabType)
	ZChkFormat<-QcOutZeroChkFormat()

	if(is.null(notebookTab)){
		tableDisp<-DisplayQcHom(parent,ZChkFormat,titleTab)
		tabType[[ntab+1]]<-'arrzc'
		tabData[[ntab+1]]<-tableDisp
		notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent,ntab)
		popupAddRemoveRow0(parent,tabData,ntab+1)
	}else{
		if(ntab>0){
			AllNoteTab<-sapply(1:ntab,function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs<-which(AllNoteTab==notebookTab)
			if(length(idTabs)>0){
				.Tcl(paste('destroy',tclvalue(tkwinfo("children",tabData[[idTabs]][[1]][[2]]))))

				dtab<-tclArrayVar(ZChkFormat[[1]])
				tabData[[idTabs]][[3]][[1]]<-ZChkFormat[[2]]
				tabData[[idTabs]][[2]]<-displayTable(tabData[[idTabs]][[1]][[2]],tclArray=dtab,colwidth=10)
				tcl(parent,'tab',tabData[[idTabs]][[1]][[1]],'-text',titleTab)
				notebookTab<-tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent,idTabs-1)
				popupAddRemoveRow0(parent,tabData,idTabs)
			}else{
				tableDisp<-DisplayQcHom(parent,ZChkFormat,titleTab)
				tabType[[ntab+1]]<-'arrzc'
				tabData[[ntab+1]]<-tableDisp
				notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent,ntab)
				popupAddRemoveRow0(parent,tabData,ntab+1)
			}
		}else{
			tableDisp<-DisplayQcHom(parent,ZChkFormat,titleTab)
			tabType[[ntab+1]]<-'arrzc'
			tabData[[ntab+1]]<-tableDisp
			notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent,ntab)
			popupAddRemoveRow0(parent,tabData,ntab+1)
		}
	}
	retTab<-list(notebookTab=notebookTab,tab.type=tabType,tab.data=tabData)
	return(retTab)
}

######################################
##Get Tab containing console output display


consolOutNotebookTab_unik<-function(parent,Todisplay,titleTab,notebookTab,tabType,tabData,rhtests=FALSE){
	ntab<-length(tabType)
	if(is.null(notebookTab)){
		containertab<-displayConsOutputTabs(parent,Todisplay,title=titleTab,rhtests=rhtests)
		tabType[[ntab+1]]<-'ctxt'
		tabData[[ntab+1]]<-containertab
		notebookTab<-tabData[[ntab+1]][[1]]$ID
		tkselect(parent,ntab)
	}else{
		if(ntab>0){
			AllNoteTab<-sapply(1:ntab,function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs<-which(AllNoteTab==notebookTab)
			if(length(idTabs)>0){
				##
				.Tcl(paste('destroy',tclvalue(tkwinfo("children",tabData[[idTabs]][[2]]))))
				displayConsOutput(tabData[[idTabs]][[2]],Todisplay,rhtests=rhtests)
				tcl(parent,'tab',tabData[[idTabs]][[1]][[1]],'-text',titleTab)
				##
				notebookTab<-tabData[[idTabs]][[1]]$ID
				tkselect(parent,idTabs-1)
			}else{
				containertab<-displayConsOutputTabs(parent,Todisplay,title=titleTab,rhtests=rhtests)
				tabType[[ntab+1]]<-'ctxt'
				tabData[[ntab+1]]<-containertab
				notebookTab<-tabData[[ntab+1]][[1]]$ID
				tkselect(parent,ntab)
			}
		}else{
			containertab<-displayConsOutputTabs(parent,Todisplay,title=titleTab,rhtests=rhtests)
			tabType[[ntab+1]]<-'ctxt'
			tabData[[ntab+1]]<-containertab
			notebookTab<-tabData[[ntab+1]][[1]]$ID
			tkselect(parent,0)
		}
	}
	retTab<-list(notebookTab=notebookTab,tab.type=tabType,tab.data=tabData)
	return(retTab)
}



######################################
##Display data to interpolate

tableInterpNotebookTab_unik<-function(parent,donne,notebookTab,tabType,tabData){
	ntab<-length(tabType)

	dat2disp<-data.frame(id=donne$id,lon=donne$lon,lat=donne$lat,z=donne$z)
	dat2disp<-dat2disp[!is.na(dat2disp$z),]
	dat2disp<-list(donne$date,dat2disp,donne$elv)
	titleTab<-paste('Obs',donne$date,sep='_')

	if(is.null(notebookTab)){
		tableDisp<-DisplayInterpData(parent,dat2disp,titleTab)
		tabType[[ntab+1]]<-'arrInterp'
		tabData[[ntab+1]]<-tableDisp
		notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent,ntab)
		#popupAddRemoveRow(parent)
	}else{
		if(ntab>0){
			AllNoteTab<-sapply(1:ntab,function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs<-which(AllNoteTab==notebookTab)
			if(length(idTabs)>0){
				.Tcl(paste('destroy',tclvalue(tkwinfo("children",tabData[[idTabs]][[1]][[2]]))))

				dtab<-tclArrayVar(dat2disp)
				table1<-displayTable(tabData[[idTabs]][[1]][[2]],tclArray=dtab,colwidth=10)
				tcl(parent,'tab',tabData[[idTabs]][[1]][[1]],'-text',titleTab)

				notebookTab<-tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent,idTabs-1)
			}else{
				tableDisp<-DisplayInterpData(parent,dat2disp,titleTab)
				tabType[[ntab+1]]<-'arrInterp'
				tabData[[ntab+1]]<-tableDisp
				notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent,ntab)
				#popupAddRemoveRow(parent)
			}
		}else{
			tableDisp<-DisplayInterpData(parent,dat2disp,titleTab)
			tabType[[ntab+1]]<-'arrInterp'
			tabData[[ntab+1]]<-tableDisp
			notebookTab<-tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent,ntab)
			#popupAddRemoveRow(parent)
		}
	}
	retTab<-list(notebookTab=notebookTab,tab.type=tabType,tab.data=tabData)
	return(retTab)
}


