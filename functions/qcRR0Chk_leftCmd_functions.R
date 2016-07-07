QcZeroCheckCmd<-function(stateReplaceAll){

	cmd.frame<-tkframe(panel.left,relief='groove',bd=2)

	pnddframe<-tkframe(cmd.frame)
	tkgrid(pnddframe ,sticky='nwes')

	dispPnddTab<-tkbutton(pnddframe, text="View & Edit Result")
	replacePndd<-tkbutton(pnddframe, text="Replace false zeros")
	sep_pndd1<-ttkseparator(pnddframe)
	replaceAllPndd<-tkbutton(pnddframe, text="Replace All false zeros",state=stateReplaceAll)

	infobulle(replacePndd,'Replaces false zeros with missing values')
	status.bar.display(replacePndd,TextOutputVar,'Replaces false zeros with missing values')
	infobulle(replaceAllPndd,'Replaces all false zeros with missing values at one time')
	status.bar.display(replaceAllPndd,TextOutputVar,'Replaces all false zeros with missing values at one time')

	sep_pndd2<-ttkseparator(pnddframe)
	
	dispNeighbors<-tclVar()
	dispNbrsLab<-tklabel(pnddframe,text='Display neighbors stations',anchor='center',justify='center')
	dispNbrsPrev<-tkbutton(pnddframe, text="<<")
	dispNbrsCmb<-ttkcombobox(pnddframe, values='', textvariable=dispNeighbors)
	dispNbrsNext<-tkbutton(pnddframe, text=">>")

	infobulle(dispNbrsPrev,'Display the neighbors values on a table for the same month')
	status.bar.display(dispNbrsPrev,TextOutputVar,'Display the neighbors values on a table for the same month')
	infobulle(dispNbrsCmb,'Display the neighbors values on a table for the same month')
	status.bar.display(dispNbrsCmb,TextOutputVar,'Display the neighbors values on a table for the same month')
	infobulle(dispNbrsNext,'Display the neighbors values on a table for the same month')
	status.bar.display(dispNbrsNext,TextOutputVar,'Display the neighbors values on a table for the same month')

	tkgrid(dispPnddTab,row=0,column=0,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(replacePndd,row=0,column=4,sticky='we',rowspan=1,columnspan=4,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep_pndd1,row=1,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)
	tkgrid(replaceAllPndd,row=2,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(sep_pndd2,row=3,column=0,sticky='we',rowspan=1,columnspan=8,pady=5)

	tkgrid(dispNbrsLab,row=4,column=0,sticky='we',rowspan=1,columnspan=8,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(dispNbrsPrev,row=5,column=0,sticky='w',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(dispNbrsCmb,row=5,column=1,sticky='we',rowspan=1,columnspan=6,padx=1,pady=1,ipadx=1,ipady=1)
	tkgrid(dispNbrsNext,row=5,column=7,sticky='e',rowspan=1,columnspan=1,padx=1,pady=1,ipadx=1,ipady=1)

	###################################################
	##display pNDD table
	pNddQcIdTab<-NULL

	tkconfigure(dispPnddTab,command=function(){
		if(!is.null(ReturnExecResults)){
			if(ReturnExecResults$AllOrOne=='one'){
				IJstation<-ReturnExecResults$station
			}
			if(ReturnExecResults$AllOrOne=='all'){
				stns<-sapply(ReturnExecResults$res, function(x) x$station)
				ijstn<-which(stns==tclvalue(stn.choix.val))
				IJstation<-ReturnExecResults$res[[ijstn]]$station
			}

			if(ReturnExecResults$action=='zero.check'){
				retNBTab<-tableZeroCheckNotebookTab_unik(tknotes,paste(IJstation,'-Zero-Check'),pNddQcIdTab,AllOpenTabType,AllOpenTabData)
				pNddQcIdTab<<-retNBTab$notebookTab
				AllOpenTabType<<-retNBTab$AllOpenTabType
				AllOpenTabData<<-retNBTab$AllOpenTabData
			}else InsertMessagesTxt(main.txt.out,'NOT a zeros check outputs',format=TRUE)
		}else InsertMessagesTxt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})

	###################################################
	##replace
	tkconfigure(replacePndd,command=function(){
		if(!is.null(ReturnExecResults)){
			if(ReturnExecResults$AllOrOne=='one'){
				IJstation<-ReturnExecResults$station
			}
			if(ReturnExecResults$AllOrOne=='all'){
				stns<-sapply(ReturnExecResults$res, function(x) x$station)
				ijstn<-which(stns==tclvalue(stn.choix.val))
				IJstation<-ReturnExecResults$res[[ijstn]]$station
			}
			repret<-try(replaceZeroChkbyNA(IJstation,ReturnExecResults),silent=TRUE)
			if(!inherits(repret, "try-error")){
				if(!is.null(repret)){
					if(repret==0) InsertMessagesTxt(main.txt.out,paste("Zeros replacement finished successfully for", IJstation))
					#else InsertMessagesTxt(main.txt.out,"Zeros replacement failed",format=TRUE)
				}else  InsertMessagesTxt(main.txt.out,"Zeros replacement failed",format=TRUE)
			}else{
				InsertMessagesTxt(main.txt.out,"Zeros replacement failed",format=TRUE)
				InsertMessagesTxt(main.txt.out,gsub('[\r\n]','',repret[1]),format=TRUE)
			}
		}else InsertMessagesTxt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})


	###################################################
	##replace all
	tkconfigure(replaceAllPndd,command=function(){
		if(!is.null(ReturnExecResults)){
			stns<-sapply(ReturnExecResults$res, function(x) x$station)
			tkconfigure(main.win,cursor='watch');tcl("update","idletasks")
			lapply(stns,function(IJstation){
				repret<-try(replaceZeroChkbyNA(IJstation,ReturnExecResults),silent=TRUE)
				if(!inherits(repret, "try-error")){
					if(!is.null(repret)){
						if(repret==0) InsertMessagesTxt(main.txt.out,paste("Zeros replacement finished successfully for", IJstation))
						#else InsertMessagesTxt(main.txt.out,"Zeros replacement failed",format=TRUE)
					}else  InsertMessagesTxt(main.txt.out,"Zeros replacement failed",format=TRUE)
				}else{
					InsertMessagesTxt(main.txt.out,"Zeros replacement failed",format=TRUE)
					InsertMessagesTxt(main.txt.out,gsub('[\r\n]','',repret[1]),format=TRUE)
				}
				tcl("update")
			})
			tkconfigure(main.win,cursor='')
		}else InsertMessagesTxt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})

	###################################################
	neighborsIdTab<-NULL
	actualSTN<-'INITSTN'

	getZeroChkMonths<-function(){
		if(ReturnExecResults$AllOrOne=='one'){
			IJstation<-ReturnExecResults$station
			YYYYMM<-ReturnExecResults$res$YYYYMM
		}
		if(ReturnExecResults$AllOrOne=='all'){
			stns<-sapply(ReturnExecResults$res, function(x) x$station)
			ijstn<-which(stns==tclvalue(stn.choix.val))
			IJstation<-ReturnExecResults$res[[ijstn]]$station
			YYYYMM<-ReturnExecResults$res[[ijstn]]$res$YYYYMM
		}
		YYYYMM<-YYYYMM[!is.na(YYYYMM)]
		return(list(IJstation,YYYYMM))
	}

	###################
	tkconfigure(dispNbrsNext,command=function(){
		if(!is.null(ReturnExecResults) & !is.null(GeneralParameters)){
			if(ReturnExecResults$action=='zero.check'){
				retMon<-getZeroChkMonths()
				IJstation<-retMon[[1]]
				YYYYMM<-retMon[[2]]
				if(length(YYYYMM)>0){
					if(IJstation!=actualSTN){
						tkconfigure(dispNbrsCmb,values=YYYYMM)
						tclvalue(dispNeighbors)<-YYYYMM[1]
						actualSTN<<-IJstation
					}
					ijsp<-as.numeric(tclvalue(tcl(dispNbrsCmb,"current")))+1
			   		ijsp<-ijsp+1
			 		if(ijsp>length(YYYYMM)) ijsp<-1
					tclvalue(dispNeighbors)<-YYYYMM[ijsp]
					neibMonData<-QcOutZeroChk_Neighbors(IJstation,YYYYMM[ijsp])
					neibMonData<-list(neibMonData,NA)

					retNBTab<-tableZeroCheckNotebookTab_unik1(tknotes,neibMonData,paste(IJstation,'-Neighbors'),neighborsIdTab,AllOpenTabType,AllOpenTabData)
					neighborsIdTab<<-retNBTab$notebookTab
					AllOpenTabType<<-retNBTab$AllOpenTabType
					AllOpenTabData<<-retNBTab$AllOpenTabData
				}else{
					tkconfigure(dispNbrsCmb,values='')
					tclvalue(dispNeighbors)<-''
					actualSTN<<-'INITSTN'
					InsertMessagesTxt(main.txt.out,'No suspicious zeros found')
				}
			}else InsertMessagesTxt(main.txt.out,'NOT a zeros check outputs',format=TRUE)
		}else InsertMessagesTxt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})

	###################
	tkconfigure(dispNbrsPrev,command=function(){
		if(!is.null(ReturnExecResults) & !is.null(GeneralParameters)){
			if(ReturnExecResults$action=='zero.check'){
				retMon<-getZeroChkMonths()
				IJstation<-retMon[[1]]
				YYYYMM<-retMon[[2]]
				if(length(YYYYMM)>0){
					if(IJstation!=actualSTN){
						tkconfigure(dispNbrsCmb,values=YYYYMM)
						tclvalue(dispNeighbors)<-YYYYMM[1]
						actualSTN<<-IJstation
					}
					ijsp<-as.numeric(tclvalue(tcl(dispNbrsCmb,"current")))+1
			   		ijsp<-ijsp-1
			 		if(ijsp<1) ijsp<-length(YYYYMM)
					tclvalue(dispNeighbors)<-YYYYMM[ijsp]
					neibMonData<-QcOutZeroChk_Neighbors(IJstation,YYYYMM[ijsp])
					neibMonData<-list(neibMonData,NA)

					retNBTab<-tableZeroCheckNotebookTab_unik1(tknotes,neibMonData,paste(IJstation,'-Neighbors'),neighborsIdTab,AllOpenTabType,AllOpenTabData)
					neighborsIdTab<<-retNBTab$notebookTab
					AllOpenTabType<<-retNBTab$AllOpenTabType
					AllOpenTabData<<-retNBTab$AllOpenTabData
				}else{
					tkconfigure(dispNbrsCmb,values='')
					tclvalue(dispNeighbors)<-''
					actualSTN<<-'INITSTN'
					InsertMessagesTxt(main.txt.out,'No suspicious zeros found')
				}
			}else InsertMessagesTxt(main.txt.out,'NOT a zeros check outputs',format=TRUE)
		}else InsertMessagesTxt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})
	
	###################
	tkbind(dispNbrsCmb,"<<ComboboxSelected>>",function(){
		if(!is.null(ReturnExecResults) & !is.null(GeneralParameters)){
			if(ReturnExecResults$action=='zero.check'){
				retMon<-getZeroChkMonths()
				IJstation<-retMon[[1]]
				YYYYMM<-retMon[[2]]
				if(length(YYYYMM)>0){
					ijsp<-as.numeric(tclvalue(tcl(dispNbrsCmb,"current")))+1
					neibMonData<-QcOutZeroChk_Neighbors(IJstation,YYYYMM[ijsp])
					neibMonData<-list(neibMonData,NA)

					retNBTab<-tableZeroCheckNotebookTab_unik1(tknotes,neibMonData,paste(IJstation,'-Neighbors'),neighborsIdTab,AllOpenTabType,AllOpenTabData)
					neighborsIdTab<<-retNBTab$notebookTab
					AllOpenTabType<<-retNBTab$AllOpenTabType
					AllOpenTabData<<-retNBTab$AllOpenTabData
				}else{
					tkconfigure(dispNbrsCmb,values='')
					tclvalue(dispNeighbors)<-''
					actualSTN<<-'INITSTN'
					InsertMessagesTxt(main.txt.out,'No suspicious zeros found')
				}
			}else InsertMessagesTxt(main.txt.out,'NOT a zeros check outputs',format=TRUE)
		}else InsertMessagesTxt(main.txt.out,'There is no zeros check outputs',format=TRUE)
	})

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame,sticky='nswe',pady=5)
	######
	return(cmd.frame)
}


